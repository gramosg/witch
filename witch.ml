open Sys
open Unix

module Ms = Map.Make(String)
module Mc = Map.Make(Char)

type 'a or_error = Ok of 'a | Error of string

let identity x = x

let try_finally body cleanup =
  let res = try body ()
            with exn -> cleanup (); raise exn in
  cleanup ();
  res

let exn_to_opt body =
  try Some (body ())
  with exn -> None

let exec cmd = execvp cmd.(0) cmd

let launch cmd =
  if fork () = 0
  then exec cmd

let print_usage () =
  List.iter print_endline ["Usage: " ^ Sys.argv.(0) ^ " <title> <console>\n"
                           ^ "  Consoles: [gb gba]"];
  exit 0

let die msg =
  print_endline ("ERROR! (" ^ msg ^ ")"); exit (-1)

let get_or_die = function
  | Ok x -> x
  | Error msg -> die msg

let launch_get_output cmd  =
  let input_lines ic =
    let rec aux lines =
      try aux (input_line ic :: lines) with End_of_file -> lines in
    aux [] in
  let (ifd,ofd) = pipe () in
  match fork () with
  | 0 -> dup2 ofd stdout; dup2 ofd stderr;
         close stdin; close ifd; close ofd;
         exec cmd
  | n -> close ofd;
         let lns = input_lines (in_channel_of_descr ifd) in
         close ifd; ignore (waitpid [] n);
         lns

module Net = struct
  let tcp_server s_handler addr =
    let setup_tcp_socket addr =
      let s = socket PF_INET SOCK_STREAM 0 in
      try bind s addr;
          listen s 10;
          s
      with z -> close s; raise z in
    ignore (signal sigpipe Signal_ignore);
    let ss = setup_tcp_socket addr in
    while true do
      let client = accept ss in
      s_handler ss client
    done

  let double_fork server service (cfd, _ as client) =
    let treat () = match fork () with
      | 0 -> if fork () <> 0 then exit 0;
             close server; service client; exit 0
      | n -> ignore (waitpid [] n) in
    try_finally treat (fun () ->  close cfd)

  let s_handler c_handler sock (_, caddr as client) =
    print_endline
      (match caddr with
       | ADDR_INET (a,_) -> "Connected to " ^ string_of_inet_addr a
       | ADDR_UNIX _     -> "Local connection (dafuq?)");
    double_fork sock c_handler client
end

module type WM = sig
    type wid

    val pp_wid : wid -> string

    val search_by_title : string -> wid list
    val title : wid -> string
    val focus : wid -> unit
    val send : string -> unit
end
module Xdotool : WM = struct
  type wid = string

  let cmdname = "xdotool"
  let delay = "50"

  let pp_wid = identity

  let search_by_title name =
    launch_get_output [|cmdname; "search"; "--name"; name|]
  let title wid =
    List.hd (launch_get_output [|cmdname; "getwindowname"; wid|])
  let focus wid =
    launch [|cmdname; "windowfocus"; wid|]
  let send keys =
    launch [|cmdname; "type"; "--delay"; delay; keys|]
end

module Console : sig
  type t = Gb | Gba
  type key

  val of_string : string -> t or_error
  val pp : t -> string
  val keys : t -> key list
end = struct
  type t = Gb | Gba
  type key =
    | Kup | Kdown | Kleft | Kright
    | Ka | Kb | Kl | Kr
    | Kstart | Kselect

  let of_string c = match c with
    | "gb"  -> Ok Gb
    | "gba" -> Ok Gba
    | _     -> Error ("console " ^ c ^ " not supported")
  let pp = function
    | Gb  -> "gb (Game Boy)"
    | Gba -> "gba (Game Boy Advanced)"
  let keys = function
    | Gb  -> [Kup;Kdown;Kleft;Kright;Ka;Kb;Kstart]
    | Gba -> [Kup;Kdown;Kleft;Kright;Ka;Kb;Kl;Kr;Kstart]
end

module Comm (Wm : WM) = struct
  type mode = Cmd | Key
  let pp_mode = function
    | Cmd -> "cmd"
    | Key -> "key"

  type state = {
    wid  : Wm.wid;
    mode : mode;
    (* bds : char Bindings.t; *)
    console : Console.t
  }
  let mk_state wid mode (* bds *) console =
    { wid     = wid;
      mode    = mode;
      (* bds     = bds; *)
      console = console }
  let switch_mode st =
    let mode' = match st.mode with Cmd -> Key | Key -> Cmd in
    print_endline ("Mode switched to " ^ pp_mode mode');
    { st with mode = mode' }

  let cmdmap =
    let press k st = Wm.focus st.wid; Wm.send k; st in
    let default_cmds =
      [("up","w"); ("down","s"); ("left","a"); ("right","d");
       ("a","u"); ("b","h"); ("l","i"); ("r","o"); ("start","j")] in
    let keys = List.fold_left (fun m (k,v) -> Ms.add k (press v) m)
                              Ms.empty default_cmds in
    let cmdbds = [("switch", switch_mode)] in
    let cmds = List.fold_left (fun m (k,f) -> Ms.add k f m)
                              Ms.empty cmdbds in
    Ms.fold Ms.add keys cmds

  let find_window name =
    let wids = Wm.search_by_title name in
    let falses = [Sys.argv.(0); "mednafen"] in (* May have the same title *)
    let falsewids = List.concat (List.map Wm.search_by_title falses) in
    match List.filter (fun wid -> not (List.mem wid falsewids)) wids with
    | []  -> Error "window not found"
    | [x] -> Ok (x, Wm.title x)
    | _   -> Error "multiple windows found ()" (* TODO print titles found *)

  let c_handler st0 (cfd, _) =
    dup2 cfd stdin; dup2 cfd stdout; dup2 cfd stderr; close cfd;
    let rec loop st =
      match st.mode with
      | Cmd ->
         print_string "> ";
         flush_all ();
         let cmd = read_line () in
         let f = match exn_to_opt (fun () -> Ms.find cmd cmdmap) with
           | None -> print_endline ("Unknown cmd '" ^ cmd ^ "'"); identity
           | Some f -> f in
         loop (f st)
      | Key ->
         print_string "> "; in
    loop st0
end

module MyWm = Xdotool
module MyComm = Comm(MyWm)

let () =
  if Array.length Sys.argv <> 3 then print_usage ();

  let addr = inet_addr_any in
  let port = 8080 in
  let inaddr = ADDR_INET (addr, port) in
  let title = Sys.argv.(1) in

  let console = get_or_die (Console.of_string Sys.argv.(2)) in
  print_endline ("Using " ^ Console.pp console ^ " settings");

  print_string ("Searching window with title '" ^ title ^ "'... ");
  let (wid, title) = get_or_die (MyComm.find_window title) in
  print_endline ("found '" ^ title ^ "' (WID " ^ MyWm.pp_wid wid ^ ")");

  let st0 = MyComm.mk_state wid MyComm.Cmd console in
  print_endline ("Starting server in port " ^ string_of_int port ^ "...");
  Net.tcp_server (Net.s_handler (MyComm.c_handler st0)) inaddr
