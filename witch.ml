open Sys
open Unix

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
  List.iter print_endline ["Usage: " ^ Sys.argv.(0) ^ " <title>"];
  exit 0

let die reason =
  print_endline ("ERROR! " ^ reason); exit (-1)

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

module type XWIN = sig
    type wid
    val cmdname : string

    val search_by_title : string -> wid list
    val title : wid -> string
    val focus : wid -> unit
    val send : wid -> string -> unit
end
module Xdotool : (XWIN with type wid := string) = struct
  let cmdname = "xdotool"
  let delay = "200"

  let search_by_title name =
    launch_get_output [|cmdname; "search"; "--name"; name|]
  let title wid =
    List.hd (launch_get_output [|cmdname; "getwindowname"; wid|])
  let focus wid =
    launch [|cmdname; "windowfocus"; wid|]
  let send wid keys =
    launch [|cmdname; "type"; "--delay"; delay; keys|]
end

module Comm = struct
  module Window = Xdotool
  module Ms = Map.Make(String)

  let cmds = [("start","m");
              ("up","w"); ("down","s"); ("left","a"); ("right","d");
              ("a","k"); ("b","j");
              ("l","h"); ("r","l")]
  let cmds = List.fold_left (fun m (k,v) -> Ms.add k v m) Ms.empty cmds

  let get_wid name =
    let wids = Window.search_by_title name in
    let falses = [Sys.argv.(0); "mednafen"] in (* May have the same title *)
    let falsewids = List.concat (List.map Window.search_by_title falses) in
    match List.filter (fun wid -> not (List.mem wid falsewids)) wids with
    | [x] -> Some (x, Window.title x)
    | _   -> None

  let c_handler wid (cfd, _) =
    dup2 cfd stdin; dup2 cfd stdout; dup2 cfd stderr; close cfd;
    Window.focus wid;
    while true do
      print_string "> ";
      flush_all ();
      let cmd = read_line () in
      match exn_to_opt (fun () -> Ms.find cmd cmds) with
      | None -> print_endline ("Unknown cmd '" ^ cmd ^ "'")
      | Some key -> print_endline ("Sending cmd '" ^ cmd ^ "' (" ^ key ^ ")...");
                    Window.send wid key
    done

    (* Window.send wid "awdwawdwawdwawdwa" *)
    (* Window.send wid "aaaaaaaaaaaaaaaaa" *)
end

let () =
  if Array.length Sys.argv <> 2 then print_usage ();

  let addr = inet_addr_any in
  let port = 8080 in
  let inaddr = ADDR_INET (addr, port) in
  let name = Sys.argv.(1) in

  match Comm.get_wid name with
  | None              -> die ("Window '" ^ name ^ "' not found")
  | Some (wid, title) ->
     print_endline ("Found window '" ^ title ^ "' (WID " ^ wid ^ ")");
     Net.tcp_server (Net.s_handler (Comm.c_handler wid)) inaddr
