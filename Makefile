build:
	ocamlbuild witch.byte -libs unix

native:
	ocamlbuild witch.native -libs unix

clean:
	rm -rf _build witch.native witch.byte
