#!/bin/sh
echo $1
ocamlbuild -pkgs llvm friendly.native
./friendly.native -l $1.fr > $1.out
