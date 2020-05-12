#!/bin/sh
echo $1
ocamlbuild -pkgs llvm microc.native
./microc.native -l $1.fr > $1.out
lli $1.out
