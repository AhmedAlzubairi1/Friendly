#!/bin/sh
ocamlbuild -pkgs llvm microc.native
./microc.native -l newTest.mc > newTest.out
lli newTest.out
