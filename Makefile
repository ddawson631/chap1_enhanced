#
# Makefile for enhanced chapter 1 interpeter. 
# The chapter 1 interpreter from Samuel Kamin's Programming Languages texbook
# was enhanced to use a more Pascal-like syntax instead of Lisp-style syntax.
#
SHELL = /bin/bash

#
# The fpc compile options used below have the following meanings.
#
#  -Mmacpas        # Mac Pascal Dialect
#  -Sg             # Allow goto
#  -gl             # Debugger support plus line info for backtraces
#  -vewnhibq       # Show Errors, Warnings, Notes, Hints, Info,
#                  # Full path in file names, message numbers
#
# The chap1 target below depends directly on the .pas source 
# because fpc issues errors if I try to directly compile the object file.
#
chap1: chap1.pas
	fpc chap1.pas -Mmacpas -Sg -vewnhibq

debug: chap1.pas
	fpc chap1.pas -dDEBUG -Mmacpas -Sg -gl -vewnhibq

clean:
	rm -f chap1.o

