# chap1_enhanced

## Introduction
This repo builds and tests my enhanced version of the chapter 1 interpreter presented in Samuel Kamin's book, [Programming Languages: An Interpreter-Based Approach](https://www.amazon.com/Programming-Languages-Samuel-N-Kamin/dp/0201068249/ref=sr_1_1). The original grammar, design and source code are described in detail at [chap1_orig](https://github.com/ddawson631/chap1_orig). 

My enhancements replaced the original Lisp-style syntax with a Pascal-like syntax and added new commands (load & sload) to read the user's input from a file. The overall design of the enhanced interpreter is the same as the original with the same builtin control and value operations. All that has changed is the input syntax and the added feature of reading input from a file.

For a good review of the overall design, please read Section 1.2, entitled *The Structure of the Interpreter*, in the following chapter that Professor Kamin kindly provided from his textbook.

- __[Chapter 1](docs/chapter1.pdf)__ - original grammar, syntax, design & documentation

It describes the data structures and functions used in the original source code which it divides into the following 8 sections: DECLARATIONS, DATA STRUCTURE OPS, NAME MANAGEMENT, INPUT, ENVIRONMENTS, NUMBERS, EVALUATION, MAIN. 

In order to implement the new syntax & commands, I updated the DECLARATIONS, NAME MANAGEMENT, INPUT & MAIN sections and also added a new section entitled NEW PARSING ROUTINES. See the following document for a side by side comparison of the old vs. new grammar & syntax and a detailed description of the program changes.

- __[Comparison](docs/comparison.pdf)__ - new vs. old grammar/syntax and related program changes. **NOTE:** Table 3 in this document lists some old & new code side by side in 2 columns. About halfway down column 1, the old code lines get out of synch with column 2 by being shifted down one line. This occurred when converting my original RTF to PDF because github does not render RTFs. The misalignment begins at the line `parseNL := mkNamelist(nm, nl)` in column 1. If you want to see perfect alignment then download the RTF (`comparison.rtf`) from the docs folder and open it in a word processor like LibreOffice Writer.

## Interactive Run

Below is an interactive test run of the example commands discussed in section 1.1.3 of the above chapter 1 document. The last two examples define and run a non-recursive gcd function and a recursive one.

```console
~/pascal/proglang/chap1$ ./chap1
-> 3$
3

-> 4+7$
11

-> x:=4$
4

-> x+x$
8

-> print x$
4
4

-> y:=5$
5

-> seq print x; print y; x*y qes$
4
5
20

-> if y>0 then 5 else 10 fi$
5

-> while y>0 do
>    seq x:=x+x; y:=y-1 qes
>  od$
0

-> x$
128

-> fun #1 (x) := x + 1 nuf$
#1
-> #1(4)$
5

-> fun double(x):=x+x nuf$
double
-> double(4)$
8

-> x$
128

-> fun setx(x,y):= seq x:=x+y; x qes nuf$
setx
-> setx(x,1)$
129

-> x$
128

-> fun not(boolval):= if boolval then 0 else 1 fi nuf$
not
-> fun ## (x,y):= not(x=y) nuf$
##
-> fun mod(m,n):=m-n*(m/n)nuf$
mod
-> fun gcd(m,n):=
>   seq
>    r:=mod(m,n);
>    while ##(r,0) do
>     seq
>      m:=n;
>      n:=r;
>      r:=mod(m,n)
>     qes
>    od;
>    n
>   qes
>  nuf$
gcd
-> gcd(6,15)$
3

-> fun gcd(m,n):=
>    if n=0 then m else gcd(n,mod(m,n)) fi nuf$
gcd
-> gcd(6,15)$
3

-> quit$
~/pascal/proglang/chap1$
```

## Unit Test Strategy

The unit test (UT) script is `chap1_ut.sh`.
The interpreter reads its input from `chap1_ut.input` and the results are saved
to `chap1_ut.rs1` which is then compared to `chap1_ut.rs0`.

The extensions `.rs1` & `.rs0` stand for "result 1" & "result 0", respectively.\
The `.rs1` file contains the current result from most recent UT run.\
The `.rs0` file contains the last saved, validated result from a previous UT run. 

When the current and previous results are compared (via diff command), the only
differences should be due to new or modified test cases. Unexpected differences
must be investigated and corrected if necessary. Once all differences are 
confirmed to be valid then the `.rs1` file is copied to the `.rs0` file which is 
then saved as the latest valid result.

Below is an example UT run followed by a listing of its `.rs1` file. Since it 
reports that the `.rs1` and `.rs0` files are identical, there is no need to
copy the `.rs1` to `.rs0` in this case.

## UT Run

```console
~/pascal/proglang/chap1$ touch chap1.pas
~/pascal/proglang/chap1$ ./chap1_ut.sh
making chap1
fpc chap1.pas -Mmacpas -Sg -vewnhibq
Hint: (11030) Start of reading config file /home/dawsond/.fpc.cfg
Hint: (11031) End of reading config file /home/dawsond/.fpc.cfg
(1002) Target OS: Linux for x86-64
(3104) Compiling chap1.pas
/home/dawsond/pascal/proglang/chap1/chap1.pas(765,48) Warning: (4045) Comparison might be always true due to range of constant and expression
(9015) Linking chap1
/usr/bin/ld.bfd: warning: link.res contains output sections; did you forget -T?
(1008) 1537 lines compiled, 0.5 sec
(1021) 1 warning(s) issued
(1022) 2 hint(s) issued

Running chap1 Test Cases - Saving results to chap1_ut.rs1

Files chap1_ut.rs1 and chap1_ut.rs0 are identical
```

## UT Result

The UT script passes the new load command to the interpreter as a Here String via
the following command.

./chap1 <<< ")load chap1_ut.input"

The load command tells the interpreter to read its input from `chap1_ut.input`.
It echoes each command as it is read then executes it.

Below is a listing of the current result file. It repeats the examples from the above
interactive run then performs the others described below. The comments are from the 
input file. 

```console
~/pascal/proglang/chap1$ cat chap1_ut.rs1
./chap1 <<< ")load chap1_ut.input"
->
Current Directory is : /home/dawsond/pascal/proglang/chap1
 Loading file : chap1_ut.input

!Redo tests from section 1.1.3 of Kamin's text using the Pascal-style syntax

3$
3

4+7$
11

x:=4$
4

x+x$
8

print x$
4
4

y:=5$
5

seq print x; print y; x*y qes$
4
5
20

if y>0 then 5 else 10 fi$
5

while y>0 do
  seq x:=x+x; y:=y-1 qes
od$
0

x$
128

fun #1 (x) := x + 1 nuf$
#1

#1(4)$
5

fun double(x):=x+x nuf$
double

double(4)$
8

x$
128

fun setx(x,y):= seq x:=x+y; x qes nuf$
setx

setx(x,1)$
129

x$
128

fun not(boolval):= if boolval then 0 else 1 fi nuf$
not

fun ## (x,y):= not(x=y) nuf$
##

fun mod(m,n):=m-n*(m/n)nuf$
mod

fun gcd(m,n):=
 seq
  r:=mod(m,n);
  while ##(r,0) do
   seq
    m:=n;
    n:=r;
    r:=mod(m,n)
   qes
  od;
  n
 qes
nuf$
gcd

gcd(6,15)$
3

fun gcd(m,n):=
  if n=0 then m else gcd(n,mod(m,n)) fi nuf$
gcd

gcd(6,15)$
3

!Normal precedence and associativity are implemented.
5*3+7$
22

5+3*7$
26

14-7-3$
4

48/12/2$
2

!Keywords cannot be redefined
fun if (x) := x+5 nuf$
mutate: found if  where nameid or funid is expected.

if := 20$

Error parsing expr.  Found := where one of the following is expected:
"if", "while", "seq", "print", nameid, funid, number, or "("

!Names may contain any char that is not a delimiter and must
!not contain only digits.
!Delimiters = ' ','(',')','+','-','*','/',':','=','<','>',';',',','$','!'
~12#ab:=25$
25

~12#ab$
25

x:=15-~12#ab+7$
-3

!A string of digits is not a valid name.
fun 222  (x) := x+222 nuf$
mutate: found 222  where nameid or funid is expected.

!Inserting a non-delimiter char into a string of digits makes a valid name.
fun 222# (x) := x+222 nuf$
222#

222#(3)$
225

x:=100-222#(3)-50$
-175

!Inserting a delimiter in a name causes erroneous results.
a(b:=25$
Undefined variable: a

!Function name may not be reused as a variable name.
fun inc10 (x) := x+10 nuf$
inc10

inc10:=25$
Error in match. Found :=  where ( is expected.

!Multiplie assignment
i:=j:=k:=25$
25

i$
25

j$
25

k$
25

quit$
~/pascal/proglang/chap1$
```

Below are listings of the UT script, UT input file and Makefile.

## UT Script

```sh
~/pascal/proglang/chap1$ cat chap1_ut.sh
#!/bin/bash
#
# Unit Test script for chap1 program
# See Backup Strategy and Log of Example Run at end of script
#

#
# Make the exe then run the unit test cases
#

name=chap1
echo "making $name"
make

#
# Run test cases, save results, diff with previous result.
# Redirection of stdout, stderr to result file follows closing brace below
#

echo -e "\nRunning $name Test Cases - Saving results to ${name}_ut.rs1\n"

{

echo -e "./$name <<< \")load ${name}_ut.input\""

./$name <<< ")load ${name}_ut.input"

} > ${name}_ut.rs1 2>&1

#
# Diff Results
# Below we compare current result (.rs1) with previous good result (.rs0).
#

diff -qs ${name}_ut.rs1 ${name}_ut.rs0

#
# Backup Strategy
# If diffs are valid then copy .rs1 to .rs0 to save it as latest good result.
# Backup Makefile, source, ut script and result (.rs0) together.
#
#
# Log of running this script
#
# ~$ touch chap1.pas
# ~$ ./chap1_ut.sh
# making chap1
# fpc chap1.pas
# Hint: (11031) End of reading config file /home/dawsond/.fpc.cfg
# Free Pascal Compiler version 3.0.4+dfsg-23 [2019/11/25] for x86_64
# Copyright (c) 1993-2017 by Florian Klaempfl and others
# (1002) Target OS: Linux for x86-64
# (3104) Compiling chap1.pas
# /home/dawsond/pascal/proglang/chap1/chap1.pas(755,48) Warning: (4045) Comparison might be always true due to range of constant and expression
# (9015) Linking chap1
# /usr/bin/ld.bfd: warning: link.res contains output sections; did you forget -T?
# (1008) 1529 lines compiled, 0.2 sec
# (1021) 1 warning(s) issued
# (1022) 1 hint(s) issued
# Running chap1 Test Cases - Saving results to chap1_ut.rs1
# Files chap1_ut.rs1 and chap1_ut.rs0 are identical
#
```

## UT Input

```
~/pascal/proglang/chap1$ cat chap1_ut.input
!Redo tests from section 1.1.3 of Kamin's text using the Pascal-style syntax

3$
4+7$
x:=4$
x+x$
print x$
y:=5$
seq print x; print y; x*y qes$
if y>0 then 5 else 10 fi$
while y>0 do
  seq x:=x+x; y:=y-1 qes
od$
x$
fun #1 (x) := x + 1 nuf$
#1(4)$
fun double(x):=x+x nuf$
double(4)$
x$
fun setx(x,y):= seq x:=x+y; x qes nuf$
setx(x,1)$
x$
fun not(boolval):= if boolval then 0 else 1 fi nuf$
fun ## (x,y):= not(x=y) nuf$
fun mod(m,n):=m-n*(m/n)nuf$
fun gcd(m,n):=
 seq
  r:=mod(m,n);
  while ##(r,0) do
   seq
    m:=n;
    n:=r;
    r:=mod(m,n)
   qes
  od;
  n
 qes
nuf$
gcd(6,15)$

fun gcd(m,n):=
  if n=0 then m else gcd(n,mod(m,n)) fi nuf$
gcd(6,15)$

!Normal precedence and associativity are implemented.
5*3+7$
5+3*7$
14-7-3$
48/12/2$

!Keywords cannot be redefined
fun if (x) := x+5 nuf$
if := 20$

!Names may contain any char that is not a delimiter and must
!not contain only digits.
!Delimiters = ' ','(',')','+','-','*','/',':','=','<','>',';',',','$','!'
~12#ab:=25$
~12#ab$
x:=15-~12#ab+7$

!A string of digits is not a valid name.
fun 222  (x) := x+222 nuf$

!Inserting a non-delimiter char into a string of digits makes a valid name.
fun 222# (x) := x+222 nuf$
222#(3)$
x:=100-222#(3)-50$

!Inserting a delimiter in a name causes erroneous results.
a(b:=25$

!Function name may not be reused as a variable name.
fun inc10 (x) := x+10 nuf$
inc10:=25$

!Multiplie assignment
i:=j:=k:=25$
i$
j$
k$

quit$
```

## Makefile

```sh
~/pascal/proglang/chap1$ cat Makefile
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
```
