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
