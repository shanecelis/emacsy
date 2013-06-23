#!/bin/bash
# print-install.sh.in
set -e; # abort if anything fails
builddir="/Users/shane/School/uvm/CSYS-395-evolutionary-robotics/noweb-eracs/ctrnn"
cd $builddir
make dist
dirname=minimal-cognition-0.01
filename=$dirname.tar.gz
tar xfz $filename
cd $dirname
./configure --prefix="$builddir/print-install"

# Make sure it can be built without noweb and cleaned without removing
# anything important.
make
make install
cd $builddir
find "print-install"
#rm -rf "$builddir/test-install"
