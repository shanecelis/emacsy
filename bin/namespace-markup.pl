#!/usr/bin/perl -w
# namespace-markup.pl

use strict;
use Getopt::Long;

my %options = ();
my %namespaces = ();
my $help;
GetOptions ("namespace|n=s" => \%namespaces,
            "help" => \$help);


if (@ARGV == 0 || $help) {
    print STDERR "usage: namespace-markup [-n basename=namespace] [-n ...] file ...\n";
    print STDERR << 'END';

Creates a namespace in noweb files such that for a file named, say,
'A.nw' all references inside it like 'Function' will be re-labeled
'A:Function'.  However, say I have an appendix file 'A-appendix.nw'
that I want to still be in the same namespace so that when it
references 'Function' it will reference 'A:Function' and not
'A-appendix:Function'.  I can manually specify the namespace through
the arguments of this command that the basename of the filename
'A-appendix' is actually in the 'A' namespace by using the following
option -n 'A-appendix=A'.

END
    exit(2);
}
my $file;
open(INPUT, "/opt/local/libexec/noweb/markup " . join(' ', @ARGV) . "|") or die("noweb/markup failed.\n");
while (<INPUT>) {
    if (/^\@file ([^\.:]+)[^:]*$/) { 
        $file = $1; 
        if (! (defined $namespaces{$file})) { 
            $namespaces{$file} = $file; 
        } 
    } 
    s/^\@(defn|use) ([^+][^:]*)$/\@$1 $namespaces{$file}:$2/;
    print;
}

