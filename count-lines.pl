#!/usr/bin/perl
#
#

use warnings;
use strict;

my @filenames = qw/algebras.lisp
                   basic-algebras.lisp
                   basic-functions.lisp
                   equations.lisp
                   functions-and-relations.lisp
                   functions.lisp
                   math-like-notation.lisp
                   relations.lisp
                   sets.lisp
                   signatures.lisp
                   subalgebras.lisp
                   tables.lisp
                   technicals.lisp
                   terms.lisp
                   test-cases.lisp
                   test-universal-algebra.lisp
                   uacalc-interface.lisp
                   uacalc-technicals.lisp/;

my $cmd_string = "wc -l " . join(" ", @filenames);
print qx/$cmd_string/;
