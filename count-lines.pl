#!/usr/bin/perl
#
#

use warnings;
use strict;

my @filenames = qw/equations-equations.lisp
                   functions-and-relations-homomorphisms.lisp
                   fundamental-functions-basic-functions.lisp
                   fundamental-functions-functions.lisp
                   fundamental-functions-relations.lisp
                   fundamental-functions-tables.lisp
                   subalgebras-subalgebras.lisp
                   technicals-math-like-notation.lisp
                   technicals-sets.lisp
                   technicals-technicals.lisp
                   technicals-test-cases.lisp
                   terms-terms.lisp
                   test-universal-algebra.lisp
                   uacalc-interface-free-algebra.lisp
                   uacalc-interface-io.lisp
                   uacalc-interface.lisp
                   uacalc-interface-uab-standard.lisp
                   universal-algebra-algebras.lisp
                   universal-algebra-basic-algebras.lisp
                   universal-algebra-signatures.lisp/;

my $cmd_string = "wc -l " . join(" ", @filenames);
print qx/$cmd_string/;
