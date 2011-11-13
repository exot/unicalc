UniCalc -- Universal Algebra Calculator
=======================================

A Universal Algebra Calculator, wrapping uacalc and written in Common Lisp.


Installation
------------

Just start up your favourite Common Lisp implementation and write

    (asdf:load-system :unicalc)
 
(provided that you have ASDF2 installed, which I really recommend).  Then

    (use-package 'unicalc)
    
will give you everything UniCalc has to offer.  Note that the batch version of uacalc (named uab)
must be in your patch, for otherwise some functionality is simply not there (that's the whole point
with wrapper programs!)
