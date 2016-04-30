
# HLisp

This is an attempt to make a lisp who's base is very minimal. This is for
theoretical purposes only. See doc/formal/formal.pdf for a specification of
HLisp. See lib/ for example code.

## Running

To run HLisp enter the following at the command line.
```
> cabal run
```

To use the base library run
```
> cabal run lib/*
```

I would suggest using [rlwrap](http://utopia.knoware.nl/~hlub/uck/rlwrap/) when
using the interpreter.

## Example Session

```
--> rlwrap cabal run lib/core.lisp lib/natural.lisp
Preprocessing executable 'hlisp' for hlisp-0.3.0.0...
Running hlisp...
> (= 6 (+ 1 2))
()
> (= 3 (+ 1 2))
t
> (defun rev (x y) (y x))
()
> (rev 'a id)
a
> ((const 9) 8)
(inc (inc (inc (inc (inc (inc (inc (inc (inc 0)))))))))
> (const 9)
<lambda (y) ...>
> (let (x 1) (lambda (y) (+ x y)))
<lambda (y) ...>
> ((let (x 1) (lambda (y) (+ x y))) 2)
(inc (inc (inc 0)))
> 
```