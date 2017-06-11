
# HLisp

This is an attempt to make a lisp who's base is very minimal. This is for
theoretical purposes only. See doc/spec/spec.pdf for an informal specification
of HLisp. See lib/ for example code.

HLisp is fairly unique among lisps in that it is lazily evaluated. It also
doesn't support recursion through bindings, so `(label loop (lambda () (loop)))`
will not work. However, lambdas do support a syntax for recursion, so the last
statement could be written `(label loop (lambda loop () (loop)))`.

## Running

To build HLisp enter the following at the command line.
```
> stack build
```

To run HLisp run
```
> stack exec hlisp
```

To use the base library run
```
> stack exec hlisp lib/*
```

## Example Session

```
--> stack exec hlisp lib/core.lisp lib/natural.lisp
> (+ 1 2)
(inc (inc (inc 0)))
> ((= 6 (+ 1 2)) 't 'f)
f
> ((= 3 (+ 1 2)) 't 'f)
t
> ((const 9) 1)
(inc (inc (inc (inc (inc (inc (inc (inc (inc 0)))))))))
> (id id)
<lambda (x) ...>
```
