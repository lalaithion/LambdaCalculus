# LambdaCalculus

This is an interpreter for a variant of Church's Lambda Calculus written in Haskell

There are some sample programs in the tests directory, which demonstrate the full capabilities of the language. To sum up:

Lines beginning with hashtags are comments.

A function is declared using (variable -> body) `(x -> x)`.

Multiple variable functions can be declared in two ways; `(x -> y -> y x)` or `(x, y -> y x)`.

For convenience, constants can be declared above the main program as `let` statements:
```
let identity = x -> x;

identity y
```
The above will evaluate to `y`.

The interpreter will follow rules for beta reduction until the expression is in a normalized form, if possible.
