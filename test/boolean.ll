let false = (x, y -> y);
let true = (x, y -> x);
let and = (p, q -> p q p);
let or = (p, q -> p p q );

or false true
