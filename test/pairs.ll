# We define boolean values as functions of two variables that return one of
# those variables.
let false = x, y -> y;
let true = x, y -> x;

# We can define pairs as a function of two variables that returns a function
# that takes a function and applies it to the original two variables.
let pair = x, y -> f -> f x y;

# Then we can use booleans to access the elements in a pair.
let first = p -> p true;
let second = p -> p false;

# We can build lists slowly by creating an empty list.
let nil = x -> true;

# And then building the lists by using pairs.
let prepend = head, body -> pair head body;
let singleton = value -> pair value nil;

# We can access elements of lists by taking the first element
# of the list produced by taking the second item of the list
# number times.
let index = ls, number -> first (number second ls);

# The way we define numbers has to support the above indexing
# so we define a function as a function of two variables that applies the
# first variable, a function, to the second variable some number of times.
let zero = f, x -> x;
let next = n -> f, x -> f (n f x);
let one = next zero;
let two = next one;

# Now we can build lists, by prepending elements to a singleton list.
let list = prepend two (prepend one (singleton zero));

# And we can access elements by index; this should be the last element, aka
# zero, aka f, x -> x, aka (f -> (x -> x)).
index list two
