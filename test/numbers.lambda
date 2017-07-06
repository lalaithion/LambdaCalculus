# In the lambda calculus, zero is represented as a function that takes a function
# and an argument, and applies the function to that argument zero times.
let zero = f, x -> x;

# We can get the number after n by calling n on f and x, reducing that to the body
# of n, which should be f applied to x, n times. Then, we apply f one more time
let next = n -> f, x -> f (n f x);
    
# one evaluates to f, x -> f x
let one = next zero;

# two evaluates to f, x -> f (f x)
let two = next one;
    
# three evaluates to f,x -> f (f (f x))
let three = next two;

# We can add two numbers by first, applying n to f and x, and then applying m
# to f and the result of the first application
let plus = m, n -> f, x -> m f (n f x);

# multiplying two numbers is easier; we just make the function we pass into m
# be `add n`, and then have x be zero. This makes us add n to zero m times.
let times = m, n -> m (plus n) zero;

times three three
