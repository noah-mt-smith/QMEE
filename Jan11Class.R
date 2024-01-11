# this is a new R script file that I'm trying to push to my Git

# notes for Jan 11 class 

32/4*4

# requires parentheses because ambiguous

# you can type ls() to list your objects (variables and whatnot)

# R also has vectors, lists, and functions. 

# vectors: a bunch of things (a list) of things that have the same storage mode
# -- vectors are the basic way that r thinks about things.

# you can use "c()" to concatenate a vector to another vector. 

# list: a bunch of things in order, not necessarily of the same type.

# you can "Call" functions by using parentheses. 

# for example, you can use the function "mean()" to calculate the mean of a set of numbers.

# functions take arguments, and these go inside the parentheses of the function. Many functions have multiple arguments that you must fulfill

# you should really use "<-" (called "Gets") instead of "=" because you can get in trouble sometimes if you're using "=". 

# style: always refer to the "introduction to R" class document for good R style, along with the Baath (2012) citation

# DATA FRAMES

# list of vectors that all need to be the same length.

# CONTROL STRUCTURES

# FOR LOOPS 

# We can create an array and then a for loop to print the array in various ways

v <- 1:10
for (i in v) {
  print(i^2)
}

# here, we're printing each value of the array as its square

# you can also do more complex stuff, like changing values of the array only if the array value is larger than a specified value (e.g. 4)

for (i in v) {
  if(i>=4) {
    print(i^2)
  }
}

# The script that we put onto git needs to be tidy. 

# like they don't want to see ls() or other stuff like that.

#if you're picking one thing out of an array, it's important that you actually use a double square bracket (e.g. [[x]]).
# however, if you're trying to get multiple things, then try 

# assignment is due tomorrow afternoon, but don't have to do second half?