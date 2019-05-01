# Project

Follow the [instructions](INSTRUCTIONS.md), use this space to document your project for yourself and the graders.

## Names
- David Kirk (dsak@bu.edu)
- Jason Hong (jason810@bu.edu)
- James Mortenson (jimmort@bu.edu)

## Summary
This is the programming language with which we will defeat ThanOS. Through our masterful use of recursion, we will create a stack overflow so large that ThanOS itself will split, shattering the Infinity Monad into its six components: Maybe, Reader, Writer, IO, State, and Parser. With the power of these Monadic Gems, we will conquer the universe and divide the number of sane CS majors by half.

But actually, we're planning to add the following features to the bare minimum plan:
- all "simple" additions (4*5pts = 20pts)
- 2/3 best practices (2*5pts = 10pts)
- error reporting for parser monad (10pts)

## Plan
### Vanilla Features
Next meeting, we will compare efficiencies between our codes and choose the most efficient versions for the functions we already have from the homeworks. The following functions, which we do not have, will be evenly divided up in the following manner.

TO DO LATER:
- print() keyword and logging support (like a console)


David Kirk
- Equals
- Not-equal
- Less-than
- Less-than-or-equal
- Separator (;)
- Predefined functions:
  - elem, map, filter, ord, chr, float, int

Jason Hong
- Greater-than-or-equal
- Greater-than
- Floating-Point Division
- Floating-Point Exponentiation
- Tests
- Chars/strings and related synax (', ")
- New list syntax


James Mortenson
- Integer Exponential
- List indexing operator
- Unary minus
- (some) atomic expressions
- Update existing operators (+,-,*,//) to work with multiple types
- Multiline and single comments
- Static check for unused variables


### Mix-In Features
We will be implementing ALL of the "simple" additions, as well as TWO of the engineering best practices, and the Parser error reporting features.

David Kirk


Jason Hong & James Mortenson
- Multi argument lambda
- infix operator
- sequential let
- haddock doc + html generator

Together: Error reporting Parser monad errors, w/ clear error messages
  - For error reporting, we might consider changing the eval<Bool, Int, List> method behaviors to produce strings without returning a value so that we can maintain the context that produced the error.


