
Steps in the transformation:

Transformation to async-specific ANF:

- Calls to await function cannot have complex expressions in their parameters


# What was the project supposed to do?

It was supposed to simplify the treatment of asynchronous calls by transforming
a set of dependent asynchronous calls (in scala encapsulated in the class Future)
by a state machine that could represent the dependencies between the calls and return
a failure in case any of the calls failed.

The transformed code would have the same performance than code written with normal futures
but will release the programmer from having to define the dependencies between the futures
by doing nested calls to flatMap on the futures or using for loops.

A further explanation of the project can be found at: 

http://docs.scala-lang.org/sips/pending/async.html

# How would it do it?

It would do it by defining two macro methods:

 - async : when applied to a simple future it returns its input, when applied to a block it transforms the code inside the block into a statemachine where nested calls to async are
 transformed recursively and calls to await, ifs and matchs would be transformed to states of
 the machine.

 - await : receives an object of type Future and returns the value of the future. 


