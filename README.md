
# Steps in the transformation


- [ ] Calls to await function cannot have complex expressions in their parameters. This should be 
normally enforced by the macro through the conversion to a specific form of ANF that is explained in:

http://docs.scala-lang.org/sips/pending/async.html

- [X] Construction of the skeleton of the StateMachine class
- [X] Separation of the code passed into the macro into the different states that will be inserted
into the skeleton of the state machine class.
- [X] Generate code for every state: case for handling the state on the resume$async method
 and another case for the state on the apply(result:Try[]), including the insertion of the code result
 of the separation made on the previous step.
- [X] Aggregation of the cases for every state into the definition of the resume$async and the
 apply(result:Try[]) methods and assignment of the right symbols and owners to the methods and
 its members.
- [X] Lifting of variables. Variable used in between different states are lifted into fields of the
 state machine class and the references that are made to them in the methods resume$async and apply
 are transformed into references to the fields.
- [ ] Treatment of if and match expressions. Those expressions require a different treatment because 
 they affect the shape of the abstract statemachine. In a final version support should be added for
 this structures but due to its complexity the project only support expressions of type await
 what allows to generate blocks like the following:

```scala
val result : Future[Boolean] = async {
            val f1 : Future[Boolean] = ..
            var a1 = Macros.await(f1)
            //........
            val f2 : Future[Boolean] = ..
            a2 = Macros.await(f2)
            //........
            a1 && a2

}
```
 
 

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

 - async : when applied to a simple future it returns its input, when applied to a block it  
 transforms the code inside the block into a statemachine where nested calls to async are
 transformed recursively and calls to await, ifs and matchs would be transformed to states of
 the machine.

 - await : receives an object of type Future and returns the value of the future. In the state 
 machine this call is transformed into an state where the code between the last await / if or 
 match and this await represents what will be executed. When the future is finished (method 
 onComplete is called, the result of the future will be assigned to a variable and the state 
 machine will pass to the next state.

# Inspiration

The project was born to be a simplified version of scala/async. I did not want to take that 
project and extend it to add new functionality but the idea was to start from scratch and use
scala/async as an inspiration that would allow me to understand all the concepts about how
the project could be implemented and how the scala compiler works but without an initial base
of code. Over the development of the project I have tried to use an assemble some of the pieces
of scala/async but due to the complexity and my lack of understanding of the scala compiler
I have managed to implement only a subset of the features.

I have had the luck of being able to count on the collaboration of some of the members of
the mailing list scala-internals, where I created a thread that has been dedicated to answer
all the questions that I had during the development of the project. The thread is available at:

https://groups.google.com/forum/#!topic/scala-internals/rIyJ4yHdPDU

# What has been done ?

For the simple case of an async that has an await with a simple future on it, without if/matchs
and defs like the following:

```scala
val result : Future[Boolean] = async {
          val f1 : Future[Boolean] = Future.failed[Boolean]{ new RuntimeException("future that failed") } 
          var a1 = Macros.await(f1) 
          a1
}
```

The project can convert it to the following:

```scala
class MyStateMachine extends AnyRef {
    def <init>(): MyStateMachine = {
      MyStateMachine.super.<init>();
      ()
    };
    private[this] val context: scala.concurrent.ExecutionContextExecutor = scala.concurrent.ExecutionContext.Implicits.global;
    <stable> <accessor> def context: scala.concurrent.ExecutionContextExecutor = MyStateMachine.this.context;
    private[this] var result$async: scala.concurrent.Promise[Boolean] = Promise.apply[Boolean]();
    <accessor> def result$async: scala.concurrent.Promise[Boolean] = MyStateMachine.this.result$async;
    <accessor> def result$async_=(x$1: scala.concurrent.Promise[Boolean]): Unit = MyStateMachine.this.result$async = x$1;
    private[this] var state: Int = 0;
    <accessor> def state: Int = MyStateMachine.this.state;
    <accessor> def state_=(x$1: Int): Unit = MyStateMachine.this.state = x$1;
    private[this] var f1: scala.concurrent.Future[Boolean] = null;
    <accessor> def f1: scala.concurrent.Future[Boolean] = MyStateMachine.this.f1;
    <accessor> def f1_=(x$1: scala.concurrent.Future[Boolean]): Unit = MyStateMachine.this.f1 = x$1;
    private[this] var a1: Boolean = false;
    <accessor> def a1: Boolean = MyStateMachine.this.a1;
    <accessor> def a1_=(x$1: Boolean): Unit = MyStateMachine.this.a1 = x$1;
    def apply(result: scala.util.Try[Boolean]): Unit = MyStateMachine.this.state match {
      case 0 => {
        if (result.isFailure)
          {
            MyStateMachine.this.result$async.complete(result.asInstanceOf[scala.util.Try[Boolean]]);
            ()
          }
        else
          {
            MyStateMachine.this.a1_=(result.get.asInstanceOf[Boolean]);
            MyStateMachine.this.state_=(1);
            MyStateMachine.this.resume$async()
          };
        ()
      }
    };
    def resume$async(): Unit = MyStateMachine.this.state match {
      case 0 => {
        {
          MyStateMachine.this.f1.onComplete[Unit]({
            ((result: scala.util.Try[Boolean]) => MyStateMachine.this.apply(result))
          })(MyStateMachine.this.context);
          ()
        };
        ()
      }
    };
    def apply2(): Unit = MyStateMachine.this.resume$async()
  };
  val stateMachine: MyStateMachine = new MyStateMachine();
  Future.apply(stateMachine.apply2())(ExecutionContext.global);
  stateMachine.result$async.future
}
```

# How to run the project

- Install sbt 0.13 as explained in:

http://www.scala-sbt.org/0.13.0/docs/Getting-Started/Setup.html

- Clone the project from github

- From the root of the project run sbt and once in the command line execute *project macros* to get into the run project and *test* to execute the tests


# Difficulties found 

- Lack of experience working with the internals of the compiler.

- Lack of documentation found on the internals of the compiler. Many articles could be found but most were outdated, the main source of help has ended up being the help in the forum.

- Macro API not thought for symbol manipulation / lack of support for variable capture

- Unclear compiler errors. Most errors apart from trivial mistakes show a really uninformative message,
most of the work to debug the erros was only possible through debugging of the compilation after
macro expansion.


# Limitations

- Due to problems with managing the scope and the imports calls the application has problem dealing
with some of the methods that are defined in the predef package and that are automatically imported. To
ensure that calls to this methods are well handled giving the fully qualified name is a good strategy.

E.g. To print something: scala.Predef.println(...) is prefered over println(...)

- For simplicity in this test project only futures of Boolean types are accepted, however overcoming 
this limitation wouldn't be too difficult converting reference to booleans into a generic type T.

- No support for if/match expressions in the async blocks

- No automatic conversion to ANF. The project requires that the expressions passed are already
normalized. The project started with some work in this sense as shown but some of the first commits,
however due to the complexity I decided to focus on having a version working with constraints on
the code rather than having a plugin that convert to ANF but doesn't do anything else.

