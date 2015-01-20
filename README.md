DirectEmbedding [![Build Status](https://travis-ci.org/directembedding/directembedding.png?branch=master)](https://travis-ci.org/directembedding/directembedding)
===============

An experimental macro-based library for Embedded DSLs.

# DirectEmbedding for DSLs


DirectEmbedding is an experimental project that attempts to provide direct embedding in Scala for Domain Specific Languages (DSLs). Although, this project is currently exploratory, this library could become a very useful tool. For instance, it could replace the complex shadow embedding of [Yin-Yang](https://github.com/scala-yinyang/scala-yinyang/tree/cbfaf02405c5e273498c15ce943965eeba8afa31) library in existing or future projects such as [Slick](http://slick.typesafe.com/).

This library provides **one** effortless logic for the reification of embedded DSLs in Scala. This comes along many **advantages**:

- no knowledge of the [Reflection API](http://en.wikipedia.org/wiki/Reflection_%28computer_programming%29)
- no overloading resolution
- no dependence with types and count of arguments
- no verbosity
- no code duplication

Our solution makes use of [annotations](http://docs.scala-lang.org/overviews/reflection/annotations-names-scopes.html) and [macros](http://www.scala-lang.org/api/2.11.0/scala-reflect/index.html#scala.reflect.macros.blackbox.Context) to achieve this goal.

##Table of contents:

1. [Overview](#overview)
2. [Details](#details)
3. [Project Structure](#project-structure)
4. [Usage](#usage)
5. [References](#references)
6. [Licence](#licence)



--

<!--
The simplicity for the DSLs users is kept because direct embedding implies that the code to be written by the users will be done in Scala with few restrictions on it and few overhead due to the DSL synthax.
Furthermore, it also benefits the developpers of DSL because DirectEmbedding handles the difficult task of lifting the Scala AST to an Intermediate Representation (IR) thanks to macro.
-->

# Overview
Writing embedding DSLs implies to go though the difficult task of reification. 
Reification means when a function *take(x: Int): Query[T]* is written in Scala it will be converted to a corresponding DSL object *Take*. *Take* is the Intermediate Representation (IR) of the function *take()*. The naive method would code conditional statements that would **identify** which function has been called. Identifying is strenuous and redundant because it must check many criteria, takes in account aspects such as overriding of the function and this for all functions or classes. It is not a best practices and it creates as much reification logics than there is cases to reify.
Hopefully, this can be facilitated by libraries and DirectEmbedding is one of them.

*[More in  Quasiquotes](#quasiquotes)*

### Attach Metadata
The idea developed by DirectEmbedding is to attach metadata about the corresponding IR and thus it get rid of the demanding task of identification. It becomes straight forward to solve the previous issues. When it was required to verify the name of the function, its types, its types arguments, its arguments count, now this is declared along the function. Overriding? It is also very simple, it simply needs to attach the corresponding IR.

### Annotate
Scala annotations can attribute an object to declarations. Thus, annotations permits to attach metadata, that is to say, the IR. The annotation is accessible through the symbol of the AST.

### Reify
DirectEmbedding reifies the DSL while the compilation. This means that the given AST for an annotated function will be reified into the DSL representation at compile-time. In order to do so, there is the need to *pattern match* all the different ASTs that can be generated by an annotated declaration. Once the arguments, the types arguments and the symbol of the annotated code are extracted from the AST then the reification occurs via a macro. The macro applies the arguments and types arguments to the obtained IR from the symbol. The macro returns the reified tree i.e. the IR with the arguments and types arguments correctly applied. 

There is not special difficulty for the identification, DirectEmbedding does the hard task of *pattern matching* the ASTs and the macro, the solution is elegant and proposes only one reification logic. **Pretty simple!**



### Overview: An example
**On the DSL's developper side**

* Let's imagine, we want to define for a DSL a function take() that returns a Query[T]:

	```scala
		class Query[T] {
			val take(x: Int): Query[T] = ???
		}
	```

* For this DSL, the function take() should be reified into the following IR:

	```scala
		case class Take[T](self: Exp[QueryIR[T]], n: Exp[Int])
		   extends Exp[Query[T]]
	```

* Thus, the function take() needs to be annotated:

	```scala
		class Query[T] {
			@reifyAs(Take) //Annotation with its corresponding IR
			val take(x: Int): Query[T] = ???
		}
	```

**On the DSL's user side**

* One user calls the function take() in his code:

	```scala
	   lift {
	      	new Query[Movie].take(3)
	   }
	```

* The function is reified inside at compile-time into:

	```scala
	   Expr[T](Take.apply[Int](QueryIR.apply[Movie], 3))
	```

**What happened?**

* A Scala AST is generated and caught
* The annotation, the arguments and the type arguments are extracted
* A macro reified the AST into the result


# Details
## Quasiquotes?

```scala
	case q"$a.take($b)" if a.tpe =:= typeOf[Query[_]]
  	 		&& b.tpe =:= typeOf[Int] => Take(a, b)
```
	
The code above shows what would be reification without DirectEmbedding. Although, in this case, we used quasiquotes, there is none of the advantages of DirectEmbedding:
	
1. **no knowledge of the [Reflection API](http://en.wikipedia.org/wiki/Reflection_%28computer_programming%29)**
	- this code obviously necessitates knowledge of the Reflection API
- **no overloading resolution**
	- if *take(x: Int, b: Boolean): Query[T]* overrides *take(x: Int): Query[T]* then another conditional statement would be needed

	```scala
		case q"$a.take($b, $c)" if a.tpe =:= typeOf[Query[_]]
  	 		&& b.tpe =:= typeOf[Int]
  	 		&& c.tpe =:= typeOf[Boolean] => Take(a, b, c)
	```
- **no dependence with types and count of arguments**
	- as shown in the previous example, a new argument implies a new code
- **no verbosity**
	- this kind of code can become illegible
- **no code duplication**
	- again in the override example, there is unnecessary code duplication


## Persisted
why persisted

## Inline
why inline


# Project Structure
| *Component*                                   		  | *Description*                        | 
|:---------                                   		  |:-----------                        | 
| `directembedding/...` <br> `/DirectEmbedding.scala` | **DirectEmbedding code:** reification code with macro | 
| `dsls/main/.../BasicSpec.scala`             		  | **Test:** Intermediate Representation        | 
| `dsls/test/.../TestBase.scala`              		  | **Test:** Corners cases tests                | 

## DirectEmbedding.scala

This file contains the reification code. It consists of the definition of:

* reifyAs()

	```scala
	   class reifyAs(to: Any) extends scala.annotation.StaticAnnotation
	```
	
	* This defines the annotation *reifyAs* which accepts *Any* object so the IR can be attach into the symbols

* and lift()

	```scala
		def lift[T](c: Context)(block: c.Expr[T]): c.Expr[T] = {
   			import c.universe._
		    class LiftingTransformer extends Transformer {
		    	...
		    }
		}
	```
	
	* This is the method that encompasses all the reification process. 

The class LiftingTransformer defines:

* reify()

	```scala
		def reify(methodSym: Symbol, targs: List[Tree], args: List[Tree]): Tree = { ... }
	```

	* This code uses the macro with its parameters to reify the captured function into its IR.
	
* and transform()

	```scala
		override def transform(tree: Tree): Tree = { ... }
	```

	* transform() *pattern matches* over the different ASTs to extract the essential data for reify() that is to say the symbol, the type arguments and the arguments.

## TestBase.scala
This file contains the functions and IR that represent a DSL. It is used to for testing purpose in BasiSpec.scala

Example for a class:

```scala
case object ClassCons extends Exp[ClassExample] // Note: IR extends the real returned type

@reifyAs(ClassCons)
class ClassExample {
  val dummyVal: Int = 1
}
```

Example for a function with *many* arguments:

```scala
case class AppManyArgs[T](self: Exp[TArgClassExample[T]], p1: Exp[T]*) extends Exp[T] // Note: all function have as first argument a self

@reifyAs(TArgClassExampleCase)
class TArgClassExample[T] {
	  @reifyAs(AppManyArgs)
	  def app1[T](p1: T*): T = ???
}
```

## BasicSpec.scala
This fils contains the tests.

For example the test for take():

```scala
"lift" should "work with TArgClassExample methods with take" in {
  testReify(implicit collec =>
    lift {
      new TArgClassExample[Int].take(3)
    }) should be(List(Take[Int](TArgClassExampleCase[Int](), 3)))
}
```
	
# Usage
The project has been tested under [Sbt 0.13.6](http://www.scala-sbt.org/) and [Scala 2.11.2](http://www.scala-lang.org/)

### Dependencies
The project depends on **ScalaTest 2.2.1** library, copy paste the hereafter dependency into the *build.sbt* in *\<sbtRootFolder\>/0.13/plugins/*

    libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"

### Launching the project
SBT is required to compile the project.

To launch the tests:

     sbt test
	 
To configure the project for [Eclipse](http://scala-ide.org/download/sdk.html) run the command:

     sbt eclipse
 

# Progress

| Todos                        | Done | Details                     |
|------------------------------|------|-----------------------------|
| values                       | Yes  | val x = ...                 |
| function                     | Yes  | def foo: ...                |
| function with args           | Yes  | def foo(x: Int): Int = ...  |
| function with targs          | Yes  | def foo[T, U]: (T, U) = ... |
| function with args and targs | Yes  | def foo[T, U]\(t: T, u: U): (T, U) = ... |
| objects                      | Yes  |                             
| nested objects               | Yes  |                             
| classes                      | Yes  |                             
| language specification       | No   | if, while, read-var         |
| operator                     | No   |                             
| recursion                    | No   |                             
| override                     | No   |                             

# References
* [Experimental direct embedding for Slick](https://github.com/slick/slick/blob/master/src/sphinx/direct-embedding.rst)
* [Master thesis: "An Embedded Query Language in Scala" by Amir Shaikhha ](https://github.com/amirsh/master-thesis)
* []()
* []()
* []()

# License

DirectEmbedding is licensed under the [EPFL License](https://raw.githubusercontent.com/directembedding/directembedding/master/LICENCE).
