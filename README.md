DirectEmbedding [![Build Status](https://travis-ci.org/directembedding/directembedding.png?branch=master)](https://travis-ci.org/directembedding/directembedding)
===============

An experimental macro-based library for Embedded DSLs.

# DirectEmbedding for DSLs


DirectEmbedding is an experimental project that attempts to provide direct embedding in Scala for Domain Specific Languages (DSLs). Although, this project is currently exploratory, this library could become a very useful tool. For instance, it could replace the complex shadow embedding of [Yin-Yang](https://github.com/scala-yinyang/scala-yinyang/tree/cbfaf02405c5e273498c15ce943965eeba8afa31) library in existing or future projects such as [Slick](http://slick.typesafe.com/).

DirectEmbedding has **one** effortless logic for reification. Thus, the reification comes along with some advantages among:

- no knowledge of the [Reflection API](http://en.wikipedia.org/wiki/Reflection_%28computer_programming%29)
- no overloading resolution
- no dependence with types and count of arguments
- no verbosity
- no code duplication

This is permitted by [*annotations*](http://docs.scala-lang.org/overviews/reflection/annotations-names-scopes.html) which attach metadata about the Intermediate Representation (IR) to Symbols such that the annotated function can easily be reified at compile-time.
During the compilation, the ASTs with annotations can be identified and reified by a [macro](http://www.scala-lang.org/api/2.11.0/scala-reflect/index.html#scala.reflect.macros.blackbox.Context) which takes informations such as arguments, type arguments, the IR from the annotation. **Pretty simple!**

<!--
The simplicity for the DSLs users is kept because direct embedding implies that the code to be written by the users will be done in Scala with few restrictions on it and few overhead due to the DSL synthax.
Furthermore, it also benefits the developpers of DSL because DirectEmbedding handles the difficult task of lifting the Scala AST to an Intermediate Representation (IR) thanks to macro.
-->

### Explanation with an example
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

* The function is reified inside the compile into:

	```scala
	   Expr[T](Take.apply[Int](QueryIR.apply[Movie], 3))
	```

**What happened?**

* A Scala AST is generated and caught
* The annotation, the arguments and the type arguments are extracted
* A macro reified the AST into the result


## About the code

### Project Structure
| Component                                   		  | Description                        | 
|:---------                                   		  |:-----------                        | 
| `directembedding/...` <br> `/DirectEmbedding.scala` | **DirectEmbedding code:** reification code with macro | 
| `dsls/main/.../BasicSpec.scala`             		  | **Test:** Intermediate Representation        | 
| `dsls/test/.../TestBase.scala`              		  | **Test:** Corners cases tests                | 

## Usage
The project has been tested under [Sbt 0.13.6](http://www.scala-sbt.org/) and [Scala 2.11.2](http://www.scala-lang.org/)

### Dependencies
To add the **ScalaTest** library, copy paste the hereafter dependency into the *build.sbt* in *\<sbtRootFolder\>/0.13/plugins/*

    libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"

### Launching the project
SBT is required to compile the project.

To launch the tests:

     sbt test
	 
To configure the project for [Eclipse](http://scala-ide.org/download/sdk.html) run the command:

     sbt eclipse
    



## License

DirectEmbedding is licensed under the [EPFL License](https://raw.githubusercontent.com/directembedding/directembedding/master/LICENCE).