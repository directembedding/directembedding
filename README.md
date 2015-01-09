Direct Embedding for Scala [![Build Status](https://travis-ci.org/directembedding/directembedding.png?branch=master)](https://travis-ci.org/directembedding/directembedding)
===============

DirectEmbedding library for macro based direct embedding of DSLs.

# DirectEmbedding


DirectEmbedding is a library that attemps to allow direct embedding in Scala for Domain Specific Languages (DSLs). Although, this project is, for now, a first sketch, this library could become a very useful tool. For instance, it can replace the complex shadow embedding of [Yin-Yang](https://github.com/scala-yinyang/scala-yinyang/tree/cbfaf02405c5e273498c15ce943965eeba8afa31) library in existing or future projects as [Slick](http://slick.typesafe.com/).
This library is based on [macro](http://www.scala-lang.org/api/2.11.0/scala-reflect/index.html#scala.reflect.macros.blackbox.Context) which permit modification of [annoted code](http://docs.scala-lang.org/overviews/reflection/annotations-names-scopes.html) during *compile time*.

The simplicity for the DSLs users is kept because direct embedding implies that the code to be written by the users will be done in Scala with few restrictions on it and few overhead due to the DSL synthax.
Furthermore, it also benefits the developpers of DSL because DirectEmbedding handles the difficult task of lifting the Scala AST to an Intermediate Representation (IR) thanks to macro.

### Preview of usage 


**Developper code**

```scala
	object ObjectExample {
		@reifyAs(ValDef)
		val valDef: Int = ???
	}
```

**User code**:

```scala
    ObjectExample.valDef
```

## Code Structure

### Project Structure
| Component                                   		  | Description                        | 
|:---------                                   		  |:-----------                        | 
| `directembedding/...` <br> `/DirectEmbedding.scala` | Core code: lifting code with macro | 
| `dsls/main/.../BasicSpec.scala`             		  | Intermediate Representation        | 
| `dsls/test/.../TestBase.scala`              		  | Corners cases tests                | 

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