package ch.epfl

import scala.language.experimental.macros
import scala.language.implicitConversions

package object directembedding {
  implicit def extractMethodTree(x: Any): MethodTree = macro Macros.extractMethodTree
}
