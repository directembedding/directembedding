package ch.epfl.directembedding

import scala.reflect.macros.blackbox.Context

// Inspired by https://github.com/scala-yinyang/scala-yinyang/blob/16732662470992e10a7ae479d8be5419f13d3654/components/core/src/Utils.scala

trait MacroModule {
  type Ctx <: Context
  val c: Ctx
}

trait DirectEmbeddingModule extends MacroModule {
  import c.universe._
  def lift(tree: Tree): Tree
}

trait DirectEmbeddingUtils {
  def debugLevel: Int
  def log(s: => String, level: Int = 0) = if (debugLevel < level) println(s)
}
