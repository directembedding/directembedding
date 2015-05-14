package ch.epfl.directembedding

import ch.epfl.yinyang.TransformationUtils

import scala.reflect.macros.TypecheckException
import scala.reflect.macros.blackbox.Context

// Inspired by https://github.com/scala-yinyang/scala-yinyang/blob/16732662470992e10a7ae479d8be5419f13d3654/components/core/src/Utils.scala

trait MacroModule {
  type Ctx <: Context
  val c: Ctx
}

trait DirectEmbeddingModule extends MacroModule {
  import c.universe._
  val dslName: String
  val failCompilation: Boolean = false
  val virtualizeFunctions: Boolean = false
  val virtualizeVal: Boolean = true
  val flattenCurriedFunctions: Boolean = true

  // We use Strings as keys to resolve aliased types
  val typeMap: Map[String, Type]
  // To override lift with some custom lift
  val customLifts: Map[Type, String]
  val liftIgnore: Set[Type]

  /**
   * Full name of configuration module
   */
  val configPath: Tree
  val logLevel: Int
}

/**
 * Skeleton for Dsl config type
 */
trait DslConfig {
  val failCompilation: Boolean = false
  val virtualizeVal: Boolean = true
  val virtualizeFunctions: Boolean = true
  val flattenCurriedFunctions: Boolean = true

  /**
   * The type which literals get lifted to
   * @tparam T
   */
  type Literal[T]
  /**
   * The IR top level type
   * @tparam T
   */
  type Rep[T]

  /**
   * Method compile needs to be implemented
   */

  /**
   * The method invoked for lifting literals. Mandatory.
   * @param e
   * @tparam T
   * @return
   */
  def lift[T](e: T): Literal[T]
}

trait DirectEmbeddingUtils extends DirectEmbeddingModule with TransformationUtils {
  import c.universe._

  def debugLevel: Int = 0

  def logTree(t: Tree, level: Int = logLevel) = {
    log(s"$t", level)
    log(showRaw(t, printTypes = true), level + 1)
  }

  def logType(t: Type, level: Int = logLevel): Unit = log(showRaw(t), level)

  def logStarred(msg: String, level: Int = logLevel, tree: Tree = EmptyTree) = {
    val len = msg.length + 2
    log(s"*" * len, level)
    log(s"* $msg", level)
    log(s"*" * len, level)
    logTree(tree, level)
  }

  def logIndented[T](e: T, indent: Int, level: Int = logLevel) = if (debugLevel > level) {
    print(" " * indent)
    super.log(s"$e", level)
  }

  def LogHereAndContinue(where: String)(t: Tree): Tree = {
    super.log(where)
    logTree(t)
    t
  }

}

trait TypeHelper extends MacroModule {
  import c.universe._

  private def name(s: Symbol): String = s.name.toString.trim

  def equalSymbols(a: Symbol)(b: Symbol): Boolean =
    name(a) == name(b)

  def findMatchingSymbol(symbol: Symbol, typ: Type): Option[Symbol] =
    typ.member(symbol.name).alternatives.find(equalSymbols(symbol))

}
