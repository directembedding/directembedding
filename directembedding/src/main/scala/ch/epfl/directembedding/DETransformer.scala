package ch.epfl.directembedding

import ch.epfl.directembedding.transformers.{ LiftLiteralTransformation, EndpointTransformation, DSLVirtualization, ReifyAsEmbedding }
import ch.epfl.yinyang.analysis.FreeIdentAnalysis
import ch.epfl.yinyang.transformers.{ LanguageVirtualization, NullPreProcessing, NullPostProcessing, PreProcessing, PostProcessing }

import scala.reflect.macros.{ TypecheckException, blackbox }

object DETransformer {

  def typechecks[C <: blackbox.Context](c: C)(path: c.Tree): Boolean = {
    import c.universe._
    try {
      c.typecheck(path)
    } catch {
      case e: TypecheckException =>
        return false
    }
    true
  }

  def apply[C <: blackbox.Context, T](c: C)(
    _dslName: String,
    _configType: c.universe.Type,
    postProcessing: Option[PostProcessing[c.type]],
    preProcessing: Option[PreProcessing[c.type]]): DETransformer[c.type, T] = {
    import c.universe._

    val configName = _configType.typeSymbol.fullName
    val dslConfig: Tree = c.parse(configName)
    assert(typechecks(c)(q"import $dslConfig.lift"), s"Method lift is not a member of type $configName")
    assert(typechecks(c)(q"import $dslConfig.dsl"), s"Method dsl is not a member of $configName")

    new DETransformer[c.type, T](c) {
      val postProcessor = postProcessing.getOrElse(new NullPostProcessing[c.type](c))
      val preProcessor = preProcessing.getOrElse(new NullPreProcessing[c.type](c))
      val dslName: String = _dslName
      val configPath: Tree = dslConfig
      val logLevel: Int = 1
    }
  }
}

abstract class DETransformer[C <: blackbox.Context, T](val c: C)
  extends DirectEmbeddingModule
  with ReifyAsEmbedding
  with LanguageVirtualization
  with DSLVirtualization
  with FreeIdentAnalysis
  with LiftLiteralTransformation
  with EndpointTransformation {
  type Ctx = C
  import c.universe._
  override val debugLevel: Int = 0
  val postProcessor: PostProcessing[c.type]
  val preProcessor: PreProcessing[c.type]
  import postProcessor._
  import preProcessor._

  def apply[T](block: c.Expr[T]): c.Expr[T] = {

    val tree = block.tree

    val captured = freeVariables(tree)
    val toLifted = captured.map(_.symbol)

    val transformed: Tree = {
      (PreProcess andThen
        (x => c.untypecheck(x)) andThen
        (x => VirtualizationTransformer(x)._1) andThen
        DSLVirtualizer andThen
        ReifyAsTransformer andThen
        LiftLiteralTransformer(toLifted) andThen
        EndpointTransformer andThen
        (x => c.untypecheck(x)) andThen
        PostProcess)(tree)
    }
    log("**********************", 2)
    log("* After transformation", 2)
    log("**********************", 2)
    logTree(transformed, 2)

    c.Expr[T](transformed)
  }

}
