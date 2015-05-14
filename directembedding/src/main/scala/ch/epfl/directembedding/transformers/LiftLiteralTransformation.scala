package ch.epfl.directembedding.transformers

import ch.epfl.directembedding.{ DirectEmbeddingModule, DirectEmbeddingUtils }

import language.experimental.macros

trait LiftLiteralTransformation extends DirectEmbeddingModule with DirectEmbeddingUtils {
  import c.universe._
  object LiftLiteralTransformer {
    def apply(toLift: List[Symbol])(tree: Tree): Tree = {
      val t = new LiftLiteralTransformer(toLift).transform(tree)
      log("lifted: " + t, 2)
      t
    }
  }

  class LiftLiteralTransformer(toLift: List[Symbol])
    extends Transformer {

    def genApply(t: List[Tree], name: TermName = TermName("lift")) =
      q"$configPath.$name(..$t)"

    def genApplyWithName(t: List[Tree], name: String = "lift") =
      genApply(t, TermName(name))

    def genApplyForType(t: List[Tree], tpe: Type) = {
      val name = customLifts.collect {
        case (customType, customName) if tpe <:< customType => customName
      }.headOption.getOrElse("lift")

      genApply(t, TermName(name))
    }

    override def transform(tree: Tree): Tree = {
      tree match {
        case _: ClassDef => tree
        case t @ Literal(Constant(_)) if !liftIgnore.exists(ignoreType => t.tpe.widen <:< ignoreType) =>
          genApplyForType(List(t), tree.tpe)
        case t @ Ident(_) if toLift.contains(t.symbol) && !liftIgnore.exists(ignoreType => tree.tpe.widen <:< ignoreType) =>
          genApplyForType(List(Ident(TermName(t.name.decodedName.toString))), t.tpe.widen)
        // the type associated with the identifier will remain if we don't that
        case t @ Ident(n) =>
          log(s"local variable: $t, type=${t.tpe}", logLevel)
          Ident(n)
        case _ =>
          super.transform(tree)
      }
    }
  }

}
