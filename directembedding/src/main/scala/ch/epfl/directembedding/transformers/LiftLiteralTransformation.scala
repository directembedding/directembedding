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

    def genApply(t: List[Tree]) =
      q"$configPath.lift(..$t)"

    override def transform(tree: Tree): Tree = {
      tree match {
        case t @ Literal(Constant(_)) =>
          genApply(List(t))
        case t @ Ident(_) if toLift.contains(t.symbol) =>
          genApply(List(Ident(TermName(t.name.decodedName.toString))))
        // the type associated with the identifier will remain if we don't that
        case t @ Ident(n) =>
          log("local variable: " + t, 3)
          Ident(n)
        case _ =>
          super.transform(tree)
      }
    }
  }

}
