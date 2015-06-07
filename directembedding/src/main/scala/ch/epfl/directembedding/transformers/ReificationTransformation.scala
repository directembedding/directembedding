package ch.epfl.directembedding.transformers

import ch.epfl.directembedding.{ DirectEmbeddingModule, DirectEmbeddingUtils }

trait ReificationTransformation extends DirectEmbeddingModule with DirectEmbeddingUtils {
  import c.universe._

  sealed trait Reification

  case class PassThroughReification(tree: Tree) extends Reification

  case class ReifyAsReification(
    body: Tree,
    targs: List[Tree],
    args: List[Tree]) extends Reification

  case class ReifyAsInvokedReification(
    self: Tree,
    method: TermName,
    args: List[Tree]) extends Reification

  val ReifyTransform = new (Reification => Tree) {
    def apply(reification: Reification): Tree = {
      logStarred("ReifyTransform")
      log(s"$reification")
      val result = reification match {
        case PassThroughReification(tree) => tree
        case ReifyAsInvokedReification(self, method, args) =>
          q"$self.$method(..$args)"
        case ReifyAsReification(body, targs, args) =>
          (targs, args) match {
            case (Nil, Nil)    => body
            case (targs, Nil)  => q"$body[..$targs]"
            case (Nil, args)   => q"$body(..$args)"
            case (targs, args) => q"$body[..$targs](..$args)"
          }
      }
      logTree(result)
      result
    }
  }
}
