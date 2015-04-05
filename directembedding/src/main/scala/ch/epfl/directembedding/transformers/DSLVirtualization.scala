package ch.epfl.directembedding.transformers

import ch.epfl.directembedding.{ DirectEmbeddingUtils, DirectEmbeddingModule }
import ch.epfl.yinyang.EmbeddedControls

import scala.reflect.macros.TypecheckException

object RewireEmbeddedControls extends EmbeddedControls

trait DSLVirtualization extends DirectEmbeddingModule with DirectEmbeddingUtils {
  import c.universe._

  def DSLVirtualizer = new (Tree => Tree) {
    def apply(tree: Tree) = {
      logStarred("Before DSLVirtualization", logLevel, tree)
      try {
        c.typecheck(q"""
          import _root_.ch.epfl.directembedding.transformers.RewireEmbeddedControls._
          {
              import $configPath._
              $tree
          }
       """)
      } catch {
        case e: TypecheckException =>
          // Can happen when the custom embedded controls don't
          // overload for the types used in this block.
          c.abort(c.enclosingPosition, s"Typecheck during DSLVirtualization failed, ${e.getMessage}")
      }
    }
  }

}
