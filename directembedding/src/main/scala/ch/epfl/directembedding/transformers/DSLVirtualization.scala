package ch.epfl.directembedding.transformers

import ch.epfl.directembedding.{ DirectEmbeddingUtils, DirectEmbeddingModule }
import ch.epfl.yinyang.EmbeddedControls

object RewireEmbeddedControls extends EmbeddedControls

trait DSLVirtualization extends DirectEmbeddingModule with DirectEmbeddingUtils {
  import c.universe._

  def DSLVirtualizer = new (Tree => Tree) {
    def apply(tree: Tree) = {
      c.typecheck(q"""
      import _root_.ch.epfl.directembedding.transformers.RewireEmbeddedControls._
      {
          import $configPath._
          $tree
      }
       """)
    }
  }

}
