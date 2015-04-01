package ch.epfl.directembedding.transformers

import ch.epfl.directembedding.{ DirectEmbeddingModule, DirectEmbeddingUtils }

trait EndpointTransformation extends DirectEmbeddingModule with DirectEmbeddingUtils {
  import c.universe._

  def EndpointTransformer = new (Tree => Tree) {

    //    ${c.parse(dslEndpointMethod)}($tree)
    def apply(tree: Tree): Tree = {
      q"""
          $configPath.dsl($tree)
       """
    }
  }

}
