package ch.epfl.directembedding.test
import ch.epfl.directembedding._

object PersistedObject {
  @_root_.ch.epfl.directembedding.persist(())
  def m = 1

  @ch.epfl.directembedding.persist(())
  def y: Int = 1
  import ch.epfl
  @epfl.directembedding.persist(())
  def m(x: Int) = x

  import ch.epfl.directembedding
  @directembedding.persist(())
  def m[T](x: T) = x

  @persist(())
  @persist(())
  @inline
  def m[T]: T = ???

  object PersistedInnerObject {
    @persist(())
    def m = 1

    @persist(())
    def y: Int = 1

    @persist(())
    def m(x: Int) = x

    @persist(())
    def m[T](x: T) = x

    @persist(())
    def m[T]: T = ???
  }
}

class PersistedClass {
  @persist(())
  def m = 1

  @persist(())
  def y: Int = 1

  @persist(())
  def m(x: Int) = x

  @persist(())
  def m[T](x: T) = x

  @persist(())
  def m[T]: T = ???
}
