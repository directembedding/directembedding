package ch.epfl.directembedding.test
import org.scalatest.{ FlatSpec, ShouldMatchers }

class PersistenceSpec extends FlatSpec with ShouldMatchers {
  "Trees of @persist annotated methods" should "be persisted for objects" in {
    persisted(PersistedObject.m) should be(
      """DefDef(Modifiers(), TermName("m"), List(), List(), TypeTree(), Literal(Constant(1)))""")
    persisted(PersistedObject.y) should be(
      """DefDef(Modifiers(), TermName("y"), List(), List(), TypeTree(), Literal(Constant(1)))""")
    persisted(PersistedObject.m(1)) should be(
      """DefDef(Modifiers(), TermName("m"), List(), List(List(ValDef(Modifiers(PARAM), TermName("x"), TypeTree(), EmptyTree))), TypeTree(), Ident(TermName("x")))""")
    persisted(PersistedObject.m[Int](1)) should be(
      """DefDef(Modifiers(), TermName("m"), List(TypeDef(Modifiers(PARAM), TypeName("T"), List(), TypeTree())), List(List(ValDef(Modifiers(PARAM), TermName("x"), TypeTree(), EmptyTree))), TypeTree(), Ident(TermName("x")))""")
    persisted(PersistedObject.m[Int]) should be(
      """DefDef(Modifiers(), TermName("m"), List(TypeDef(Modifiers(PARAM), TypeName("T"), List(), TypeTree())), List(), TypeTree(), Select(Select(This(TypeName("scala")), scala.Predef), TermName("$qmark$qmark$qmark")))""")
  }

  it should "be persisted for nested objects" in {
    persisted(PersistedObject.PersistedInnerObject.m) should be(
      """DefDef(Modifiers(), TermName("m"), List(), List(), TypeTree(), Literal(Constant(1)))""")
    persisted(PersistedObject.PersistedInnerObject.y) should be(
      """DefDef(Modifiers(), TermName("y"), List(), List(), TypeTree(), Literal(Constant(1)))""")
    persisted(PersistedObject.PersistedInnerObject.m(1)) should be(
      """DefDef(Modifiers(), TermName("m"), List(), List(List(ValDef(Modifiers(PARAM), TermName("x"), TypeTree(), EmptyTree))), TypeTree(), Ident(TermName("x")))""")
    persisted(PersistedObject.PersistedInnerObject.m[Int](1)) should be(
      """DefDef(Modifiers(), TermName("m"), List(TypeDef(Modifiers(PARAM), TypeName("T"), List(), TypeTree())), List(List(ValDef(Modifiers(PARAM), TermName("x"), TypeTree(), EmptyTree))), TypeTree(), Ident(TermName("x")))""")
    persisted(PersistedObject.PersistedInnerObject.m[Int]) should be(
      """DefDef(Modifiers(), TermName("m"), List(TypeDef(Modifiers(PARAM), TypeName("T"), List(), TypeTree())), List(), TypeTree(), Select(Select(This(TypeName("scala")), scala.Predef), TermName("$qmark$qmark$qmark")))""")
  }

  it should "be persisted for classes" in {
    persisted(new PersistedClass().m) should be(
      """DefDef(Modifiers(), TermName("m"), List(), List(), TypeTree(), Literal(Constant(1)))""")
    persisted(new PersistedClass().y) should be(
      """DefDef(Modifiers(), TermName("y"), List(), List(), TypeTree(), Literal(Constant(1)))""")
    persisted(new PersistedClass().m(1)) should be(
      """DefDef(Modifiers(), TermName("m"), List(), List(List(ValDef(Modifiers(PARAM), TermName("x"), TypeTree(), EmptyTree))), TypeTree(), Ident(TermName("x")))""")
    persisted(new PersistedClass().m[Int](1)) should be(
      """DefDef(Modifiers(), TermName("m"), List(TypeDef(Modifiers(PARAM), TypeName("T"), List(), TypeTree())), List(List(ValDef(Modifiers(PARAM), TermName("x"), TypeTree(), EmptyTree))), TypeTree(), Ident(TermName("x")))""")
    persisted(new PersistedClass().m[Int]) should be(
      """DefDef(Modifiers(), TermName("m"), List(TypeDef(Modifiers(PARAM), TypeName("T"), List(), TypeTree())), List(), TypeTree(), Select(Select(This(TypeName("scala")), scala.Predef), TermName("$qmark$qmark$qmark")))""")
  }
}
