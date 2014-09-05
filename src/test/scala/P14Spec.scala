import org.scalatest._

class P14Spec extends FlatSpec with Matchers {
  import P14._

  "duplicate" should "return empty list on empty list" in {
    duplicate(List()) should be(List())
  }

  it should "duplicate elements on non-empty lists" in {
    duplicate(List(1, 2, 3)) should be(List(1, 1, 2, 2, 3, 3))
    duplicate(List('a, 'b, 'c, 'c, 'd)) should be (List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))
  }

}
