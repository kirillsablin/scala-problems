import org.scalatest._

class P16Spec extends FlatSpec with Matchers {

  import P16._

  "drop" should "return same list if not enough elements" in {
    drop(3, List(1, 2)) should be(List(1, 2))
  }

  it should "return list without Nth element if size of list < n*2" in {
    drop(1, List(1)) should be(List())
    drop(2, List(1, 2, 3)) should be(List(1, 3))
  }

  it should "return list with each nth element" in {
    drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be(List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
  }
}
