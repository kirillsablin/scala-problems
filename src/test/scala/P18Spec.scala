import org.scalatest._

class P18Spec extends FlatSpec with Matchers {

  import P18._

  "slice" should "return empty list if indexes out of bounds" in {
    slice(10, 12, List(1, 2)) should be (List())
  }

  it should "return slice of list" in {
    slice(0, 10, List(1, 2)) should be (List(1, 2))
    slice(2, 3, List(1, 2, 3)) should be (List(3))
    slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should
      be (List('d, 'e, 'f, 'g))

    slice(1, 1, List(1, 2, 3)) should be (List())

  }

  it should "works for large lists" in {
    val largeList = (0 to 10000).toList

    slice(9000, 9001, largeList) should be (List(9000))
  }

}
