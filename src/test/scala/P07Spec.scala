import org.scalatest._

class P07Spec extends FlatSpec with Matchers {
  import P07._

  "flatten" should "return same list if list is flat" in {
    flatten(List(1, 2, 3)) should be(List(1, 2, 3))
  }

  it should "return flatten version of list" in {
    flatten(List(1, 2, List(3, 4))) should be (List(1, 2, 3, 4))
    flatten(List(1, 2, List(3, List(4, 5)))) should be (List(1, 2, 3, 4, 5))
  }

  it should "be able flatten big lists" in {
    val bigList = ((1 to 10000) map (_ => List(1, 1, List(1, 1, 1)))).toList

    flatten(bigList) should be ((1 to 50000) map (_ => 1))
  }

}
