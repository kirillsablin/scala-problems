import org.scalatest._

class P27Spec extends FlatSpec with Matchers {

  import P27._

  "group3" should "return empty list if list size is not 9" in {
    group3(List(1, 2)) should be(List())
    group3(List(1, 2, 3)) should be(List())
    group3((1 to 100).toList) should be(List())
  }

  it should "return all combinations of 2,3 and 4 groups" in {
    val result = group3((1 to 9).toList)

    result.length should be (1260)

    result.forall(_.length == 3) should be (true)

    val firstGroups = result.map(_(0))
    firstGroups.forall(_.length == 2) should be (true)

    val secondGroups = result.map(_(1))
    secondGroups.forall(_.length == 3) should be (true)

    val thirdGroups = result.map(_(2))
    thirdGroups.forall(_.length == 4) should be (true)
  }

  "group" should "return empty list if size of source does not equal to sum of parts" in {
    group(List(1, 2), List(5)) should be(List())
    group(List(1, 2), List(1, 1, 1, 1)) should be(List())
  }

  it should "return all disjoint groups with such sizes" in {
    group(List(1, 2), List(1, 2, 3)) should
      be (List(List(List(1), List(2, 3)), List(List(2), List(1, 3)), List(List(3), List(1, 2))))
  }

}
