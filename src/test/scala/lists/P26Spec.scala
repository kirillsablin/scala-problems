package lists

import P26._
import org.scalatest._

class P26Spec extends FlatSpec with Matchers {

  "combinations" should "generation singleton list with empty list if k equals zero" in {
    combinations(0, List(1, 2, 3)) should be(List(List()))
  }

  it should "generate list of singleton lists for each element of source list if k equals one" in {
    combinations(1, List(1, 2, 3)) should be(List(List(1), List(2), List(3)))
  }


  it should "generate list of all possible combinations if k more then one" in {
    combinations(2, List(1, 2, 3)) should be(List(List(1, 2), List(1, 3), List(2, 3)))
    combinations(2, List(1, 2, 3, 4)) should be(List(List(1, 2), List(1, 3), List(1, 4), List(2, 3), List(2, 4), List(3, 4)))
  }
}
