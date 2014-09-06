import org.scalatest._
import Matchers.bePermutationOf

class P23Spec extends FlatSpec with Matchers {

  import P23._

  "randomSelect" should "return empty list if n is zero" in {
    randomSelect(0, List(1, 2)) should be(List())
  }

  it should "return empty list if source list is empty" in {

    randomSelect(10, List()) should be(List())

  }

  it should "return permutation of original list if n is greater or equal to length of list" in {
    randomSelect(1, List(1)) should be(List(1))
    randomSelect(3, List(1, 2, 3)) should bePermutationOf(List(1, 2, 3))
    randomSelect(10, List(1, 2, 3)) should bePermutationOf(List(1, 2, 3))
  }



}
