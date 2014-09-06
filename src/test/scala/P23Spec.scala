import org.scalactic.Prettifier
import org.scalatest._
import org.scalatest.matchers.{MatchResult, Matcher}

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

  def bePermutationOf[T](right: List[T]):Matcher[List[T]] = {
    new Matcher[List[T]] {
      def apply(left: List[T]): MatchResult = {
        // assume all elements are distinct
        val result = right.forall( left.contains(_) ) && right.length == left.length

        MatchResult(result, Prettifier.default(left) + "  is not a permutation of " + Prettifier.default(right),
          "is a permutation")
      }
      override val toString: String = "be a permutation of " + Prettifier.default(right)
    }
  }

}
