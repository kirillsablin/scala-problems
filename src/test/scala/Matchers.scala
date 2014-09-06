import org.scalactic.Prettifier
import org.scalatest.matchers.{MatchResult, Matcher}

object Matchers {
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
