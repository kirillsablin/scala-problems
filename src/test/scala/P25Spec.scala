import org.scalatest._
import Matchers.bePermutationOf

class P25Spec extends FlatSpec with Matchers {

  import P25._

  "randomPermute" should "generate permutation of list" in {
    randomPermute(List(1, 2, 3)) should bePermutationOf(List(1, 2, 3))
  }

  "inPlacePermute" should "generate permutation of list" in {
    inPlacePermute(List(1, 2, 3)) should bePermutationOf(List(1, 2, 3))
  }

}
