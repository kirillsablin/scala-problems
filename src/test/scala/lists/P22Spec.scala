package lists

import P22._
import org.scalatest._

class P22Spec extends FlatSpec with Matchers {

  "range" should "generate range of integers" in {
    range(2, 5) should be(List(2, 3, 4, 5))
    range(4, 2) should be(List())
  }

  it should "be able to generate big ranges" in {
    range(0, 10000)
  }


}
