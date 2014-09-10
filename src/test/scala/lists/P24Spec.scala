package lists

import P24._
import org.scalatest._

class P24Spec extends FlatSpec with Matchers {

  "lotto" should "return elements less then range bound" in {
    lotto(6, 36) forall(_ < 37) should be(true)
  }

  it should "return required number of elements if bound is greater or equal to it" in {
    lotto(6, 36).length should be (6)
  }

}
