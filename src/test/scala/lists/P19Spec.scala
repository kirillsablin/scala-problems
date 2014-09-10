package lists

import P19._
import org.scalatest._

class P19Spec extends FlatSpec with Matchers {

  "rotate" should "return same list if n is zero" in {
    rotate(0, List(1, 2)) should be (List(1, 2))
  }

  it should "return same list if n equals to length of list" in {
    rotate(3, List(1, 2, 3)) should be (List(1, 2, 3))
  }

  it should "return rotated list" in {
    rotate(1, List(1, 2, 3)) should be (List(2, 3, 1))
    rotate(2, List(1, 2, 3)) should be (List(3, 1, 2))
  }

  it should "rotate several times if n is bigger then length of list" in {
    rotate(10, List(1, 2, 3)) should be (List(2, 3, 1))
  }

  it should "rotate is other direction if n is negative" in {
     rotate(-1, List(1, 2, 3)) should be (List(3, 1, 2))
  }

}
