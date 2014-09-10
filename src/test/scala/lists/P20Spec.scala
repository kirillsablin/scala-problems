package lists

import P20._
import org.scalatest._

class P20Spec extends FlatSpec with Matchers  {

  "removeAt" should "extract element on given place" in {
    removeAt(0, List(1, 2, 3)) should be (List(2, 3), 1)
    removeAt(1, List('a, 'b, 'c, 'd)) should be ((List('a, 'c, 'd),'b))
    removeAt(2, List(1, 2, 3)) should be (List(1, 2), 3)
  }

  it should "throw exception if index out of bounds" in {

    a [NoSuchElementException] should be thrownBy {
      removeAt(5, List(1, 2))
    }

    a [NoSuchElementException] should be thrownBy {
      removeAt(-1, List(1, 2, 3))
    }
  }

}
