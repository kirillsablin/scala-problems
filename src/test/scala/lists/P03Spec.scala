package lists

import P03._
import org.scalatest._

class P03Spec extends FlatSpec with Matchers {

  def testMethod(method: (Int, List[Int]) => Int) {
    method(2, List(2, 3, 4, 5)) should be (4)
    method(0, List(2, 3, 4, 5)) should be (2)

    a [NoSuchElementException] should be thrownBy {
      method(2, List(2, 3))
    }

    a [NoSuchElementException] should be thrownBy {
      method(0, List())
    }

    a [NoSuchElementException] should be thrownBy {
      method(1, List())
    }
  }

  "nth" should "return Nth element of list if exists" in {
    testMethod(nth)
  }

  "native nth" should "behave same way as non-native" in {
    testMethod(nativeNth)
  }

}
