package lists

import P02._
import org.scalatest._

class P02Spec extends FlatSpec with Matchers {

  def testMethod(method: List[Int] => Int):Unit = {
    method(List(1,2,3,4)) should be (3)
    method(List(1,2)) should be (1)

    a [NoSuchElementException] should be thrownBy {
      method(List(1))
    }

    a [NoSuchElementException] should be thrownBy {
      method(List())
    }

  }

  it should "return last but one element if exists" in {
    testMethod(penultimate)
  }

  "native based" should "return same as normal one" in {
    testMethod(penultimateNativeBased)
  }

}
