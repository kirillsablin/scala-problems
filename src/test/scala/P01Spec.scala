import org.scalatest._

class P01Spec extends FlatSpec with Matchers {

  import P01._

  def testMethod(method: List[Int] => Int):Unit = {
    method(List(1, 2, 3)) should be (3)
    method(List(2)) should be (2)
    a [NoSuchElementException] should be thrownBy {
      method(List())
    }

  }

  "last" should "return last element if it exists" in {
    testMethod(last)
  }

  "internalLast" should "return same results" in {
    testMethod(internalLast)
  }

  "functionalLast" should "return same results" in {
    testMethod(functionalLast)

  }
}
