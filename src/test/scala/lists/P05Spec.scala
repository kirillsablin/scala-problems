package lists

import P05._
import org.scalatest._

class P05Spec extends FlatSpec with Matchers {

  def assertBehaveAsNative( reverseFunction: (List[Int]) => List[Int]):Unit = {
    val lists = List(List(), List(1), List(1,2,3), (1 to 100000).toList)

    for (list <- lists)
      reverseFunction(list) should be (list.reverse)

  }

  "reverse" should "behave same way as native" in {
    assertBehaveAsNative(reverse)
  }

  "functionalReverse" should "behave same way as native" in {
    assertBehaveAsNative(functionalReverse)

  }

}
