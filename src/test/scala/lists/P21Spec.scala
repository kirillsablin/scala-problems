package lists

import P21._
import org.scalatest._

class P21Spec extends FlatSpec with Matchers {

  "insertAt" should "insert element at given position" in {
    insertAt(5, 1, List(1, 2, 3)) should be(List(1, 5, 2, 3))
    insertAt('new, 1, List('a, 'b, 'c, 'd)) should be(List('a, 'new, 'b, 'c, 'd))
  }

}
