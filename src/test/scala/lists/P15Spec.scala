package lists

import P15._
import org.scalatest._

class P15Spec extends FlatSpec with Matchers {

  "duplicateN" should "return empty list on empty list for any N" in {
    duplicateN(10, List()) should be (List())
  }

  "duplicateN" should "return empty list on zero N for any list" in {
    duplicateN(0, List(1, 2, 3)) should be(List())
  }

  it should "return list with duplicates" in {
    duplicateN(3, List(1, 2)) should be(List(1, 1, 1, 2, 2, 2))
    duplicateN(3, List('a, 'b, 'c, 'c, 'd)) should
      be(List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd))
  }

}
