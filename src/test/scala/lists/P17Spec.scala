package lists

import P17._
import org.scalatest._

class P17Spec extends FlatSpec with Matchers {

  "split" should "return tuple of two empty list on empty list" in {
    split(2, List()) should be ((List(), List()))
  }

  it should "return original and empty list if index out of list" in {
    split(100, List(1, 2, 3)) should be(List(1, 2, 3), List())
  }

  it should "split list by n" in {
    split(2, List(1, 2, 3, 4)) should be(List(1,2), List(3, 4))
    split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should
      be ((List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  }

}
