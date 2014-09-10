package lists

import P28._
import org.scalatest._

class P28Spec extends FlatSpec with Matchers {

  "lsort" should "sort by sub lists lengths" in {
    lsort(List(List(1, 2, 3), List(1), List(1, 2))) should be (List(List(1), List(1, 2), List(1, 2, 3)))

    lsort(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))) should
      be (List(List('o), List('d, 'e), List('d, 'e), List('m, 'n), List('a, 'b, 'c), List('f, 'g, 'h), List('i, 'j, 'k, 'l)))
  }


  "lsortFreq" should "sort by frequency of sub lists lengths" in {
    lsortFreq(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))) should
      be (List(List('i, 'j, 'k, 'l), List('o), List('a, 'b, 'c), List('f, 'g, 'h), List('d, 'e), List('d, 'e), List('m, 'n)))

  }

}
