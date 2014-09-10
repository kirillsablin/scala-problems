package lists

import P09._
import org.scalatest._

class P09Spec extends FlatSpec with Matchers {

  "pack" should "return empty list on empty list" in {
    pack(List()) should be (List())
  }

  it should "return list of singleton lists on distinct list" in {
    pack(List(1)) should be(List(List(1)))
    pack(List(1, 2, 3)) should be(List(List(1), List(2), List(3)))

  }

  it should "remove consecutive duplicates" in {
    pack(List(1,1,1)) should be (List(List(1, 1, 1)))
    pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should
      be (List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)))
  }

  it should "work for big lists" in {
    val bigList = (1 to 10000).toList map (List(_))

    pack(bigList)
  }

}
