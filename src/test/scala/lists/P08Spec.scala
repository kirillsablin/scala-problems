package lists

import P08._
import org.scalatest._

class P08Spec extends FlatSpec with Matchers {

  "compress" should "return empty array " in {
    compress(List()) should be (List())
  }

  it should "return list without consecutive duplicates as is" in {
    compress(List(1, 2, 3, 2, 1)) should be (List(1, 2, 3, 2, 1))
  }

  it should "remove repeated consecutive elements" in {
    compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be(List('a, 'b, 'c, 'a, 'd, 'e))
  }

  it should "work for big lists" in {
    val bigList = (1 to 100000).toList

    compress(bigList) should be(bigList)
  }

  "compressFunctional" should "behave same way as normal one" in {
    compressFunctional(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be(List('a, 'b, 'c, 'a, 'd, 'e))
  }

}
