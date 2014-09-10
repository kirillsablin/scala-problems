package arithmetic

import org.scalatest._

class P32Spec extends FlatSpec with Matchers {

  import P32._

  "gcd" should "be one if one of arguments is one" in {

    gcd(1, 1) should be(1)
    gcd(1, 2) should be(1)
    gcd(12, 1) should be (1)

  }

  it should "calculate gcd" in {
    gcd(8, 12) should be (4)
    gcd(36, 63) should be(9)
  }

}
