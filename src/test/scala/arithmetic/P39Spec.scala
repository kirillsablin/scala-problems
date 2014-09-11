package arithmetic

import org.scalatest._

class P39Spec extends FlatSpec with Matchers {

  import P39._

  "listPrimesInRange" should "generate prime numbers in range" in {
    listPrimesInRange(7 to 31) should be (List(7, 11, 13, 17, 19, 23, 29, 31))
  }

}
