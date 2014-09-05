import org.scalatest._

class P12Spec extends FlatSpec with Matchers {
  import P12._

  "decode" should "decode empty list to empty list" in {
    decode(List()) should be (List())
  }

  it should "decode pairs to sequences" in {
    decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) should
      be (List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  }
}
