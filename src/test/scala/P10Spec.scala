import org.scalatest._

class P10Spec extends FlatSpec with Matchers {

  import P10._

  "encode" should "return empty list on empty list" in {
    encode(List()) should be (List())
  }

  "encode" should "return lengths of consequences with element" in {
    encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should
      be (List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
  }
}
