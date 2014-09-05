import org.scalatest._

class p11Spec extends FlatSpec with Matchers {
  import P11._

  "encodeModified" should "not encode single-sized sequences" in {
    encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should
      be (List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e)))

  }

}
