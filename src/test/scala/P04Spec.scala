import org.scalatest._

class P04Spec extends FlatSpec with Matchers {
  import P04._

  "len" should "return 0 for empty list" in {
    len(List()) should be(0)
  }

  it should "return length of list" in {
    len(List(1,2,3)) should be(3)
    len(List("a", "b", "c", "d")) should be(4)
  }

  def assertMethodBehaviorSameAsNative(method: (List[_]) => Int):Unit = {
    val lists = List(List(1,2,3), List(), List(1))

    for (list <- lists)
      method(list) should be(list.length)
  }

  it should "be same as native length" in {
    assertMethodBehaviorSameAsNative(len)
  }

  "functional length" should "behave same as native length" in {
    assertMethodBehaviorSameAsNative(funcLen)
  }

}
