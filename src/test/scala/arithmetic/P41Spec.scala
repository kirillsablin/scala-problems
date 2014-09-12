package arithmetic

import org.scalatest._

import scala.collection.immutable.Range.Inclusive

class P41Spec extends FlatSpec with Matchers {

  import P41._

  "goldbachList" should "return all goldbach sums inside range" in {
    goldbachList(9 to 20) should be (List(
      (10, 3, 7),
      (12, 5, 7),
      (14, 3, 11),
      (16, 3, 13),
      (18, 5, 13),
      (20, 3, 17)
    ))
  }

  "goldbachListLimited" should "return only sums who has at least one number bigger then threshold" in {
    goldbachListLimited(1 to 2000, 50) should be (List(
      (992, 73, 919),
      (1382, 61, 1321),
      (1856, 67, 1789),
      (1928, 61, 1867)
    ))
  }
}
