import org.scalatest._

class P06Spec extends FlatSpec with Matchers {
  import P06._

  "isPalindrome" should "return true on empty list" in {
    isPalindrome(List()) should be (true)
  }

  it should "return true on one element lists" in {
    isPalindrome(List(1)) should be (true)
    isPalindrome(List(2)) should be (true)
  }

  it should "return true only if palindrome" in {
    isPalindrome(List(2,2)) should be (true)
    isPalindrome(List(2,3)) should be (false)
    isPalindrome((1 to 10000).map(_ => 1).toList) should be (true)
    isPalindrome(List(1,2,1)) should be (true)
    isPalindrome(List(1,2,3)) should be (false)
  }

}
