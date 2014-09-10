package lists

object P06 {
  def isPalindrome[T](lst: List[T]):Boolean = lst == lst.reverse
}
