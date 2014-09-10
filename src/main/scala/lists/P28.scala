package lists

object P28 {
  def lsort[T](lst: List[List[T]]): List[List[T]] = lst.sortWith(_.length < _.length)

  def lsortFreq[T](lst: List[List[T]]): List[List[T]] = {
    val byLengths = lst.map(_.length).groupBy( k => k )
    lst.sortWith( (a,b) => byLengths(a.length).length < byLengths(b.length).length)

  }

}
