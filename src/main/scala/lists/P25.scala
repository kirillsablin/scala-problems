package lists

import scala.collection.mutable.ArrayBuffer

object P25 {
  import lists.P23.randomSelect

  def randomPermute[T](lst: List[T]):List[T] = randomSelect(lst.length, lst)

  def inPlacePermute[T](lst: List[T]):List[T] = {
    val v = new ArrayBuffer[T]
    v.appendAll(lst)

    for (i <- v.length - 1 to 1 by -1) {
      val newIndex = util.Random.nextInt(i+1)
      val a1 = v(newIndex)
      v.update(newIndex, v(i))
      v.update(i, a1)
    }

    v.toList
  }
}
