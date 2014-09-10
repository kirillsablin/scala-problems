package lists

object P11 {
  import lists.P10._

  def encodeModified(lst: List[Any]):List[Any] = encode(lst) map (x => if (x._1 == 1) x._2 else x)

}
