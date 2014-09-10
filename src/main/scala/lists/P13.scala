package lists

object P13 {
  def encodeDirect[T](lst: List[T]): List[(Int, T)] = lst.foldRight[List[(Int, T)]](List())((current, soFar) =>
    soFar match {
      case (count, elem) :: xs if elem == current => (count + 1, elem) :: xs
      case Nil => List((1, current))
      case _ => (1, current) :: soFar
    }
  )
}
