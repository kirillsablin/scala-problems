package misc

object EightQueens {
  case class Queen(x:Int, y:Int)

  def isValidPosition(queens: List[Queen], newX: Int, newY: Int):Boolean = queens.forall(
    q =>  q.y != newY && Math.abs(q.x - newX) != Math.abs(q.y - newY)
  )

  def findSolution:List[Queen] = {
    def findSolutionR(prev:List[Queen], currentX:Int):Option[List[Queen]] =
      (1 to 8).filter(isValidPosition(prev, currentX, _)).map(newY =>
        if (currentX == 8)
          Some(Queen(currentX, newY) :: prev)
        else
          findSolutionR(Queen(currentX, newY) :: prev, currentX + 1)
      ).find(_.isDefined).flatten

    findSolutionR(List(), 1).get
  }

}
