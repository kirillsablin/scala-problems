package misc

object KnightsTour {
  case class Position(x:Int, y:Int) {
    def moved(dX:Int, dY:Int):Position = Position(x + dX, y + dY)
  }

  class Board(n: Int) {

    def validMoves(position: Position):Stream[Position] =
      List(
        position.moved(-1, -2),
        position.moved(-1, +2),
        position.moved(-2, -1),
        position.moved(-2, +1),
        position.moved(+1, -2),
        position.moved(+1, +2),
        position.moved(+2, -1),
        position.moved(+2, +1)

      ).filter(p => p.x > 0 && p.x <= n && p.y > 0 && p.y <= n).toStream

    def tours(start: Position):Stream[List[Position]] = {
      def toursR(visited:Set[Position], path:List[Position]):Stream[List[Position]] =
        if (visited.size == n * n) {
          List(path).toStream
        } else {
          validMoves(path.head).filter(!visited.contains(_)).flatMap(np => toursR(visited + np, np::path))
        }

      toursR(Set(start), List(start))
    }

    def closedTours(start: Position):Stream[List[Position]] =
      tours(start).filter( p => validMoves(p.head).contains(start))

  }

}
