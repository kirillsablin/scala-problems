package misc

import org.scalatest._

class KnightsTourSpec extends FlatSpec with Matchers {
  import KnightsTour._

  val board = new Board(8)

  "validMoves" should "return lazy list of valid moves from given position" in {

    board.validMoves(Position(5, 5)).toList should be (List(Position(4,3), Position(4, 7), Position(3, 4), Position(3, 6),
      Position(6, 3), Position(6, 7), Position(7, 4), Position(7, 6)
    ))

    board.validMoves(Position(1, 1)).toList should be (List(
      Position(2, 3),
      Position(3, 2)
    ))
  }

  "tours" should "return stream of tours" in {
    val tour = board.tours(Position(1, 1)).head
    tour.length should be (64)
    tour.toSet.size should be (64)
    tour.last should be (Position(1, 1))
    tour.sliding(2).foreach {
      case p1 :: p2 :: Nil =>
        Math.abs(p1.x - p2.x) should (equal(2) or equal(1))
        Math.abs(p1.y - p2.y) should (equal(2) or equal(1))
      case _ =>
    }

  }

}
