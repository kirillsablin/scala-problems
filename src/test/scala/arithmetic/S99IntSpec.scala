package arithmetic

import org.scalatest._
import scala.concurrent.{Await, ExecutionContext}
import ExecutionContext.Implicits.global
import scala.async.Async.async
import scala.concurrent.duration._

class S99IntSpec extends FlatSpec with Matchers {

  import S99Int._

  "isPrime" should "determine if number is prime" in {
    1.isPrime should be (true)
    2.isPrime should be (true)
    3.isPrime should be (true)
    4.isPrime should be (false)
    5.isPrime should be (true)
    8.isPrime should be (false)
    9.isPrime should be (false)
    13.isPrime should be (true)
    25.isPrime should be (false)
  }

  it should "calculate fast for big prime numbers" in {

    val futureResult = async {
       1611623773.isPrime
    }

    Await.result(futureResult, 1 second) should be(true)

  }

  "isCoprimeTo" should "return true if and only if numbers are coprimes" in {
    35.isCoprimeTo(64) should be(true)
    35.isCoprimeTo(65) should be(false)
    35.isCoprimeTo(35) should be(false)
  }

}
