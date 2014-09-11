package arithmetic

import org.scalatest._
import scala.concurrent.{Await, ExecutionContext}
import ExecutionContext.Implicits.global
import scala.async.Async.async
import scala.concurrent.duration._

class S99IntSpec extends FlatSpec with Matchers {

  import S99Int._

  val bigPrime = 1611623773

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
      bigPrime.isPrime
    }

    Await.result(futureResult, 1 second) should be(true)
  }

  "isCoprimeTo" should "return true if and only if numbers are coprimes" in {
    35.isCoprimeTo(64) should be(true)
    35.isCoprimeTo(65) should be(false)
    35.isCoprimeTo(35) should be(false)
  }

  "totient" should "calculate number of coprime numbers, lesser or equals to given number" in {
    10.totient should be (4)
    13.totient should be (12)
  }

  "primeFactors" should "return empty list for 1" in {
    1.primeFactors should be (List())
  }

  it should "return singleton lists with given number for prime numbers" in {

    for (primeNumber <- List(2, 3, 13, 29))
      primeNumber.primeFactors should be (List(primeNumber))

  }

  it should "return list of prime factors if not prime number" in {
    315.primeFactors should be (List(3, 3, 5, 7))
    32.primeFactors should be (List(2, 2, 2, 2, 2))
    12.primeFactors should be (List(2, 2, 3))
    15.primeFactors should be (List(3, 5))
    9.primeFactors should be (List(3, 3))
  }

  it should "calculates fast for big prime numbers" in {
    val futureResult = async {
      bigPrime.primeFactors
    }

    Await.result(futureResult, 1 second) should be (List(bigPrime))
  }

  "primeFactorMultiplicity" should "return row encoded prime factors" in {
    315.primeFactorMultiplicity should be (List((3,2), (5,1), (7,1)))
  }

  "totientImproved" should "calculate totient number same way as regular one" in {
    10.totientImproved should be (4)
    13.totientImproved should be (12)
  }

  it should "works faster then regular one " in {

    val regularFutureResult = async {
      10090.totient
    }

    a [java.util.concurrent.TimeoutException] should be thrownBy {
      Await.result(regularFutureResult, 5 millis)
    }


    val improvedFutureResult = async {
      10090.totientImproved
    }

    Await.result(improvedFutureResult, 5 millis) should be (4032)

  }

}
