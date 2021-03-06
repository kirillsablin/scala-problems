package logic

import org.scalatest._

class S99LogicSpec extends FlatSpec with Matchers {

  import S99Logic._

  "and" should "return true if and only if both arguments are true" in {
    and(true, true) should be (true)
    and(false, true) should be (false)
    and(true, false) should be (false)
    and(false, false) should be (false)
  }

  "or" should "return true if any of arguments is true" in {
    or(true, true) should be(true)
    or(true, false) should be(true)
    or(false, true) should be(true)
    or(false, false) should be(false)
  }


  "nand" should "return false if and only if both arguments are true" in {
    nand(true, true) should be (false)
    nand(false, true) should be (true)
    nand(true, false) should be (true)
    nand(false, false) should be (true)
  }

  "table2" should "generate truth table for given function" in {
    table2((a: Boolean, b: Boolean) => and(a, or(a, b))) should
      be (List(
        ((true, true), true),
        ((true, false), true),
        ((false, true), false),
        ((false, false), false)
      ))
  }

  "boolean function" should "be able to be used in infix form" in {
    table2((a: Boolean, b: Boolean) => a and (a or notOp(b))) should
      be (List(
        ((true, true), true),
        ((true, false), true),
        ((false, true), false),
        ((false, false), false)
      ))
  }

  "gray" should "return 0,1 list for 1" in {
    gray(1) should be (List("0", "1"))
  }

  it should "return List of previous with prepended 0 and reversed previous with prepened 1" in {
    gray(2) should be(List("00", "01", "11", "10"))
    gray(3) should be(List("000", "001", "011", "010", "110", "111", "101", "100"))
  }

  "huffman" should "return codes for list with frequencies" in {
    huffman(List(("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5))) should
      be(List(("a","0"), ("c","100"), ("b","101"), ("f","1100") ,("e","1101"), ("d","111")))
  }


}
