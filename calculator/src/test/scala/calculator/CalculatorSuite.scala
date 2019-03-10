package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

import TweetLength.MaxTweetLength

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /******************
   ** TWEET LENGTH **
   ******************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }


  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }

  def checkPolynomialCalculation(a: Double, b: Double, c: Double, expectedResult: Set[Double]): Unit = {
    val sigA = Signal(a)
    val sigB = Signal(b)
    val sigC = Signal(c)
    val delta = Polynomial.computeDelta(sigA, sigB, sigC)

    val result = Polynomial.computeSolutions(sigA, sigB, sigC, delta)
    assert(result() == expectedResult)
  }

  test("polynomials: no solution") {
    checkPolynomialCalculation(30d, 20d, 10d, Set())
    checkPolynomialCalculation(0d, 0d, 0d, Set())
  }

  test("polynomials: one solution") {
    checkPolynomialCalculation(0d, 4d, 1d, Set(1 / 4d))
    checkPolynomialCalculation(0d, 8d, 16d, Set(2d))
  }

  test("polynomials: two solutions") {
    checkPolynomialCalculation(1d, 5d, 6d, Set(-2d, -3d))
    checkPolynomialCalculation(3d, 4d, 1d, Set(-1d, -1d / 3d))
  }

  test("polynomials: NaN") {
    checkPolynomialCalculation(Double.NaN, 2d, 3d, Set())
    checkPolynomialCalculation(1d, Double.NaN, 3d, Set())
    checkPolynomialCalculation(1d, 2d, Double.NaN, Set())
  }

  def testComputeValue(a: Expr, b: Expr, expectedValueForB: Double): Unit = {
    val sigA = Signal(a)
    val sigB = Signal(b)

    val map: Map[String, Signal[Expr]] = Map("a" -> sigA, "b" -> sigB)
    val results = Calculator.computeValues(map)

    if (expectedValueForB.isNaN) {
      assert(results("b")().isNaN)
    } else {
      assert(results("b")() == expectedValueForB)
    }
  }

  val six = Literal(6.0)
  val three = Literal(3.0)

  test("calculator: plus") {
    testComputeValue(six, Plus(Ref("a"), three), 9.0)
    testComputeValue(six, Plus(Ref("a"), Ref("a")), 12.0)
  }

  test("calculator: minus") {
    testComputeValue(six, Minus(Ref("a"), three), 3.0)
    testComputeValue(six, Minus(Ref("a"), Ref("a")), 0.0)
  }

  test("calculator: times") {
    testComputeValue(six, Times(Ref("a"), three), 18.0)
    testComputeValue(six, Times(Ref("a"), Ref("a")), 36.0)
  }

  test("calculator: divide") {
    testComputeValue(six, Divide(Ref("a"), three), 2.0)
    testComputeValue(six, Divide(Ref("a"), Ref("a")), 1.0)
  }

  test("calculator: non existing var") {
    testComputeValue(Ref("c"), Ref("a"), Double.NaN)
  }

  test("calculator: cyclic reference") {
    testComputeValue(Ref("b"), Ref("a"), Double.NaN)
  }

  test("calculator: self dependency") {
    testComputeValue(Ref("a"), Ref("b"), Double.NaN)
  }

}
