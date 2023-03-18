import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class TraitTypeClassTest extends AnyFlatSpec with Matchers {
  //Instance of All the Classes
  val numInt = new IntType
  val numLong = new LongType
  val numDouble = new DoubleType
  val position = Position(0, 1)

  //It Check with the integer type and gives Positive Result
  "Integer type" should " match with integer after converting to String " in {
    val actualOutput = numInt.intShow.show(18000)
    val expectedOutput = "18000"
    actualOutput shouldBe expectedOutput
  }

  //It Check with the float type and gives Negative Result
  "Integer type" should "not match with float after converting to String" in {
    val actualOutput = numInt.intShow.show(18000)
    val expectedOutput = "1800.0"
    assert(actualOutput != expectedOutput)
  }

  //It Check with the long integer type and gives Positive Result
  "Long type" should " match with long integer after converting to String " in {
    val actualOutput = numLong.longShow.show(20000012020L)
    val expectedOutput = "20000012020"
    actualOutput shouldBe expectedOutput
  }

  //It Check with the float type and gives Negative Result
  "Long type" should "not match with float integer after converting to String " in {
    val actualOutput = numLong.longShow.show(20000012020L)
    val expectedOutput = "2000001202.0"
    assert(actualOutput != expectedOutput)
  }

  //It Check with the double integer type and gives Positive Result
  "Double type" should " match with double integer after converting to String " in {
    val actualOutput = numDouble.doubleShow.show(363636772.1212)
    val expectedOutput = "3.636367721212E8"
    actualOutput shouldBe expectedOutput
  }

  //It Check with the float type and gives Negative Result
  "Double type" should "not match with float integer after converting to String " in {
    val actualOutput = numDouble.doubleShow.show(363636772.1212)
    val expectedOutput = "2000001202.0"
    assert(actualOutput != expectedOutput)
  }

  //It Check with the Position and gives Positive Result
  "Position" should " match with Position of x and y after converting to String " in {
    val actualOutput = position.positionShow.show(position)
    val expectedOutput = "Pos(x: 0, y: 1)"
    actualOutput shouldBe expectedOutput
  }

  //It Check with the Position another than parameter and gives Negative Result
  "Position" should "not match with Position of x and y after converting to String after passing" in {
    val actualOutput = position.positionShow.show(position)
    val expectedOutput = "Pos(x: 1, y: 0)"
    assert(actualOutput != expectedOutput)
  }
}
