//trait type class of Show
trait Show[A] {
  def show(a: A): String
}

//Declare the Class IntType to Represent type of Integer
class IntType {
  implicit val intShow = new Show[Int] {
    //override the method show and pass the parameter of type integer
    override def show(numInt: Int): String = {
      if (numInt.isNaN) {
        throw new NumberFormatException("Invalid Number")
      }
      else {
        try numInt.toString
        catch {
          case ex: Exception => throw new Exception("Failed to Change to String" + ex.getMessage)
        }
      }
    }
  }
}

//Declare the Class LongType to Represent type of Long Integer
class LongType {
  implicit val longShow = new Show[Long] {
    //override the method show and pass the parameter of type long integer
    override def show(numLong: Long): String = {
      if (numLong.isNaN) {
        throw new NumberFormatException("Invalid Number")
      }
      else {
        try numLong.toString
        catch {
          case ex: Exception => throw new Exception("Failed to Change String" + ex.getMessage)
        }
      }
    }
  }
}

//Declare the Class DoubleType to Represent type of Double Integer
class DoubleType {
  implicit val doubleShow = new Show[Double] {
    //override the method show and pass the parameter of type double integer
    override def show(numDouble: Double): String = {
      if (numDouble.isNaN) {
        throw new NumberFormatException("Invalid Number")
      }
      else {
        try numDouble.toString
        catch {
          case ex: Exception => throw new Exception("Failed to Change String" + ex.getMessage)
        }
      }
    }
  }
}

//Declare the Class Position to Represent Position with respect to x-y axis
case class Position(x: Int, y: Int) {
  implicit val positionShow = new Show[Position] {
    //override the method show and pass the parameter of type Position Class
    override def show(p: Position): String = {
      val pos: String = s"Pos(x: ${p.x}, y: ${p.y})"
      try pos.toString
      catch {
        case ex: Exception => throw new Exception("Failed to Change the position to String" + ex.getMessage)
      }
    }
  }
}

//Singleton Object of Type Main
object TraitTypeClass extends App {
  val numInt = new IntType
  val numLong = new LongType
  val numDouble = new DoubleType
  val position = Position(0, 1)
  println("Integer Type: " + numInt.intShow.show(18000))
  println("Long Type: " + numLong.longShow.show(20000012020L))
  println("Double Type: " + numDouble.doubleShow.show(363636772.1212))
  println("Position: " + position.positionShow.show(position))
}