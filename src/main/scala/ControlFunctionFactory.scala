// based on src/main/scala/Bot.scala of Reference
import scala.util.Random

case class InputParam(
  name: String, collision: Option[String], generation: Long, energy: Long, time: Long, slaves: Option[Long], view: View) {
  
  // just to ignore view
  override def toString =
    """InputParam{name: "%s", collision: "%s", generation: %d, energy: %d, time: %d, slaves: %s}""" format (name, collision, generation, energy, time, slaves)
}

private object ControlFunctionFactory {
  /** "Command(..)" => ("Command", Map( ("key" -> "value"), ("key" -> "value"), ..}) */
  def parse(command: String): (String, Map[String, String]) = {
    /** "key=value" => ("key","value") */
    def splitParameterIntoKeyValue(param: String): (String, String) = {
      val segments = param.split('=')
      (segments(0), if(segments.length>=2) segments(1) else "")
    }

    val segments = command.split('(')
    if( segments.length != 2 )
      throw new IllegalStateException("invalid command: " + command)
    val opcode = segments(0)
    val params = segments(1).dropRight(1).split(',')
    val keyValuePairs = params.map(splitParameterIntoKeyValue).toMap
    (opcode, keyValuePairs)
  }
}

class ControlFunctionFactory {
  import ControlFunctionFactory._
  var bot: Option[Bot] = None

  def create: String => String = (input: String) => {
    val (opcode, keyValuePairs) = parse(input)
    (opcode, bot) match {
      case ("React", Some(bot)) => {
        bot.react(InputParam(
          keyValuePairs("name"),
          keyValuePairs.get("collision"),
          keyValuePairs("generation").toLong,
          keyValuePairs("energy").toLong,
          keyValuePairs("time").toLong,
          keyValuePairs.get("slaves").map(_.toLong),
          View(keyValuePairs("view"))))
      }
      case ("Welcome", _) =>
        bot = Some(Bot())
        ""
      case e =>
        println('e, e)
        "" // OK
    }
  }
}



case class XY(x: Int, y: Int) {
    override def toString = x + ":" + y

    def isNonZero = x != 0 || y != 0
    def isZero = x == 0 && y == 0
    def isNonNegative = x >= 0 && y >= 0

    def updateX(newX: Int) = XY(newX, y)
    def updateY(newY: Int) = XY(x, newY)

    def addToX(dx: Int) = XY(x + dx, y)
    def addToY(dy: Int) = XY(x, y + dy)

    def +(pos: XY) = XY(x + pos.x, y + pos.y)
    def -(pos: XY) = XY(x - pos.x, y - pos.y)
    def *(factor: Double) = XY((x * factor).intValue, (y * factor).intValue)

    def distanceTo(pos: XY): Double = (this - pos).length // Phythagorean
    def length: Double = math.sqrt(x * x + y * y) // Phythagorean

    def stepsTo(pos: XY): Int = (this - pos).stepCount // steps to reach pos: max delta X or Y
    def stepCount: Int = x.abs.max(y.abs) // steps from (0,0) to get here: max X or Y

    def signum = XY(x.signum, y.signum)

    def negate = XY(-x, -y)
    def negateX = XY(-x, y)
    def negateY = XY(x, -y)

    /** Returns the direction index with 'Right' being index 0, then clockwise in 45 degree steps. */
    def toDirection45: Int = {
        val unit = signum
        unit.x match {
            case -1 =>
                unit.y match {
                    case -1 =>
                        if(x < y * 3) Direction45.Left
                        else if(y < x * 3) Direction45.Up
                        else Direction45.UpLeft
                    case 0 =>
                        Direction45.Left
                    case 1 =>
                        if(-x > y * 3) Direction45.Left
                        else if(y > -x * 3) Direction45.Down
                        else Direction45.LeftDown
                }
            case 0 =>
                unit.y match {
                    case 1 => Direction45.Down
                    case 0 => throw new IllegalArgumentException("cannot compute direction index for (0,0)")
                    case -1 => Direction45.Up
                }
            case 1 =>
                unit.y match {
                    case -1 =>
                        if(x > -y * 3) Direction45.Right
                        else if(-y > x * 3) Direction45.Up
                        else Direction45.RightUp
                    case 0 =>
                        Direction45.Right
                    case 1 =>
                        if(x > y * 3) Direction45.Right
                        else if(y > x * 3) Direction45.Down
                        else Direction45.DownRight
                }
        }
    }

    def rotateCounterClockwise45 = XY.fromDirection45((signum.toDirection45 + 1) % 8)
    def rotateCounterClockwise90 = XY.fromDirection45((signum.toDirection45 + 2) % 8)
    def rotateClockwise45 = XY.fromDirection45((signum.toDirection45 + 7) % 8)
    def rotateClockwise90 = XY.fromDirection45((signum.toDirection45 + 6) % 8)


    def wrap(boardSize: XY) = {
        val fixedX = if(x < 0) boardSize.x + x else if(x >= boardSize.x) x - boardSize.x else x
        val fixedY = if(y < 0) boardSize.y + y else if(y >= boardSize.y) y - boardSize.y else y
        if(fixedX != x || fixedY != y) XY(fixedX, fixedY) else this
    }
}

object Direction45 {
    val Right = 0
    val RightUp = 1
    val Up = 2
    val UpLeft = 3
    val Left = 4
    val LeftDown = 5
    val Down = 6
    val DownRight = 7
}


object Direction90 {
    val Right = 0
    val Up = 1
    val Left = 2
    val Down = 3
}

object XY {
    /** Parse an XY value from XY.toString format, e.g. "2:3". */
    def apply(s: String) : XY = { val a = s.split(':'); XY(a(0).toInt,a(1).toInt) }

    val Zero = XY(0, 0)
    val One = XY(1, 1)

    val Right = XY(1, 0)
    val RightUp = XY(1, -1)
    val Up = XY(0, -1)
    val UpLeft = XY(-1, -1)
    val Left = XY(-1, 0)
    val LeftDown = XY(-1, 1)
    val Down = XY(0, 1)
    val DownRight = XY(1, 1)

    def randomDirection90(rnd: Random) = fromDirection90(rnd.nextInt(4))
    def randomDirection45(rnd: Random) = fromDirection45(rnd.nextInt(8))

    def fromDirection45(index: Int): XY = index match {
        case Direction45.Right => Right
        case Direction45.RightUp => RightUp
        case Direction45.Up => Up
        case Direction45.UpLeft => UpLeft
        case Direction45.Left => Left
        case Direction45.LeftDown => LeftDown
        case Direction45.Down => Down
        case Direction45.DownRight => DownRight
    }

    def fromDirection90(index: Int): XY = index match {
        case Direction90.Right => Right
        case Direction90.Up => Up
        case Direction90.Left => Left
        case Direction90.Down => Down
    }


    def randomUnit(rnd: Random) = XY(rnd.nextInt(3) - 1, rnd.nextInt(3) - 1)

    def apply(array: Array[Int]): XY = XY(array(0), array(1))
}

case class View(cells: String) {
    val size = math.sqrt(cells.length).toInt
    val center = XY(size / 2, size / 2)

    // def apply(relPos: XY) = cellAtRelPos(relPos)

    def indexFromAbsPos(absPos: XY) = absPos.x + absPos.y * size
    def absPosFromIndex(index: Int) = XY(index % size, index / size)
    def absPosFromRelPos(relPos: XY) = relPos + center
    def cellAtAbsPos(absPos: XY) = cells.charAt(indexFromAbsPos(absPos))

    def indexFromRelPos(relPos: XY) = indexFromAbsPos(absPosFromRelPos(relPos))
    def relPosFromAbsPos(absPos: XY) = absPos - center
    def relPosFromIndex(index: Int) = relPosFromAbsPos(absPosFromIndex(index))
    def cellAtRelPos(relPos: XY) = cells.charAt(indexFromRelPos(relPos))

    def offsetToNearest(c: Char) = {
        val matchingXY = cells.view.zipWithIndex.filter(_._1 == c)
        if( matchingXY.isEmpty )
            None
        else {
            val nearest = matchingXY.map(p => relPosFromIndex(p._2)).minBy(_.length)
            Some(nearest)
        }
    }
}

private object Bot {
  def aroundScore(view: View, xy: XY): Int = {
    view.cellAtRelPos(xy) match {
      case '?' | 'W' | 'm' | 's' | 'p' | 'b' => -1
      case 'P' | 'B' => 1
      case '_' | 'M' | 'S' => 0
      case _ => println('omg); 0
    }
  }
}

case class Bot() {
  import Bot._
  var history = collection.immutable.Queue[XY]()

  def react(params: InputParam): String = {
    history :+= params.view.center
    if (history.size > 1000) {
      history = history.tail
    }

    val xys = for {
      x <- -1 to 1
      y <- -1 to 1
    } yield XY(x, y)

    val enemies = params.view.cells.view.zipWithIndex.filter {
      case ('m' | 's' | 'p' | 'b', _) => true
      case _ => false
    }
    val goods = params.view.cells.view.zipWithIndex.filter {
      case ('P' | 'B', _) => true
      case _ => false
    }

    val nearMap: Seq[(XY, Double)] = for { xy <- xys } yield {
      val v1: Int = aroundScore(params.view, xy)
      val v2: Int = history.filter(_ == xy).size
      val v3: Double = enemies.map {
        case (c, idx) =>
          val enemyXY = params.view.relPosFromIndex(idx)
          -1.0 / (xy.distanceTo(enemyXY) + 1)
        case _ => println('omgomg); 0
      }.sum
      // almost dead copy of above..
      val v4: Double = goods.map {
        case (c, idx) =>
          val goodXY = params.view.relPosFromIndex(idx)
          1.0 / (xy.distanceTo(goodXY) + 1)
        case _ => println('omgomg); 0
      }.sum

      // println(v1, v2, v3, v4)
      (xy, 1.0 * v1 + 0.1 * v2 + 1.0 * v3 + 1.0 * v4)
    }
    val (_, bests): (Double, Seq[(XY, Double)]) = nearMap.
      groupBy { case (xy: XY, v: Double) => v }.
      maxBy { case (v: Double, x: Seq[(XY, Double)]) => v }
    val (xy: XY, _) = bests(util.Random.nextInt(bests.size))
    "Move(direction=%s)".format(xy)
  }
}
