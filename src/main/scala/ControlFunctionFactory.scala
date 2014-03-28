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
          View(keyValuePairs("view")))) + "|Status(text=uji-1.0)"
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
  var history = Vector[XY]() // collection.immutable.Queue[XY]()
  var current = XY(0, 0)

  def react(params: InputParam): String = {
    history :+= current
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
    // println('enemies, enemies.size, 'goods, goods.size)

    val scores: Seq[(XY, Double)] = for { xy <- xys } yield {
      val v1: Int = aroundScore(params.view, xy)
      val v2: Double = history.view.zipWithIndex.filter { case (e, _) => e == xy }.map { case (_, i) => i - 1000 }.sum
      val v3: Double = enemies.map {
        case (c, idx) =>
          val enemyXY = params.view.relPosFromIndex(idx)
          // TODO Don't depend on direct distance but path distance
          -1.0 / (xy.stepsTo(enemyXY) + 1)
        case _ => println('omgomg); 0
      }.sum
      // almost dead copy of above..
      val v4: Double = goods.map {
        case (c, idx) =>
          val goodXY = params.view.relPosFromIndex(idx)
          // TODO Don't depend on direct distance but path distance
          1.0 / (xy.stepsTo(goodXY) + 1)
        case _ => println('omgomg); 0
      }.sum
      // random weight for safe spot
      val v5 = 0 // TODO if (util.Random.nextInt(100) == 0) 1 else 0

      (xy, 2.0 * v1 + 0.0005 * v2 + 1.0 * v3 + 1.0 * v4 + 1.0 * v5)
    }
    // TODO no need for granularity
    val (_, bests): (Double, Seq[(XY, Double)]) = scores.
      groupBy { case (xy: XY, v: Double) => v }.
      maxBy { case (v: Double, x: Seq[(XY, Double)]) => v }
    val (xy: XY, _) = bests(util.Random.nextInt(bests.size))
    current = current + xy
    "Move(direction=%s)".format(xy)
  }
}
