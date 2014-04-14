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
          View(keyValuePairs("view")))) + "|Status(text=uji-2.0)"
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
      // 'W' is already filtered
      case '?' | 'm' | 's' | 'p' | 'b' => -1
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
      val xy = XY(x, y)
      if params.view.cellAtRelPos(xy) != 'W'
    } yield xy

    val unitPreference = params.view.cells.view.zipWithIndex.map {
      case ('P' | 'B', i) => (1, i)
      case ('m' | 's' | 'p' | 'b', i) => (-1, i)
      case (_, i) => (0, i) // ???
    }

    val scores: Seq[(XY, Double)] = for { xy <- xys } yield {
      val v1: Int = aroundScore(params.view, xy)
      val v2: Double = history.view.zipWithIndex.
        filter { case (e, _) => e == xy }.
        map { case (_, i) => - Math.pow(i - 1000, 2) / Math.pow(1000, 2) }.
        sum
      val v3: Double = unitPreference.map {
        case (c, idx) =>
          val enemyXY = params.view.relPosFromIndex(idx)
          c / (xy.distanceTo(enemyXY) + 1)
        case _ => println('omgomg); 0
      }.sum
      val v4 = if (util.Random.nextInt(100) == 0) 1 else 0

      val v5 = history.take(10).distinct.map(xy.stepsTo(_)).sum / 10.0

      (xy, 2.0 * v1 + 1.0 * v2 + 2.0 * v3 + 0.1 * v4 + 0.5 * v5)
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
