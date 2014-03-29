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

  def react(params: InputParam): String = {
    try {
      val pList = params.view.cells.view.zipWithIndex.filter { case (c, idx) => c == 'P' }
      // val pDist = pList.map { case(c, idx) =>
      //   val coordinate = view.params.relPosFromIndex(idx)
      //   Math.sqrt(Math.pow(coordinate.x, 2) + Math.pow(coordinate.y, 2))
      // }
      if (!pList.isEmpty) {
        val (_, idx) = pList.minBy { case (c, idx) => 
          val coordinate = params.view.relPosFromIndex(idx)
          Math.sqrt(Math.pow(coordinate.x, 2) + Math.pow(coordinate.y, 2))
        }
        val coordinate = params.view.relPosFromIndex(idx)
        val (x, y) = (
          coordinate.x match {
            case 0 => 0
            case x if x < 0 => -1
            case _ => 1
          },
          coordinate.y match {
            case 0 => 0
            case y if y < 0 => -1
            case _ => 1
          }
        )
        val xy = XY(x, y) 
        println('xy, xy)
        
        // val xy = XY(1, 1)
        "Move(direction=%s)".format(xy)
      } else {
        val xy = XY(0, 0)
        "Move(direction%s)".format(xy)
      }
    } catch {
      case e =>
        println(e)
        ""
    }
  }
}
