package solver

import akka.actor.ActorSystem
import akka.pattern.ask
import akka.util.Timeout
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

/**
  * Created by wojciech on 23.02.17.
  */
class ActorSpec  extends FlatSpec with Matchers {

  import BoardImplSpec._

  " Nonogram actors" should " g " in {

    val board = new BoardImpl(columns, rows)
    val system = ActorSystem("system")
    val master = system.actorOf(Master.props(board),"master")
    implicit val timeout = Timeout(5.seconds)
    val future = master ? Master.Task(board)
    future.collect{
      case Master.Task(res) => res
    }.foreach{res =>
      List.range(0, res.height).map(res.rowFields) should contain theSameElementsInOrderAs result
      system.terminate()
    }
  }

}
