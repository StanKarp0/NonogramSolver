package solver

import akka.actor.{ActorSystem, Props}
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

  " Nonogram actors small " should " g " in {

    val system = ActorSystem("system")
    val master = system.actorOf(Props[Master],"master")
    implicit val timeout = Timeout(5.seconds)
    val future = master ? Master.Task(columns, rows)
    future.collect{
      case Master.Result(res) => res
    }.foreach{res =>
      res should contain theSameElementsInOrderAs result
      system.terminate()
    }
  }

  " Nonogram actors medium " should " g " in {

    val system = ActorSystem("system")
    val master = system.actorOf(Props[Master],"master")
    implicit val timeout = Timeout(5.seconds)
    val future = master ? Master.Task(columns2, rows2)
    future.collect{
      case Master.Result(res) => res
    }.foreach{res =>
      res should contain theSameElementsInOrderAs result2
      system.terminate()
    }
  }

}
