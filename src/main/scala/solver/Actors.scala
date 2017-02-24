package solver

import akka.actor.{Actor, ActorRef, Props}
import akka.routing.BalancingPool

/**
  * Created by wojciech on 23.02.17.
  */
object Actors {

}
class Worker extends Actor {

  import Worker._
  def receive: PartialFunction[Any, Unit] = {
    case Work(list, pattern, position) =>
      val s = sender()
      val res = Field.pattern(list, pattern)
      s ! Master.WorkResult(res, position)
  }

}
object Worker {

  case class Work(list: List[Int], pattern: List[Field], position: Master.Position)

}

//class Master(board: Board) extends Actor {
class Master extends Actor {

  import Master._

  val router: ActorRef = context.actorOf(BalancingPool(4).props(Props[Worker]), "router")

  var actorResp: Option[ActorRef] = None
  var boardOpt: Option[Board] = None
  var cnt: Int = 0

  def sendPosition(position: Position): Unit = position match {
    case c@ Column(x) => boardOpt.foreach{ board =>
      router ! Worker.Work(board.column(x), board.columnFields(x), c)}
    case r@ Row(y) => boardOpt.foreach{ board =>
      router ! Worker.Work(board.row(y), board.rowFields(y), r)}
  }

  def checkReady(list: List[Field]): Boolean = list.forall{
    case Black(_) | White => true
    case _ => false
  }

  def end(): Unit = {
    (boardOpt, actorResp) match {
      case (Some(b), Some(a)) => a ! Result(List.range(0,b.height).map(b.rowFields))
      case _ =>
    }
  }


  def receive: PartialFunction[Any, Unit] = {
    case Task(columns, rows) =>
      val board = new BoardImpl(columns, rows)
      boardOpt = Some(board)
      actorResp = Some(sender())
      cnt = board.height + board.width
      (Iterator.range(0, board.width).map(Column) ++ Iterator.range(0, board.height).map(Row))
        .foreach(sendPosition)

    case WorkResult(list, c@ Column(x)) => boardOpt.foreach{ board =>
      board.setColumn(x, list)
      if(checkReady(list)) {
        cnt -= 1
        if(cnt == 0) end()
      }
      else sendPosition(c)}
    case WorkResult(list, r@ Row(y)) => boardOpt.foreach{ board =>
      board.setRow(y, list)
      if(checkReady(list)) {
        cnt -= 1
        if(cnt == 0) end()
      }
      else sendPosition(r)}
  }

}
object Master {

  sealed trait Position {
    def index: Int
  }
  case class Column(index: Int) extends Position
  case class Row(index: Int) extends Position

  case class WorkResult(list: List[Field], position: Position)
  case class Task(columns: List[List[Int]], rows: List[List[Int]])
  case class Result(res: List[List[Field]])

}