package solver

/**
  * Created by wojciech on 23.02.17.
  */
class BoardImpl(c: List[List[Int]], r: List[List[Int]]) extends Board {

  case class ElemXY(x: Int, y: Int, var f: Field)

  val fields: List[ElemXY] = List.tabulate(c.size, r.size)(ElemXY(_, _, Empty)).flatten

  def column(x: Int): List[Int] = c(x)
  def row(y: Int): List[Int] = r(y)

  def columnFields(x: Int): List[Field] = fields.filter(_.x == x).sortWith(_.y < _.y).map(_.f)
  def rowFields(y: Int): List[Field] = fields.filter(_.y == y).sortWith(_.x < _.x).map(_.f)

  def setter(arg: (ElemXY, Field)): Unit = arg match {
    case (e@ ElemXY(_,_,Empty), _: Black) => e.f = BlackAny
    case (e@ ElemXY(_,_,Empty), f) => e.f = f
    case _ =>
  }

  def setColumn(x: Int, arg: List[Field]): Unit = fields.filter(_.x == x).sortWith(_.y < _.y).zip(arg).foreach(setter)
  def setRow(y: Int, arg: List[Field]): Unit = fields.filter(_.y == y).sortWith(_.x < _.x).zip(arg).foreach(setter)

  val height: Int = r.size
  val width: Int = c.size

  override def toString: String = List.range(0, height).map(rowFields).mkString("\n")


}
