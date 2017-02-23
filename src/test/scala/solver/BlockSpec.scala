package solver

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by wojciech on 22.02.17.
  */
object BlockSpec {
  sealed trait Elem
  case object E extends Elem
  case object W extends Elem
  case class B(i: Int) extends Elem
  case object A extends Elem
  def convert(args: (Int,Elem)*): List[Field] = args.flatMap{
    case(x, E) => List.fill(x)(Empty)
    case(x, W) => List.fill(x)(White)
    case(x, B(i)) => List.fill(x)(Black(i))
    case(x, A) => List.fill(x)(BlackAny)
  }.toList
}
class BlockSpec extends FlatSpec with Matchers {

  import BlockSpec._

  "Field pattern (3) 7" should "7E" in {
    Field.pattern(
      List(3),
      convert((7,E))
    ) should contain theSameElementsInOrderAs
      convert((7,E))
  }

  "Field pattern (2,2) 7" should "7E" in {
    Field.pattern(
      List(2,2),
      convert((7,E))
    ) should contain theSameElementsInOrderAs
      convert((7,E))
  }

  "Field pattern (5) 7" should "2E 3B 2E" in {
    Field.pattern(
      List(5),
      List.fill(7)(Empty)
    ) should contain theSameElementsInOrderAs
      List.fill(2)(Empty) ::: List.fill(3)(Black(0)) ::: List.fill(2)(Empty)
  }

  "Field pattern (5,4) 20 | 5E A E A 12E" should "W 4E A E A 12E" in {
    Field.pattern(
      List(5,4),
      List.fill(5)(Empty) ::: List(BlackAny,Empty,BlackAny) ::: List.fill(12)(Empty)
    ) should contain theSameElementsInOrderAs
      List(White) ::: List.fill(4)(Empty) ::: List(Black(0),Empty,BlackAny) ::: List.fill(12)(Empty)
  }

  "Field pattern (2,1) E E E E A E E " should "4E A W E" in {
    Field.pattern(
      List(2,1),
      List.fill(4)(Empty) ::: List(BlackAny) ::: List.fill(2)(Empty)
    ) should contain theSameElementsInOrderAs
      List.fill(4)(Empty) ::: List(BlackAny,White,Empty)
  }

  "Field pattern (2,1,1) E E E E A E E " should " 2B 2W B W B " in  {
    Field.pattern(
      List(2,1,1),
      List.fill(4)(Empty) ::: List(BlackAny) ::: List.fill(2)(Empty)
    ) should contain theSameElementsInOrderAs
      List(Empty,Black(0),Empty,White,Black(1),White,Black(2))
  }

  "Field pattern (2,2) 2E A 6E " should " W E B 6E " in  {
    Field.pattern(
      List(2,2),
      List(Empty,Empty,BlackAny) ::: List.fill(6)(Empty)
    ) should contain theSameElementsInOrderAs
      List(White,Empty,Black(0)) ::: List.fill(6)(Empty)
  }

  "Field pattern (1,1) 2E A 6E " should " E W A W 5E " in  {
    Field.pattern(
      List(1,1),
      List(Empty,Empty,BlackAny) ::: List.fill(6)(Empty)
    ) should contain theSameElementsInOrderAs
      List(Empty,White,BlackAny,White) ::: List.fill(5)(Empty)
  }

  "Field pattern (5,1) W 9E " should " W 2E 3B 4E " in  {
    Field.pattern(
      List(5,1),
      List(White) ::: List.fill(9)(Empty)
    ) should contain theSameElementsInOrderAs
      List(White,Empty,Empty) ::: List.fill(3)(Black(0)) ::: List.fill(4)(Empty)
  }

  "Field pattern (1,1) 2E A 4E A E " should " 2W B 4W B W " in  {
    Field.pattern(
      List(1,1),
      List(Empty,Empty,BlackAny) ::: List.fill(4)(Empty) ::: List(BlackAny, Empty)
    ) should contain theSameElementsInOrderAs
      List(White,White,Black(0)) ::: List.fill(4)(White) ::: List(Black(1), White)
  }

  "Field pattern (6) 2E A 4E A E " should " 2W 6B W " in  {
    Field.pattern(
      List(6),
      List(Empty,Empty,BlackAny) ::: List.fill(4)(Empty) ::: List(BlackAny, Empty)
    ) should contain theSameElementsInOrderAs
      List(White,White) ::: List.fill(6)(Black(0)) ::: List(White)
  }

  "Field pattern (3) W 2E W A 5E " should " 4W 3B 3W " in  {
    Field.pattern(
      List(3),
      convert((1,W),(2,E),(1,W),(1,A),(5,E))
    ) should contain theSameElementsInOrderAs
      convert((4,W),(3,B(0)),(3,W))
  }

  "Field pattern (2,2) 2E A 2E W A 2E " should " W E B E 2W 2B W" in  {
    Field.pattern(
      List(2,2),
      convert((2,E),(1,A),(2,E),(1,W),(1,A),(2,E))
    ) should contain theSameElementsInOrderAs
      convert((1,W),(1,E),(1,B(0)),(1,E),(2,W),(2,B(1)),(1,W))
  }

  "Field pattern (2) W 2E W A E W 3E " should " 4W 2B 4W" in  {
    Field.pattern(
      List(2),
      convert((1,W),(2,E),(1,W),(1,A),(1,E),(1,W),(3,E))
    ) should contain theSameElementsInOrderAs
      convert((4,W),(2,B(0)),(4,W))
  }

}
