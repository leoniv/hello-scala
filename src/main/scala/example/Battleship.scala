package example

import scala.annotation.tailrec

object Battleship {
  val SHIP_MAX_LENGTH = 4
  val FIELD_SIZE = 10
  type Point = (Int, Int)
  type Ship = List[Point]
  type Fleet = Map[String, Ship]
  type Field = Vector[Vector[Boolean]]
  type Game = (Field, Fleet)
  type Input = List[String]
  type ReadedInput[T] = (T, Input)
  type ShipsCount = Int
  type InputHeader = ShipsCount
  type Points = Int
  type ShipName = String
  type ShipHeader = (ShipName, Points)

  trait ShipLine {
    val get: Ship
    def xProjection = get.unzip match { case (x, y) => x }
    def yProjection = get.unzip match { case (x, y) => y }
    def xWidth = xProjection.distinct.length
    def yWidth = yProjection.distinct.length
    def validateProjection(projection: List[Int]): Boolean = {
      def isMonotonic(xs: List[Int]): Boolean = {
        xs.size == xs.distinct.size && xs.last - xs.head == xs.size - 1
      }
      def isConst(xs: List[Int]): Boolean = {
        xs.distinct.size == 1
      }
      if (projection.isEmpty) false
      else isConst(projection) || isMonotonic(projection.sorted)
    }

    def validate: Boolean =
      validateProjection(xProjection) &&
      validateProjection(yProjection) &&
      SHIP_MAX_LENGTH + 1 >= xWidth + yWidth &&
      xProjection.sorted.last < FIELD_SIZE &&
      yProjection.sorted.last < FIELD_SIZE &&
      xProjection.sorted.head > -1 &&
      yProjection.sorted.head > -1
  }
  object ShipLine {
    class ShipWrapper(val get: Ship) extends ShipLine

    def apply(ship: Ship): ShipLine = {
      val shipw = new ShipWrapper(ship)
      if (shipw.xWidth == 1) ShipLineY(ship)
      else if (shipw.yWidth == 1) ShipLineX(ship)
      else ShipLineXY(ship)
    }
  }
  case class ShipLineX(get: Ship) extends ShipLine
  case class ShipLineY(get: Ship) extends ShipLine
  case class ShipLineXY(get: Ship) extends ShipLine {
    override def validate: Boolean = ???
  }

  def pop[T](in: List[T]) = Some(in).collect{ case (x :: xs) => (x, xs) }

  def readInput(in: Input): Option[Fleet] = {
    for {
      (shipCount, xs) <- readInputHead(in)
      ships <- readShips(xs, shipCount)
    } yield ships.foldLeft(Map[String, Ship]().empty)(
      (fleet, ship) => enrichFleet(fleet, ship._1, ship._2)
    )
  }

  def readShips(in: Input, count: Int): Option[List[(ShipName, Ship)]] = {
    if (count == 0) Some(List().empty)
    else for {
      (sh, xs) <- readShip(in)
      list <- readShips(xs, count - 1)
    } yield sh +: list
  }

  def readInputHead(in: Input): Option[ReadedInput[InputHeader]] = {
    for {
      (x, xs) <- pop(in)
      count <- x.toIntOption
    } yield (count, xs)
  }

  def readShip(in: Input): Option[ReadedInput[(ShipName, Ship)]] = for {
    ((name, pointsCount), xs) <- readShipHeader(in)
    (points, xs) <- readPoints(xs, pointsCount)
  } yield ((name, points.sorted), xs)

  val readPairOfInt: (String) => Option[(Int, Int)] = s => {
    val regex = """(\d+)\s+(\d+)""".r
    for {
      regex(w1, w2) <- Some(s)
      x <- w1.toIntOption
      y <- w2.toIntOption
    } yield (x, y)
  }

  def sequence[T](xs: List[Option[T]]): Option[List[T]] = {
    xs.foldRight(Option(List[T]().empty))( (o, os) =>
      for {
        x <- o
        xs <- os
      } yield x +: xs
    )
  }

  def readPoints(in: Input, count: Int): Option[ReadedInput[List[Point]]] = {
    val (ps, xs) = in.splitAt(count)
    for {
      points <- sequence(ps.map(readPairOfInt))
    } yield (points, xs)
  }

  def readShipHeader(in: Input): Option[ReadedInput[ShipHeader]] = {
    val regex = """^(.+)\s(\d+)""".r
    for {
      (header, xs)   <- pop(in)
      regex(name, number) <- Some(header)
      points <- number.toIntOption
    } yield (name, points) -> xs
  }

  def validateShip(ship: Ship): Boolean = {
    val shipl = ShipLine(ship)
    shipl match {
      case ShipLineXY(_) => false
      case _ => shipl.validate
    }
  }

  def validatePosition(ship: Ship, field: Field): Boolean = ???

  def enrichFleet(fleet: Fleet, name: String, ship: Ship): Fleet = {
    if (validateShip(ship)) (fleet + (name -> ship))
    else fleet
  }

  def markUsedCells(field: Field, ship: Ship): Field = {
    ship.foldLeft(field)(setPoint _)
  }

  def setPoint(field: Field, point: Point): Field = {
    val (x, y) = point
    val (headL :+ line, tailL) = field.splitAt(x + 1)
    val (head :+ _, tail) = line.splitAt(y + 1)
    (headL :+ (head :+ true) ++ tail) ++ tailL
  }

  def tryAddShip(game: Game, name: String, ship: Ship): Game = ???
}
