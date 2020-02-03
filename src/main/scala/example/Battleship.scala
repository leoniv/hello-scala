package example

object Battleship {
  val SHIP_MAX_LENGTH = 4
  type Point = (Int, Int)
  type Ship = List[Point]
  type Fleet = Map[String, Ship]
  type Field = Vector[Vector[Boolean]]
  type Game = (Field, Fleet)

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
      SHIP_MAX_LENGTH + 1 >= xWidth + yWidth
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

  def validateShip(ship: Ship): Boolean = {
    val shipl = ShipLine(ship)
    shipl match {
      case ShipLineXY(_) => false
      case _ => shipl.validate
    }
  }

  def validatePosition(ship: Ship, field: Field): Boolean = ???
  def enrichFleet(fleet: Fleet, name: String, ship: Ship): Fleet = ???
  def markUsedCells(field: Field, ship: Ship): Field = ???
  def tryAddShip(game: Game, name: String, ship: Ship): Game = ???
}
