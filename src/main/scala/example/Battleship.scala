package example

object Battleship {
  val SHIP_MAX_LENGTH = 4
  type Point = (Int, Int)
  type Ship = List[Point]
  type Fleet = Map[String, Ship]
  type Field = Vector[Vector[Boolean]]
  type Game = (Field, Fleet)


  def validateShip(ship: Ship): Boolean = {
    false
  }

  def validatePosition(ship: Ship, field: Field): Boolean = ???
  def enrichFleet(fleet: Fleet, name: String, ship: Ship): Fleet = ???
  def markUsedCells(field: Field, ship: Ship): Field = ???
  def tryAddShip(game: Game, name: String, ship: Ship): Game = ???
}
