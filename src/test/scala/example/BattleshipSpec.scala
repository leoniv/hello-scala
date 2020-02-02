package example

import org.scalatest._
import example.Battleship._
import org.scalatest.prop.TableDrivenPropertyChecks._

class BattleshipSpec extends FunSpec with Matchers {
  //Helper
  def ship(points: Point*): Ship = points.toList
  def shipX(fX: Int, toX: Int, y: Int = 0) = ship(
    (fX to toX).map(x => (x, y)): _*
  )
  def shipY(fY: Int, toY: Int, x: Int = 0) = ship(
    (fY to toY).map(y => (x, y)): _*
  )

  def knownShips = Map(
    "MillenniumFalcon" -> ship(2 -> 5, 3 -> 5, 4 -> 5, 5 -> 5),
    "Varyag" -> ship(9 -> 9),
    "BlackPearl" -> ship(1 -> 6, 1 -> 7, 1 -> 8)
  )

  describe("#validateShip") {
//    val ships = Table(
//      ("ship", "isValid"),
//      (ship(), false),
//      (knownShips("MillenniumFalcon"), true)
//    )
//    forAll(ships) { (ship, isValid) =>
//      it(s"Ship $ship should ${if (isValid) "" else "not "}be valid") {
//        validateShip(ship) should be (isValid)
//      }
//    }

    describe("Длина корабля не может быть более ${SHIP_MAX_LENGTH}х клеток") {
      describe("По горизонтали") {
        val ships = for (x <- 1 to SHIP_MAX_LENGTH + 2) yield
          (shipX(0, x), x <= SHIP_MAX_LENGTH)

        forAll(Table(("ship", "isValid"), ships: _*)) { (ship, isValid) =>
          it(s"$ship ${if (!isValid) "не " else ""}валидный корабль") {
            validateShip(ship) should be (isValid)
          }
        }
      }

      describe("По вертикали") {
        val ships = for (y <- 1 to SHIP_MAX_LENGTH + 2) yield
          (shipY(0, y), y <= SHIP_MAX_LENGTH)

        forAll(Table(("ship", "isValid"), ships: _*)) { (ship, isValid) =>
          it(s"$ship ${if (!isValid) "не " else ""}валидный корабль") {
            validateShip(ship) should be (isValid)
          }
        }
      }
    }

    describe("Ширина корабля не более одной клетки") {
      describe("По горизотали") {
        it("Валидный корабль") { pending }
        it("Не валидный корабль") { pending }
      }
      describe("Или по вертикали") {
        it("Валидный корабль") { pending }
        it("Не валидный корабль") { pending }
      }
    }

    describe("Корабль не имеет пропусков") {
      it ("Вадидный корабль") {
        validateShip(ship(1 -> 1, 1 -> 2, 1 -> 3)) should be (true)
      }
      it ("Не валидный корабль") {
        validateShip(ship(1 -> 1, 1 -> 3, 1 -> 4)) should be (false)
      }
    }
  }

  describe("#validatePosition") {
    describe("Корабли не могут касаться дугруга") {
      it ("Ни ботрами") { pending }
      it ("Ни углами") { pending }
      it ("Тем более пересекаться") { pending }
    }

    it("Валидная позиция корабля") { pending }
  }

  describe("#enrichFleet") {
    it("Добавляет корабль во флот") { pending }
  }
}
