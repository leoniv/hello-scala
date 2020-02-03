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

  describe("#readInput") {
    it("Считывает из строки позиции кораблей возвращает флот") {
      pending
    }
  }

  describe("#validateShip") {

    describe(s"Длина корабля не может быть более ${SHIP_MAX_LENGTH}х клеток") {
      describe("По горизонтали") {

        it("Валидный корабль") {
          validateShip(shipX(1, SHIP_MAX_LENGTH)) should be (true)
        }

        it("Не валидный корабль") {
          validateShip(shipX(1, SHIP_MAX_LENGTH + 1)) should be (false)
        }
      }

      describe("По вертикали") {

        it("Валидный корабль") {
          validateShip(shipY(1, SHIP_MAX_LENGTH)) should be (true)
        }

        it("Не валидный корабль") {
          validateShip(shipY(1, SHIP_MAX_LENGTH + 1)) should be (false)
        }
      }
    }

    describe("Ширина корабля не более одной клетки") {
      describe("По горизотали") {

        it("Валидный корабль") {
          validateShip(ship(0 -> 1, 0 -> 2, 0 -> 3)) should be (true)
        }

        it("Не валидный корабль") {
          validateShip(ship(0 -> 1, 0 -> 2, 1 -> 2)) should be (false)
        }
      }
      describe("Или по вертикали") {

        it("Валидный корабль") {
          validateShip(ship(1 -> 0, 2 -> 0, 3 -> 0)) should be (true)
        }

        it("Не валидный корабль") {
          validateShip(ship(1 -> 0, 2 -> 0, 2 -> 1)) should be (false)
        }
      }

      it("Корабль размером в одну клетку валидный") {
        validateShip(ship(0->0)) should be (true)
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
    it("Добавляет валидный корабль во флот") {
      enrichFleet(Map().empty, "Name", ship(0 -> 1)) should
        contain (("Name" -> ship(0 -> 1)))
    }

    it("Не валидный корабль не добавляет") {
      enrichFleet(Map[String, Ship]().empty, "Не важно", ship()) should
        be (empty)
    }
  }
}
