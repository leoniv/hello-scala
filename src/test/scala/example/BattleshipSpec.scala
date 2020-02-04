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
  def field(size: Int): Field = Vector.fill(size)(
    Vector.fill(size)(false)
  )

  val T = true
  val F = false

  val TEST_INPUT_1 = """3
      |BlackPearl 3
      |1 6
      |1 7
      |1 8
      |MillenniumFalcon 4
      |2 5
      |3 5
      |4 5
      |5 5
      |Varyag 1
      |9 9""".stripMargin

  def KNOWN_SHIPS = Map(
    "BlackPearl" -> ship(1 -> 6, 1 -> 7, 1 -> 8),
    "MillenniumFalcon" -> ship(2 -> 5, 3 -> 5, 4 -> 5, 5 -> 5),
    "Varyag" -> ship(9 -> 9)
  )

  describe("#readInput") {
    it("Считывает из строки позиции кораблей возвращает флот") {
      readInput(TEST_INPUT_1.split("\\n").toList) should
        equal (Some(KNOWN_SHIPS))
    }

    describe("#readInputHead") {
      it("Возвращает опционльно ReadedInput[InputHeader]") {
        readInputHead(List("42","tail")) should
          equal (Some(42 -> List("tail")))
      }

      it("При неудаче чтения возвращает None") {
        readInputHead(List("NaN","tail")) should be (None)
      }
    }

    describe("#readShipHeader") {
      it("Возвращает опционльно ReadedInput[ShipHeader]") {
        readShipHeader(List("Ship name 42", "tail")) should
          equal (Some(("Ship name" -> 42) -> List("tail")))
      }

      it("При неудаче чтения возвращает None") {
        readShipHeader(List("Ship name", "NaN", "tail")) should be (None)
      }
    }

    describe("#readShip") {
      it("Возвращает опционльно ReadedInput[(Shiname, Ship)]") {
        readShip(List("S 3", "0 1", "0 2", "0 3", "tail")) should
          equal (Some((("S", ship(0 -> 1, 0 -> 2, 0 -> 3)), List("tail"))))
      }

      it("При неудаче чтения возвращает None") {
        readShip(List("S 3", "0 A", "0 2", "0 3", "tail")) should be (None)
      }
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

    describe("Корабль должен помещаться на поле") {
      describe("По горизонтали") {
        it("Не валидный корабль начинается за пределами поля") {
          validateShip(shipX(-1, 1)) should be (false)
        }

        it("Не валидный корабль заканчивается за пределами поля") {
          validateShip(shipX(FIELD_SIZE - 1, 2)) should be (false)
        }
      }

      describe("По вертикали") {
        it("Не валидный корабль начинается за пределами поля") {
          validateShip(shipY(-1, 1)) should be (false)
        }

        it("Не валидный корабль заканчивается за пределами поля") {
          validateShip(shipY(FIELD_SIZE - 1, 2)) should be (false)
        }
      }
    }
  }

  describe("#validatePosition") {
    describe("Корабли не могут касаться дугруга") {
      it ("Ни бортами") { pending }
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

  describe("#markUsedCells") {
    it("Помечает клетки занимаемые кораблем") {
      val givenShip = ship(1->1, 1->2, 1->3, 2 -> 3, 3 -> 3)
      val expectedField = Vector(
        Vector.fill(5)(F),
        Vector.fill(5)(F, T, T, T, F),
        Vector.fill(5)(F, F, F, T, F),
        Vector.fill(5)(F, F, F, T, F),
        Vector.fill(5)(F)
      )

      markUsedCells(field(5), givenShip) should equal (expectedField)
    }

    describe("#setPoint") {
      it("Когда точка находится в поле") {
        setPoint(field(3), (1, 1)) should equal (
          Vector(
            Vector(F, F, F),
            Vector(F, T, F),
            Vector(F, F, F)
          )
        )
      }

      describe("Когда точка находится вне поля") {
        it("Левее") {
          setPoint(field(3), (1, -1)) should equal (field(3))
        }
        it("Правее") {
          setPoint(field(3), (1, 3)) should equal (field(3))
        }
        it("Выше") {
          setPoint(field(3), (-1, 1)) should equal (field(3))
        }
        it("Ниже") {
          setPoint(field(3), (3, 1)) should equal (field(3))
        }
      }
    }
  }
}
