package taller4

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatest.time.Span
import org.scalatestplus.junit.JUnitRunner

import scala.concurrent.duration.DurationInt

@RunWith(classOf[JUnitRunner])
class TestNewton extends AnyFunSuite{
  def timeLimit: Span = 10.seconds

  val newton = new Newton()
  import newton._

  test("mostrar - número y suma") {
    println("Iniciando test: mostrar - número y suma")
    assert(mostrar(Suma(Numero(3), Numero(5))) == "(3.0 + 5.0)")
    println("Test completado: mostrar - número y suma")
  }

  test("mostrar - átomo") {
    println("Iniciando test: mostrar - átomo")
    assert(mostrar(Atomo('x')) == "x")
    println("Test completado: mostrar - átomo")
  }


  test("mostrar - producto") {
    assert(mostrar(Prod(Numero(3), Numero(5))) == "(3.0 * 5.0)")
  }

  test("mostrar - resta") {
    assert(mostrar(Resta(Numero(5), Numero(3))) == "(5.0 - 3.0)")
  }

  test("mostrar - división") {
    assert(mostrar(Div(Numero(10), Numero(2))) == "(10.0 / 2.0)")
  }

  test("evaluar - suma de números") {
    assert(evaluar(Suma(Numero(3), Numero(5)), Atomo('x'), 2) == 8.0)
  }

  test("evaluar - átomo coincide") {
    assert(evaluar(Atomo('x'), Atomo('x'), 2) == 2.0)
  }

  test("evaluar - átomo no coincide") {
    assertThrows[IllegalArgumentException] {
      evaluar(Atomo('y'), Atomo('x'), 2)
    }
  }

  test("evaluar - producto") {
    assert(evaluar(Prod(Numero(3), Numero(5)), Atomo('x'), 2) == 15.0)
  }

  test("evaluar - resta") {
    assert(evaluar(Resta(Numero(5), Numero(3)), Atomo('x'), 2) == 2.0)
  }

  test("derivar - constante") {
    assert(derivar(Numero(3), Atomo('x')) == Numero(0))
  }

  test("derivar - variable coincide") {
    assert(derivar(Atomo('x'), Atomo('x')) == Numero(1))
  }

  test("derivar - variable no coincide") {
    assert(derivar(Atomo('y'), Atomo('x')) == Numero(0))
  }

  test("derivar - suma") {
    assert(derivar(Suma(Atomo('x'), Numero(3)), Atomo('x')) == Suma(Numero(1), Numero(0)))
  }

  test("derivar - producto") {
    assert(derivar(Prod(Atomo('x'), Numero(3)), Atomo('x')) == Suma(Prod(Numero(1), Numero(3)), Prod(Atomo('x'), Numero(0))))
  }

  test("limpiar - suma con cero") {
    assert(limpiar(Suma(Numero(0), Numero(5))) == Numero(5))
  }

  test("limpiar - resta con cero") {
    assert(limpiar(Resta(Numero(5), Numero(0))) == Numero(5))
  }

  test("limpiar - producto con uno") {
    assert(limpiar(Prod(Numero(1), Numero(5))) == Numero(5))
  }

  test("limpiar - producto con cero") {
    assert(limpiar(Prod(Numero(0), Numero(5))) == Numero(0))
  }

  test("limpiar - división por uno") {
    assert(limpiar(Div(Numero(5), Numero(1))) == Numero(5))
  }

  test("buenaAprox - valor cerca de cero") {
    assert(buenaAprox(Resta(Atomo('x'), Numero(5)), Atomo('x'), 5.0))
  }

  test("buenaAprox - valor lejos de cero") {
    assert(!buenaAprox(Resta(Atomo('x'), Numero(5)), Atomo('x'), 10.0))
  }


  test("raizNewton - raíz cuadrada de 2") {
    val expr = Resta(Prod(Atomo('x'), Atomo('x')), Numero(2))
    val raiz = raizNewton(expr, Atomo('x'), 1.0, buenaAprox)
    assert(math.abs(raiz - math.sqrt(2)) < 0.001)
  }

  test("raizNewton - raíz cúbica de 8") {
    val expr = Resta(Prod(Prod(Atomo('x'), Atomo('x')), Atomo('x')), Numero(8))
    val raiz = raizNewton(expr, Atomo('x'), 2.0, buenaAprox)
    assert(math.abs(raiz - math.cbrt(8)) < 0.001)
  }

  test("raizNewton - sin solución cercana") {
    val expr = Resta(Atomo('x'), Numero(100))
    val raiz = raizNewton(expr, Atomo('x'), 2.0, buenaAprox)
    assert(math.abs(raiz - 100.0) < 0.001)
  }

  test("raizNewton - valor inicial cercano a solución") {
    val expr = Resta(Atomo('x'), Numero(5))
    val raiz = raizNewton(expr, Atomo('x'), 4.9, buenaAprox)
    assert(math.abs(raiz - 5.0) < 0.001)
  }

}
