package taller4

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestNewtonParalela extends AnyFunSuite {
  val newtonParalela = new NewtonParalela()

  import newtonParalela._

  test("mostrar - número y suma") {
    assert(mostrar(Suma(Numero(3), Numero(5))) == "(3.0 + 5.0)")
  }

  test("mostrar - átomo") {
    assert(mostrar(Atomo('x')) == "x")
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

  test("evaluar - producto") {
    assert(evaluar(Prod(Numero(3), Numero(5)), Atomo('x'), 2) == 15.0)
  }

  test("evaluar - resta") {
    assert(evaluar(Resta(Numero(5), Numero(3)), Atomo('x'), 2) == 2.0)
  }

  test("evaluar - evaluación de una suma") {
    val expr = Suma(Numero(10), Numero(5))
    val resultado = evaluar(expr, Atomo('x'), 0)
    assert(resultado == 15)
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

  test("limpiar - producto con uno") {
    assert(limpiar(Prod(Numero(1), Numero(5))) == Numero(5))
  }

  test("limpiar - producto con cero") {
    assert(limpiar(Prod(Numero(0), Numero(5))) == Numero(0))
  }

  test("buenaAprox - valor cerca de cero") {
    assert(buenaAprox(Resta(Atomo('x'), Numero(5)), Atomo('x'), 5.0))
  }

  test("buenaAprox - valor lejos de cero") {
    assert(!buenaAprox(Resta(Atomo('x'), Numero(5)), Atomo('x'), 10.0))
  }

  test("buenaAprox - valor muy cercano a cero") {
    val expr = Resta(Atomo('x'), Numero(0.0001))
    val resultado = buenaAprox(expr, Atomo('x'), 0.0)
    assert(resultado)
  }

  test("raizNewtonParalelo - raíz cúbica de 8") {
    val expr = Resta(Prod(Prod(Atomo('x'), Atomo('x')), Atomo('x')), Numero(8))
    val raiz = raizNewtonParalelo(expr, Atomo('x'), 2.0, buenaAprox)
    assert(math.abs(raiz - math.cbrt(8)) < 0.001)
  }

  test("raizNewtonParalelo - raíz cuadrada de 100") {
    val expr = Resta(Prod(Atomo('x'), Atomo('x')), Numero(100))
    val raiz = raizNewtonParalelo(expr, Atomo('x'), 10.0, buenaAprox)
    assert(math.abs(raiz - 10.0) < 0.001)
  }

  test("raizNewtonParalelo - raíz cúbica de 27") {
    val expr = Resta(Prod(Prod(Atomo('x'), Atomo('x')), Atomo('x')), Numero(27))
    val raiz = raizNewtonParalelo(expr, Atomo('x'), 3.0, buenaAprox)
    assert(math.abs(raiz - 3.0) < 0.001)
  }

  test("raizNewtonParalelo - raíz cuarta de 16") {
    val expr = Resta(Prod(Prod(Prod(Atomo('x'), Atomo('x')), Atomo('x')), Atomo('x')), Numero(16))
    val raiz = raizNewtonParalelo(expr, Atomo('x'), 2.0, buenaAprox)
    assert(math.abs(raiz - 2.0) < 0.001)
  }

  test("raizNewtonParalelo - raíz quinta de 32") {
    val expr = Resta(Prod(Prod(Prod(Prod(Atomo('x'), Atomo('x')), Atomo('x')), Atomo('x')), Atomo('x')), Numero(32))
    val raiz = raizNewtonParalelo(expr, Atomo('x'), 2.0, buenaAprox)
    assert(math.abs(raiz - 2.0) < 0.001)
  }
}

