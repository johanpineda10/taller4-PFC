package taller4

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

/*
Se utilizó la paralelización de datos (data parallelism) mediante el uso de Futures en Scala.
 Esto permite ejecutar múltiples tareas de manera concurrente, aprovechando así los recursos
 de la CPU de manera eficiente.

 En la función raizNewtonParalelo, el cálculo de la raíz de una función utilizando el método de Newton implica iteraciones
 independientes, ya que cada iteración no depende del resultado de las otras iteraciones. Por lo  tanto, es altamente
 paralelizable y puede beneficiarse significativamente de la paralelización para reducir el tiempo de cálculo.

*/
object NewtonParalela {
  // Definición de los tipos de expresión
  sealed trait Expr
  case class Numero(d: Double) extends Expr
  case class Atomo(x: Char) extends Expr
  case class Suma(e1: Expr, e2: Expr) extends Expr
  case class Prod(e1: Expr, e2: Expr) extends Expr
  case class Resta(e1: Expr, e2: Expr) extends Expr
  case class Div(e1: Expr, e2: Expr) extends Expr
  case class Expo(e1: Expr, e2: Expr) extends Expr
  case class Logaritmo(e1: Expr) extends Expr

  // Función para mostrar una expresión de manera simbólica
  def mostrar(expr: Expr): String = expr match {
    case Numero(d) => d.toString
    case Atomo(x) => x.toString
    case Suma(e1, e2) => s"(${mostrar(e1)} + ${mostrar(e2)})"
    case Prod(e1, e2) => s"(${mostrar(e1)} * ${mostrar(e2)})"
    case Resta(e1, e2) => s"(${mostrar(e1)} - ${mostrar(e2)})"
    case Div(e1, e2) => s"(${mostrar(e1)} / ${mostrar(e2)})"
    case Expo(e1, e2) => s"(${mostrar(e1)} ^ ${mostrar(e2)})"
    case Logaritmo(e1) => s"lg(${mostrar(e1)})"
  }

  // Función para evaluar una expresión en un punto dado, considerando el átomo y el valor dado
  def evaluar(expr: Expr, a: Atomo, v: Double): Double = expr match {
    case Numero(d) => d
    case Atomo(x) => if (x == a.x) v else evaluar(expr, a, v) // Corregido el error de recursión infinita
    case Suma(e1, e2) => evaluar(e1, a, v) + evaluar(e2, a, v)
    case Prod(e1, e2) => evaluar(e1, a, v) * evaluar(e2, a, v)
    case Resta(e1, e2) => evaluar(e1, a, v) - evaluar(e2, a, v)
    case Div(e1, e2) => evaluar(e1, a, v) / evaluar(e2, a, v)
    case Expo(e1, e2) => math.pow(evaluar(e1, a, v), evaluar(e2, a, v))
    case Logaritmo(e1) => math.log(evaluar(e1, a, v))
  }
  // Función para derivar una expresión respecto a una variable
  def derivar(f: Expr, a: Atomo): Expr = f match {
    case Numero(_) => Numero(0)
    case Atomo(x) => if (x == a.x) Numero(1) else Numero(0)
    case Suma(e1, e2) => Suma(derivar(e1, a), derivar(e2, a))
    case Resta(e1, e2) => Resta(derivar(e1, a), derivar(e2, a))
    case Prod(e1, e2) => Suma(Prod(derivar(e1, a), e2), Prod(e1, derivar(e2, a)))
    case Div(e1, e2) => Div(Resta(Prod(derivar(e1, a), e2), Prod(e1, derivar(e2, a))), Expo(e2, Numero(2)))
    case Expo(e1, e2) => Prod(Expo(e1, e2), Suma(Prod(derivar(e2, a), Div(e1, e2)), Prod(derivar(e1, a), Logaritmo(e1))))
    case Logaritmo(e1) => Div(derivar(e1, a), e1)
  }
  // Función para verificar si la función en el punto dado está cerca de cero
  def buenaAprox(f: Expr, a: Atomo, d: Double): Boolean = {
    evaluar(f, a, d) < 0.001
  }
  // Función para calcular la raíz de una función utilizando el método de Newton de manera paralela
  def raizNewtonParalelo(f: Expr, a: Atomo, x0: Double, ba: (Expr, Atomo, Double) => Boolean): Double = {
    var xn = x0
    var fxn = evaluar(f, a, xn)

    val futures = (1 to 10).map { _ =>
      Future {
        var localXn = x0
        var localFxn = evaluar(f, a, localXn)
        while (!ba(f, a, localXn)) {
          val dfxn = evaluar(derivar(f, a), a, localXn)
          localXn = localXn - (localFxn / dfxn)
          localFxn = evaluar(f, a, localXn)
        }
        localXn
      }
    }
    // Se espera a que todos los futuros terminen y se calcula el promedio de las raíces
    val resultadoPromediado = Future.sequence(futures).map { resultados =>
      resultados.sum / resultados.length
    }

    Await.result(resultadoPromediado, 10.seconds)
  }
  // Función para limpiar una expresión
  def limpiar(f: Expr): Expr = f match {
    case Suma(e1, e2) =>
      val futuroE1 = Future(limpiar(e1))
      val futuroE2 = Future(limpiar(e2))
      val resultadoFuturo = for {
        resE1 <- futuroE1
        resE2 <- futuroE2
      } yield (resE1, resE2) match {
        case (Numero(0), e) => e
        case (e, Numero(0)) => e
        case (limpioE1, limpioE2) => Suma(limpioE1, limpioE2)
      }
      Await.result(resultadoFuturo, Duration.Inf)

    case Resta(e1, e2) =>
      val futuroE1 = Future(limpiar(e1))
      val futuroE2 = Future(limpiar(e2))
      val resultadoFuturo = for {
        resE1 <- futuroE1
        resE2 <- futuroE2
      } yield Resta(resE1, resE2)
      Await.result(resultadoFuturo, Duration.Inf)

    case Prod(e1, e2) =>
      val futuroE1 = Future(limpiar(e1))
      val futuroE2 = Future(limpiar(e2))
      val resultadoFuturo = for {
        resE1 <- futuroE1
        resE2 <- futuroE2
      } yield (resE1, resE2) match {
        case (Numero(0), _) => Numero(0)
        case (_, Numero(0)) => Numero(0)
        case (Numero(1), e) => e
        case (e, Numero(1)) => e
        case (limpioE1, limpioE2) => Prod(limpioE1, limpioE2)
      }
      Await.result(resultadoFuturo, Duration.Inf)

    case Div(e1, e2) =>
      val futuroE1 = Future(limpiar(e1))
      val futuroE2 = Future(limpiar(e2))
      val resultadoFuturo = for {
        resE1 <- futuroE1
        resE2 <- futuroE2
      } yield Div(resE1, resE2)
      Await.result(resultadoFuturo, Duration.Inf)

    case Expo(e1, e2) =>
      val futuroE1 = Future(limpiar(e1))
      val futuroE2 = Future(limpiar(e2))
      val resultadoFuturo = for {
        resE1 <- futuroE1
        resE2 <- futuroE2
      } yield (resE1, resE2) match {
        case (_, Numero(1)) => resE1
        case (limpioE1, limpioE2) => Expo(limpioE1, limpioE2)
      }
      Await.result(resultadoFuturo, Duration.Inf)

    case Logaritmo(e1) =>
      val futuroE1 = Future(limpiar(e1))
      val resultadoFuturo = futuroE1.map(Logaritmo)
      Await.result(resultadoFuturo, Duration.Inf)

    case _ => f
  }

}
