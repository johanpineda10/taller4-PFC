package taller4

class Newton {
  object Newton {

    // Definición del trait Expr y sus casos
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
    def mostrar(e: Expr): String = e match {
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
    def evaluar(f: Expr, a: Atomo, v: Double): Double = f match {
      case Numero(d) => d
      case Atomo(x) => if (x == a.x) v else throw new IllegalArgumentException("La expresión contiene un átomo diferente al proporcionado.")
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

    // Función limpiar
    def limpiar(f: Expr): Expr = f match {
      case Suma(Numero(0), e) => limpiar(e)
      case Suma(e, Numero(0)) => limpiar(e)
      case Suma(e1, e2) => (limpiar(e1), limpiar(e2)) match {
        case (Numero(0), e) => e
        case (e, Numero(0)) => e
        case (limpioE1, limpioE2) => Suma(limpioE1, limpioE2)
      }
      case Resta(e, Numero(0)) => limpiar(e)
      case Resta(e1, e2) => (limpiar(e1), limpiar(e2)) match {
        case (limpioE1, limpioE2) => Resta(limpioE1, limpioE2)
      }
      case Prod(Numero(1), e) => limpiar(e)
      case Prod(e, Numero(1)) => limpiar(e)
      case Prod(Numero(0), _) => Numero(0)
      case Prod(_, Numero(0)) => Numero(0)
      case Prod(e1, e2) => (limpiar(e1), limpiar(e2)) match {
        case (Numero(0), _) => Numero(0)
        case (_, Numero(0)) => Numero(0)
        case (Numero(1), e) => e
        case (e, Numero(1)) => e
        case (limpioE1, limpioE2) => Prod(limpioE1, limpioE2)
      }
      case Div(Numero(0), _) => Numero(0)
      case Div(e, Numero(1)) => limpiar(e)
      case Div(e1, e2) => (limpiar(e1), limpiar(e2)) match {
        case (limpioE1, limpioE2) => Div(limpioE1, limpioE2)
      }
      case Expo(e, Numero(1)) => limpiar(e)
      case Expo(e1, e2) => (limpiar(e1), limpiar(e2)) match {
        case (limpioE1, Numero(1)) => limpioE1
        case (limpioE1, limpioE2) => Expo(limpioE1, limpioE2)
      }
      case Logaritmo(e1) => Logaritmo(limpiar(e1))
      case e => e
    }

    // Función para verificar si la función en el punto dado está cerca de cero
    def buenaAprox(f: Expr, a: Atomo, d: Double): Boolean = {
      math.abs(evaluar(f, a, d)) < 0.001
    }

    def raizNewton(f: Expr, a: Atomo, x0: Double, ba: (Expr, Atomo, Double) => Boolean): Double = {
      var xn = x0
      var fxn = evaluar(f, a, xn)
      while (!ba(f, a, xn)) {
        val dfxn = evaluar(derivar(f, a), a, xn)
        xn = xn - (fxn / dfxn)
        fxn = evaluar(f, a, xn)
      }
      xn
    }
  }
}
