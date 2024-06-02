package taller4

import org.scalameter._

object Benchmarking {
  // Definición de la función de benchmarking para la limpieza de expresiones
  def benchmarkLimpiar(): Unit = {
    val expr = NewtonParalela.Suma(NewtonParalela.Prod(NewtonParalela.Numero(1000000), NewtonParalela.Atomo('x')), NewtonParalela.Numero(0))
    val time = withWarmer(new Warmer.Default) measure {
      NewtonParalela.limpiar(expr)
    }
    println(s"Tiempo de limpieza de expresiones: $time ms")
  }

  // Definición de la función de benchmarking para el cálculo de raíces utilizando el método de Newton paralelo
  def benchmarkRaizNewtonParalelo(): Unit = {
    val expr = NewtonParalela.Suma(NewtonParalela.Prod(NewtonParalela.Numero(1000000), NewtonParalela.Atomo('x')), NewtonParalela.Numero(0))
    val tiempo = withWarmer(new Warmer.Default) measure {
      NewtonParalela.raizNewtonParalelo(expr, NewtonParalela.Atomo('x'), 1.0, (f, a, d) => NewtonParalela.buenaAprox(f, a, d))
    }
    println(s"Tiempo de cálculo de raíces utilizando Newton paralelo: $tiempo ms")
  }
}
