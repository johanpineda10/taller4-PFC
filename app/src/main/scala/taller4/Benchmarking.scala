package taller4

import org.scalameter._

object Benchmarking {
  val newtonParalela = new NewtonParalela()

  // Definición de la función de benchmarking para la limpieza de expresiones
  def benchmarkLimpiar(): Unit = {
    val expr = newtonParalela.Suma(newtonParalela.Prod(newtonParalela.Numero(1000000), newtonParalela.Atomo('x')), newtonParalela.Numero(0))
    val time = withWarmer(new Warmer.Default) measure {
      newtonParalela.limpiar(expr)
    }
    println(s"Tiempo de limpieza de expresiones: $time ms")
  }

  // Definición de la función de benchmarking para el cálculo de raíces utilizando el método de Newton paralelo
  def benchmarkRaizNewtonParalelo(): Unit = {
    val expr = newtonParalela.Suma(newtonParalela.Prod(newtonParalela.Numero(1000000), newtonParalela.Atomo('x')), newtonParalela.Numero(0))
    val tiempo = withWarmer(new Warmer.Default) measure {
      newtonParalela.raizNewtonParalelo(expr, newtonParalela.Atomo('x'), 1.0, (f, a, d) => newtonParalela.buenaAprox(f, a, d))
    }
    println(s"Tiempo de cálculo de raíces utilizando Newton paralelo: $tiempo ms")
  }
}
