import org.scalameter.api._

/**
 * Ejemplo de aplicacion de benchmark usando ScalaMeter
 * Se usa generador de informacion de ejecucion que
 * calcula intervalos de tiempo para las ejecuciones
 * realizadas y tambien un grafico. El grafico puede
 * accederse mediante un documento html presente en
 * la carpeta del proyecto, directorio target, benchmarks,
 * report, archivo index.html
 */
object RangeBenchmark extends Bench.OfflineRegressionReport {

  // se hace un generador para las listas con las que trabajar,
  // con tamaños desde 10000 hasta 100000, usando saltos de
  // 10000 en 10000
  val ranges: Gen[Range] = for {
    size <- Gen.range("size")(1000, 10000, 1000)
  } yield 0 until size

  // se define la funcion a usar para ordenacion
  def ordenar(a:Int, b:Int) = a > b;

  // definicion de la prueba a realizar
  performance of "Busqueda" config(
    // 10 ejecuciones para cada metodo
    exec.benchRuns -> 10,
    // se indica que se usan 10 hebras independientes
    exec.independentSamples -> 10
  ) in {
    // se define que se va a medir
    measure method "busqueda binaria" in {
      // primer metodo a probar: usando el generador ranges se
      // obtiene lista de enteros. Se indica que la curva del
      // grafico a generar se denominara Busqueda binaria
      using(ranges) curve "Busqueda binaria" in {
        r => {
          // llamada al metodo de busqueda sobre r (un rango del tamaño
          // correspondiente; siempre se busca el valor que ocupa la
          // posicon 10)
          GenericBinarySearch.calc[Int](r.toArray, r(10), ordenar)
        }
      }

      // prueba del metodo de busqueda a saltos, de la misma forma
      using(ranges) curve "Busqueda saltos" in {
        r => {
          GenericJumpSearch.calc[Int](r.toList, ordenar, r(10))
        }
      }
    }
  }
}