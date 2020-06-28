import java.security.InvalidParameterException

import scala.annotation.tailrec
import scala.runtime.Nothing$

/**
 * Interfaz generica para la lista
 *
* @tparam A
*/
sealed trait Lista[+A]

/**
* Objeto para definir lista vacia
*/
case object Nil extends Lista[Nothing]

/**
* Clase para definir la lista como compuesta por elemento inicial
* (cabeza) y resto (cola)
* @param cabeza
* @param cola
* @tparam A
*/
case class Cons[+A](cabeza : A, cola : Lista[A]) extends Lista[A]


object Lista{
  /**
  * Metodo para permitir crear listas sin usar new
  * @param elementos secuencia de elementos a incluir en la lista
  * @tparam A
  * @return
  */
  def apply[A](elementos : A*) : Lista[A] = elementos.size match {
    case 0 => Nil
    case _ => Cons(elementos.head, apply(elementos.tail: _*)).asInstanceOf[Lista[A]]
  }

  /**
  * Obtiene la longitud de una lista
  * @param lista
  * @tparam A
  * @return
  */
  def longitud[A](lista : Lista[A]) : Int = lista match {
    case Nil => 0
    case Cons(_ , cola) => 1 + longitud(cola)
  }

  /**
   * Metodo para sumar los valores de una lista de enteros
   * @param enteros
   * @return
   */
  def sumaEnteros(enteros : Lista[Int]) : Int = enteros match {
    case Nil => 0
    case Cons(head, tail) => head + sumaEnteros(tail)
  }

  /**
   *  Metodo para multiplicar los valores de una lista de enteros
   * @param enteros
   * @return
   */
  def productoEnteros(enteros : Lista[Int]) : Int = enteros match {
    case Nil => 1
    case Cons(head, tail) => head * productoEnteros(tail)
  }

  /**
  * Reemplaza la cabeza por nuevo valor. Se asume que si la lista esta vacia
  * se devuelve una lista con el nuevo elemento
  *
  * @param lista
  * @param cabezaNueva
  * @tparam A
  * @return
  */
  def asignarCabeza[A](lista : Lista[A], cabezaNueva : A) : Lista[A] = lista match {
    case Nil => Cons(cabezaNueva, Nil)
    case Cons(_, tail) => Cons(cabezaNueva, tail)
  }

  /**
  * Obtiene el elemento que ocupa la cabeza de la lista
  *
  * @param lista No puede ser vacia
  * @tparam A
   * @throws InvalidParameterException
  * @return
  */
  @throws[InvalidParameterException]
  def obtenerCabeza[A](lista : Lista[A]): A = lista match {
    case Nil => throw new InvalidParameterException
    case Cons(head, _) => head
  }

  /**
   * Elimina el elemento cabeza de la lista y devuelve la lista con
   * el resto de elementos
   * @param lista
   * @tparam A
   * @return
   */
  def obtenerCola[A](lista : Lista[A]): Lista[A] = lista match {
    case Nil => Nil
    case Cons(_, tail) => tail
  }

  /**
   * Elimina los n primeros elementos de una lista
  * @param lista lista con la que trabajar
  * @param n numero de elementos a eliminar
  * @tparam A tipo de datos
  * @return
  */
  def eliminar[A](lista : Lista[A], n: Int) : Lista[A] = lista match {
    case Nil => Nil
    case Cons(_, tail) => if(n > 0) eliminar(tail, n-1) else lista
  }

  /**
   * Elimina elementos mientra se cumple la condicion pasada como
   * argumento
   * @param lista lista con la que trabajar
   * @param criterio predicado a considerar para continuar con el borrado
   * @tparam A tipo de datos a usar
   * @return
   */
  def eliminarMientras[A](lista : Lista[A], criterio: A => Boolean) : Lista[A] = lista match {
    case Nil => Nil
    case Cons(head, tail) => if(criterio(head)) eliminarMientras(tail, criterio) else lista
  }

  /**
   * Elimina el ultimo elemento de la lista. Aqui no se pueden compartir
   * datos en los objetos y hay que generar una nueva lista copiando
   * datos
   * @param lista lista con la que trabajar
   * @tparam A tipo de datos de la lista
   * @return
   */
  def eliminarUltimo[A](lista : Lista[A]) : Lista[A] = lista match {
    case Nil => Nil
    case Cons(head, tail) => if(tail != Nil) Cons(head, eliminarUltimo(tail)) else Nil
  }

  /**
   * foldLeft con recursividad tipo tail
   * @param lista lista con la que trabajar
   * @param neutro elemento neutro
   * @param funcion funcion a aplicar
   * @tparam A parametros de tipo de elementos de la lista
   * @tparam B parametro de tipo del elemento neutro
   * @return
   */
  @annotation.tailrec
  def foldLeft[A, B](lista : Lista[A], neutro: B)(funcion : (B, A) => B): B = lista match {
    case Nil => neutro
    case Cons(head, tail) => {
      val intermedio = funcion(neutro, head)
      foldLeft(tail, intermedio)(funcion)
    }
  }

  /** suma via foldLeft
   * @param lista
   * @return
   */
  def sumaFoldLeft(lista : Lista[Int]): Int = {
    foldLeft(lista, 0)((x, y) => x+y)
  }



}
