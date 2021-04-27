package tiny

import scala.collection._

/**
  * These utility methods create Scala lists and option values.
  * Use them in your parser to create these objects easily.
  *
  * Your AST should not store any Java collection objects. If you
  * wish to use other types of collections, simply add similar
  * construction methods to this file.
  *
  * Usage:
  *
  * In Java code or CUP file, use these methods as follows, after importing
  * the Scala classes:
  *
  * {{{
  *    import scala.collection.immutable.*;
  *
  *    ...
  *
  *    List<String> l = ParserUtil.empty();
  *    l = ParserUtil.cons(l, "cow");
  *    l = ParserUtil.append(l, "moo");
  *
  *    l.length() // or any other Scala list method
  *
  *    Option<String> s = ParserUtil.none();
  *    s = ParserUtil.some("moo");
  *
  *    s.isDefined() // or any other Scala Option method
  * }}}
  */
object ParseUtil {

  /**
    * Create an empty sequence.
    */
  def empty[A](): Seq[A] = Vector()

  /**
    * Create a singleton sequence.
    */
  def single[A](a: A): Seq[A] = {
    assert(a != null)
    Vector(a)
  }

  /**
    * Insert element a onto the front of sequence l.
    */
  def cons[A,T <: A](a: T, l: Seq[A]): Seq[A] = {
    assert(a != null)
    assert(l != null)
    a +: l
  }

  /**
    * Append element a onto the sequence l.
    */
  def append[A,T <: A](l: Seq[A], a: T): Seq[A] = {
    assert(l != null)
    assert(a != null)
    l :+ a
  }

  /**
    * Create the empty value for Option[A].
    */
  def none[A]() : Option[A] = None

  /**
    * Create the value Some(a) for type Option[A].
    */
  def some[A](a : A) : Option[A] = {
    assert(a != null)
    Some(a)
  }

  /**
    * Create a 2-tuple.
    */
  def pair[A,B](a: A, b: B) : Tuple2[A,B] = {
    assert(a != null)
    assert(b != null)
    (a, b)
  }

  /**
    * Access 1st element of Tuple.
    */
  def first[A,B](tuple2: Tuple2[A,B]): A = tuple2._1
  def first[A,B,C](tuple3: Tuple3[A,B,C]): A = tuple3._1

  /**
    * Access 2nd element of Tuple.
    */
  def second[A,B](tuple2: Tuple2[A,B]): B = tuple2._2
  def second[A,B,C](tuple3: Tuple3[A,B,C]): B = tuple3._2

  /**
    * Access 1st element of Tuple.
    */
  def third[A,B,C](tuple3: Tuple3[A,B,C]): C = tuple3._3

  /**
    * Create a 3-tuple.
    */
  def triple[A,B,C](a: A, b: B, c: C) : Tuple3[A,B,C] = {
    assert(a != null)
    assert(b != null)
    assert(c != null)
    (a, b, c)
  }


}

