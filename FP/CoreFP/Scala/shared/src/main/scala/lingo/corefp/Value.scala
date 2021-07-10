package lingo.corefp

object todo {
  def apply[A](): A = {
    throw new java.lang.Exception("WIP")
  }
}

import java.util.Collections
import scala.reflect.runtime.universe._

sealed trait Value {
  private lazy val cachedHashCode: Int = super.hashCode()

  override def hashCode(): Int = cachedHashCode
}

object Value {
  private var components: java.util.concurrent.ConcurrentHashMap[ /*t: */ Type, java.util.Map[Value, Any /*: t*/ ]] = new java.util.concurrent.ConcurrentHashMap()

  def dropComponents(): Unit = {
    components = new java.util.concurrent.ConcurrentHashMap()
  }

  private def defaultC: java.util.Map[Value, Any /*: t*/ ] = Collections.synchronizedMap(new java.util.WeakHashMap())

  private def getComponents(t: Type): java.util.Map[Value, Any /*: t*/ ] = components.computeIfAbsent(t, _ => defaultC)

  def getComponentAny(t: Type, v: Value): Option[Any /*: t*/ ] = Option(getComponents(t).get(v))

  def getComponent[T](v: Value)(implicit ttag: TypeTag[T]): Option[T] = getComponentAny(typeOf[T], v).asInstanceOf[Option[T]]

  def getComponentOrAddAny(t: Type, v: Value, default: => Any /*: t*/): Any /*: t*/ = getComponents(t).computeIfAbsent(v, _ => default)

  def getComponentOrAdd[T](v: Value, default: => T)(implicit ttag: TypeTag[T]): T = getComponentOrAddAny(typeOf[T], v, default).asInstanceOf[T]

  def getComponentOrAddOptionAny(t: Type, v: Value, default: => Option[Any /*: t*/ ]): Option[Any /*: t*/ ] = Option(getComponents(t).computeIfAbsent(v, _ => default.orNull))

  def getComponentOrAddOption[T](v: Value, default: => Option[T])(implicit ttag: TypeTag[T]): Option[T] = getComponentOrAddOptionAny(typeOf[T], v, default).asInstanceOf[Option[T]]
}

final case class Atom(x: Symbol) extends Value

case object EmptyList extends Value

final case class NonEmptyList(x: Value, y: Value) extends Value

final case class Tagged(x: Value, y: Value) extends Value

final case class Exception(x: Value, y: Value) extends Value

final case class Resource(x: Value, y: Value, v: Any, t: Type) extends Value {
  def get[T](implicit ttag: TypeTag[T]): Option[T] = if (typeOf[T] == t) {
    Some(v.asInstanceOf[T])
  } else {
    None
  }
}

object Resource {
  def apply(x: Value, y: Value, v: Any, t: Type): Resource = new Resource(x, y, v, t)

  def apply[T](implicit ttag: TypeTag[T], x: Value, y: Value, v: T): Resource = new Resource(x, y, v, typeOf[T])
}