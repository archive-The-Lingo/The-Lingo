package lingo.corefp

import java.util.Collections
import izumi.reflect.macrortti.LightTypeTag
import izumi.reflect.Tag

object todo {
  def apply[A](): A = {
    throw new java.lang.Exception("WIP")
  }
}

object typeOf {
  def apply[T](implicit ev: Tag.auto.T[T]): LightTypeTag = ev.tag
}

sealed trait Value {
  // todo: check me
  //private lazy val cachedHashCode: Int = super.hashCode()
  //override def hashCode(): Int = cachedHashCode
}

object Value {
  private var components: java.util.concurrent.ConcurrentHashMap[ /*t: */ LightTypeTag, java.util.Map[Value, Any /*: t*/ ]] = new java.util.concurrent.ConcurrentHashMap()

  def dropComponents(): Unit = {
    components = new java.util.concurrent.ConcurrentHashMap()
  }

  private def defaultC: java.util.Map[Value, Any /*: t*/ ] = Collections.synchronizedMap(new java.util.WeakHashMap())

  private def getComponents(t: LightTypeTag): java.util.Map[Value, Any /*: t*/ ] = components.computeIfAbsent(t, _ => defaultC)

  def getComponentAny(t: LightTypeTag, v: Value): Option[Any /*: t*/ ] = Option(getComponents(t).get(v))

  def getComponent[T](v: Value)(implicit ev: Tag.auto.T[T]): Option[T] = getComponentAny(typeOf[T], v).asInstanceOf[Option[T]]

  def getComponentOrAddAny(t: LightTypeTag, v: Value, default: => Any /*: t*/): Any /*: t*/ = getComponents(t).computeIfAbsent(v, _ => default)

  def getComponentOrAdd[T](v: Value, default: => T)(implicit ev: Tag.auto.T[T]): T = getComponentOrAddAny(typeOf[T], v, default).asInstanceOf[T]

  def getComponentOrAddOptionAny(t: LightTypeTag, v: Value, default: => Option[Any /*: t*/ ]): Option[Any /*: t*/ ] = Option(getComponents(t).computeIfAbsent(v, _ => default.orNull))

  def getComponentOrAddOption[T](v: Value, default: => Option[T])(implicit ev: Tag.auto.T[T]): Option[T] = getComponentOrAddOptionAny(typeOf[T], v, default).asInstanceOf[Option[T]]

  def getComponentOrComputeOptionAny(t: LightTypeTag, v: Value, default: Value => Option[Any /*: t*/ ]): Option[Any /*: t*/ ] = Option(getComponents(t).computeIfAbsent(v, v1 => default(v1).orNull))

  def getComponentOrComputeOption[T](v: Value, default: Value => Option[T])(implicit ev: Tag.auto.T[T]): Option[T] = getComponentOrComputeOptionAny(typeOf[T], v, default).asInstanceOf[Option[T]]

  def addComponentAny(t: LightTypeTag, x: Any, v: Value): Value = {
    getComponents(t).put(v, x)
    v
  }

  def addComponent[T](x: Any, v: Value)(implicit ev: Tag.auto.T[T]): Value = addComponentAny(typeOf[T], x, v)
}

final case class Atom(x: Symbol) extends Value

object Atom {
  def apply(x: Symbol): Atom = new Atom(x)

  def apply(x: String): Atom = new Atom(Symbol(x))
}

case object EmptyList extends Value

final case class NonEmptyList(x: Value, y: Value) extends Value

final case class Tagged(x: Value, y: Value) extends Value

final case class Exception(x: Value, y: Value) extends Value

final case class Resource(x: Value, y: Value, v: Any, t: LightTypeTag) extends Value {
  def get[T](implicit ev: Tag.auto.T[T]): Option[T] = if (typeOf[T] == t) {
    Some(v.asInstanceOf[T])
  } else {
    None
  }
}

object Resource {
  def apply(x: Value, y: Value, v: Any, t: LightTypeTag): Resource = new Resource(x, y, v, t)

  def apply[T](x: Value, y: Value, v: T)(implicit ev: Tag.auto.T[T]): Resource = new Resource(x, y, v, typeOf[T])
}