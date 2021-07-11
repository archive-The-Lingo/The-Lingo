package lingo.corefp

import java.util.Collections
import izumi.reflect.macrortti.LightTypeTag
import izumi.reflect.macrortti.LTT
import izumi.reflect.Tag

object todo {
  def apply[A](): A = {
    throw new java.lang.Exception("WIP")
  }
}

object typeOf {
  def apply[T](implicit ev: Tag[T]): LightTypeTag = ev.tag
}

sealed trait Value {
  protected def internal_hash: Int

  private lazy val hash: Int = internal_hash

  override def hashCode(): Int = hash
}

object Value {
  private var components: java.util.concurrent.ConcurrentHashMap[ /*t: */ LightTypeTag, java.util.Map[Value, Any /*: t*/ ]] = new java.util.concurrent.ConcurrentHashMap()

  def dropComponents(): Unit = {
    components = new java.util.concurrent.ConcurrentHashMap()
  }

  private def defaultC: java.util.Map[Value, Any /*: t*/ ] = Collections.synchronizedMap(new java.util.WeakHashMap())

  private def getComponents(t: LightTypeTag): java.util.Map[Value, Any /*: t*/ ] = components.computeIfAbsent(t, _ => defaultC)

  def getComponentAny(t: LightTypeTag, v: Value): Option[Any /*: t*/ ] = Option(getComponents(t).get(v))

  def getComponent[T](v: Value)(implicit ev: Tag[T]): Option[T] = getComponentAny(typeOf[T], v).asInstanceOf[Option[T]]

  def getComponentOrAddAny(t: LightTypeTag, v: Value, default: => Any /*: t*/): Any /*: t*/ = getComponents(t).computeIfAbsent(v, _ => default)

  def getComponentOrAdd[T](v: Value, default: => T)(implicit ev: Tag[T]): T = getComponentOrAddAny(typeOf[T], v, default).asInstanceOf[T]

  def getComponentOrAddOptionAny(t: LightTypeTag, v: Value, default: => Option[Any /*: t*/ ]): Option[Any /*: t*/ ] = Option(getComponents(t).computeIfAbsent(v, _ => default.orNull))

  def getComponentOrAddOption[T](v: Value, default: => Option[T])(implicit ev: Tag[T]): Option[T] = getComponentOrAddOptionAny(typeOf[T], v, default).asInstanceOf[Option[T]]

  def getComponentOrComputeOptionAny(t: LightTypeTag, v: Value, default: Value => Option[Any /*: t*/ ]): Option[Any /*: t*/ ] = Option(getComponents(t).computeIfAbsent(v, v1 => default(v1).orNull))

  def getComponentOrComputeOption[T](v: Value, default: Value => Option[T])(implicit ev: Tag[T]): Option[T] = getComponentOrComputeOptionAny(typeOf[T], v, default).asInstanceOf[Option[T]]

  def addComponentAny(t: LightTypeTag, x: Any, v: Value): Value = {
    getComponents(t).put(v, x)
    v
  }

  def addComponent[T](x: Any, v: Value)(implicit ev: Tag[T]): Value = addComponentAny(typeOf[T], x, v)
}

final case class Atom(x: Symbol) extends Value {
  override def internal_hash: Int = scala.runtime.ScalaRunTime._hashCode(this)
}

object Atom {
  def apply(x: Symbol): Atom = new Atom(x)

  def apply(x: String): Atom = new Atom(Symbol(x))

  def unapply(x: Value): Option[Symbol] = x match {
    case v: Atom => Some(v.x)
    case v: PossiblyRecursive => unapply(v.get)
    case _ => None
  }
}

final case class EmptyList() extends Value {
  override def internal_hash: Int = scala.runtime.ScalaRunTime._hashCode(this)
}

object EmptyList {
  private def instance: EmptyList = new EmptyList()

  def apply(): EmptyList = instance

  def unapply(x: Value): Boolean = x match {
    case v: EmptyList => true
    case v: PossiblyRecursive => unapply(v.get)
    case _ => false
  }
}

final case class NonEmptyList(x: Value, y: Value) extends Value {
  override def internal_hash: Int = scala.runtime.ScalaRunTime._hashCode(this)
}

object NonEmptyList {
  def unapply(x: Value): Option[(Value, Value)] = x match {
    case v: NonEmptyList => Some((v.x, v.y))
    case v: PossiblyRecursive => unapply(v.get)
    case _ => None
  }
}

final case class Tagged(x: Value, y: Value) extends Value {
  override def internal_hash: Int = scala.runtime.ScalaRunTime._hashCode(this)
}

object Tagged {
  def unapply(x: Value): Option[(Value, Value)] = x match {
    case v: Tagged => Some((v.x, v.y))
    case v: PossiblyRecursive => unapply(v.get)
    case _ => None
  }
}

final case class Exception(x: Value, y: Value) extends Value {
  override def internal_hash: Int = scala.runtime.ScalaRunTime._hashCode(this)
}

object Exception {
  def unapply(x: Value): Option[(Value, Value)] = x match {
    case v: Exception => Some((v.x, v.y))
    case v: PossiblyRecursive => unapply(v.get)
    case _ => None
  }
}

final case class Resource(x: Value, y: Value, v: Any, t: LightTypeTag) extends Value {
  override def internal_hash: Int = scala.runtime.ScalaRunTime._hashCode(this)

  def get[T](implicit ev: Tag[T]): Option[T] = if (typeOf[T] == t) {
    Some(v.asInstanceOf[T])
  } else {
    None
  }
}


object Resource {
  def apply(x: Value, y: Value, v: Any, t: LightTypeTag): Resource = new Resource(x, y, v, t)

  def apply[T](x: Value, y: Value, v: T)(implicit ev: Tag[T]): Resource = new Resource(x, y, v, typeOf[T])

  def unapply(x: Value): Option[(Value, Value, Any, LightTypeTag)] = x match {
    case v: Resource => Some((v.x, v.y, v.v, v.t))
    case v: PossiblyRecursive => unapply(v.get)
    case _ => None
  }
}

final class PossiblyRecursive(x: => Value) extends Value {
  private lazy val result: Value = x

  def get: Value = result

  private val hashingLock: java.lang.Object = new java.lang.Object()
  private var hashing: Boolean = false

  override def internal_hash: Int = hashingLock.synchronized {
    if (hashing) {
      0
    } else {
      hashing = true
      var r: Option[Int] = None
      try {
        r = Some(result.hashCode())
      } finally {
        hashing = false
      }
      r.get
    }
  }
}