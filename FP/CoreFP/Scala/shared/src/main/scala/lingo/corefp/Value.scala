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

  def unapply(x: Value): Option[Symbol] = UnPossiblyRecursive(x) match {
    case v: Atom => Some(v.x)
    case _ => None
  }
}

final case class EmptyList() extends Value {
  override def internal_hash: Int = scala.runtime.ScalaRunTime._hashCode(this)
}

object EmptyList {
  private def instance: EmptyList = new EmptyList()

  def apply(): EmptyList = instance

  def unapply(x: Value): Boolean = UnPossiblyRecursive(x) match {
    case v: EmptyList => true
    case _ => false
  }
}

final case class NonEmptyList(x: Value, y: Value) extends Value {
  override def internal_hash: Int = scala.runtime.ScalaRunTime._hashCode(this)
}

object NonEmptyList {
  def unapply(x: Value): Option[(Value, Value)] = UnPossiblyRecursive(x) match {
    case v: NonEmptyList => Some((UnPossiblyRecursive(v.x), UnPossiblyRecursive(v.y)))
    case _ => None
  }
}

final case class Tagged(x: Value, y: Value) extends Value {
  override def internal_hash: Int = scala.runtime.ScalaRunTime._hashCode(this)
}

object Tagged {
  def unapply(x: Value): Option[(Value, Value)] = UnPossiblyRecursive(x) match {
    // inner UnPossiblyRecursive are for patterns like Tagged(<constant>, ...)
    case v: Tagged => Some((UnPossiblyRecursive(v.x), UnPossiblyRecursive(v.y)))
    case _ => None
  }
}

final case class Exception(x: Value, y: Value) extends Value {
  override def internal_hash: Int = scala.runtime.ScalaRunTime._hashCode(this)
}

object Exception {
  def unapply(x: Value): Option[(Value, Value)] = UnPossiblyRecursive(x) match {
    case v: Exception => Some((UnPossiblyRecursive(v.x), UnPossiblyRecursive(v.y)))
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

  def unapply(x: Value): Option[(Value, Value, Any, LightTypeTag)] = UnPossiblyRecursive(x) match {
    case v: Resource => Some((UnPossiblyRecursive(v.x), UnPossiblyRecursive(v.y), v.v, v.t))
    case _ => None
  }
}

private class MutableBoolean(var x: Boolean = false) {
  def in[T](failed: => T, body: => T): T = this.synchronized {
    if (this.x) {
      failed
    } else {
      this.x = true
      var r: Option[T] = None
      try {
        r = Some(body)
      } finally {
        this.x = false
      }
      r.get
    }
  }
}

final class PossiblyRecursive(x: => Value) extends Value {
  private val computing: MutableBoolean = new MutableBoolean()
  private lazy val result: Value = computing.in({
    ExceptionSeq(Atoms.TopLevelExceptionTags.WillNotTerminate)
  }, {
    var r = x
    while (r.isInstanceOf[PossiblyRecursive]) {
      r = r.asInstanceOf[PossiblyRecursive].result
    }
    r
  })

  def get: Value = result

  private val hashing: MutableBoolean = new MutableBoolean()

  override def internal_hash: Int = hashing.in(
    {
      0
    },
    {
      result.hashCode()
    }
  )

  override def equals(that: Any): Boolean = that.equals(result)
}

object UnPossiblyRecursive {
  def apply(x: Value): Value = if (x.isInstanceOf[PossiblyRecursive]) {
    apply(x.asInstanceOf[PossiblyRecursive].get)
  } else {
    x
  }
}