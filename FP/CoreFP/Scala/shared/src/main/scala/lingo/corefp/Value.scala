package lingo.corefp

import java.util.Collections
import scala.reflect.runtime.universe._

sealed trait Value {

}

object Value {
  private val components: java.util.concurrent.ConcurrentHashMap[ /*t: */ Type, java.util.Map[Value, Any /*: t*/ ]] = new java.util.concurrent.ConcurrentHashMap()

  private def defaultC: java.util.Map[Value, Any /*: t*/ ] = Collections.synchronizedMap(new java.util.WeakHashMap())

  private def getComponents(t: Type): java.util.Map[Value, Any /*: t*/ ] = components.computeIfAbsent(t, _ => defaultC)

  def getComponent(t: Type, v: Value): Option[Any /*: t*/ ] = Option(getComponents(t).get(v))

  def getComponent[T](implicit ttag: TypeTag[T], v: Value): Option[T] = Option(getComponents(typeOf[T]).get(v).asInstanceOf[T])

  def getComponentOrAdd(t: Type, v: Value, default: => Any /*: t*/): Any /*: t*/ = getComponents(t).computeIfAbsent(v, _ => default)

  def getComponentOrAdd[T](implicit ttag: TypeTag[T], v: Value, default: => T): T = getComponents(typeOf[T]).computeIfAbsent(v, _ => default).asInstanceOf[T]
}