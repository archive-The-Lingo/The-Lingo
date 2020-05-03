/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the_lingo.lang

final class Delay(continue: => Value, stop: => (Mapping, Exp)) extends NotWeakHeadNormalForm {
  private lazy val cont = continue
  private lazy val readbck = stop

  def reduce_rec() = cont.reduce_rec()

  def eval(context: Mapping, stack: DebugStack) = cont.eval(context, stack)

  def readback() = readbck

  def app(xs: List[Value], stack: DebugStack) = cont.app(xs, stack)
}
