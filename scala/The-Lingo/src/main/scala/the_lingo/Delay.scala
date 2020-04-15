/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the_lingo

final class Delay(continue: => Value, stop: => (Mapping, Exp)){
  private lazy val cont = continue
  private lazy val readbck =  stop
  def reduce() = cont.reduce()
  def eval(context: Mapping) = cont.eval(context)
  def readback() = readbck
  def apply(xs: List[Value]) = cont.apply(xs)
}