package com.rayrobdod.stringContextParserCombinator
package parsers

import scala.language.higherKinds
import com.rayrobdod.stringContextParserCombinator.MacroCompat.Context

private[parsers]
final class Repeat[CA[U <: Context with Singleton], CZ[U <: Context with Singleton]](
	inner:Parser[CA],
	min:Int,
	max:Int,
	ev:Implicits.RepeatTypes[CA, CZ]
) extends AbstractParser[CZ] {
	def parse(c:Context)(input:Input[c.type]):Result[c.type, CZ[c.type]] = {
		var counter:Int = 0
		val accumulator = ev.init(c)
		var remaining:Input[c.type] = input
		var continue:Boolean = true
		var innerExpecting:Failure = null

		while (continue && counter < max) {
			inner.parse(c)(remaining) match {
				case Success(a, r) => {
					counter += 1
					ev.append(c)(accumulator, a)
					continue = (remaining != r) // quit if inner seems to be making no progress
					remaining = r
				}
				case failure:Failure => {
					innerExpecting = failure
					continue = false
				}
			}
		}
		if (min <= counter && counter <= max) {
			return Success(ev.result(c)(accumulator), remaining)
		} else {
			return innerExpecting
		}
	}

	override def andThen[CB[U <: Context with Singleton], CZ2[U <: Context with Singleton]](rhs:Parser[CB])(implicit ev:Implicits.AndThenTypes[CZ,CB,CZ2]):Parser[CZ2] = {
		new RepeatAndThen[CA, CZ, CB, CZ2](this.inner, this.min, this.max, this.ev, rhs, ev)
	}
}
