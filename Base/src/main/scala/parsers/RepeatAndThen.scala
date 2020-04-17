package com.rayrobdod.stringContextParserCombinator
package parsers

import scala.collection.immutable.Seq
import scala.language.higherKinds
import com.rayrobdod.stringContextParserCombinator.MacroCompat.Context

/**
 * `Repeat(inner, min, max, evL).andThen(rhs)(evR)`
 *
 * Required since Repeat is greedy, and AndThen doesn't know how to make a Repeat backtrack.
 *
 * e.x. `"1".repeat().andThen("1")` would fail to match "11", since the repeat would match the entire
 * string, leaving nothing for the andThen, which will not match an EOF and the entire expression fails.
 * With this, after failing to match when the repeat sucks up everything, this will try again with the
 * repeat accepting one less "1" than before, which then allows the rest of the parser to succeed
 */
private[parsers]
final class RepeatAndThen[CA[U <: Context with Singleton], CAS[U <: Context with Singleton], CB[U <: Context with Singleton], CZ[U <: Context with Singleton]](
	inner:Parser[CA],
	min:Int,
	max:Int,
	evL:Implicits.RepeatTypes[CA, CAS],
	rhs:Parser[CB],
	evR:Implicits.AndThenTypes[CAS, CB, CZ]
) extends AbstractParser[CZ] {
	def parse(c:Context)(input:Input[c.type]):Result[c.type, CZ[c.type]] = {
		var counter:Int = 0
		val accumulator = evL.init(c)
		var remaining:Input[c.type] = input
		var continue:Boolean = true
		var innerExpecting:Failure = null
		val states = scala.collection.mutable.Stack[Success[c.type, CAS[c.type]]]()

		states.push(Success(evL.result(c)(accumulator), input))
		while (continue && counter < max) {
			inner.parse(c)(remaining) match {
				case Success(a, r) => {
					counter += 1
					evL.append(c)(accumulator, a)
					states.push(Success(evL.result(c)(accumulator), r))
					continue = (remaining != r) // quit if inner seems to be making no progress
					remaining = r
				}
				case failure:Failure => {
					innerExpecting = failure
					continue = false
				}
			}
		}

		var rhsExpecting:Failure = null
		while (counter >= min && states.nonEmpty) {
			val top = states.pop()
			rhs.parse(c)(top.remaining) match {
				case Success(a, r) => {
					return Success(evR.aggregate(c)(top.value, a), r)
				}
				case failure:Failure => {
					if (rhsExpecting == null) {
						rhsExpecting = failure
					}
					counter = counter - 1
					// try next
				}
			}
		}

		if (null == innerExpecting) {
			// means that input saturates the repeat portion of this aggregate
			rhsExpecting
		} else if (null == rhsExpecting) {
			// means that input does not meet minimum requirements the repeat portion of this aggregate
			innerExpecting
		} else {
			Failure(innerExpecting.found, Failure.Or(Seq(innerExpecting.expecting, rhsExpecting.expecting)))
		}
	}

	override def andThen[CC[U <: Context with Singleton], CZ2[U <: Context with Singleton]](newParser:Parser[CC])(implicit ev:Implicits.AndThenTypes[CZ,CC,CZ2]):Parser[CZ2] = {
		new RepeatAndThen[CA, CAS, ContextTypesTuple2[CB, CC]#X, CZ2](
			this.inner,
			this.min,
			this.max,
			this.evL,
			this.rhs.andThen[CC, ContextTypesTuple2[CB, CC]#X](newParser)(Implicits.AndThenTypes.andThenGeneric[CB, CC]),
			new Implicits.AndThenTypes[CAS, ContextTypesTuple2[CB, CC]#X, CZ2] {
				def aggregate(c:Context)(as:CAS[c.type], bc:(CB[c.type], CC[c.type])):CZ2[c.type] = ev.aggregate(c)(evR.aggregate(c)(as, bc._1), bc._2)
			}
		)
	}
}
