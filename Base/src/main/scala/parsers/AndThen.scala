package com.rayrobdod.stringContextParserCombinator
package parsers

import scala.language.higherKinds
import com.rayrobdod.stringContextParserCombinator.MacroCompat.Context

private[parsers] final class AndThen[CA[U <: Context with Singleton], CB[U <: Context with Singleton], CZ[U <: Context with Singleton]](
	left:Parser[CA], right:Parser[CB], ev:Implicits.AndThenTypes[CA, CB, CZ]
) extends Parser[CZ] {
	def parse(c:Context)(input:Input[c.type]):Result[c.type, CZ[c.type]] = {
		left.parse(c)(input) match {
			case Success(a, resa) => right.parse(c)(resa) match {
				case Success(b, resb) => Success(ev.aggregate(c)(a,b), resb)
				case Failure(found, expect) => Failure(found, expect)
			}
			case Failure(found, expect) => Failure(found, expect)
		}
	}
}
