package com.rayrobdod.stringContextParserCombinator
package parsers

import scala.language.higherKinds
import com.rayrobdod.stringContextParserCombinator.MacroCompat.Context

private[parsers] final class FlatMap[CA[U <: Context with Singleton], CZ[U <: Context with Singleton]](
	left:Parser[CA], right:ContextFunction1[CA, ContextTypes[Parser[CZ]]#Ident]
) extends AbstractParser[CZ] {
	def parse(c:Context)(input:Input[c.type]):Result[c.type, CZ[c.type]] = {
		left.parse(c)(input) match {
			case Success(a, resa) => right(c)(a).parse(c)(resa)
			case Failure(found, expect) => Failure(found, expect)
		}
	}
}
