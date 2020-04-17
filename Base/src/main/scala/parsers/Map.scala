package com.rayrobdod.stringContextParserCombinator
package parsers

import scala.language.higherKinds
import com.rayrobdod.stringContextParserCombinator.MacroCompat.Context

private[parsers] final class Map[CA[U <: Context with Singleton], CZ[U <: Context with Singleton]](
	backing:Parser[CA], mapping:ContextFunction1[CA, CZ]
) extends AbstractParser[CZ] {
	def parse(c:Context)(input:Input[c.type]):Result[c.type, CZ[c.type]] = {
		backing.parse(c)(input) match {
			case Success(v, r) => Success(mapping(c)(v), r)
			case Failure(found, expect) => Failure(found, expect)
		}
	}
}
