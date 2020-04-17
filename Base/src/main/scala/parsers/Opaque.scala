package com.rayrobdod.stringContextParserCombinator
package parsers

import scala.language.higherKinds
import com.rayrobdod.stringContextParserCombinator.MacroCompat.Context

private[parsers] final class Opaque[CA[U <: Context with Singleton]](
	backing:Parser[CA], description:Failure.Expecting
) extends AbstractParser[CA] {
	def parse(c:Context)(input:Input[c.type]):Result[c.type, CA[c.type]] = {
		backing.parse(c)(input) match {
			case Success(v, r) => Success(v,r)
			case Failure(f, _) => Failure(f, description)
		}
	}
}
