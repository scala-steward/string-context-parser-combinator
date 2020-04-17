package com.rayrobdod.stringContextParserCombinator
package parsers

import scala.language.higherKinds
import com.rayrobdod.stringContextParserCombinator.MacroCompat.Context

private[parsers] final class DelayedConstruction[CA[U <: Context with Singleton]](
	backing:Function0[Parser[CA]]
) extends AbstractParser[CA] {
	def parse(c:Context)(input:Input[c.type]):Result[c.type, CA[c.type]] = {
		backing.apply.parse(c)(input)
	}
}
