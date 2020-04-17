package com.rayrobdod.stringContextParserCombinator
package parsers

import scala.language.higherKinds
import com.rayrobdod.stringContextParserCombinator.MacroCompat.Context

private[parsers] final class Filter[CA[U <: Context with Singleton]](
	backing:Parser[CA], predicate:ContextFunction1[CA, ContextTypes[Boolean]#Ident], description:Failure.Expecting
) extends AbstractParser[CA] {
	def parse(c:Context)(input:Input[c.type]):Result[c.type, CA[c.type]] = {
		backing.parse(c)(input) match {
			case Success(value, remain) if predicate(c)(value) => Success(value, remain)
			case Success(_, _) => Failure(input.next, description)
			case Failure(found, exp) => Failure(found, exp)
		}
	}
}
