package com.rayrobdod.stringContextParserCombinator
package parsers

import com.rayrobdod.stringContextParserCombinator.MacroCompat.Context

private[parsers]
final class OfType[A](
	tpetag:ContextFunction0[ContextTypes[A]#TypeTag]
) extends AbstractParser[ContextTypes[A]#Expr] {
	def parse(c:Context)(input:Input[c.type]):Result[c.type, c.Expr[A]] = {
		input.consume(
			_ => None,
			arg => Some(arg).filter(x => x.actualType <:< tpetag(c).tpe).map(_.asInstanceOf[c.Expr[A]]),
			Failure.Leaf(tpetag(c).tpe.toString)
		)
	}
}
