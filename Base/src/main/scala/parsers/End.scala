package com.rayrobdod.stringContextParserCombinator
package parsers

import com.rayrobdod.stringContextParserCombinator.MacroCompat.Context

private[parsers] final class End
extends AbstractParser[ContextTypes[Unit]#Ident] {
	override def parse(c:Context)(input:Input[c.type]):Result[c.type, Unit] = {
		if (input.isEmpty) {
			Success((), input)
		} else {
			Failure(input.next, this.expecting)
		}
	}
	def expecting:Failure.Expecting = Failure.Leaf("EOF")
}
