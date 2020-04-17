package com.rayrobdod.stringContextParserCombinator
package parsers

import scala.language.higherKinds
import com.rayrobdod.stringContextParserCombinator.MacroCompat.Context

object Lifted {
	def apply[Lifter[A], Z](
		lifterType:ContextTypeFunction,
		lift:LiftFunction[Lifter, Z],
		description:Failure.Expecting
	):AbstractParser[ContextTypes[Z]#Expr] = {
		new AbstractParser[ContextTypes[Z]#Expr] {
			def parse(c:Context)(input:Input[c.type]):Result[c.type, c.Expr[Z]] = {
				input.consume(
					_ => None,
					arg => (Some(arg)
						.map(x => ((c.inferImplicitValue(lifterType(c)(x.actualType)), x.tree)))
						.filter(! _._1.isEmpty)
						.map(x => lift.apply(c)(c.Expr(x._1), c.Expr(x._2)))
					),
					description
				)
			}
		}
	}
}
