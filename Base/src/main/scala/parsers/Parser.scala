package com.rayrobdod.stringContextParserCombinator
package parsers

private[stringContextParserCombinator]
trait Parser[-Expr, +A] {
	def interpolate[ExprZ <: Expr, Pos](input:Input[ExprZ, Pos])(implicit ev1:Ordering[Pos]):Result[ExprZ, Pos, A]
}
