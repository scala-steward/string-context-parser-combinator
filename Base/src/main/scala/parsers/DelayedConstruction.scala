package com.rayrobdod.stringContextParserCombinator
package parsers

private[stringContextParserCombinator]
final class DelayedConstruction[Expr, A](
	backing:Function0[Parser[Expr, A]]
) extends AbstractParser[Expr, A] {
	def parse[ExprZ <: Expr, Pos](input:Input[ExprZ, Pos]):Result[ExprZ, Pos, A] = {
		backing.apply().parse(input)
	}
}
