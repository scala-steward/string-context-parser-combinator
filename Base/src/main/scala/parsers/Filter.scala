package com.rayrobdod.stringContextParserCombinator
package parsers

private[stringContextParserCombinator]
final class Filter[Expr, A](
	backing:Parser[Expr, A],
	predicate:Function1[A, Boolean],
	predicateDescription:ExpectingDescription
) extends AbstractParser[Expr, A] {
	def parse[Pos](input:Input[Expr, Pos]):Result[Expr, Pos, A] = {
		backing.parse(input) match {
			case Success(choicesHead, choicesTail) => {
				val choices = choicesHead :: choicesTail
				val filteredChoices = choices.filter(x => predicate(x.value))
				filteredChoices match {
					case head :: tail => Success(head, tail)
					case Nil => {
						choices.map(s => Failure(s.expecting.map(_.where(predicateDescription)), s.isCut)).reduce[Failure[Pos]](_ or _)
					}
				}
			}
			case failure => failure
		}
	}
}