package com.rayrobdod.stringContextParserCombinator
package parsers

private[stringContextParserCombinator]
final class Repeat[Expr, A, Z](
	inner:Parser[Expr, A],
	min:Int,
	max:Int,
	delimiter:Parser[Expr, Unit],
	ev:typelevel.Repeated[A, Z]
) extends AbstractParser[Expr, Z] {
	require(min >= 0)
	require(max >= 1)
	require(max >= min)

	def parse(input:Input[Expr]):Result[Expr, Z] = {
		Repeat.parse0(input, inner, min, max, delimiter, true) match {
			case f:Failure => f
			case s:Success[Expr, List[A]] => s.mapValues({parts =>
				val acc = ev.init()
				parts.foreach(part => ev.append(acc, part))
				ev.result(acc)
			})
		}
	}
}

private[stringContextParserCombinator]
object Repeat {
	private def parse0[Expr, A](input:Input[Expr], inner:Parser[Expr, A], min:Int, max:Int, delimiter:Parser[Expr, Unit], isFirst:Boolean):Result[Expr, List[A]] = {
		(if (isFirst) {Success((), input, Set.empty, Cut.False)} else {delimiter.parse(input)}) match {
			case Failure(expectingDelimiter, cutDelimiter) => {
				if (min != 0 || cutDelimiter.toBoolean) {
					Failure(expectingDelimiter, cutDelimiter)
				} else {
					Success(Nil, input, expectingDelimiter, cutDelimiter)
				}
			}
			case successDelimiter:Success[Expr, Unit] => successDelimiter.flatMap[Expr, List[A]]({case Success1((), restDelimiter, expectingDelimiter, cutDelimiter) =>
				inner.parse(restDelimiter) match {
					case Failure(expectingA, cutA) => {
						if (min != 0 || cutDelimiter.toBoolean || cutA.toBoolean) {
							Failure(expectingDelimiter ++ expectingA, cutDelimiter | cutA)
						} else {
							Success(Nil, input, expectingDelimiter ++ expectingA, cutDelimiter | cutA)
						}
					}
					case successA:Success[Expr, A] => successA.flatMap[Expr, List[A]]({case Success1(valueA, restA, expectingA, cutA) =>
						if (max == 1 || restA == input) {
							// `restA == input` means quit if inner seems to be making no progress
							if (min != 0 || cutA.toBoolean) {
								Success(valueA :: Nil, restA, expectingDelimiter ++ expectingA, cutDelimiter | cutA)
							} else {
								Success(
									Success1(valueA :: Nil, restA, expectingDelimiter ++ expectingA, cutDelimiter | cutA),
									List(
										Success1(Nil, input, expectingDelimiter, cutDelimiter)
									)
								)
							}
						} else {
							parse0(restA, inner, math.max(0, min - 1), max - 1, delimiter, false) match {
								case Failure(expectingC, cutC) => Failure(expectingA ++ expectingDelimiter ++ expectingC, cutA | cutDelimiter | cutC)
								case successC:Success[Expr, List[A]] => {
									val successCWithValA = successC.map({case Success1(valueC, restC, expectingC, cutC) =>
										Success1(valueA :: valueC, restC, expectingA ++ expectingDelimiter ++ expectingC, cutA | cutDelimiter | cutC)
									})
									if (min == 0 && !successCWithValA.choicesHead.isCut.toBoolean) {
										successCWithValA :+
											Success1(Nil, input, Set.empty, Cut.False)
									} else {
										successCWithValA
									}
								}
							}
						}
					})
				}
			})
		}
	}
}
