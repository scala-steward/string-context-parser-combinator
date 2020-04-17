package com.rayrobdod.stringContextParserCombinator
package parsers

import scala.language.higherKinds
import com.rayrobdod.stringContextParserCombinator.MacroCompat.Context

private[parsers]
final class OrElse[CA[U <: Context with Singleton]](
	left:Parser[CA],
	right:Parser[CA]
) extends AbstractParser[CA] {
	def parse(c:Context)(input:Input[c.type]):Result[c.type, CA[c.type]] = {
		left.parse(c)(input) match {
			case Success(v, r) => Success(v, r)
			case Failure(found1, expect1) => right.parse(c)(input) match {
				case Success(v, r) => Success(v, r)
				case Failure(found2, expect2) => {
					if (found1._2 == found2._2) {Failure(found1, Failure.Or(Seq(expect1, expect2)))}
					else if (found1._2 > found2._2) {Failure(found1, expect1)}
					else {Failure(found2, expect2)}
				}
			}
		}
	}
}
