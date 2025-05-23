package name.rayrobdod.stringContextParserCombinatorExample.datetime

import name.rayrobdod.stringContextParserCombinator.typeclass.Repeated

/** Represents a base-ten digit. */
final class Digit private (private val value:Int)
/** Represents a sequence of base-ten digits. */
final class Digits(val value:Int)

object Digit {
	def apply(x:Char):Digit = {
		require('0' <= x && x <= '9', "Expected ascii digit")
		new Digit(x - '0')
	}

	implicit def given_Repeated:Repeated[Any, Digit, Digits] = new Repeated[Any, Digit, Digits]{
		type Acc = Int
		def init()(implicit ctx:Any):Acc = 0
		def append(acc:Acc, elem:Digit)(implicit ctx:Any):Acc = (acc * 10) + elem.value
		def result(acc:Acc)(implicit ctx:Any):Digits = new Digits(acc)
	}
}
