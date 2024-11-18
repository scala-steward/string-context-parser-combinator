package name.rayrobdod.stringContextParserCombinator
package typeclass
package repeated

import scala.collection.mutable.Builder
import scala.quoted.*
import munit.Location
import Interpolator.{charWhere, ofType, end}
import Repeated.SplicePiece

object QuotedConcatenateStringTestImpls {
	def assertParseSuccessImpl(
		self: Expr[munit.FunSuite],
		sc: Expr[StringContext],
		args: Expr[Seq[Any]],
		expecting: Expr[String],
		loc: Expr[Location])(
		using Quotes
	):Expr[Unit] = {

		val dut = (charWhere(_ => true).repeat(1).mapToExpr orElse ofType[String])
			.repeat()(using Repeated.quotedConcatenateString)
			.andThen(end)

		val actual = dut.interpolate(sc, args)

		'{
			given Location = ${loc}
			$self.assertEquals($actual, $expecting)
		}
	}
}

object QuotedFromSplicesUsingBuilderTestImpls {
	def assertParseSuccessImpl[Int, Z](
		self: Expr[munit.FunSuite],
		newAcc: Expr[Builder[Int,Z]],
		ifZeroDefined: Expr[Boolean],
		ifZeroApply: Expr[() => Z],
		ifOneScalarDefined: Expr[Boolean],
		ifOneScalarApply: Expr[(Int) => Z],
		ifOneSpliceDefined: Expr[Boolean],
		ifOneSpliceApply: Expr[(IterableOnce[Int]) => Z],
		elems: Expr[Seq[Int | Seq[Int]]],
		expecting: Expr[Z],
		loc: Expr[Location])(
		using Quotes, Type[Int], Type[Z]
	):Expr[Unit] = {
		val Varargs(elems2) = elems: @unchecked
		val elems3: Seq[SplicePiece[Expr, Int]] = elems2.map:
			case '{ $x: Int } => SplicePiece.One[Expr, Int](x)
			case '{ $xs: Seq[Int] } => SplicePiece.Many[Expr, Int](xs)

		val ifZeroDefinedAt2 = ifZeroDefined.valueOrAbort
		val ifZero = Option.when(ifZeroDefinedAt2)({() => '{${ifZeroApply}()}})

		val ifOneScalar = new PartialFunction[Expr[Int], Expr[Z]] {
			def isDefinedAt(x:Expr[Int]):Boolean = ifOneScalarDefined.valueOrAbort
			def apply(x:Expr[Int]):Expr[Z] = '{${ifOneScalarApply}($x)}
		}

		val ifOneSplice = new PartialFunction[Expr[IterableOnce[Int]], Expr[Z]] {
			def isDefinedAt(x:Expr[IterableOnce[Int]]):Boolean = ifOneSpliceDefined.valueOrAbort
			def apply(x:Expr[IterableOnce[Int]]):Expr[Z] = '{${ifOneSpliceApply}($x)}
		}

		val dut = Repeated.quotedFromSplicesUsingBuilder(newAcc, ifZero, ifOneScalar, ifOneSplice)

		val actual = dut.result(elems3.foldLeft(dut.init())((acc, elem) => dut.append(acc, elem)))

		'{
			given Location = ${loc}
			$self.assertEquals($actual, $expecting)
		}
	}
}
