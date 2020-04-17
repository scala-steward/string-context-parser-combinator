package com.rayrobdod.stringContextParserCombinatorExample.datetime

import java.time._
import scala.Predef.charWrapper
import com.rayrobdod.stringContextParserCombinator.{Parsers => scpcParsers, _}
import com.rayrobdod.stringContextParserCombinator.Parsers._
import com.rayrobdod.stringContextParserCombinator.MacroCompat.Context

/** Implicit methods to convert things to parsers or to add symbolic methods to parsers */
trait ParsersImplictly {
	import scala.language.implicitConversions
	implicit def str2parser(str:String):Parser[ContextTypes[Unit]#Ident] = scpcParsers.IsString(str)
	implicit def type2parser[A](tpe:ctx.TypeTag[A]):Parser[ContextTypes[A]#Expr] = scpcParsers.OfType(tpe)
	implicit def parserWithSymbolic[A](psr:Parser[A]) = new ParserWithSymbolic[ctx.type, A](psr)
	implicit def str2parserWithSymbolic(str:String) = this.parserWithSymbolic(this.str2parser(str))
	implicit def type2parserWithSymbolic[A](tpe:ctx.TypeTag[A]) = this.parserWithSymbolic(this.type2parser(tpe))
}

/** Adds symbolic methods to Parsers */
class ParserWithSymbolic[CA[U <: Context with Singleton]](val backing:Parser[CA]) extends AnyVal {
	def ~[CB[U <: Context with Singleton], CZ[U <: Context with Singleton]](rhs:Parser[CB])(implicit ev:Implicits.AndThenTypes[CA,CB,CZ]) = backing.andThen(rhs)(ev)
	def |(rhs:Parser[CA]) = backing.orElse(rhs)
	def rep[CZ[U <: Context with Singleton]](min:Int = 0, max:Int = Integer.MAX_VALUE)(implicit ev:Implicits.RepeatTypes[CA, CZ]) = backing.repeat(min, max)(ev)
	def opt[CZ[U <: Context with Singleton]](implicit ev:Implicits.OptionallyTypes[CA, CZ]) = backing.optionally(ev)
}

object MacroImpl {
	/** Represents a base-ten digit. */
	private[this] final class Digit(val value:Int)
	private[this] final class Digits(val value:Int)

	private[this] implicit object DigitRepeatTypes extends Implicits.RepeatTypes[Digit, Digits] {
		class Box(var value:Int)
		type Acc = Box
		def init():Acc = new Box(0)
		def append(acc:Acc, elem:Digit):Unit = {acc.value *= 10; acc.value += elem.value}
		def result(acc:Acc):Digits = new Digits(acc.value)
	}

	private[this] trait Parsers extends ParsersImplictly {
		val IsDigit:Parser[ContextTypes[Digit]#Ident] = CharIn('0' to '9').map(x => new Digit(x - '0'))

		def Int2Digits(min:Int, max:Int) = (IsDigit.rep(2, 2))
			.map(_.value)
			.filter(x => min <= x && x <= max, String.format(""""$1%02d" - "$2%02d"""", Integer.valueOf(min), Integer.valueOf(max)))

		def YearP:Parser[ContextTypes[Year]#Expr] = {
			val LiteralP:Parser[ContextTypes[Year]#Expr] = {
				(CharIn("-+").opt ~ IsDigit.rep(1, 9).map(_.value))
					.map({x => if (x._1 == Some('-')) {-x._2} else {x._2}})
					.opaque("\"-999999999\"-\"999999999\"")
					.map(x =>
						{
							val xExpr = ctx.Expr[Int](ctx.universe.Literal(ctx.universe.Constant(x)))
							ctx.universe.reify(java.time.Year.of(xExpr.splice))
						}
					)
			}
			val VariableP:Parser[ContextTypes[Year]#Expr] = OfType(ctx.typeTag[Year])
			VariableP | LiteralP
		}

		def MonthP:Parser[ContextTypes[Month]#Expr] = {
			def monthOfTree(name:String):ctx.Expr[Month] = {
				ctx.Expr(
					ctx.universe.Select(
						ctx.universe.Select(
							ctx.universe.Select(
								ctx.universe.Ident(
									MacroCompat.newTermName(ctx)("java")
								),
								MacroCompat.newTermName(ctx)("time")
							),
							MacroCompat.newTermName(ctx)("Month")
						),
						MacroCompat.newTermName(ctx)(name)
					)
				)
			}
			val LiteralP:Parser[ContextTypes[Month]#Expr] = {
				Int2Digits(1, 12)
					.map(Month.of _)
					.map(_.name)
					.map(monthOfTree _)
			}
			val VariableP:Parser[ContextTypes[Month]#Expr] = OfType(ctx.typeTag[Month])
			VariableP | LiteralP
		}

		def Day31P:Parser[ContextTypes[Int]#Expr] = {
			val LiteralP:Parser[ContextTypes[Int]#Expr] = {
				Int2Digits(1, 31)
					.map(x => ctx.Expr(ctx.universe.Literal(ctx.universe.Constant(x))))
			}
			LiteralP
		}

		def YearMonthP:Parser[ContextTypes[YearMonth]#Expr] = {
			val PartsP:Parser[ContextTypes[YearMonth]#Expr] = (YearP ~ "-" ~ MonthP).map(x => {
				val (y, m) = x
				ctx.universe.reify(y.splice.atMonth(m.splice))
			})
			val VariableP:Parser[ContextTypes[YearMonth]#Expr] = OfType(ctx.typeTag[YearMonth])
			VariableP | PartsP
		}

		def LocalDateP:Parser[ContextTypes[LocalDate]#Expr] = {
			val YearMonthVariantP:Parser[ContextTypes[LocalDate]#Expr] = (YearMonthP ~ "-" ~ Day31P).map(x => {
				val (ym, day) = x
				ctx.universe.reify(ym.splice.atDay(day.splice))
			})
			val VariableP:Parser[ContextTypes[LocalDate]#Expr] = OfType(ctx.typeTag[LocalDate])
			VariableP | YearMonthVariantP
		}

		def HourP:Parser[ContextTypes[Int]#Expr] = {
			val LiteralP:Parser[ContextTypes[Int]#Expr] = {
				Int2Digits(0, 23)
					.map(x => ctx.Expr(ctx.universe.Literal(ctx.universe.Constant(x))))
			}
			LiteralP
		}

		def MinuteP:Parser[ContextTypes[Int]#Expr] = {
			val LiteralP:Parser[ContextTypes[Int]#Expr] = {
				Int2Digits(0, 59)
					.map(x => ctx.Expr(ctx.universe.Literal(ctx.universe.Constant(x))))
			}
			LiteralP
		}

		def SecondP:Parser[ContextTypes[Int]#Expr] = MinuteP

		def NanoP:Parser[ContextTypes[Int]#Expr] = {
			val LiteralP = CharIn('0' to '9').rep(1, 9)
				.map(x => s"${x}000000000".substring(0, 9))
				.map(Integer.parseInt _)
				.opaque("\"0\"-\"999999999\"")
				.map(x => ctx.Expr(ctx.universe.Literal(ctx.universe.Constant(x))))
			LiteralP
		}

		def LocalTimeP:Parser[ContextTypes[LocalTime]#Expr] = {
			val LiteralP:Parser[ContextTypes[LocalTime]#Expr] = (HourP ~ ":" ~ MinuteP ~ (":" ~ SecondP ~ ("." ~ NanoP).opt).opt)
				.map({hmsn =>
					val constZero = ctx.Expr(ctx.universe.Literal(ctx.universe.Constant(0)))
					val (hm, sn) = hmsn
					val (hour, minute) = hm
					val (second, n) = sn.getOrElse((constZero, None))
					val nano = n.getOrElse(constZero)

					ctx.universe.reify(java.time.LocalTime.of(hour.splice, minute.splice, second.splice, nano.splice))
				})
			val VariableP:Parser[ContextTypes[LocalTime]#Expr] = OfType(ctx.typeTag[LocalTime])
			VariableP | LiteralP
		}

		def LocalDateTimeP:Parser[ContextTypes[LocalDateTime]#Expr] = {
			(LocalDateP ~ "T" ~ LocalTimeP)
				.map({dt =>
					val (date, time) = dt
					ctx.universe.reify(date.splice.atTime(time.splice))
				})
		}
	}

	private[this] val extensionClassName = "com.rayrobdod.stringContextParserCombinatorExample.datetime.package.DateTimeStringContext"

	def stringContext_localdate(c:Context {type PrefixType = DateTimeStringContext})(args:c.Expr[Any]*):c.Expr[LocalDate] = {
		object parsers extends Parsers {
			val ctx:c.type = c
			def Aggregate = (this.LocalDateP ~ this.End())
		}

		macroimpl(c)(extensionClassName, parsers.Aggregate)(args.toList)
	}

	def stringContext_localtime(c:Context {type PrefixType = DateTimeStringContext})(args:c.Expr[Any]*):c.Expr[LocalTime] = {
		object parsers extends Parsers {
			val ctx:c.type = c
			def Aggregate = (this.LocalTimeP ~ this.End())
		}

		macroimpl(c)(extensionClassName, parsers.Aggregate)(args.toList)
	}

	def stringContext_localdatetime(c:Context {type PrefixType = DateTimeStringContext})(args:c.Expr[Any]*):c.Expr[LocalDateTime] = {
		object parsers extends Parsers {
			val ctx:c.type = c
			def Aggregate = (this.LocalDateTimeP ~ this.End())
		}

		macroimpl(c)(extensionClassName, parsers.Aggregate)(args.toList)
	}
}
