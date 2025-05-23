package name.rayrobdod.stringContextParserCombinatorExample.datetime

import java.time._
import scala.quoted._
import name.rayrobdod.stringContextParserCombinator._

object MacroImpl {
	import ExprConversions.given

	private given TypeCreator[DayOfMonth] = TypeCreator.derived
	private given TypeCreator[Int] = TypeCreator.derived

	private given given_Sequenced_Expr_YearMonth:typeclass.BiSequenced[Quotes, Expr[Year], Expr[Month], Expr[YearMonth]] with {
		def aggregate(left:Expr[Year], right:Expr[Month])(using Quotes):Expr[YearMonth] = (left, right) match {
			case (Expr(year), Expr(month)) => '{YearMonth.of(${Expr(year.getValue())}, ${Expr(month.getValue())})}
			case (Expr(year), month) => '{YearMonth.of(${Expr(year.getValue())}, $month)}
			case (year, Expr(month)) => '{$year.atMonth(${Expr(month.getValue())})}
			case (year, month) => '{$year.atMonth($month)}
		}

		def separate(value:Expr[YearMonth])(using Quotes):(Expr[Year], Expr[Month]) = value match {
			case '{YearMonth.of($year:Int, $month:Month)} => ('{Year.of($year)}, month)
			case '{($year:Year).atMonth($month:Month)} => (year, month)
			case _ => ('{Year.of($value.getYear)}, '{$value.getMonth})
		}
	}

	private given given_Sequenced_Expr_Date:typeclass.ContraSequenced[Quotes, Expr[YearMonth], Expr[Int], Expr[LocalDate]] with {
		def separate(value:Expr[LocalDate])(using Quotes):(Expr[YearMonth], Expr[Int]) = value match {
			case _ => ('{YearMonth.of($value.getYear, $value.getMonth)}, '{$value.getDayOfMonth})
		}
	}

	private given given_Sequenced_Expr_DateTime:typeclass.BiSequenced[Quotes, Expr[LocalDate], Expr[LocalTime], Expr[LocalDateTime]] with {
		def aggregate(left:Expr[LocalDate], right:Expr[LocalTime])(using Quotes):Expr[LocalDateTime] = (left, right) match {
			case (Expr(date), Expr(time)) => '{
				LocalDateTime.of(
					${Expr(date.getYear())}, ${Expr(date.getMonthValue())}, ${Expr(date.getDayOfMonth())},
					${Expr(time.getHour())}, ${Expr(time.getMinute())}, ${Expr(time.getSecond())}, ${Expr(time.getNano())},
				)
			}
			case ('{LocalDate.of($y:Int, $mo:Int, $d:Int)}, '{LocalTime.of($h, $mi, $s, $n)}) => '{
				LocalDateTime.of($y, $mo, $d, $h, $mi, $s, $n)
			}
			case ('{LocalDate.of($y:Int, $mo:Month, $d:Int)}, '{LocalTime.of($h, $mi, $s, $n)}) => '{
				LocalDateTime.of($y, $mo, $d, $h, $mi, $s, $n)
			}
			case (date, Expr(time)) => '{
				$date.atTime(
					${Expr(time.getHour())}, ${Expr(time.getMinute())}, ${Expr(time.getSecond())}, ${Expr(time.getNano())}
				)
			}
			case (date, time) => '{ $date.atTime($time) }
		}

		def separate(value:Expr[LocalDateTime])(using Quotes):(Expr[LocalDate], Expr[LocalTime]) = {
			('{$value.toLocalDate}, '{$value.toLocalTime})
		}
	}

	import Parser.end
	import Interpolator.quotedInterpolators.ofType

	private val intTwoDigits = TimeParsers.intTwoDigits({(chars: Seq[Char]) => Interpolator.quotedInterpolators.charIn(chars)})

	private def dayOfMonth(max: Int): Interpolator[quoted.Quotes, quoted.Expr[Any], quoted.Expr[Int]] = {
		ofType[DayOfMonth].map(d => '{$d.getValue}) <|> intTwoDigits(1, max).mapToExpr
	}

	private val timeParsers = {
		val leafParsers:Parser.Parsers[Quotes, quoted.Expr, quoted.ToExpr, TypeCreator] =
					Parser.quotedParsers

		TimeParsers(leafParsers)(
			(value, ctx) => {
				given Quotes = ctx
				value match {
					case Expr(ym) => {
						ofType[DayOfMonth].map(day => '{LocalDate.of(${Expr(ym.getYear)}, ${Expr(ym.getMonth)}, ${day}.getValue)}) <|>
							intTwoDigits(1, ym.lengthOfMonth).map(day => Expr(ym.atDay(day)))
					}
					case '{YearMonth.of($y:Int, ${Expr(m)}:Int)} => dayOfMonth(Month.of(m).maxLength).map(day => '{LocalDate.of($y, ${Expr(m)}, $day)})
					case '{YearMonth.of($y:Int, ${Expr(m)}:Month)} => dayOfMonth(m.maxLength).map(day => '{LocalDate.of($y, ${Expr(m)}, $day)})
					case '{($year:Year).atMonth(${Expr(m)}:Int)} => dayOfMonth(Month.of(m).maxLength).map(day => '{$year.atMonth(${Expr(m)}).atDay($day)})
					case '{($year:Year).atMonth(${Expr(m)}:Month)} => dayOfMonth(m.maxLength).map(day => '{$year.atMonth(${Expr(m)}).atDay($day)})
					case '{YearMonth.of($y:Int, $m:Int)} => dayOfMonth(31).map(day => '{LocalDate.of($y, $m, $day)})
					case '{YearMonth.of($y:Int, $m:Month)} => dayOfMonth(31).map(day => '{LocalDate.of($y, $m, $day)})
					case ym => dayOfMonth(31).map(day => '{$ym.atDay($day)})
				}
			},
			(hour, minute, second, nano, ctx) =>
				given Quotes = ctx
				'{ LocalTime.of($hour, $minute, $second, $nano) },
			(value, ctx) =>
				given Quotes = ctx
				'{ DayOfMonth.of($value) },
		)
	}


	def interpolate_localDate(sc:Expr[scala.StringContext], args:Expr[Seq[Any]])(using Quotes):Expr[LocalDate] = {
		(timeParsers.localDate <~ end).interpolate(sc, args)
	}

	def interpolate_localTime(sc:Expr[scala.StringContext], args:Expr[Seq[Any]])(using Quotes):Expr[LocalTime] = {
		(timeParsers.localTime <~ end).interpolate(sc, args)
	}

	def interpolate_localDateTime(sc:Expr[scala.StringContext], args:Expr[Seq[Any]])(using Quotes):Expr[LocalDateTime] = {
		(timeParsers.localDateTime <~ end).interpolate(sc, args)
	}

	def extractor_localDate(sc:Expr[scala.StringContext])(using Quotes):Expr[Unapply[LocalDate]] = {
		(timeParsers.localDate <~ end).extractor(sc)
	}

	def extractor_localTime(sc:Expr[scala.StringContext])(using Quotes):Expr[Unapply[LocalTime]] = {
		(timeParsers.localTime <~ end).extractor(sc)
	}

	def extractor_localDateTime(sc:Expr[scala.StringContext])(using Quotes):Expr[Unapply[LocalDateTime]] = {
		(timeParsers.localDateTime <~ end).extractor(sc)
	}
}

extension (inline sc:scala.StringContext)
	inline def localdate(inline args:Any*):LocalDate =
		${MacroImpl.interpolate_localDate('sc, 'args)}
	inline def localtime(inline args:Any*):LocalTime =
		${MacroImpl.interpolate_localTime('sc, 'args)}
	inline def localdatetime(inline args:Any*):LocalDateTime =
		${MacroImpl.interpolate_localDateTime('sc, 'args)}
	transparent inline def localdate:Unapply[LocalDate] =
		${MacroImpl.extractor_localDate('sc)}
	transparent inline def localtime:Unapply[LocalTime] =
		${MacroImpl.extractor_localTime('sc)}
	transparent inline def localdatetime:Unapply[LocalDateTime] =
		${MacroImpl.extractor_localDateTime('sc)}

extension (sc:scala.StringContext)
	def localdate2(args:Any*):LocalDate =
		IdImpl.interpolate_localDate(sc, args*)
	def localtime2(args:Any*):LocalTime =
		IdImpl.interpolate_localTime(sc, args*)
	def localdatetime2(args:Any*):LocalDateTime =
		IdImpl.interpolate_localDateTime(sc, args*)
	def localdate2:Unapply.Seq[LocalDate, Any] =
		(scrutinee) => IdImpl.extract_localDate(sc, scrutinee)
	def localtime2:Unapply.Seq[LocalTime, Any] =
		(scrutinee) => IdImpl.extract_localTime(sc, scrutinee)
	def localdatetime2:Unapply.Seq[LocalDateTime, Any] =
		(scrutinee) => IdImpl.extract_localDateTime(sc, scrutinee)
