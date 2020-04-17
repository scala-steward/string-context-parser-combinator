package com.rayrobdod.stringContextParserCombinator

import scala.collection.immutable.Seq
import scala.language.higherKinds
import com.rayrobdod.stringContextParserCombinator.MacroCompat.Context

package parsers {
	/** An intermediary class to lessen the weight of implementing Parser repeatedly, Parser being a trait with several concrete methods */
	private[parsers] abstract class AbstractParser[CA[U <: Context with Singleton]] extends Parser[CA]

	/** A parser that extracts a value from an input's parts, and returns None for all args */
	private[parsers] final class PartsParser[A](
		partsFn:String => Option[(A, Int)],
		expecting: => Failure.Expecting
	) extends AbstractParser[ContextTypes[A]#Ident] {
		def parse(c:Context)(input:Input[c.type]):Result[c.type, A] = {
			input.consume(
				partsFn,
				_ => None,
				expecting
			)
		}
	}
}

package object parsers {
	/* * * Leaf parsers * * */

	/** Succeeds if the next character is a member of the given Seq; captures that character */
	private[stringContextParserCombinator]
	def CharIn(
		chooseFrom:Seq[Char]
	):Parser[ContextTypes[Char]#Ident] = CharWhere(
		chooseFrom.contains _,
		Failure.Or(chooseFrom.map(x => Failure.Leaf("\"" + x.toString + "\"")))
	)

	/** Succeeds if the next character matches the given predicate; captures that character */
	private[stringContextParserCombinator]
	def CharWhere(
		predicate:Function1[Char, Boolean], description:Failure.Expecting
	):Parser[ContextTypes[Char]#Ident] = new PartsParser(
		pt => Option((pt.charAt(0), 1)).filter(x => predicate(x._1)),
		description
	)

	/** Succeeds if the next codepoint is a member of the given string; captures that code point */
	private[stringContextParserCombinator]
	def CodePointIn(
		chooseFrom:String
	):Parser[ContextTypes[CodePoint]#Ident] = {
		def IntEqualsCodePoint(x:CodePoint) = new java.util.function.IntPredicate{def test(y:Int) = {y == x.value}}
		val CodePointString = new java.util.function.IntFunction[String]{def apply(y:Int) = new String(Array[Int]('"', y, '"'), 0, 3)}
		type ToExpectingBuffer = scala.collection.mutable.Builder[Failure.Expecting, Seq[Failure.Expecting]]
		val ToExpecting = new java.util.stream.Collector[String, ToExpectingBuffer, Failure.Expecting]{
			override def supplier = new java.util.function.Supplier[ToExpectingBuffer] {def get = Seq.newBuilder}
			override def accumulator = new java.util.function.BiConsumer[ToExpectingBuffer, String]{def accept(buf:ToExpectingBuffer, a:String) = buf += Failure.Leaf(a)}
			override def combiner = new java.util.function.BinaryOperator[ToExpectingBuffer]{def apply(lhs:ToExpectingBuffer, rhs:ToExpectingBuffer) = {lhs ++= rhs.result; lhs}}
			override def finisher = new java.util.function.Function[ToExpectingBuffer, Failure.Expecting]{def apply(buf:ToExpectingBuffer) = Failure.Or(buf.result)}
			override def characteristics = java.util.Collections.emptySet()
		}

		this.CodePointWhere(
			{x:CodePoint => chooseFrom.codePoints.anyMatch(IntEqualsCodePoint(x))},
			chooseFrom.codePoints.mapToObj(CodePointString).collect(ToExpecting)
		)
	}

	/** Succeeds if the next codepoint matches the given predicate; captures that code point */
	private[stringContextParserCombinator]
	def CodePointWhere(
		predicate:Function1[CodePoint, Boolean], description:Failure.Expecting
	):Parser[ContextTypes[CodePoint]#Ident] = new PartsParser(
		pt => Option((CodePoint(pt.codePointAt(0)), pt.offsetByCodePoints(0, 1))).filter(x => predicate(x._1)),
		description
	)

	/** Succeeds if the next set of characters in the input is equal to the given string */
	private[stringContextParserCombinator]
	def IsString(
		value:String
	):Parser[ContextTypes[Unit]#Ident] = new PartsParser(
		pt => Option(((), value.length())).filter(_ => pt.startsWith(value)),
		Failure.Leaf("\"" + value + "\"")
	)

	/** Succeeds if the net character data matches the given regex; captures the matched string */
	private[stringContextParserCombinator]
	def Regex(
		reg:scala.util.matching.Regex
	):Parser[ContextTypes[String]#Ident] = new PartsParser(
		pt => reg.findPrefixMatchOf(pt).map(m => (m.matched, m.end - m.start)),
		Failure.Leaf("s/" + reg.toString + "/")
	)

	/** Succeeds if the next input element is an `arg` with the given type; captures the expression */
	private[stringContextParserCombinator]
	def OfType[A](
		tpetag:ContextFunction0[ContextTypes[A]#TypeTag]
	):Parser[ContextTypes[A]#Expr] = {
		new OfType(tpetag)
	}

	/** Succeeds only at the end of the given input */
	private[stringContextParserCombinator]
	def End(
	):Parser[ContextTypes[Unit]#Ident] = {
		new End()
	}

	/* * * Mapping * * */

	private[stringContextParserCombinator]
	def Map[CA[U <: Context with Singleton], CZ[U <: Context with Singleton]](
		backing:Parser[CA], mapping:ContextFunction1[CA, CZ]
	):Parser[CZ] = {
		new Map(backing, mapping)
	}

	private[stringContextParserCombinator]
	def FlatMap[CA[U <: Context with Singleton], CZ[U <: Context with Singleton]](
		backing:Parser[CA], mapping:ContextFunction1[CA, ContextTypes[Parser[CZ]]#Ident]
	):Parser[CZ] = {
		new FlatMap(backing, mapping)
	}

	private[stringContextParserCombinator]
	def Filter[CA[U <: Context with Singleton]](
		backing:Parser[CA], predicate:ContextFunction1[CA, ContextTypes[Boolean]#Ident], description:Failure.Expecting
	):Parser[CA] = {
		new Filter(backing, predicate, description)
	}

	private[stringContextParserCombinator]
	def Opaque[CA[U <: Context with Singleton]](
		backing:Parser[CA], description:Failure.Expecting
	):Parser[CA] = {
		new Opaque(backing, description)
	}

	/** Used to allow mutually recursive parsers */
	private[stringContextParserCombinator]
	def DelayedConstruction[CA[U <: Context with Singleton]](
		backing:Function0[Parser[CA]]
	):Parser[CA] = {
		new DelayedConstruction(backing)
	}

	private[stringContextParserCombinator]
	def Repeat[CA[U <: Context with Singleton], CZ[U <: Context with Singleton]](
		backing:Parser[CA],
		min:Int,
		max:Int,
		ev:Implicits.RepeatTypes[CA, CZ]
	):Parser[CZ] = {
		new Repeat(backing, min, max, ev)
	}

	private[stringContextParserCombinator]
	def Optionally[CA[U <: Context with Singleton], CZ[U <: Context with Singleton]](
		backing:Parser[CA], ev:Implicits.OptionallyTypes[CA, CZ]
	):Parser[CZ] = {
		new Repeat(backing, 0, 1, new Implicits.RepeatTypes[CA, CZ] {
			final class Box[BoxType](var value:BoxType)
			type Acc[U <: Context with Singleton] = Box[CZ[U]]
			def init(c:Context):Acc[c.type] = new Box(ev.none(c))
			def append(c:Context)(acc:Acc[c.type], elem:CA[c.type]):Unit = acc.value = ev.some(c)(elem)
			def result(c:Context)(acc:Acc[c.type]):CZ[c.type] = acc.value
		})
	}

	/* * * Combinations * * */

	private[stringContextParserCombinator]
	def AndThen[CA[U <: Context with Singleton], CB[U <: Context with Singleton], CZ[U <: Context with Singleton]](
		left:Parser[CA], right:Parser[CB], ev:Implicits.AndThenTypes[CA, CB, CZ]
	):Parser[CZ] = {
		new AndThen(left, right, ev)
	}

	private[stringContextParserCombinator]
	def OrElse[CA[U <: Context with Singleton]](
		left:Parser[CA], right:Parser[CA]
	):Parser[CA] = {
		new OrElse(left, right)
	}
}
