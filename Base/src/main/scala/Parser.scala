package com.rayrobdod.stringContextParserCombinator

import scala.language.higherKinds
import com.rayrobdod.stringContextParserCombinator.MacroCompat.Context

/**
 * @group Parser
 */
trait Parser[+CA[U <: Context with Singleton]] {
	def parse(c:Context)(input:Input[c.type]):Result[c.type, CA[c.type]]

	/** Returns a parser which invokes this parser, then modifies a successful result according to fn */
	def map[CZ[U <: Context with Singleton]](fn:ContextFunction1[CA, CZ]):Parser[CZ] = parsers.Map[CA, CZ](this, fn)
	/** Returns a parser which invokes this parser, then modifies a successful result according to the parser returned by fn */
	def flatMap[CZ[U <: Context with Singleton]](fn:ContextFunction1[CA, ContextTypes[Parser[CZ]]#Ident]):Parser[CZ] = parsers.FlatMap[CA, CZ](this, fn)
	/** Returns a parser which invokes this parser, then fails a successful result if it does not pass the predicate */
	def filter(predicate:ContextFunction1[CA, ContextTypes[Boolean]#Ident], description:String):Parser[CA] = parsers.Filter(this, predicate, Failure.Leaf(description))

	/** Returns a parser which invokes this parser, but has the given description upon failure */
	def opaque(description:String):Parser[CA] = this.opaque(Failure.Leaf(description))
	private[stringContextParserCombinator] def opaque(description:Failure.Expecting) = parsers.Opaque(this, description)

	/** Returns a parser which invokes this parser, and upon success invokes the other parser */
	def andThen[CB[U <: Context with Singleton], CZ[U <: Context with Singleton]](rhs:Parser[CB])(implicit ev:Implicits.AndThenTypes[CA,CB,CZ]):Parser[CZ] = parsers.AndThen[CA, CB, CZ](this, rhs, ev)
	/** Returns a parser which invokes this parser and the other parser, and returns the first successful result */
	def orElse[CZ[U <: Context with Singleton] >: CA[U]](rhs:Parser[CZ]):Parser[CZ] = parsers.OrElse[CZ](this, rhs)
	/** Returns a parser which invokes this parser repeatedly and returns the aggregated result */
	def repeat[CZ[U <: Context with Singleton]](min:Int = 0, max:Int = Integer.MAX_VALUE)(implicit ev:Implicits.RepeatTypes[CA, CZ]):Parser[CZ] = parsers.Repeat(this, min, max, ev)
	/** Returns a parser which invokes this parser and provides a value whether this parser succeeded or failed */
	def optionally[CZ[U <: Context with Singleton]](implicit ev:Implicits.OptionallyTypes[CA, CZ]):Parser[CZ] = parsers.Optionally(this, ev)
}
