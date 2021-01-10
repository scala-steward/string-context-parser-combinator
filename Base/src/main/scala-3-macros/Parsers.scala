package com.rayrobdod.stringContextParserCombinator

import scala.collection.immutable.Seq
import scala.language.higherKinds
import scala.quoted.Expr
import scala.quoted.Quotes
import scala.quoted.Type
import com.rayrobdod.stringContextParserCombinator.{Parser => SCParser}

/**
 * Methods to create leaf parsers
 */
trait Parsers {
	type Parser[A] = SCParser[Expr[_], A]

	/** Succeeds if the next character is a member of the given String; captures that character */
	def CharIn(str:Seq[Char]):Parser[Char] =
		parsers.CharIn(str)

	/** Succeeds if the next character is a member of the given Seq; captures that character */
	def CharIn(str:String):Parser[Char] =
		parsers.CharIn(scala.Predef.wrapString(str))

	/** Succeeds if the next character matches the given predicate; captures that character */
	def CharWhere(fn:Function1[Char, Boolean], description:String):Parser[Char] =
		parsers.CharWhere(fn, Expecting(description))

	/** Succeeds if the next codepoint is a member of the given string; captures that code point */
	def CodePointIn(str:String):Parser[CodePoint] =
		parsers.CodePointIn(str)

	/** Succeeds if the next codepoint is matches the given predicate; captures that code point */
	def CodePointWhere(fn:Function1[CodePoint, Boolean], description:String):Parser[CodePoint] =
		parsers.CodePointWhere(fn, Expecting(description))

	/** Succeeds if the next set of characters in the input is equal to the given string */
	def IsString(str:String):Parser[Unit] =
		parsers.IsString(str)

	/** A parser that succeeds iff the next part of the input is an `arg` with the given type, and captures the arg's tree */
	def OfType[A](using tpe:Type[A])(using Quotes):Parser[Expr[A]] =
		parsers.OfType(tpe)

	/** A parser that succeeds if a "lift" type can be implicitly summoned
	 *
	 * The type of object to attempt to summon is determined by calling lifterType using the type of the next `arg` input
	 * The implicitly summoned value and the `arg` value are passed to `lift`; the returned value is returned by this parser
	 */
	def Lifted[Lifter[A], Z](lift:LiftFunction[Lifter, Z], description:Expecting)(using Quotes, Type[Lifter]):Parser[Expr[Z]] =
		parsers.Lifted(lift, description)

	/** A parser that succeeds iff the input is empty */
	def End():Parser[Unit] =
		new parsers.End()

	/** Indirectly refers to a parser, to allow for mutual-recursion */
	def DelayedConstruction[A](fn:Function0[Parser[A]]):Parser[A] =
		parsers.DelayedConstruction(fn)
}

/**
 * Methods to create leaf parsers
 */
object Parsers extends Parsers {
}
