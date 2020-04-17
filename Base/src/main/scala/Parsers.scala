package com.rayrobdod.stringContextParserCombinator

import scala.collection.immutable.Seq
import scala.language.higherKinds

/**
 * Methods to create leaf parsers
 */
object Parsers {
	/** Succeeds if the next character is a member of the given String; captures that character */
	def CharIn(str:Seq[Char]):Parser[ContextTypes[Char]#Ident] = parsers.CharIn(str)
	/** Succeeds if the next character is a member of the given Seq; captures that character */
	def CharIn(str:String):Parser[ContextTypes[Char]#Ident] = parsers.CharIn(scala.Predef.wrapString(str))
	/** Succeeds if the next character matches the given predicate; captures that character */
	def CharWhere(fn:Function1[Char, Boolean], description:String):Parser[ContextTypes[Char]#Ident] = parsers.CharWhere(fn, Failure.Leaf(description))
	/** Succeeds if the next codepoint is a member of the given string; captures that code point */
	def CodePointIn(str:String):Parser[ContextTypes[CodePoint]#Ident] = parsers.CodePointIn(str)
	/** Succeeds if the next codepoint is matches the given predicate; captures that code point */
	def CodePointWhere(fn:Function1[CodePoint, Boolean], description:String):Parser[ContextTypes[CodePoint]#Ident] = parsers.CodePointWhere(fn, Failure.Leaf(description))
	/** Succeeds if the next set of characters in the input is equal to the given string */
	def IsString(str:String):Parser[ContextTypes[Unit]#Ident] = parsers.IsString(str)
	/** A parser that succeeds iff the next part of the input is an `arg` with the given type, and captures the arg's tree */
	def OfType[A](tpetag:ContextFunction0[ContextTypes[A]#TypeTag]):Parser[ContextTypes[A]#Expr] = parsers.OfType[A](tpetag)
	/** A parser that succeeds if a "lift" type can be implicitly summoned
	 *
	 * The type of object to attempt to summon is determined by calling lifterType using the type of the next `arg` input
	 * The implicitly summoned value and the `arg` value are passed to `lift`; the returned value is returned by this parser
	 */
	def Lifted[Lifter[A], Z](lifterType:ContextTypeFunction, lift:LiftFunction[Lifter, Z], description:Failure.Expecting):Parser[ContextTypes[Z]#Expr] = parsers.Lifted(lifterType, lift, description)
	/** A parser that succeeds iff the input is empty */
	def End():Parser[ContextTypes[Unit]#Ident] = parsers.End()
	/** Indirectly refers to a parser, to allow for mutual-recursion */
	def DelayedConstruction[CA[U]](fn:Function0[Parser[CA]]):Parser[CA] = parsers.DelayedConstruction[CA](fn)
}
