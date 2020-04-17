package com.rayrobdod.stringContextParserCombinator

import scala.collection.mutable.Builder
import scala.language.higherKinds
import com.rayrobdod.stringContextParserCombinator.MacroCompat.Context

/**
 * Container for type-level logic
 */
object Implicits {
	/** Describes how to combine two adjacent values into one value */
	trait AndThenTypes[-CA[U <: Context with Singleton], -CB[U <: Context with Singleton], +CZ[U <: Context with Singleton]] {
		def aggregate(c:Context)(a:CA[c.type], b:CB[c.type]):CZ[c.type]
	}
	/** Predefined implicit implementations of AndThenTypes */
	object AndThenTypes extends LowPrioAndThenTypes {
		implicit def andThenUnitBoth:AndThenTypes[ContextTypes[Unit]#Ident, ContextTypes[Unit]#Ident, ContextTypes[Unit]#Ident] = new AndThenUnit
		private[this] final class AndThenUnit extends AndThenTypes[ContextTypes[Unit]#Ident, ContextTypes[Unit]#Ident, ContextTypes[Unit]#Ident] {
			def aggregate(c:Context)(a:Unit, b:Unit):Unit = ()
		}
		implicit def andThenUnitLeft[CB[U <: Context with Singleton]]:AndThenTypes[ContextTypes[Unit]#Ident, CB, CB] = new AndThenUnitLeft
		private[this] final class AndThenUnitLeft[CB[U <: Context with Singleton]] extends AndThenTypes[ContextTypes[Unit]#Ident, CB, CB] {
			def aggregate(c:Context)(u:Unit, b:CB[c.type]):CB[c.type] = b
		}
		implicit def andThenUnitRight[CA[U <: Context with Singleton]]:AndThenTypes[CA, ContextTypes[Unit]#Ident, CA] = new AndThenUnitRight
		private[this] final class AndThenUnitRight[CA[U <: Context with Singleton]] extends AndThenTypes[CA, ContextTypes[Unit]#Ident, CA] {
			def aggregate(c:Context)(a:CA[c.type], u:Unit):CA[c.type] = a
		}
	}
	private[Implicits] trait LowPrioAndThenTypes {
		implicit def andThenGeneric[CA[U <: Context with Singleton], CB[U <: Context with Singleton]]:AndThenTypes[CA, CB, ContextTypesTuple2[CA, CB]#X] = new AndThenGeneric
		private[this] final class AndThenGeneric[CA[U <: Context with Singleton], CB[U <: Context with Singleton]] extends AndThenTypes[CA, CB, ContextTypesTuple2[CA, CB]#X] {
			def aggregate(c:Context)(a:CA[c.type], b:CB[c.type]):(CA[c.type], CB[c.type]) = (a,b)
		}
	}

	/** Describes the type that represents the repetition of a type */
	trait RepeatTypes[-CA[U <: Context with Singleton], +CZ[U <: Context with Singleton]] {
		/** A mutable accumulator appropriate for holding `A` and transforming into `Z` */
		type Acc[U <: Context with Singleton]
		/** Returns an empty accumulator */
		def init(c:Context):Acc[c.type]
		/** Inserts `elem` into `acc` */
		def append(c:Context)(acc:Acc[c.type], elem:CA[c.type]):Unit
		/** Transforms `acc` into Z */
		def result(c:Context)(acc:Acc[c.type]):CZ[c.type]
	}
	/** Predefined implicit implementations of RepeatTypes */
	object RepeatTypes extends LowPrioRepeatTypes {
		implicit def repeatUnit:RepeatTypes[ContextTypes[Unit]#Ident, ContextTypes[Unit]#Ident] = new RepeatTypesUnit
		private[this] final class RepeatTypesUnit extends RepeatTypes[ContextTypes[Unit]#Ident, ContextTypes[Unit]#Ident] {
			type Acc[U] = Unit
			def init(c:Context):Acc[c.type] = ()
			def append(c:Context)(acc:Acc[c.type], elem:Unit):Unit = {}
			def result(c:Context)(acc:Acc[c.type]):Unit = ()
		}
		implicit def repeatChar:RepeatTypes[ContextTypes[Char]#Ident, ContextTypes[String]#Ident] = new RepeatTypesChar
		private[this] final class RepeatTypesChar extends RepeatTypes[ContextTypes[Char]#Ident, ContextTypes[String]#Ident] {
			type Acc[U] = StringBuilder
			def init(c:Context):Acc[c.type] = new StringBuilder
			def append(c:Context)(acc:Acc[c.type], elem:Char):Unit = {acc += elem}
			def result(c:Context)(acc:Acc[c.type]):String = acc.toString
		}
		implicit def repeatCodepoint:RepeatTypes[ContextTypes[CodePoint]#Ident, ContextTypes[String]#Ident] = new RepeatTypesCodepoint
		private[this] final class RepeatTypesCodepoint extends RepeatTypes[ContextTypes[CodePoint]#Ident, ContextTypes[String]#Ident] {
			type Acc[U] = java.lang.StringBuilder
			def init(c:Context):Acc[c.type] = new java.lang.StringBuilder
			def append(c:Context)(acc:Acc[c.type], elem:CodePoint):Unit = {acc.appendCodePoint(elem.value)}
			def result(c:Context)(acc:Acc[c.type]):String = acc.toString
		}
	}
	private[Implicits] trait LowPrioRepeatTypes {
		implicit def repeatGenericToList[CA[U <: Context with Singleton]]:RepeatTypes[CA, ContextTypes2[CA]#List] = new RepeatGenericToList
		private[this] final class RepeatGenericToList[CA[U <: Context with Singleton]] extends RepeatTypes[CA, ContextTypes2[CA]#List] {
			type Acc[U <: Context with Singleton] = Builder[CA[U], List[CA[U]]]
			def init(c:Context):Acc[c.type] = List.newBuilder[CA[c.type]]
			def append(c:Context)(acc:Acc[c.type], elem:CA[c.type]):Unit = {acc += elem}
			def result(c:Context)(acc:Acc[c.type]):ContextTypes2[CA]#List[c.type] = acc.result()
		}
	}

	/** Describes the type that represents the option of a type */
	trait OptionallyTypes[-CA[U <: Context with Singleton], +CZ[U <: Context with Singleton]] {
		/** Returns a `Z` value representing a missing `A` */
		def none(c:Context):CZ[c.type]
		/** Returns a `Z` value representing the given `A` */
		def some(c:Context)(elem:CA[c.type]):CZ[c.type]
	}
	/** Predefined implicit implementations of OptionallyTypes */
	object OptionallyTypes extends LowPrioOptionallyTypes {
		implicit def optionallyUnit:OptionallyTypes[ContextTypes[Unit]#Ident, ContextTypes[Unit]#Ident] = new OptionallyUnit
		private[this] final class OptionallyUnit extends OptionallyTypes[ContextTypes[Unit]#Ident, ContextTypes[Unit]#Ident] {
			def none(c:Context):Unit = ()
			def some(c:Context)(elem:Unit):Unit = elem
		}
	}
	private[Implicits] trait LowPrioOptionallyTypes {
		implicit def optinallyGeneric[CA[U <: Context with Singleton]]:OptionallyTypes[CA, ContextTypes2[CA]#Option] = new OptinallyGeneric[CA]
		private[this] final class OptinallyGeneric[CA[U <: Context with Singleton]] extends OptionallyTypes[CA, ContextTypes2[CA]#Option] {
			def none(c:Context):Option[CA[c.type]] = None
			def some(c:Context)(elem:CA[c.type]):Option[CA[c.type]] = Some(elem)
		}
	}
}
