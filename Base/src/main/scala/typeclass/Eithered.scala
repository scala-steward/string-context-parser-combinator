package name.rayrobdod.stringContextParserCombinator
package typeclass

import scala.reflect.ClassTag
import com.eed3si9n.ifdef.ifdef

/**
 * Describes how to represent a result that may be one of two results
 *
 * Below is example of defining and using a custom Eithered.
 *
 * ```scala
 * import java.io.File
 * import java.net.URI
 * import java.util.UUID
 * import name.rayrobdod.stringContextParserCombinator.IdCtx
 * import name.rayrobdod.stringContextParserCombinator.Interpolator.idInterpolators._
 * import name.rayrobdod.stringContextParserCombinator.typeclass.Eithered
 *
 * given Eithered[IdCtx, File, UUID, URI] with {
 *   def left(f: File)(implicit ctx:IdCtx): URI = f.toURI
 *   def right(id: UUID)(implicit ctx:IdCtx): URI = new URI("urn", "uuid:" + id.toString, null)
 * }
 *
 * val uuidParser:Interpolator[UUID] = ofType[UUID]
 * val fileParser:Interpolator[File] = ofType[File]
 * val p:Interpolator[URI] = (fileParser:Interpolator[File]) orElse (uuidParser:Interpolator[UUID]) // using Eithered[IdCtx, File, UUID, URI]
 *
 * p.interpolate(StringContext("", ""), new File("/tmp") :: Nil) // `file:///tmp`: URI
 * p.interpolate(StringContext("", ""), UUID.randomUUID() :: Nil) // `urn:uuid:429bf7eb-650e-4f8c-be3f-1420913a6bd7`: URI
 * ```
 *
 * @see [[name.rayrobdod.stringContextParserCombinator.Interpolator.orElse Interpolator.orElse]]
 * @tparam A the first choice
 * @tparam B the second choice
 * @tparam Z the result container
 */
trait Eithered[-Ctx, -A, -B, +Z] {
	def left(elem:A)(implicit ctx:Ctx):Z
	def right(elem:B)(implicit ctx:Ctx):Z
}

/**
 * Describes how to disambiguate the union of two types
 *
 * The parser determines whether the left or right branch is taken.
 * The return value's `Expr[Boolean]` indicates whether the value matches the branch
 *
 * @see [[name.rayrobdod.stringContextParserCombinator.Extractor.orElse Extractor.orElse]]
 * @tparam A the first choice
 * @tparam B the second choice
 * @tparam Z the result container
 */
trait ContraEithered[Ctx, +Expr[+_], +A, +B, -Z] {
	def contraLeft:PartialExprFunction[Ctx, Expr, Z, A]
	def contraRight:PartialExprFunction[Ctx, Expr, Z, B]
}

/**
 * @see [[name.rayrobdod.stringContextParserCombinator.Parser.orElse Parser.orElse]]
 * @tparam A the first choice
 * @tparam B the second choice
 * @tparam Z the result container
 */
trait BiEithered[Ctx, Expr[+_], A, B, Z]
		extends Eithered[Ctx, A, B, Z]
		with ContraEithered[Ctx, Expr, A, B, Z]

/**
 * Predefined implicit implementations of Eithered
 * and methods to create new Eithereds
 *
 * @groupname Support Support
 * @groupprio Support 100
 * @groupname AnyContext Any Context
 * @groupprio AnyContext 1000
 * @groupname QuotedContext Quotes Context
 * @groupprio QuotedContext 1010
 * @groupname MacroContext Macro Context
 * @groupprio MacroContext 1020
 * @groupname IdContext Identity Context
 * @groupprio IdContext 1030
 */
object Eithered extends LowPrioEithered {
	/**
	 * Constructs an `Eithered` from a set of functions corresponding to each of Eithered's methods
	 * @group Support
	 */
	def apply[Ctx, A, B, Z](leftFn:(A, Ctx) => Z, rightFn:(B, Ctx) => Z):Eithered[Ctx, A, B, Z] = {
		final class Apply extends Eithered[Ctx, A, B, Z] {
			def left(elem:A)(implicit ctx:Ctx):Z = leftFn(elem, ctx)
			def right(elem:B)(implicit ctx:Ctx):Z = rightFn(elem, ctx)
		}
		new Apply()
	}

	/**
	 * @group AnyContext
	 */
	@ifdef("scalaEpochVersion:2")
	implicit def unitUnit:Eithered[Any, Unit, Unit, Unit] = symmetric[Unit]
	/**
	 * @group AnyContext
	 */
	@ifdef("scalaBinaryVersion:3")
	implicit def unitUnit:Eithered[Any, Unit, Unit, Unit] = Eithered.generic

	/**
	 * @group AnyContext
	 */
	implicit def unitGeneric[Ctx, B, Z](implicit ev:Optionally[Ctx, B, Z]):Eithered[Ctx, Unit, B, Z] =
		Eithered(
			(_:Unit, ctx) => ev.none(ctx),
			(value, ctx) => ev.some(value)(ctx),
		)
	/**
	 * @group AnyContext
	 */
	implicit def genericUnit[Ctx, A, Z](implicit ev:Optionally[Ctx, A, Z]):Eithered[Ctx, A, Unit, Z] =
		Eithered(
			(value, ctx) => ev.some(value)(ctx),
			(_:Unit, ctx) => ev.none(ctx),
		)

	/**
	 * @version 0.1.1
	 * @group MacroContext
	 */
	@ifdef("scalaEpochVersion:2")
	def contextSplicePiece[Ctx <: scala.reflect.macros.blackbox.Context with Singleton, A]: Eithered[Ctx, Ctx#Expr[A], Ctx#Expr[Iterable[A]], Repeated.SplicePiece[Ctx#Expr, A]] =
		Eithered((value, _) => new Repeated.SplicePiece.One(value), (value, _) => new Repeated.SplicePiece.Many(value))

	/**
	 * @version 0.1.1
	 * @group QuotedContext
	 */
	@ifdef("scalaBinaryVersion:3")
	def quotedSplicePiece[A]: Eithered[scala.quoted.Quotes, scala.quoted.Expr[A], scala.quoted.Expr[Iterable[A]], Repeated.SplicePiece[scala.quoted.Expr, A]] =
			Eithered((value, ctx) => new Repeated.SplicePiece.One(value), (value, ctx) => new Repeated.SplicePiece.Many(value))

	/**
	 * An Eithered that wraps the value in a `scala.Either`

	 * @example
	 * In the following interpolator, even digits are placed in a Left while odd digits are placed in a Right
	 * ```scala
	 * //{
	 * import name.rayrobdod.stringContextParserCombinator.Interpolator.idInterpolators._
	 * import name.rayrobdod.stringContextParserCombinator.typeclass.Eithered
	 *
	 * //}
	 * val evenOdd:Interpolator[Either[Char, Char]] = charIn("02468").orElse(charIn("13579"))(using Eithered.discriminatedUnion)
	 *
	 * evenOdd.interpolate(StringContext("4"), Nil) // Left(4): Either[Char, Char]
	 * evenOdd.interpolate(StringContext("7"), Nil) // Right(7): Either[Char, Char]
	 * ```
	 * @group AnyContext
	 */
	def discriminatedUnion[A, B]:Eithered[Any, A, B, Either[A, B]] =
		Eithered(
			(value, _) => Left(value),
			(value, _) => Right(value),
		)
}

private[typeclass] trait LowPrioEithered {
	/**
	 * @group AnyContext
	 */
	@ifdef("scalaEpochVersion:2")
	implicit def symmetric[A]:Eithered[Any, A, A, A] = {
		Eithered(
			{(value: A, _: Any) => value},
			{(value: A, _: Any) => value},
		)
	}

	/**
	 * The fallback Eithered;
	 * creates a union type of the two component types.
	 *
	 * Since the union of a type with itself is equivalent to that same type,
	 * if this Eithered is used for two parsers of the same type,
	 * then the result is a parser of that type.
	 * @group AnyContext
	 */
	@ifdef("scalaBinaryVersion:3")
	implicit def generic[A, B]:Eithered[Any, A, B, A | B] =
		Eithered.apply(
			(value, _: Any) => value,
			(value, _: Any) => value,
		)
}

/**
 * Predefined implicit implementations of ContraEithered
 * and methods to create new ContraEithereds
 *
 * @groupname Support Support
 * @groupprio Support 100
 * @groupname AnyContext Any Context
 * @groupprio AnyContext 1000
 * @groupname QuotedContext Quotes Context
 * @groupprio QuotedContext 1010
 * @groupname MacroContext Macro Context
 * @groupprio MacroContext 1020
 * @groupname IdContext Identity Context
 * @groupprio IdContext 1030
 */
object ContraEithered extends LowPrioContraEithered {
	/**
	 * Constructs an `ContraEithered` from a set of functions corresponding to each of ContraEithered's methods
	 * @group Support
	 */
	def apply[Ctx, Expr[+_], A, B, Z](
		contraLeftFn:PartialExprFunction[Ctx, Expr, Z, A],
		contraRightFn:PartialExprFunction[Ctx, Expr, Z, B]
	):ContraEithered[Ctx, Expr, A, B, Z] = {
		final class Apply extends ContraEithered[Ctx, Expr, A, B, Z] {
			override def contraLeft:PartialExprFunction[Ctx, Expr, Z, A] = contraLeftFn
			override def contraRight:PartialExprFunction[Ctx, Expr, Z, B] = contraRightFn
		}
		new Apply()
	}

	/**
	 * @group MacroContext
	 */
	@ifdef("scalaEpochVersion:2")
	implicit def contextUnitUnit[Ctx <: scala.reflect.macros.blackbox.Context with Singleton, A]:ContraEithered[Ctx, Ctx#Expr, Unit, Unit, Unit] = BiEithered.contextUnitUnit[Ctx]

	/**
	 * @group QuotedContext
	 */
	@ifdef("scalaBinaryVersion:3")
	implicit def quotedUnitUnit:ContraEithered[scala.quoted.Quotes, scala.quoted.Expr, Unit, Unit, Unit] = quotedSymmetric[Unit]

	/**
	 * @group IdContext
	 */
	implicit def idUnitUnit:ContraEithered[IdCtx, Id, Unit, Unit, Unit] = idSymmetric[Unit]
}

private[typeclass] trait LowPrioContraEithered {
	/**
	 * @group QuotedContext
	 */
	@ifdef("scalaBinaryVersion:3")
	implicit def quotedSymmetric[A]:ContraEithered[scala.quoted.Quotes, scala.quoted.Expr, A, A, A] = BiEithered.quotedSymmetric

	/**
	 * @group MacroContext
	 */
	@ifdef("scalaEpochVersion:2")
	implicit def contextSymmetric[Ctx <: scala.reflect.macros.blackbox.Context with Singleton, A]:ContraEithered[Ctx, Ctx#Expr, A, A, A] = BiEithered.contextSymmetric[Ctx, A]

	/**
	 * @group IdContext
	 */
	implicit def idSymmetric[A]:ContraEithered[IdCtx, Id, A, A, A] = BiEithered.idSymmetric
}

/**
 * Predefined implicit implementations of BiEithered
 * and methods to create new BiEithereds
 *
 * @groupname Support Support
 * @groupprio Support 100
 * @groupname AnyContext Any Context
 * @groupprio AnyContext 1000
 * @groupname QuotedContext Quotes Context
 * @groupprio QuotedContext 1010
 * @groupname MacroContext Macro Context
 * @groupprio MacroContext 1020
 * @groupname IdContext Identity Context
 * @groupprio IdContext 1030
 */
object BiEithered extends LowPrioBiEithered {
	/**
	 * Constructs an `BiEithered` from a set of functions corresponding to each of BiEithered's methods
	 * @group Support
	 */
	def apply[Ctx, Expr[+_], A, B, Z](
		leftFn:(A, Ctx) => Z,
		rightFn:(B, Ctx) => Z,
		contraLeftFn:PartialExprFunction[Ctx, Expr, Z, A],
		contraRightFn:PartialExprFunction[Ctx, Expr, Z, B]
	):BiEithered[Ctx, Expr, A, B, Z] = {
		final class Apply extends BiEithered[Ctx, Expr, A, B, Z] {
			override def left(elem:A)(implicit ctx:Ctx):Z = leftFn(elem, ctx)
			override def right(elem:B)(implicit ctx:Ctx):Z = rightFn(elem, ctx)

			override def contraLeft:PartialExprFunction[Ctx, Expr, Z, A] = contraLeftFn
			override def contraRight:PartialExprFunction[Ctx, Expr, Z, B] = contraRightFn
		}
		new Apply()
	}

	/**
	 * @group MacroContext
	 */
	@ifdef("scalaEpochVersion:2")
	implicit def contextUnitUnit[Ctx <: scala.reflect.macros.blackbox.Context with Singleton]:BiEithered[Ctx, Ctx#Expr, Unit, Unit, Unit] = this.contextSymmetric[Ctx, Unit]

	/**
	 * @group QuotedContext
	 */
	@ifdef("scalaBinaryVersion:3")
	implicit def quotedUnitUnit:BiEithered[scala.quoted.Quotes, scala.quoted.Expr, Unit, Unit, Unit] = quotedSymmetric[Unit]

	/**
	 * @group AnyContext
	 */
	implicit def eitherUnitAny[Ctx, Expr[+_], B, Z](implicit ev:BiOptionally[Ctx, Expr, B, Z]):BiEithered[Ctx, Expr, Unit, B, Z] =
		BiEithered[Ctx, Expr, Unit, B, Z](
			(_, ctx) => ev.none(ctx),
			(value, ctx) => ev.some(value)(ctx),
			PartialExprFunction[Ctx, Expr, Z, Unit](
				(value, ctx) => ev.contraNone(value)(ctx),
				(_, _) => ()
			),
			ev.contraSome,
		)
	/**
	 * @group AnyContext
	 */
	implicit def eitherAnyUnit[Ctx, Expr[+_], A, Z](implicit ev:BiOptionally[Ctx, Expr, A, Z]):BiEithered[Ctx, Expr, A, Unit, Z] =
		BiEithered(
			(value, ctx) => ev.some(value)(ctx),
			(_, ctx) => ev.none(ctx),
			ev.contraSome,
			PartialExprFunction[Ctx, Expr, Z, Unit](
				(value, ctx) => ev.contraNone(value)(ctx),
				(_, _) => ()
			),
		)

	/**
	 * @group IdContext
	 */
	@ifdef("scalaBinaryVersion:3")
	implicit def idUnitUnit:BiEithered[IdCtx, Id, Unit, Unit, Unit] = idSymmetric[Unit]
}

private[typeclass] trait LowPrioBiEithered {
	/**
	 * @group QuotedContext
	 */
	@ifdef("scalaBinaryVersion:3")
	implicit def quotedSymmetric[A]:BiEithered[scala.quoted.Quotes, scala.quoted.Expr, A, A, A] = {
		BiEithered.apply[scala.quoted.Quotes, scala.quoted.Expr, A, A, A](
			(value, ctx) => value,
			(value, ctx) => value,
			PartialExprFunction.identity,
			PartialExprFunction.identity,
		)
	}

	/**
	 * @group MacroContext
	 */
	@ifdef("scalaEpochVersion:2")
	implicit def contextSymmetric[Ctx <: scala.reflect.macros.blackbox.Context with Singleton, A]:BiEithered[Ctx, Ctx#Expr, A, A, A] = {
		BiEithered.apply[Ctx, Ctx#Expr, A, A, A](
			(value, _) => value,
			(value, _) => value,
			PartialExprFunction.identity[Ctx, Ctx#Expr, Ctx#TypeTag, A](using typeclass.Exprs.forContext[Ctx]),
			PartialExprFunction.identity[Ctx, Ctx#Expr, Ctx#TypeTag, A](using typeclass.Exprs.forContext[Ctx]),
		)
	}

	/**
	 * @group IdContext
	 */
	implicit def idSymmetric[A]:BiEithered[IdCtx, Id, A, A, A] = {
		BiEithered.apply[IdCtx, Id, A, A, A](
			(value, _) => value,
			(value, _) => value,
			PartialExprFunction.identity[IdCtx, Id, ClassTag, A],
			PartialExprFunction.identity[IdCtx, Id, ClassTag, A],
		)
	}
}
