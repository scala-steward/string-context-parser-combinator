package com.rayrobdod.stringContextParserCombinator
package parsers

import scala.collection.immutable.Set
import org.scalatest.funspec.AnyFunSpec
import TestUtilities._

final class FilterTest extends AnyFunSpec {
	describe ("Filter") {
		it ("if base parser fails, parser passes through the failure") {
			val initialInput = SinglePartInput("expect", 42)
			val expected = Failure(SingleExpecting("Left", 102), Cut.False)
			val parser = new ConstResult(expected).filter({(x:Any) => false}, "false")
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("if base parser succeeds and predicate succeeds for all, parser passes through the success") {
			val initialInput = SinglePartInput("expect", 42)
			val leftExpect = SingleExpecting("Left", 102)
			val expected = Success(
				Success1(
					"one",
					SinglePartInput("expectOne", 42),
					SingleExpecting("One", 42),
					Cut.True
				),
				List(
					Success1(
						"two",
						SinglePartInput("expectTwo", 42),
						SingleExpecting("Two", 42),
						Cut.False
					)
				)
			)
			val parser = new ConstResult(expected).filter(x => true, "true")
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("if base parser succeeds and predicate fails for all, returns a failure") {
			val initialInput = SinglePartInput("expect", 42)
			val leftExpect = SingleExpecting("Left", 102)
			val leftResult = Success(
				Success1(
					"one",
					SinglePartInput("expectOne", 42),
					SingleExpecting("One", 42),
					Cut.True
				),
				List(
					Success1(
						"two",
						SinglePartInput("expectTwo", 42),
						SingleExpecting("Two", 42),
						Cut.False
					)
				)
			)
			val expected = Failure(
				SingleExpecting("One where false", 42) ++ SingleExpecting("Two where false", 42),
				Cut.True
			)
			val parser = new ConstResult(leftResult).filter(x => false, "false")
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("if base parser succeeds and predicate fails for one, returns a success with the variants that passed the predicate") {
			val initialInput = SinglePartInput("expect", 42)
			val leftExpect = SingleExpecting("Left", 102)
			val leftResult = Success(
				Success1(
					"one",
					SinglePartInput("expectOne", 42),
					SingleExpecting("One", 42),
					Cut.True
				),
				List(
					Success1(
						"two",
						SinglePartInput("expectTwo", 42),
						SingleExpecting("Two", 42),
						Cut.False
					)
				)
			)
			val expected = Success(
				Success1(
					"two",
					SinglePartInput("expectTwo", 42),
					SingleExpecting("Two", 42),
					Cut.False
				),
				List()
			)
			val parser = new ConstResult(leftResult).filter(x => x == "two", "is two")
			assertResult(expected){parser.parse(initialInput)}
		}
	}
}
