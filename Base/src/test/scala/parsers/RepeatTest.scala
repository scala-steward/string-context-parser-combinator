package com.rayrobdod.stringContextParserCombinator
package parsers

import org.scalatest.funspec.AnyFunSpec
import TestUtilities._

final class RepeatTest extends AnyFunSpec {
	describe ("Repeat") {
		it ("`a*` matches ``") {
			val initialInput = SinglePartInput("", 42)
			val childParser = CharIn("a")

			val expected = Success[Nothing, StubPosition, String](
				"",
				SinglePartInput("", 42),
				SingleExpecting("CharIn(\"a\")", 42),
				Cut.False
			)
			val parser = childParser.repeat()
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("`a*` matches `a`") {
			val initialInput = SinglePartInput("a", 42)
			val childParser = CharIn("a")
			val childExpecting = SingleExpecting("CharIn(\"a\")", 43)

			val expected = Success[Nothing, StubPosition, String](
				Success1(
					"a",
					SinglePartInput("", 43),
					SingleExpecting("CharIn(\"a\")", 42) ++ SingleExpecting("CharIn(\"a\")", 43),
					Cut.False
				),
				List(
					Success1(
						"",
						SinglePartInput("a", 42),
						EmptyExpecting,
						Cut.False
					)
				)
			)
			val parser = childParser.repeat()
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("`a*` matches `aaaa`") {
			val initialInput = SinglePartInput("aaaa", 42)
			val childParser = CharIn("a")

			val expected = Success[Nothing, StubPosition, String](
				Success1(
					"aaaa",
					SinglePartInput("", 46),
					RepeatedExpecting("CharIn(\"a\")", 42 to 46),
					Cut.False
				),
				List(
					Success1(
						"aaa",
						SinglePartInput("a", 45),
						RepeatedExpecting("CharIn(\"a\")", 42 to 44),
						Cut.False
					),
					Success1(
						"aa",
						SinglePartInput("aa", 44),
						RepeatedExpecting("CharIn(\"a\")", 42 to 43),
						Cut.False
					),
					Success1(
						"a",
						SinglePartInput("aaa", 43),
						RepeatedExpecting("CharIn(\"a\")", 42 to 42),
						Cut.False
					),
					Success1(
						"",
						SinglePartInput("aaaa", 42),
						EmptyExpecting,
						Cut.False
					)
				)
			)
			val parser = childParser.repeat()
			assertResult(expected){parser.parse(initialInput)}
		}

		it ("`a+` does not match ``") {
			val initialInput = SinglePartInput("", 42)
			val childParser = CharIn("a")

			val expected = Failure(
				SingleExpecting("CharIn(\"a\")", 42),
				Cut.False
			)
			val parser = childParser.repeat(1)
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("`a+` matches `a`") {
			val initialInput = SinglePartInput("a", 42)
			val childParser = CharIn("a")

			val expected = Success[Nothing, StubPosition, String](
				"a",
				SinglePartInput("", 43),
				RepeatedExpecting("CharIn(\"a\")", 42 to 43),
				Cut.False
			)
			val parser = childParser.repeat(1)
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("`a+` matches `aaaa`") {
			val initialInput = SinglePartInput("aaaa", 42)
			val childParser = CharIn("a")

			val expected = Success[Nothing, StubPosition, String](
				Success1(
					"aaaa",
					SinglePartInput("", 46),
					RepeatedExpecting("CharIn(\"a\")", 42 to 46),
					Cut.False
				),
				List(
					Success1(
						"aaa",
						SinglePartInput("a", 45),
						RepeatedExpecting("CharIn(\"a\")", 42 to 44),
						Cut.False
					),
					Success1(
						"aa",
						SinglePartInput("aa", 44),
						RepeatedExpecting("CharIn(\"a\")", 42 to 43),
						Cut.False
					),
					Success1(
						"a",
						SinglePartInput("aaa", 43),
						RepeatedExpecting("CharIn(\"a\")", 42 to 42),
						Cut.False
					)
				)
			)
			val parser = childParser.repeat(1)
			assertResult(expected){parser.parse(initialInput)}
		}

		it ("`a?` matches ``") {
			val initialInput = SinglePartInput("", 42)
			val childParser = CharIn("a")

			val expected = Success[Nothing, StubPosition, String](
				"",
				SinglePartInput("", 42),
				SingleExpecting("CharIn(\"a\")", 42),
				Cut.False
			)
			val parser = childParser.repeat(0, 1)
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("`a?` matches `a`") {
			val initialInput = SinglePartInput("a", 42)
			val childParser = CharIn("a")

			val expected = Success[Nothing, StubPosition, String](
				Success1(
					"a",
					SinglePartInput("", 43),
					SingleExpecting("CharIn(\"a\")", 42),
					Cut.False
				),
				List(
					Success1(
						"",
						SinglePartInput("a", 42),
						EmptyExpecting,
						Cut.False
					)
				)
			)
			val parser = childParser.repeat(0, 1)
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("`a?` does not match all of `aaaa`") {
			val initialInput = SinglePartInput("aaaa", 42)
			val childParser = CharIn("a")

			val expected = Success[Nothing, StubPosition, String](
				Success1(
					"a",
					SinglePartInput("aaa", 43),
					SingleExpecting("CharIn(\"a\")", 42),
					Cut.False
				),
				List(
					Success1(
						"",
						SinglePartInput("aaaa", 42),
						EmptyExpecting,
						Cut.False
					)
				)
			)
			val parser = childParser.repeat(0, 1)
			assertResult(expected){parser.parse(initialInput)}
		}

		it ("`a**` does not hang indefinitely") {
			val initialInput = SinglePartInput("", 42)
			val childParser = CharIn("a")

			val expected = Success[Nothing, StubPosition, Seq[String]](
				Success1(
					Seq(""),
					SinglePartInput("", 42),
					SingleExpecting("CharIn(\"a\")", 42),
					Cut.False
				),
				List(
					Success1(
						Seq(),
						SinglePartInput("", 42),
						EmptyExpecting,
						Cut.False
					)
				)
			)
			val parser = childParser.repeat().repeat()

			assertResult(expected){parser.parse(initialInput)}
		}

		it ("`a*` with delim `b` matches ``") {
			val initialInput = SinglePartInput("", 42)
			val childParser = CharIn("a")
			val delimParser = CharIn("b").map(_ => ())

			val expected = Success[Nothing, StubPosition, String](
				"",
				SinglePartInput("", 42),
				SingleExpecting("CharIn(\"a\")", 42),
				Cut.False
			)
			val parser = childParser.repeat(delimiter = delimParser)
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("`a*` with delim `b` matches `a`") {
			val initialInput = SinglePartInput("a", 42)
			val childParser = CharIn("a")
			val delimParser = CharIn("b").map(_ => ())

			val expected = Success[Nothing, StubPosition, String](
				Success1(
					"a",
					SinglePartInput("", 43),
					SingleExpecting("CharIn(\"a\")", 42) ++
						SingleExpecting("CharIn(\"b\")", 43),
					Cut.False
				),
				List(
					Success1(
						"",
						SinglePartInput("a", 42),
						EmptyExpecting,
						Cut.False
					)
				)
			)
			val parser = childParser.repeat(delimiter = delimParser)
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("`a*` with delim `b` matches `ababa`") {
			val initialInput = SinglePartInput("ababa", 42)
			val childParser = CharIn("a")
			val delimParser = CharIn("b").map(_ => ())

			val expected = Success[Nothing, StubPosition, String](
				Success1(
					"aaa",
					SinglePartInput("", 47),
					RepeatedExpecting("CharIn(\"a\")", 42 to 46 by 2) ++
						RepeatedExpecting("CharIn(\"b\")", 43 to 47 by 2),
					Cut.False
				),
				List(
					Success1(
						"aa",
						SinglePartInput("ba", 45),
						RepeatedExpecting("CharIn(\"a\")", 42 to 44 by 2) ++
							RepeatedExpecting("CharIn(\"b\")", 43 to 43 by 2),
						Cut.False
					),
					Success1(
						"a",
						SinglePartInput("baba", 43),
						SingleExpecting("CharIn(\"a\")", 42),
						Cut.False
					),
					Success1(
						"",
						SinglePartInput("ababa", 42),
						EmptyExpecting,
						Cut.False
					)
				)
			)
			val parser = childParser.repeat(delimiter = delimParser)
			assertResult(expected){parser.parse(initialInput)}
		}
		it ("`a{2,}` with delim `b` does not match `a` and report expecting 'b'") {
			val initialInput = SinglePartInput("a", 42)
			val childParser = CharIn("a")
			val delimParser = CharIn("b").map(_ => ())

			val expected = Failure(
				SingleExpecting("CharIn(\"a\")", 42) ++
					SingleExpecting("CharIn(\"b\")", 43),
				Cut.False
			)
			val parser = childParser.repeat(min = 2, delimiter = delimParser)
			assertResult(expected){parser.parse(initialInput)}
		}

		describe("`(a ~/ b ~ c)*`") {
			val childParser = (CharIn[Nothing]("a")
				andThenWithCut CharIn[Nothing]("b")
				andThen CharIn[Nothing]("c"))
			val parser = childParser.repeat()

			it ("matches ``; no cut") {
				val initialInput = SinglePartInput("zzz", 42)

				val expected = Success[Nothing, StubPosition, List[Nothing]](
					List(),
					SinglePartInput("zzz", 42),
					SingleExpecting("CharIn(\"a\")", 42),
					Cut.False
				)
				assertResult(expected){parser.parse(initialInput)}
			}
			it ("does not match `a`; has cut") {
				val initialInput = SinglePartInput("a", 42)

				val expected = Failure(
					SingleExpecting("CharIn(\"b\")", 43),
					Cut.True
				)
				assertResult(expected){parser.parse(initialInput)}
			}
			it ("matches `abcde`; has cut") {
				val initialInput = SinglePartInput("abcde", 42)

				val expected = Success[Nothing, StubPosition, List[((Char, Char), Char)]](
					Success1(
						List((('a','b'),'c')),
						SinglePartInput("de", 45),
						SingleExpecting("CharIn(\"b\")", 43) ++
							SingleExpecting("CharIn(\"c\")", 44) ++
							SingleExpecting("CharIn(\"a\")", 45),
						Cut.True
					),
					List(
					)
				)
				assertResult(expected){parser.parse(initialInput)}
			}
			it ("does not match `abca`; has cut") {
				val initialInput = SinglePartInput("abca", 42)

				val expected = Failure(
					SingleExpecting("CharIn(\"b\")", 43) ++
						SingleExpecting("CharIn(\"c\")", 44) ++
						SingleExpecting("CharIn(\"b\")", 46),
					Cut.True
				)
				assertResult(expected){parser.parse(initialInput)}
			}
		}
	}
}