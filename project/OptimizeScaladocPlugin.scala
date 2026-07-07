package name.rayrobdod.stringContextParserCombinator.build

import sbt._
import sbt.Keys._

object OptimizeScaladocPlugin extends AutoPlugin {
	override def requires = sbt.plugins.JvmPlugin
	override def trigger = allRequirements

	object autoImport {
		val optdocMakeReplacementFontawesomeCss = taskKey[(File, String)]("")
		val optdocMakeReplacementFontawesomeIcons = taskKey[Seq[(File, String)]]("")

		val optdocMakeReplacementMaterialIcons = taskKey[Seq[(File, String)]]("")
	}
	import autoImport._

	override val globalSettings = Seq(
	)

	private def states: Seq[(String, Seq[String], String, String)] = Seq(
		("default", Seq("", ":disabled", ":focus"), "#6F6E77", "#A09FA6"),
		("hover", Seq(":hover", ":active"), "#1A1523", "#CCCCCC"),
		("selected", Seq(".selected"), "#1A1523", "#EDEDEF"),
	)

	private def replaceFonts2(outDir: File)(inPath: (File, String)): (File, String) = {
		import java.io.File.separator
		if (inPath._2.endsWith(".css")) {
			val inStr = sbt.IO.read(inPath._1)
			var str = inStr
			str = str.replaceAll("""@font-face \{[^\}]+\}""", "");
			str = str.replaceAll("""font-family: "Open Sans";""", """font-family: "Open Sans", "DejaVu Sans", Arial, Helvetica, sans-serif;""");
			str = str.replaceAll("""font-family: "Lato", Arial, sans-serif;""", """font-family: "Lato", "DejaVu Sans", Arial, Helvetica, sans-serif;""");
			str = str.replaceAll("""font-family: "Source Code Pro";""", """font-family: "Source Code Pro", "Monaco", "Ubuntu Mono Regular", "Lucida Console", monospace;""");

			if (inStr == str) {
				inPath
			} else {
				val outFile = outDir / inPath._2
				sbt.IO.write(outFile, str)
				(outFile -> inPath._2)
			}
		} else {
			inPath
		}
	}

	private def replaceFonts3(outDir: File)(inPath: (File, String)): (File, String) = {
		import java.io.File.separator
		if (inPath._2.endsWith(".css")) {
			val inStr = sbt.IO.read(inPath._1)
			var str = inStr
			str = str.replaceAll("""@font-face \{[^\}]+\}""", "");
			str = str.replaceAll("""font-family: "Inter-Bold", sans-serif;""", """font-weight: bold; font-family: "DejaVu Sans", Arial, Helvetica, sans-serif;""");
			str = str.replaceAll("""font-family: "Inter-SemiBold", sans-serif;""", """font-weight: bold; font-family: "DejaVu Sans", Arial, Helvetica, sans-serif;""");
			str = str.replaceAll("""font-family: "Inter-Medium", sans-serif;""", """font-family: "DejaVu Sans", Arial, Helvetica, sans-serif;""");
			str = str.replaceAll("""font-family: "Inter-Regular", sans-serif;""", """font-family: "DejaVu Sans", Arial, Helvetica, sans-serif;""");
			str = str.replaceAll("""font-family: "FiraCode-Regular", monospace;""", """font-family: "FiraCode-Regular", "Monaco", "Ubuntu Mono Regular", "Lucida Console", monospace;""");
			str = str.replaceAll("""font-family: "FiraCode-Regular";""", """font-family: "FiraCode-Regular", "Monaco", "Ubuntu Mono Regular", "Lucida Console", monospace;""");

			if (inStr == str) {
				inPath
			} else {
				val outFile = outDir / inPath._2
				sbt.IO.write(outFile, str)
				(outFile -> inPath._2)
			}
		} else {
			inPath
		}
	}

	private def replaceMaterialIcons(outDir: File)(inPath: (File, String)): (File, String) = {
		import java.io.File.separator
		import java.io.File.separatorChar
		val rootdirPath = (0 until inPath._2.count(_ == separatorChar)).map(_ => "../").mkString
		if (inPath._2.endsWith(".html")) {
			var str = sbt.IO.read(inPath._1)
			str = str.replaceAll("""<i class="material-icons"></i>""", s"""<i style="display:inline-block;"><img src="${rootdirPath}lib/MaterialIcons/link.svg" alt="Permalink" /></i>""")
			str = str.replaceAll("""<i id="search-icon" class="material-icons"></i>""", s"""<i id="search-icon">Search: </i>""")
			str = str.replaceAll("""<i class="material-icons arrow"></i>""", s"""<i class="arrow">Options</i>""")
			str = str.replaceAll("""<i class="clear material-icons"></i>""", s"""<i class="clear">Clear</i>""")

			val outFile = outDir / inPath._2
			sbt.IO.write(outFile, str)
			(outFile -> inPath._2)
		} else if (inPath._2 == s"lib${separator}index.css") {
			var str = sbt.IO.read(inPath._1)

			val newLines = Seq(
				"",
				"""#textfilter > .input > .clear {""",
				"""  content: url("MaterialIcons/clear.svg");""",
				"""}""",
				"""#textfilter > .input > .clear:hover {""",
				"""  content: url("MaterialIcons/clear_hover.svg");""",
				"""}""",
				"""#search-icon {""",
				"""  content: url("MaterialIcons/search.svg");""",
				"""}""",
			)
			str += newLines.mkString("\n")

			val outFile = outDir / inPath._2
			sbt.IO.write(outFile, str)
			(outFile -> inPath._2)
		} else if (inPath._2 == s"lib${separator}template.css") {
			var str = sbt.IO.read(inPath._1)

			val newLines = Seq(
				"",
				"""#memberfilter > i.arrow {""",
				"""  content: url("MaterialIcons/play_arrow.svg");""",
				"""}""",
			)
			str += newLines.mkString("\n")

			val outFile = outDir / inPath._2
			sbt.IO.write(outFile, str)
			(outFile -> inPath._2)
		} else if (inPath._2 == s"lib${separator}diagrams.css") {
			var str = sbt.IO.read(inPath._1)

			str = str.replaceAll("""\.material-icons \{[^\}]+\}""", "");

			val outFile = outDir / inPath._2
			sbt.IO.write(outFile, str)
			(outFile -> inPath._2)
		} else {
			inPath
		}
	}

	private def unscopedSettings = Seq(
		optdocMakeReplacementFontawesomeCss := {
			sbt.IO.createDirectory(target.value / "optdoc")
			val outFile = target.value / "optdoc" / "fontawesome.css"
			val clockLines = {
				val icon = "clock"
				for {
					theme <- Seq("light", "dark");
					themeClass = if (theme == "light") {""} else {s".theme-$theme "};
					line <- Seq(
						s"${themeClass}.fas.fa-${icon}::before {",
						s"  content: url('../images/fontawesome/${icon}/${theme}/default.svg')",
						"}"
					)
				} yield line
			}
			val playLines = {
				val icon = "play"
				for {
					theme <- Seq("light", "dark");
					themeClass = if (theme == "light") {""} else {s".theme-$theme "};
					(state, stateClasses, _, _) <- states;
					stateClass <- stateClasses;
					line <- Seq(
						s"${themeClass}.icon-button.run-button${stateClass} .fas.fa-${icon}::before {",
						s"  content: url('../images/fontawesome/${icon}/${theme}/${state}.svg')",
						"}"
					)
				} yield line
			}

			sbt.io.IO.writeLines(outFile, playLines ++ clockLines)
			outFile -> "styles/fontawesome.css"
		},
		optdocMakeReplacementFontawesomeIcons := {
			val outDir = target.value / "optdoc" / "icons-fa"
			sbt.IO.createDirectory(outDir)

			for {
				icon <- Seq("play", "clock");
				theme <- Seq("light", "dark");
				(state, _, lightColor, darkColor) <- if (icon == "play") {states} else {states.take(1)}
			} yield {
				val color = if (theme == "light") {lightColor} else {darkColor}
				val lines = icon match {
					case "play" => Seq(
						s"""<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 384 512" width="11.5" height="15.333" fill="${color}">""",
						"<!--!Font Awesome Free 6.7.2 by @fontawesome - https://fontawesome.com License - https://fontawesome.com/license/free Copyright 2025 Fonticons, Inc.-->",
						"""<path d="M73 39c-14.8-9.1-33.4-9.4-48.5-.9S0 62.6 0 80L0 432c0 17.4 9.4 33.4 24.5 41.9s33.7 8.1 48.5-.9L361 297c14.3-8.7 23-24.2 23-41s-8.7-32.2-23-41L73 39z"/>""",
						"</svg>",
					)
					case "clock" => Seq(
						s"""<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 512 512" width="13" height="13" fill="${color}">""",
						"<!--!Font Awesome Free 6.7.2 by @fontawesome - https://fontawesome.com License - https://fontawesome.com/license/free Copyright 2025 Fonticons, Inc.-->",
						"""<path d="M256 0a256 256 0 1 1 0 512A256 256 0 1 1 256 0zM232 120l0 136c0 8 4 15.5 10.7 20l96 64c11 7.4 25.9 4.4 33.3-6.7s4.4-25.9-6.7-33.3L280 243.2 280 120c0-13.3-10.7-24-24-24s-24 10.7-24 24z"/>""",
						"</svg>",
					)
				}
				val outFile = outDir / s"$icon-$theme-$state.svg"
				sbt.io.IO.writeLines(outFile, lines)
				outFile -> s"images/fontawesome/${icon}/${theme}/${state}.svg"
			}
		},

		optdocMakeReplacementMaterialIcons := {
			val outDir = target.value / "optdoc" / "icons-mat"
			for {
				icon <- Seq("link", "search", "play_arrow", "clear", "clear_hover")
			} yield {
				val stroke = icon match {
					case "link" => """<path d="M17 7h-4v2h4c1.65 0 3 1.35 3 3s-1.35 3-3 3h-4v2h4c2.76 0 5-2.24 5-5s-2.24-5-5-5zm-6 8H7c-1.65 0-3-1.35-3-3s1.35-3 3-3h4V7H7c-2.76 0-5 2.24-5 5s2.24 5 5 5h4v-2zm-3-4h8v2H8z"/>"""
					case "search" => """<path d="M15.5 14h-.79l-.28-.27C15.41 12.59 16 11.11 16 9.5 16 5.91 13.09 3 9.5 3S3 5.91 3 9.5 5.91 16 9.5 16c1.61 0 3.09-.59 4.23-1.57l.27.28v.79l5 4.99L20.49 19l-4.99-5zm-6 0C7.01 14 5 11.99 5 9.5S7.01 5 9.5 5 14 7.01 14 9.5 11.99 14 9.5 14z"/>"""
					case "play_arrow" => """<path d="M8 5v14l11-7L8 5z"/>"""
					case "clear" | "clear_hover" => """<path d="M19 6.41L17.59 5 12 10.59 6.41 5 5 6.41 10.59 12 5 17.59 6.41 19 12 13.41 17.59 19 19 17.59 13.41 12 19 6.41z"/>"""
				}
				val color = icon match {
					case "link" | "play_arrow" | "clear_hover" => "#FFFFFF"
					case "search" | "clear" => "rgba(255,255,255, 0.4)"
				}
				val size = icon match {
					case "link" | "search" => "24px"
					case "play_arrow" => "20px"
					case "clear" | "clear_hover" => "16px"
				}

				val lines = Seq(
					s"""<svg xmlns="http://www.w3.org/2000/svg" height="$size" viewBox="0 0 24 24" width="$size" fill="$color">""",
					"""<!-- Material Icons""",
					"""https://fonts.google.com/icons?icon.set=Material+Icons""",
					"""License - https://www.apache.org/licenses/LICENSE-2.0""",
					"""-->""",
					"""<path d="M0 0h24v24H0V0z" fill="none"/>""",
					stroke,
					"""</svg>"""
				)
				val outFile = outDir / s"MaterialIcons-$icon.svg"
				sbt.io.IO.writeLines(outFile, lines)
				outFile -> s"lib/MaterialIcons/${icon}.svg"
			}
		}
	)

	private def perScopeSettings(config:sbt.librarymanagement.Configuration) = Seq(
		config / packageDoc / mappings := {
			scalaBinaryVersion.value match {
				case "3" =>
					(config / packageDoc / mappings).value
						.filterNot({filePath =>
							import java.io.File.separator
							val path = filePath._2
							path.startsWith(s"images${separator}icon-buttons${separator}twitter${separator}") ||
							path.startsWith(s"images${separator}twitter-icon-") ||
							path.startsWith(s"images${separator}icon-buttons${separator}gitter${separator}") ||
							path.startsWith(s"images${separator}gitter-icon-") ||
							path.startsWith(s"styles${separator}fontawesome.css") ||
							path.startsWith(s"webfonts${separator}fa-") ||
							path.startsWith(s"fonts${separator}Inter-") ||
							path.startsWith(s"fonts${separator}FiraCode-") ||
							false
						})
						.map(replaceFonts3(target.value / "optdoc" / "replaceFonts3"))
						.:+(optdocMakeReplacementFontawesomeCss.value)
						.++(optdocMakeReplacementFontawesomeIcons.value)
				case "2.12" | "2.13" =>
					(config / packageDoc / mappings).value
						.filterNot({filePath =>
							import java.io.File.separator
							val path = filePath._2
							path.startsWith(s"lib${separator}lato-") ||
							path.startsWith(s"lib${separator}open-sans-") ||
							path.startsWith(s"lib${separator}source-code-pro-") ||
							path.startsWith(s"lib${separator}MaterialIcons-") ||
							false
						})
						.++(optdocMakeReplacementMaterialIcons.value)
						.map(replaceMaterialIcons(target.value / "optdoc" / "replaceIcons2"))
						.map(replaceFonts2(target.value / "optdoc" / "replaceFonts2"))
				case _ =>
					(config / packageDoc / mappings).value
			}
		},
	)

	override lazy val projectSettings = unscopedSettings ++ perScopeSettings(Compile) ++ perScopeSettings(Test)
}
