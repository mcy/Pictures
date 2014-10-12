import com.github.retronym.SbtOneJar._

name := "Pictures"

version := "1.0"

exportJars := true

oneJarSettings

libraryDependencies += "com.github.scopt" %% "scopt" % "3.2.0"

libraryDependencies += "com.google.code.gson" % "gson" % "2.2"

libraryDependencies += "de.tudarmstadt.ukp.wikipedia" % "de.tudarmstadt.ukp.wikipedia.api" % "1.0.0"

libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.3.2"