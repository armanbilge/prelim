name := "prelim"
enablePlugins(GitVersioning)
scalaVersion := "2.12.2"
scalacOptions += "-opt:l:classpath"
libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1"
libraryDependencies += "org.compevol" %% "mcmc" % "0.2"
