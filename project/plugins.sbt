addSbtPlugin("com.simplytyped" % "sbt-antlr4" % "0.8.3")
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "1.2.0")

resolvers += Resolver.bintrayIvyRepo("rallyhealth", "sbt-plugins")

addSbtPlugin("com.rallyhealth.sbt" % "sbt-git-versioning" % "1.6.0")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.11.0")
