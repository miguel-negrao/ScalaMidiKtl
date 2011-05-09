import sbt._

class MyProject( info: ProjectInfo ) extends DefaultProject( info ) {
    val swing = "org.scala-lang" % "scala-swing" % "2.8.1"
}