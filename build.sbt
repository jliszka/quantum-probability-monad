name := "probability-monad" // insert clever name here

scalaVersion := "2.10.3"

crossScalaVersions := Seq("2.9.2", "2.9.3", "2.10.3")

version := "1.0.0"

organization := "org.jliszka"

publishMavenStyle := true

publishArtifact in Test := false

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

pomIncludeRepository := { _ => false }

pomExtra := (
  <url>http://github.com/jliszka/quantum</url>
  <licenses>
    <license>
      <name>Apache</name>
      <url>http://www.opensource.org/licenses/Apache-2.0</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>git@github.com:jliszka/quantum-probability-monad.git</url>
    <connection>scm:git:git@github.com:jliszka/quantum-probability-monad.git</connection>
  </scm>
  <developers>
    <developer>
      <id>jliszka</id>
      <name>Jason Liszka</name>
      <url>http://jliszka.github.io</url>
    </developer>
  </developers>)

credentials ++= {
  val sonatype = ("Sonatype Nexus Repository Manager", "oss.sonatype.org")
  def loadMavenCredentials(file: java.io.File) : Seq[Credentials] = {
    xml.XML.loadFile(file) \ "servers" \ "server" map (s => {
      val host = (s \ "id").text
      val realm = if (host == sonatype._2) sonatype._1 else "Unknown"
      Credentials(realm, host, (s \ "username").text, (s \ "password").text)
    })
  }
  val ivyCredentials   = Path.userHome / ".ivy2" / ".credentials"
  val mavenCredentials = Path.userHome / ".m2"   / "settings.xml"
  (ivyCredentials.asFile, mavenCredentials.asFile) match {
    case (ivy, _) if ivy.canRead => Credentials(ivy) :: Nil
    case (_, mvn) if mvn.canRead => loadMavenCredentials(mvn)
    case _ => Nil
  }
}

initialCommands := """
                |import org.jliszka.quantum.Complex._
                |import org.jliszka.quantum.Q._
                |import org.jliszka.quantum.Basis._
                |import org.jliszka.quantum.Gate._
                |import org.jliszka.quantum.Operator._
                |import org.jliszka.quantum.Examples._""".stripMargin('|')
