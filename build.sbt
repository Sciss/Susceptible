lazy val baseName = "Susceptible"

name         := baseName
version      := "0.1.0-SNAPSHOT"
organization := "de.sciss"
scalaVersion := "2.12.2"
description  := "An art piece"
homepage     := Some(url(s"https://github.com/Sciss/$baseName"))
licenses     := Seq("GPL v3+" -> url("http://www.gnu.org/licenses/gpl-3.0.txt"))

libraryDependencies ++= Seq(
  "de.sciss"    %% "scissdsp"   % "1.2.3",
  "de.sciss"    %% "numbers"    % "0.1.3",
  "de.sciss"    %% "fileutil"   % "1.1.2",
  "de.sciss"    %% "kollflitz"  % "0.2.1",
  "de.sciss"    %% "pdflitz"    % "1.2.2",
  "org.pegdown" %  "pegdown"    % "1.6.0"
)

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-Xfuture", "-encoding", "utf8", "-Xlint")
