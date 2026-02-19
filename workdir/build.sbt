val enumeratum = "com.beachape" %% "enumeratum" % "1.5.13" 

lazy val root = (project in file(".")).settings(
 scalaVersion := "2.12.21",
 name := "PokerCalulator",
 libraryDependencies += enumeratum
)
