val enumeratum = "com.beachape" %% "enumeratum" % "1.5.13" 

lazy val root = (project in file(".")).settings(
 name := "PokerCalulator",
 libraryDependencies += enumeratum
)
