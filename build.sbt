val enumeratum = "com.beachape" %% "enumeratum" % "1.5.13" 
val quill_core = "io.getquill" %% "quill-core" % "2.5.4"
val quill_cass = "io.getquill" %% "quill-cassandra" % "2.5.4"

lazy val root = (project in file(".")).settings(
 name := "PokerCalulator",
 libraryDependencies += enumeratum,
 libraryDependencies += quill_core,
 libraryDependencies += quill_cass
)
