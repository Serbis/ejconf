This is the very simple configuration library that I use in my Scala projects. The idea to write your own library arose from the lack of completely satisfying solutions for me from the libraries that exist at the moment. Each of them has both pluses and minuses. I tried to take the best from them, removing the cons. Here are the goals I pursue in the development of this library:
+ Human readability (Implemented)
+ Complex strings (Partially implemented)
+ Readable DSL (Implemented)
+ Inheritance
+ Comments

How it work. Add the parboiled library to your build.sbt
```sbt
libraryDependencies +=  "org.parboiled" %% "parboiled" % "2.1.4"
```
Сopy the library's files into a package convenient for you in your project.
Create config:
```
logs: {
  path: "/var/log/some.log"
  batch: 10
  rewrite: false
  logline: {
    template: "[Timestamp]-[Message]"
    timestamp: "hh:mm DD:MM:YYYY"
  }
}
```
Use config:
```scala
val config = loadConfig("/etc/myprog/main.ejc")
val template = config ~> "logs" ~> "logline" ~> "template" >|
```
For more examples of use, see Examples.scala
Complete specification of the configuration language may by found in the files langspec_versions.ejc

I do not have a specific plan for developing this project. It will develop as needed for me a new different functional.
