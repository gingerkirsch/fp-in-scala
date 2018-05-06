Running:
> cd ...   (to the location of module)
> scalac MyModule.scala  (to generate *.class)
> scala MyModule   (to execute)

OR

> scala MyModule.scala   (if has main can be passed directly)

OR

> scala   (call the REPL)
Welcome to Scala.
Type in expressions to have them evaluated.
Type :help for more information.
scala> :load MyModule.scala
Loading MyModule.scala...
defined module MyModule
scala> MyModule.abs(-42)
res0: Int = 42



