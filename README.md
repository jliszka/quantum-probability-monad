# Quantum Probability Monad

This code is mostly cribbed off of [sigfpe's vector space monad](http://sigfpe.wordpress.com/2007/03/04/monads-vector-spaces-and-quantum-mechanics-pt-ii/). I'm following along in the [Quantum Mechanics and Quantum Computation](https://class.coursera.org/qcomp-2012-001/class/index) coursera class and coding up some of the examples.

    $ scalac -d out Complex.scala Quantum.scala
    $ scala -cp out -Yrepl-sync -i boot.scala

Try out some of the examples in the Examples object.

    scala> runTeleport(state1)

Contributions welcome!
