# Clojure snippets

This is repo contains small pieces of Clojure code. 
It may be explorations, puzzle solving code, 
or samples of interesting concepts.

## euler.clj

## expressions.clj

Interpreting arithmetic expressions via [Parser combinators][e1].
Uses [The Parsatron][e2] as library. This is currently in progress,
targeting a simple scripting language. Not for use, but for fun.

[e1]: http://en.wikipedia.org/wiki/Parser_combinator "Wikipedia"
[e2]: https://github.com/youngnh/parsatron "GitHub"

## mastermind.clj

Implementation of the [Mastermind game][m1]. 
Provides two solving functions based on the 
[Five guess algorithm][m2] by [Donald Knuth][m5]
and the [Six guess algorithm][m3] by [Don Greenwell][m6]. 
Check out [MathWorld][m4] for more mathematical details.

[m1]: http://en.wikipedia.org/wiki/Mastermind_(board_game) "Wikipedia"
[m2]: http://en.wikipedia.org/wiki/Mastermind_(board_game)#Five-guess_algorithm
      "Wikipedia"
[m3]: http://en.wikipedia.org/wiki/Mastermind_(board_game)#Six-guess_algorithm
      "Wikipedia"
[m4]: http://mathworld.wolfram.com/Mastermind.html "Wolfram MathWorld"
[m5]: http://en.wikipedia.org/wiki/Donald_Knuth "Wikipedia"
[m6]: http://math2.eku.edu/greenwell/ "Homepage"

## math.clj

## queens.clj

Functional solver of the [Eight queens puzzle][q].

[q]: http://en.wikipedia.org/wiki/Eight_queens_puzzle "Wikipedia"

## util.clj

## License

Copyright © 2012-13 Markus Wappler.

Distributed under the Eclipse Public License, the same as Clojure.
