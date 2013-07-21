The code is laid out as follows:

- Interesting Stuff

  - `distribution.scm`: Data structure, combinators, and query
    langauge for explicit distributions.
  - `implicit.scm`: Implicit distribution language and search
    strategies.
  - `possibility.scm`: Representation of individual packets of
    information wherewith distributions communicate.

- Experiments

  - `decisions.scm`: Decision making on top of probabilities.
  - `memoize.scm`: Thoughts about stochastic memoization.

- Support

  - `support/*`: Data structures, streams, shift-reset.

- Test Suite

  - Run it with `rake test` or just `rake`
  - The actual tests are under `test/`
  - The `testing/` directory is a git submodule pointed at the [Test
    Manager](http://github.com/axch/test-manager/) framework that the
    test suite is written in.

- "Issue tracker" by force of will: `todo.txt`.
