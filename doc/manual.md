% Probabilistic Scheme Reference Manual

<a href="https://github.com/axch/probscheme"><img style="position: absolute; top: 0; right: 0; border: 0;" src="https://s3.amazonaws.com/github/ribbons/forkme_right_red_aa0000.png" alt="Fork me on GitHub"></a>

Background
----------

[Probabilistic Scheme](https://github.com/axch/probscheme/) is an embedding of probabilistic reasoning into
Scheme.  Probabilistic reasoning is all about probabilities and
probability distributions.  A probability is a numeric measure of the
strength with which one should believe the truth of some proposition
$A$, taking for granted some information $I$ (in the cases where that
strength can be determined).  Here we will denote such a probability
$p(A|I)$.

Probabilistic reasoning is the extension of Boolean logic to the case
of incomplete certainty about the truths and falsehoods of the
propositions about which one reasons.  We set
  $$p(\textrm{false}|I) = 0, \quad p(\textrm{true}|I) = 1$$
for any consistent collection of information $I$.  We combine
probabilities in parallel with propositions according to the Product
Rule for the logical `and` and the Sum Rule for the logical `or`.
The Product Rule is written
  $$p(A,B|I) = p(A|I) \cdot p(B|A,I) = p(B|I) \cdot p(A|B,I),$$
and says that the probability, on the strength of some particular
background information $I$, of $A$ and $B$ is equal to the probability of
$A$ given $I$ times the probability of $B$ given $A$ and $I$, and symmetrically.
The Sum Rule, written
  $$p(A\textrm{ or }B|I) = p(A|I) + p(B|I) - p(A,B|I),$$
says that the probability, on the strength of some particular
background information $I$, for either $A$ or $B$ is equal to the probability
for $A$ given $I$, plus the probability for $B$ given $I$, minus the
probability for both $A$ and $B$ given $I$.  As a specific consequence,
if $I$ entails that $A$ and $B$ are mutually exclusive, then $p(A,B|I) = 0$,
and the sum rule reduces to
  $$p(A\textrm{ or }B|I) = p(A|I) + p(B|I) \qquad
     \textrm{ if }A,B\textrm{ mutually exclusive given }I.$$
If we find ourselves with a collection of propositions that are
pairwise mutually exclusive and collectively exhaustive given some
background $I$, this has a further consequence, important enough to be
given the name 'The Law of Conservation of Belief':
  $$\sum_i p(A_i|I) = 1 \qquad
    \textrm{ if }A_i\textrm{ mutually exclusive and exhaustive given }I.$$

We delve here into neither the motivations for these laws, nor their
multitudinous mathematical consequences, but focus rather on the means
by which reasoning according to them can be embedded into Scheme, to
be specified pleasantly by the programmer and carried out effectively
by the computer.

At this point in the exposition, we make a slight but subtle change of
focus.  Heretofore, we have discussed the probabilities of arbitrary
propositions.  Let us now consider propositions of the form "the value
of such and such expression, if evaluated in the environment specified
by the background information, is $x$".  (We assume the expressions are
purely functional.)  Then, for a given environment and a given
expression, but varying the supposed value $x$, the family of all
such propositions is mutually exclusive and exhaustive, and can be
parametrized by the object $x$.  The collection of the probabilities of
all the propositions of such a family can then be called a probability
distribution over the possible objects $x$.  We move from considering
the probabilities of propositions to considering probability
distributions of this form because the latter are easier to embed in
an expression-oriented language like Scheme, and because we do not
lose any generality by doing so, for we can always represent the
probability of some (computable) proposition as the probability mass assigned to
the value `#t` as a possible value of an expression that computes the
truth of that proposition.

Let us proceed, then, to a language for describing and reasoning about
probability distributions.  This language must have some primitive
mechanisms for producing probability distributions, some mechanisms
for combining probability distributions to make other probability
distributions, some mechanisms for abstracting combined probability
distributions to make them resemble primitive ones, and some
mechanisms for extracting useful information out of probability
distributions.  We address these topics in that order below, along
with the interplay between explicitly tracking the probability
distributions that interest us, and leaving them implicit.

One more point deserves mention before we dive in.  Probability
distributions in Probabilistic Scheme are represented as lazy
computations, and only computed out as far as is needed to answer
questions about them.  At any particular time, an object representing
a probability distribution will have performed some portion of the
computation necessary to determine that distribution in full, which
will have accounted for some amount of the probability mass of that
distribution.  Conversely, some amount of that mass will remain
unaccounted for.  In the case of simple distributions, that assign
mass to only a few objects, it is possible for the system to compute
the distribution out in full, and for the undetermined mass to be
reduced to zero.  In the case of more complex distributions, however,
that assign mass to (perhaps infinitely) many objects, it may take a
(perhaps infinitely) large amount of computation to determine all of
the assignments of mass.  In such a case, the undetermined mass in any
one distribution will monotonically decrease as computation is
performed, but may never reach zero.  The query language for asking
probability distributions for information reflects this consideration.

Creating Explicit Distributions
-------------------------------

`(make-discrete-distribution possibility ...)`

  : Interprets each possibility argument as a two-element list of an
    object and its probability.  Returns the probability distribution
    that assigns that probability to those objects, and zero to all
    others.  Expects the probabilities to sum to 1.

`(alist->distribution alist)`

  : Like `make-discrete-distribution`, but accepts a association list of
    data to probabilities.

`(hash-table->distribution hash-table)`

  : Like `alist->distribution` but accepts a hash table mapping data to
    probabilities.

Manipulating Explicit Distributions
-----------------------------------

`(map-distribution distribution function)`

  : Given the distribution $p(x|I)$ and the function $f$ returns the
    distribution $p(f(x)|I)$.

`(dependent-product distribution function combiner)`

  : A distribution $p(y|X,I)$ that depends on the value of some variable $X$
    can be represented as a function of $X$ that, when given any particular
    value $x$, returns the distribution $p(y|X=x,I)$.  Given a distribution
    $p(x|I)$ and such a function $x \mapsto p(y|X=x,I)$, `dependent-product`
    returns the distribution $p(x,y|I)$ (computed in accordance with the
    Product Rule).  Instead of trying to represent a distribution over
    multiple values, `dependent-product` takes a combiner to apply to the
    values $x$ and `y`, to return $p(\textrm{combiner}(x, y)|I)$.  An oft-useful combiner
    is `cons`.

`(conditional-distribution distribution predicate)`

  : Given a distribution $p(x|I)$ and a predicate $A(x)$, returns the distribution
    over $x$es that satisfy the predicate: $p(x|A(x)\textrm{ is true}, I)$, which
    is given by
    $$\begin{eqnarray*}
    p(x|A(x), I) & = & p(x|I) / p(A|I) & \textrm{ if } & A(x)\textrm{ is true}, \\
    p(x|A(x), I) & = & 0               & \textrm{ if } & A(x)\textrm{ is false}.
    \end{eqnarray*}$$
    where $p(A|I)$ is the probability that $A$ is true.  Since the $x$es are
    mutually exclusive, we know that
      $$p(A|I) = \sum_{x:A(x)} p(x|I).$$

    If $p(A|I)$ turns out to be zero, that is, if $A(x)$ turns out not to be
    true for any $x$ that had positive probability under $p(x|I)$, the result
    will be an impossible distribution.  Such a distribution will always
    give 0 and 1 as the bounds for any datum (see [Querying](#querying-explicit-distributions), below), and
    may throw division by zero errors on some operations if completely
    determined.

Abstracting Explicit Distributions
----------------------------------

Probabilistic Scheme borrows abstraction mechanisms from Scheme.  The
evaluation of an expression that returns a distribution, and the
assigning of a name to its result, constitute abstraction.

Querying Explicit Distributions
-------------------------------

First, some more terminology.  In general, any probability distribution
$p(x|I')$ can be decomposed as $p(x|A(x), I)$, for some predicate $A$ and the
information relation $I' = A,I$. [^hidden-variables] Such a decomposition is useful if
$p(x|I)$ is comparatively easy to compute, for then the desired $p(x|I')$
can be expressed as
$$\begin{eqnarray*}
p(x|I') & = & p(x|I) / p(A|I) & \textrm{ if } & A(x)\textrm{ is true}, \\
p(x|I') & = & 0               & \textrm{ if } & A(x)\textrm{ is false}, \\
p(A|I)  & = & \sum_{x:A(x)} p(x|I). & &
\end{eqnarray*}$$

Notice that $p(A|I)$ does not depend on $x$, so the first line above means
that the desired probability $p(x|I')$ is proportional to the quantity
$p(x|I)$ for those $x$ that satisfy the predicate $A$.  In this context,
this quantity $p(x|I)$ is called the density of $x$ (with respect to the
particular decomposition $I' = A,I$), and the quantity
$p(A|I)$ is called the normalization constant.  In contrast, mass is the
term for $p(x|I')$.  The mass of $x$ does not depend upon the choice of $A$
and $I$, and is usually the quantity of interest.

Probabilistic Scheme embodies this decomposition as follows.  The
object that represents (and can compute) the distribution $p(x|I')$ can
be viewed as internally working by computing some distribution $p(x|I)$
and applying some predicate $A$ to it, such that $I' = A,I$.  This will be
actually true in a nontrivial way for objects created by
`conditional-distribution`, for which the distribution $p(x|I)$ is the one
passed in as the first argument, and the predicate $A$ is the one passed
in as the second. [^decomp-not-unique]

This decomposition is interesting because it interacts with the
on-demand computation of probability distributions.  At any given
time, an object computing $p(x|I')$ according to the decomposition $I' =
A,I$ will have discovered some values $x^a$, with densities $p(x^a|I)$, for
which $A(x)$ holds, and some other values $x$, with their densities
$p(x|I)$, for which $A(x)$ does not hold, and will still be aware of
some undetermined mass in the distribution $p(x|I)$. [^short-circuit]

As the distribution computer objects in Probabilistic Scheme do not
predict future computation, the undetermined mass could, as far as the
object knows, lead to density for more values that satisfy $A$, or more
density for already-discovered values that satisfy $A$, or be lost to
objects that do not satisfy $A$, which latter case will affect the
apparent mass of all already-discovered values that satisfy $A$.  All these
quantities and various derivables from them can be retrieved from
a probability distribution object without causing it to perform any
further computation:

`(distribution? thing)`

  : Returns #t if the given thing is an object explicitly representing a
    probability distribution, and #f otherwise.

`(distribution/determined? distribution)`

  : Returns whether the given distribution object has already been fully
    determined, as opposed to having more computation it could do to
    further refine its knowledge of the distribution it represents.

`(distribution/undetermined-density distribution)`

  : Returns the amount of density, relative the distribution's internal
    decomposition, that remains as yet undetermined.

`(distribution/min-normalizer distribution)`

  : Return the lower bound on the normalization constant $p(A|I)$.  The
    lower bound holds if all of the undetermined density goes to not $A$,
    and is equal to the total density of discovered values that satisfy $A$:
    $$\textrm{min-normalizer} = \sum_{x^a} p(x^a|I).$$

`(distribution/max-normalizer distribution)`

  : Return the upper bound on the normalization constant $p(A|I)$.  The
    upper bound holds if all the undetermined density goes to values that
    satisfy $A$, and is equal to the total density of discovered values that
    satisfy $A$ plus the undetermined density:
    $$\textrm{max-normalizer} = \textrm{min-normalizer} + \textrm{undetermined-density}.$$

`(distribution/undetermined-mass distribution)`

  : Return the amount of probability mass that remains as yet
    undetermined.
    $$\textrm{undetermined-mass} = \frac{\textrm{undetermined-density}}{\textrm{max-normalizer}}.$$

`(distribution/datum-density distribution datum)`

  : Return the density the datum is known to have in the distribution, as
    computed so far.  If the datum is known to satisfy the predicate, this
    is (the so-far-discovered portion of) $p(x^a|I)$.  If it is not, this is
    zero.

`(distribution/min-probability distribution datum)`

  : Return the minimum probability that the given datum could have in this
    distribution.  The minimum value will be realized if all the remaining
    uncomputed density goes to other data that satisfy the predicate
    (maximizing the normalization constant).
    $$\textrm{min-probability} = \frac{\textrm{datum-density}}{\textrm{max-normalizer}}.$$

`(distribution/max-probability distribution datum)`

  : Return the maximum probability that the given datum could have in this
    distribution.  The maximum value will be realized if all the remaining
    uncomputed density goes to this datum.
    $$\textrm{max-probability} = \frac{\textrm{datum-density} + \textrm{undetermined-density}}{\textrm{max-normalizer}}.$$

`(distribution/probability distribution datum)`

  : If the distribution is completely determined, then the above min and
    max probabilities will be equal, and can be called the probability.
    This procedure returns that value, or signals an error if the
    distribution is not completely determined.

Besides that, probability distribution objects can be asked to perform
more of their computations:

`(distribution/refine! distribution)`

  : Runs the computation in the given distribution for the smallest
    detectable increment, which is either until an acceptable datum $x^a$ is
    discovered (or re-discovered) or until some undetermined density is
    lost to failing to satisfy $A$.  In either case, the
    `undetermined-density` decreases.  In the former case, the
    `min-normalizer` of the distribution and the `datum-density`
    of the discovered datum increase.  In the latter case the
    `max-normalizer` decreases.  The `min/max-probability`
    quantities then behave correspondingly.  The `undetermined-mass`
    decreases unless no values have yet been found that satisfy $A$, in
    which case it remains 1.

    If the distribution computation has been finished, i.e. no
    undetermined density remains, `distribution/refine!` does nothing and
    returns `#f`.  If `distribution/refine!` changed something, it returns `#t`.
    Higher-level forcing functions can be built by iterating
    `distribution/refine!` for some desired amount of time or until some
    desired condition has been met.

`(distribution/refine-until! distribution test)`

  : Forces the distribution (with `distribution/refine!`) until the `test`
    returns true.  If the `test` is already true, does nothing.  If the
    distribution is already fully refined, but the test is not satisfied,
    signals an error.

`(distribution/refine-to-mass-bound! distribution bound)`

  : Forces the distribution until the undetermined mass in the distribution
    is less than or equal to the given bound.

Implicit Distributions
----------------------

The preceding functions specify a language for working with explicit
probability distributions.  It is often more natural, however, to
specify a probability distribution as the collection of results of
some "random" process, leaving the distributions that arise inside
that process implicit, and converting to an explicit distribution only
when it becomes useful to consider the distribution as a whole.

`(discrete-select (object mass) ...)                              syntax`

  : Takes any number of two-element lists representing object-probability
    pairs.  The expressions defining the objects and probabilities are
    evaluated lazily, and the probabilities are expected to sum to 1.
    Returns one of the objects, implicitly distributed according to the
    distribution specified by the probabilities.

Implicit distributions for the values of expressions transform
according to the combinations of the input values and the rules of
probability theory.  For example, we can

```scheme
(define (roll-die)
  (discrete-select (1 1/6) (2 1/6) (3 1/6)
                   (4 1/6) (5 1/6) (6 1/6)))
```

Then every call to the `(roll-die)` function will return one of the
numbers from 1 through 6, implicitly uniformly distributed.  In that
case, the expression `(cons (roll-die) (roll-die))` returns one of the
36 cons cells that have one of those numbers in the car slot and one
in the cdr slot, also implicitly uniformly distributed.  The
expression `(+ (roll-die) (roll-die))` will return one of the numbers
from 2 through 12, implicitly distributed according to the probability
of getting that sum when rolling two fair six sided dice.  So on and
so forth, for the purely functional subset of Scheme.

Laziness on the objects of `discrete-select` permits construction of
infinite distributions, for example the geometric distribution with
constant `alpha` and minimum value `n`:

```scheme
(define (geometric alpha n)
  (discrete-select
   (n alpha)
   ((geometric alpha (+ n 1)) (- 1 alpha))))
```

`(observe! boolean)`

  : Modifies the current implicit distribution by conditioning
    it on the argument being true.  Returns an unspecified value.

Consider, for example, the expression

```scheme
(let ((face (roll-die))) ;; Line 1
  (observe! (> face 2))  ;; Line 2
  face)                  ;; Line 3
```

In line 1, the expression `(roll-die)` returns one of the numbers from 1
through 6, implicitly uniformly distributed.  `let` then binds it to the
name `face`, whose value is then implicitly uniformly distributed over 1
through 6.  The expression `(> face 2)` on line 2 has one of the values
`#t`, `#f`, implicitly distributed as 2/3 for `#t` and 1/3 for `#f`.  `observe!`
modifies this implicit distribution to require `#t`.  This modifies
the implicit distribution for face to be consistent with `(> face 2)`
returning `#t`, that is it conditions $p(\textrm{face}|I)$ on `(> face 2)`.  The
distribution of return values from this whole `let` form is then
$p(\textrm{face}|I,\textrm{face} > 2)$, in other words uniform over the numbers from
3 through 6.

`(likelihood! p)`

  : A shortcut over `observe!` that modifies the current implicit
    distribution, multiplying the probability of any result subsequent to
    the call to `likelihood!` by `p`.  Could be defined as

    ```scheme
    (define (likelihood! p)
      (observe! (discrete-select (#t p) (#f (- 1 p)))))
    ```

`(stochastic-thunk->distribution thunk)`

  : Returns, as an explicit probability distribution, the implicit
    distribution over the possible return values of the given `thunk`.
    To continue the running example above,
    `(stochastic-thunk->distribution roll-die)`
    would return an explicit probability distribution object that
    assigned equal mass to the six numbers from 1 through 6.

`(distribution-select distribution)`

  : Returns one of the possible values from the given explicit
    distribution, implicitly distributed according thereto.
    `roll-die`, above, could have been defined as

    ```scheme
    (define (roll-die)
      (distribution-select
       (make-discrete-distribution '(1 1/6) '(2 1/6) '(3 1/6)
                                   '(4 1/6) '(5 1/6) '(6 1/6))))
    ```

Footnotes
---------

[^hidden-variables]: Actually, $A$ may refer to hidden variables as well.  So really,
the decomposition is
  $$p(x|I') = \sum_y p(x,y|A(x,y),I)$$
but omitting this subtlety simplifies the exposition in the main text.

[^decomp-not-unique]: Decomposition of distributions into generators and filters like
this is not unique.  In particular, if
$$\begin{eqnarray*}
p(x|I'') & = & p(x|A(x),I')\textrm{ and }p(x|I') = p(x|B(x),I)\textrm{ then} \\
p(x|I'') & = & p(x|(A(x)\textrm{ and }B(x)),I)
\end{eqnarray*}.$$

    This may arise nontrivially if a distribution produced by
`conditional-distribution` is itself further conditioned.  The
implementation is free to choose whatever decomposition is convenient,
with the only guarantee being that each individual distribution object
will be consistent.

[^short-circuit]: For many $A$, the negative case can be evaluated more efficiently
than generating putative $x$ and testing them with $A$.  If $x$ is a
compound object and $A$ a compound predicate, it may be possible to
determine that $A$ does not hold of $x$ just from some part of $x$, in which
case the (potentially many) possible $x$ which agree on that part need
not be generated.

<script type="text/javascript"
   src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"></script>
<script type="text/javascript">
MathJax.Hub.Config({
  asciimath2jax: {
    delimiters: [['$$','$$']]
  }
});
</script>

<style type="text/css">
sup { vertical-align:baseline; } /* Do not raise footnotes generated by Pandoc */
sup a:before { content:"["; }
sup a:after { content:"]"; }
.footnotes ol { counter-reset: item; margin-left: 0em; padding-left: 0em;}
.footnotes ol li { list-style: none; margin-left: 0em;}
.footnotes li p:first-child:before {
  content: "[" counter(item) "] ";
  counter-increment: item;
}
.footnotes hr { display: none; }
</style>
