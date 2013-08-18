I recommend considering the examples in this order:

- `bernoulli-urn` is a purely generative model playing with Bernoulli urns.
- `coin-flipping` is a simple model of updating
  one's belief in the fairness of a coin based on flipping it.
- `coin-flipping-explicitly` is the same model again, but using only explicit
  distributions (to illustrate what the implicit distributions amount to).
- `animal-tree` and `animal-tree-explicitly` are a more elaborate model, based
  on a published paper, again in two versions to compare implicit and
  explicit distributions.
- `hangman` is a complete demo: a simulator for the game Hangman,
  together with a Bayesian player (whose prior on possible words is
  the Google unigram count).
