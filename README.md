Probabilistic Scheme
====================

A probabilistic programming language embedded in MIT Scheme.
Probabilistic Scheme offers anytime exact inference over discrete (but
potentially infinite) probability spaces, including the possibility to
condition on evidence.  The embedding means that the deterministic
potions of a model are seamlessly executed as efficiently as any
normal program, and that the model query language is just Scheme (with
a rich API for interrogating probability distributions).

Installation
------------

Just `git clone` this repository,
```scheme
(load "probscheme/load")
```
and hack away.

If you want to develop Probabilistic Scheme, you will want to also get
the unit test framework that Probabilistic Scheme uses.  Type `git
submodule init` and `git submodule update`.

Usage
-----

- [Main ideas](report.pdf?raw=true)
- [Examples](examples/)
- [Reference Manual](http://alexey.radul.name/probscheme/manual)
- [Developer Guide](guide.md)

Authors
-------
Implemented by Alexey Radul, Taylor Campbell, and Yu-hsin Chen.

License
-------

Probabilistic Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Probabilistic Scheme is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Probabilistic Scheme.  If not, see <http://www.gnu.org/licenses/>.
