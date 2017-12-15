Infinity Language
=================

Layered Core
------------

The idea of Infinity Language came from the need to unify and
arrange different calculi as extensions to the core of the
language with dependent types (or MLTT core).
The language has pure functions, infinity number of universes,
fixpoint, homotopy interval [0,1], inductive types and pi/sigma as a core.

### PTS

Pure Type Systems are type system based only on Pi-type. Infinity
number of universes was added to avoid paradoxes. Sigma-type is
derivable in PTS.

### MLTT

With usage of dependent fibrations and Pi,Sigma-types Martin-Lof introduced a Type Theory.
Which is known to be sound and consistent with only one impredicative contractable
bottom space and predicative hierarchy of infinitely many spaces. It can be used both as
an operational calculus and a logical framework. Inductive data types is
an extension of MLTT with recursors for case analysis and induction for reasoning.

### HTS

Homotopy Type System is an extension of MLTT with path types, composition on paths,
gluening eliminators. It is used to model higher inductive types and stands
as a contemporary math foundation.

You can read about Infinity Language layers at http://groupoid.space/mltt/infinity/

Base Library
------------

This library is dedicated to [cubical](https://github.com/mortberg/cubicaltt)-compatible
typecheckers based on homotopy interval
<b>[0,1]</b> and MLTT as a core. The base library is founded
on top of 5 core modules: <b>proto</b> (composition, id, const),
<b>path</b> (subst, trans, cong, refl, singl, sym),
<b>propset</b> (isContr, isProp, isSet),
<b>equiv</b> (fiber, eqiuv) and <b>iso</b> (lemIso, isoPath).
This machinery is enough to prove univalence axiom.

(i) The library has rich recursion scheme primitives
in lambek module, while very basic nat, list, stream
functionality. (ii) The very basic theorems are given
in pi, iso_pi, sigma, iso_sigma, retract modules.
(iii) The library has category theory theorems from
HoTT book in cat, fun and category modules.
(iv) The library also includes some impredicative
categorical encoding sketches in coproduct_set.
lambek also includes inductive semantics modeled
with cata/ana recursion and fixpoint adjoints in/out.

This library is best to read with HoTT book at http://groupoid.space/mltt/types/

![depgr](https://github.com/groupoid/infinity/blob/master/doc/img/base.png?raw=true)

* [bool](http://groupoid.space/mltt/types/#bool) — Boolean
* [control](http://groupoid.space/mltt/types/#control) — Control Free/CoFree, fix, pure/applicative typeclasses)
* [eq](http://groupoid.space/mltt/types/#eq) — Runtime equality typeclass
* [functor](http://groupoid.space/mltt/types/#functor) — Runtime Functor typeclass
* [either](http://groupoid.space/mltt/types/#either) — Runtime Either
* [maybe](http://groupoid.space/mltt/types/#either) — Runtime Maybe
* [nat](http://groupoid.space/mltt/types/#nat) — Runtime Nat
* [list](http://groupoid.space/mltt/types/#list) — Runtime List
* [stream](http://groupoid.space/mltt/types/#stream) — Runtime Stream
* [vector](http://groupoid.space/mltt/types/#vector) — Runtime Vector
* [function](http://groupoid.space/mltt/types/#function) — Runtime Function
* [equiv](http://groupoid.space/mltt/types/#equiv) — Core Equivalence
* [propset](http://groupoid.space/mltt/types/#propset) — Core isContr isProp isSet isGroupoid
* [path](http://groupoid.space/mltt/types/#path) — Core Path Type
* [proto](http://groupoid.space/mltt/types/#proto) — Core Proto File
* [iso](http://groupoid.space/mltt/iso) — Core Iso
* [iso_pi](http://groupoid.space/mltt/iso.pi) — Iso Pi Theorems
* [iso_sigma](http://groupoid.space/mltt/iso.sigma) — Iso Sigma Theorems
* [pi](http://groupoid.space/mltt/types/#pi) — Pi
* [sigma](http://groupoid.space/mltt/types/#pi) — Sigma
* [lambek](http://groupoid.space/mltt/iso.sigma) — Inductive Models
* [cat](http://groupoid.space/mltt/types/#cat) — HoTT Category
* [univ](http://groupoid.space/mltt/univ) — HoTT Univalence
* [circle](http://groupoid.space/mltt/types/#circle) — HoTT Circle
* [retract](http://groupoid.space/mltt/types/#retract) — HoTT Retract
* [fun](http://groupoid.space/mltt/types/#fun) — HoTT Categorical Functor

Credits
-------

* Andy Melnikov
* Maxim Sokhatsky


