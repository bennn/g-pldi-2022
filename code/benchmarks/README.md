benchmarks
===

GTP benchmarks with support for Deep and Shallow types.

Based on version 6.0.

Changes from the `gtp-benchmarks`:

- `jpeg` : add (vector-map values ....)
  The math library uses `require/untyped-contract`, which leaves transient with
  a supertype and a type error. The `vector-map` fixes the type.

- `lnm` : `require/typed plot-pict`
  Transient cannot (yet) automatically recover the type from the
  `(define-typed/untyped-identifier plot-pict ....)`.

- `quadT` : `unsafe-provide` a simple macro
  Used so that deep and shallow can both use the macro.

