# tvb-fut

tvb-fut is a port of some of the Virtual Brain models to the Futhark language.

## Library

The `lib/github.com/maedoc/tvb-fut` folder contains the library sources, which
will be well organized parameter modules.

## Tests & benchmarks

Some tests can be done with `futhark test` but many of the tests are done
against Python libraries, so stubs of the form `test_thing.fut` and
`test_thing.py` create a set of entrypoints as a library and then test that
thing against a known function.  Eventually, the `test_thing.py` should just
generate a Futhark data file for testing purposes,

Benchmarks are done similarly as tests, `bench_thing.fut` has entry points
and `bench_thing.py` tests the thing from a Python user perspective.

Python tests & benchmarks should eventually converge into set of notebooks
that can be run with Jupyter e.g.
```sh
jupyter nbconvert --execute test_fft.ipynb --to html
```

## AD

Futhark nicely implements automatic differentiation primitives as `jvp` and
`vjp`, but attention should be paid to using online or incremental loss
functions with the `vjp` forms to ensure memory usage stays reasonable.
