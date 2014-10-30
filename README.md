# Melodic (compiler of **MinCaml** to **Zebius** architecture)

## How to use
```
make build/compiler
```
will create the compiler at build/compiler.<br>
For test, run
```
make
```
and files in `test/` will be tested.<br>
To run one test case (for example, `test/fib.ml` and `test/fib.cons`), run
```
make test/fib.test
```
and `test/fib.ml` is compiled and executed, and at the end of execution, register constraints are checked.<br>
To run raytracer (in `raytracer/`), run
```
make raytrace
```
and raytracer is compiled (to `raytracer/min-rt`).
# Dependencies
* omake (dependency of **Zebius**)

