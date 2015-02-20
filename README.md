# Melodic (compiler of **MinCaml** to **Zekamashi** architecture)

## How to use
```
make build/compiler
```
will create the compiler at `build/compiler`.<br>
For test, run
```
make
```
and files in `test/` will be tested.<br>
To run one test case (for example, `test/fib.ml`), run
```
make test/fib.test
```
and `test/fib.ml` is compiled and executed, and at the end of execution, the result is compared to the output of `ocaml`.<br>
To make machine code of `mandelbrot.ml`(or `newton.ml`), run
```
make graphic/mandelbrot.x (or newton.ml)
```
and `graphic/mandelbrot.ml` is compiled (to `graphic/mandelbrot.x`). <br>
To run raytracer (in `raytracer/`), run
```
make raytrace
```
and raytracer is compiled (to `raytracer/min-rt`).
# Dependencies
* omake (dependency of **Zekamashi**)

