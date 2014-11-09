sudo apt-get update -qq
sudo apt-get install ocaml omake
git submodule update --init --recursive
make
build/compiler raytracer/min-rt -glib raytracer/globals -lib min-caml/zebius/libmincaml.txt >/dev/null
build/assembler raytracer/min-rt.s >/dev/null
mv raytracer/min-rt raytracer/min-rt.x
make -C ray-run/ contest.run

