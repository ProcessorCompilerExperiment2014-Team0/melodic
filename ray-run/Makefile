CFLAGS = -g -std=c99 -O2 -Wall -Wextra
sld_bin : sld_bin.o
clean : 
	rm *.o sld_bin

%.sldb : ../raytracer/sld/%.sld sld_bin
	./sld_bin <$< >$@

%.run: %.sldb
	../build/executer ../raytracer/min-rt.x <$< >$*.ppm
%.nrun: %.sldb
	../build/executer ../raytracer/min-rt.x -n <$< >$*-n.ppm


