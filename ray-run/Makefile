CFLAGS = -g -std=c11 -O2 -Wall -Wextra
sld_bin : sld_bin.o
clean : 
	rm *.o sld_bin

%.sldb : ../raytracer/sld/%.sld sld_bin
	./sld_bin <$< >$@

%.run: %.sldb
	../build/executer ../raytracer/min-rt.x -m <$< >$*.ppm 2>/dev/null


