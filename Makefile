BUILD = build/
CMP   = $(BUILD)compiler
ASM   = $(BUILD)assembler
EXEC  = $(BUILD)executer

TESTS = fib.test

all: $(TESTS)

test/%.test: test/%.x $(EXEC)
	$(EXEC) test/$*.x
test/%.x: test/%.s $(ASM)
	$(ASM) test/$*.s
	mv test/$* test/$*.x
test/%.s: test/%.ml $(CMP)
	$(CMP) test/$*
$(CMP): min-caml
	make -C min-caml min-caml
	cp min-caml/min-caml $(CMP)

$(ASM): Zebius
	cd Zebius/asm; omake
	cp Zebius/asm/zasm $(ASM)
$(EXEC): Zebius
	make -C Zebius/sim
	cp Zebius/sim/zsim $(EXEC)

