all: mincpm.rom

%.rom: zout/%.cim
	cp $< $@

zout/%.cim: %.asm
	zmac --dri -i -8 -c -s -n $<
