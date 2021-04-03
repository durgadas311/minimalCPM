# Build for CPNOS
# Requires unix2dos utility (e.g. 'dos2unix' package) and 'vcpm'.
#
export CPMDrive_D = $(PWD)
export CPMDefault = d:

WIZBLD = /home/drmiller/cpnet-bld/w5500/mt011
WIZSNIOS = $(WIZBLD)/bin/cpnet12/snios.spr

.PRECIOUS: %.rel %.asm %.lib %.spr

all: cpnos.sys

%.lib: ../../rom/%.lib
	unix2dos -n $< $@

%.asm:: ../%.asm
	unix2dos -n $< $@

%.rel: %.asm z180.lib
	vcpm rmac $*.asm '$$SZ'

%.spr: %.rel
	vcpm link $@=$*'[os,nr]'

cpnos.sys: cobdos.spr cobios.spr ndos.spr snios.spr
	mknetboot -o $@ \
		A:=A: \
		LST:=0: \
		-ndos ndos.spr \
		-snios snios.spr \
		-bdos cobdos.spr \
		-bios cobios.spr

cpnos-w.sys: cobdos.spr cobios.spr ndos.spr $(WIZSNIOS)
	mknetboot -o $@ \
		A:=A: \
		-ndos ndos.spr \
		-snios $(WIZSNIOS) \
		-bdos cobdos.spr \
		-bios cobios.spr

ship:
	rsync -Wuv cpnos.sys ../../bin
