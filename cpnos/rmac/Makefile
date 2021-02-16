# Build for CPNOS
# Requires unix2dos utility (e.g. 'dos2unix' package) and 'vcpm'.
#
export CPMDrive_D = $(PWD)
export CPMDefault = d:

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
	mknetboot -g -o $@ \
		A:=A: \
		LST:=0: \
		-ndos ndos.spr \
		-snios snios.spr \
		-bdos cobdos.spr \
		-bios cobios.spr

ship:
	rsync -Wuv cpnos.sys ../../bin
