# Burn ROM:
#	minipro -p "SST39SF040" -s -w mincpm-sc203.rom
#	minipro -p "MX29F040C@PLCC32" -u -s -w mincpm-minz.rom
#
PLAT = sc203

# TODO: get snios.rel locally...
# WIZSNIOS = zout/snioswiz.rel
# zout/snioswiz.rel: ~/git/cpnet-z80/src/w5500/snios.asm
#	zmac --dri -8 -c -s -n --rel -o $@ $<
WIZBLD = /home/drmiller/cpnet-bld/w5500/mt011
SERBLD = /home/drmiller/cpnet-bld/ser-dri/minz

WIZCFG = $(WIZBLD)/src/libwiznt.rel $(WIZBLD)/src/libnvram.rel
WIZSNIOS = $(WIZBLD)/src/snios.rel
WIZLIB = $(WIZBLD)/src/libwiznt.rel $(WIZBLD)/src/libnvram.rel

# any copy of ser-dri snios will do...
SERSNIOS = $(SERBLD)/src/snios.rel
# NOTE: all 'netboot.rel' are identical
NBOOT = $(SERBLD)/src/netboot.rel

all:
	$(MAKE) rom PLAT=sc203
	$(MAKE) rom PLAT=minz

rom: mincpm-$(PLAT).rom

mincpm.bin: zout/mincpm.rel $(NBOOT) zout/modloc.rel
	ld80 -O obj -D 0x2000 -c -o $@ $^

mincpm-minz.rom: mincpm.bin auxminz.obj end.bin
	cat $^ >$@

mincpm-sc203.rom: mincpm.bin conmod.obj wizmod.obj wcfmod.obj wdbmod.obj end.bin
	cat $^ >$@

# CPNETLDR but over the network
cpnboot-minz.com: zout/cpnboot.rel $(NBOOT) $(SERSNIOS) zout/minzasci1.rel
	ld80 -O obj -o $@ $^

netboot-minz.com: zout/netboot.rel $(NBOOT) $(SERSNIOS) zout/minzasci1.rel
	ld80 -O obj -o $@ $^

netboot-asci0.com: zout/netboot.rel $(NBOOT) $(SERSNIOS) zout/z180asci0.rel
	ld80 -O obj -o $@ $^

netboot-wbw-wiz.mod: zout/netboot-wbw.rel $(NBOOT) $(WIZSNIOS) $(WIZLIB)
	ld80 -O obj -P 0x100 -o netboot-wbw-wiz.obj $^
	dd if=netboot-wbw-wiz.obj conv=sync bs=8k of=$@

config.lib: $(PLAT).lib __FRC__
	cp $< $@

zout/%.cim: %.asm config.lib
	zmac --dri -8 -c -s -n $<

zout/%.rel: %.asm
	zmac --dri -8 -c -s -n --rel $<

end.bin:
	/bin/echo -e -n '\377' >$@

wizmod.obj: zout/wizmod.rel $(WIZCFG) $(WIZSNIOS) zout/plug.rel
	ld80 -O obj -o $@ -P 0x3000 $^

conmod.obj: zout/conmod.rel $(SERSNIOS) zout/conio.rel zout/plug.rel
	ld80 -O obj -o $@ -P 0x3000 $^

auxminz.obj: zout/auxmod.rel $(SERSNIOS) zout/minzasci1.rel zout/plug.rel
	ld80 -O obj -o $@ -P 0x3000 $^

wcfmod.obj: zout/wcfmod.rel $(WIZLIB) zout/plug.rel
	ld80 -O obj -o $@ -P 0x3000 $^

wdbmod.obj: zout/wdbmod.rel $(WIZLIB) zout/plug.rel
	ld80 -O obj -o $@ -P 0x3000 $^

ship:
	rsync -Wuv mincpm-*.rom *.com ../bin

__FRC__:
