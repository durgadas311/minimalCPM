PLAT = sc203

all:
	$(MAKE) rom PLAT=sc203
	$(MAKE) rom PLAT=minz

rom: mincpm-$(PLAT).rom

%-$(PLAT).rom: zout/%.cim
	cp $< $@

cpnboot.com: zout/cpnboot.cim
	cp $< $@

netboot.com: zout/netboot.cim
	cp $< $@

config.lib: $(PLAT).lib __FRC__
	cp $< $@

zout/%.cim: %.asm config.lib
	zmac --dri -8 -c -s -n $<

ship:
	rsync -Wuv mincpm-*.rom *.com ../bin

__FRC__:
