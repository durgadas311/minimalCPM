PLAT = sc203

all:
	$(MAKE) rom PLAT=sc203
	$(MAKE) rom PLAT=minz

rom: mincpm-$(PLAT).rom

%-$(PLAT).rom: zout/%.cim
	cp $< $@

config.lib: $(PLAT).lib __FRC__
	cp $< $@

zout/%.cim: %.asm config.lib
	zmac --dri -8 -c -s -n $<

ship:
	rsync -Wuv mincpm-*.rom ../bin

__FRC__:
