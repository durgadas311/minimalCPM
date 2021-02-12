CLASS_PATH = .:z80cpu/z80core.jar:$(HOME)/Downloads/jSerialComm-2.6.2.jar

JAVAS = $(filter-out Cpnet%Server.java,$(wildcard *.java))
CLASSES = $(subst .java,.class,$(JAVAS))
SOURCE = mincpm.tgz
JARS = VirtualMinCpm.jar
SHIPS = $(SOURCE) $(JARS)

all: z80cpu/z80core.jar $(CLASSES)

%.class: %.java
	javac -cp $(CLASS_PATH) $?

jar: $(JARS)

src: $(SOURCE)
# make -C z80cpu src
# make -C h19-sim src
# make -C diablo630 src

build: jar src
	rm -rf temp

ship: __FRC__
#	rsync -Wuv LICENSE $(SHIPS) ../util/*.jar  h19-sim/h19-sim.tgz z80cpu/z80cpu.tgz \
#		diablo630/diablo630.tgz \
#		durgadas.com:/http/durgadas.com/sebhc/mms89
#	rsync -Wuv $(IMAGES) durgadas.com:/http/durgadas.com/sebhc/mms89/images
#	rsync -Wuv README.images.html durgadas.com:/http/durgadas.com/sebhc/mms89/images/README.html
#	rsync -rWuv www/downloads/. durgadas.com:/http/durgadas.com/sebhc/mms89/.
#	rsync -Wruv ../rc/. durgadas.com:/http/durgadas.com/sebhc/mms89/configs/.

VirtualMinCpm.jar: all temp
	echo "Main-Class: VirtualMinCpm" >Manifest.txt
	jar -cmf Manifest.txt VirtualMinCpm.jar *.class \
		*.rom \
		-C temp .

temp: __FRC__
	mkdir -p temp
	rm -rf temp/*
	cd temp; jar -xf ../z80cpu/z80core.jar
	rm -rf temp/META-INF

z80cpu/z80core.jar:
	make -C z80cpu z80core.jar

mincpm.tgz: __FRC__
	cd ..; tar -czf sim/mincpm.tgz sim/Makefile \
		sim/*.java

ship:
	rsync -Wuv VirtualMinCpm.jar ../bin

__FRC__:
