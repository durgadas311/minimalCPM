# minimalCPM

This is an exploration into the minimal amount of hardware needed to
run a "CP/M" machine. The idea is to use a CPU, RAM, ROM,
and a single serial port. The serial port is used as both the
system console as well as a CP/NET link to a host computer (server).

The idea is to initially use the serial port as a normal character-by-character
console to run a ROM monitor. When boot is selected, the serial port
is then used to exchange CP/NET packets with the host and run CP/NOS
(CP/NET Network Only System).

The intent is for the CpnetSerialServer component to handle the
initial console mode and then switch to handling CP/NET packets for the boot.

Subdirectories are organized as follows:

**bin:**
-   A collection of consumables for both the simulation and real hardware.

**sim:**
-   JAVA code for the simulator.

**rom:**
-   ROM Monitor code.

**cpnos:**
-   CP/NET code.
