// Copyright 2021 Douglas Miller <durgadas311@gmail.com>

import java.util.Arrays;
import java.util.Vector;
import java.util.Map;
import java.util.HashMap;
import java.util.concurrent.Semaphore;
import java.util.concurrent.locks.ReentrantLock;
import java.util.Properties;
import java.io.*;

import z80core.*;

// TODO: Make hardware configurable...
// No I/O (outside Z180), no interrupts, ...
// A19 selects RAM/ROM...
public class MinimalCPM implements Computer, Commander, BaseSystem,
				Interruptor, Runnable {
	private Z180 cpu;
	private boolean z180s;
	private long clock;
	private Memory mem;
	private Z180ASCI asci;
	private boolean running;
	private boolean stopped;
	private Semaphore stopWait;
	private boolean tracing;
	private int traceCycles;
	private int traceLow;
	private int traceHigh;
	private int[] intRegistry;
	private int[] intLines;
	private int intState;
	private int intMask;
	private Vector<ClockListener> clks;
	private Vector<TimeListener> times;
	private Map<Integer, IODevice> ios;
	private Vector<IODevice> devs;
	private int sysSpeed = 18432000;
	private int cpuSpeed = 18432000;
	private int extSpeed1 = 0;
	private int extSpeed2 = 0;
	private int extSpeed3 = 0;
	private int cpuCycle1ms = 18432;
	private int nanoSecCycle = 54;	// 54.25347222...
	private Z80Disassembler disas;
	private StdioDebugger dbg;
	private ReentrantLock cpuLock;

	private long backlogNs;

	public MinimalCPM(Properties props) {
		stopWait = new Semaphore(0);
		cpuLock = new ReentrantLock();
		clks = new Vector<ClockListener>();
		times = new Vector<TimeListener>();
		ios = new HashMap<Integer, IODevice>();
		devs = new Vector<IODevice>();
		intRegistry = new int[8];
		intLines = new int[8];
		Arrays.fill(intRegistry, 0);
		Arrays.fill(intLines, 0);
		intState = 0;
		intMask = 0;

		String s = props.getProperty("log");
		if (s != null) {
			String[] args = s.split("\\s");
			boolean append = false;
			for (int x = 1; x < args.length; ++x) {
				if (args[x].equals("a")) {
					append = true;
				}
			}
			try {
				FileOutputStream fo = new FileOutputStream(args[0], append);
				PrintStream ps = new PrintStream(fo, true);
				System.setErr(ps);
			} catch (Exception ee) {}
		}
		s = props.getProperty("configuration");
		if (s == null) {
			System.err.format("No config file found - using defaults\n");
		} else {
			System.err.format("Using configuration from %s\n", s);
		}
		s = props.getProperty("cpu");
		z180s = (s != null && s.equalsIgnoreCase("Z80S180"));
		s = props.getProperty("sys_speed");
		if (s != null) {
			int c = getSpeed("sys_speed", s);
			if (c != 0) sysSpeed = c;
		}
		s = props.getProperty("ext_speed1");
		if (s != null) {
			int c = getSpeed("ext_speed1", s);
			if (c != 0) extSpeed1 = c;
		}
		s = props.getProperty("ext_speed2");
		if (s != null) {
			int c = getSpeed("ext_speed2", s);
			if (c != 0) extSpeed2 = c;
		}
		s = props.getProperty("ext_speed3");
		if (s != null) {
			int c = getSpeed("ext_speed3", s);
			if (c != 0) extSpeed3 = c;
		}
		setCpuSpeed(sysSpeed);
		// Now build hardware...
		asci = new Z180ASCI(props, this, z180s);
		cpu = new Z180(this, asci, z180s);
		mem = new SimpleRAM_ROM(props);
		disas = new Z180DisassemblerMAC80(mem, cpu);
		s = props.getProperty("mt011");
		if (s != null) {
			addDevice(new MT011(props, "mt011", 0x5c, 1, this));
		}

		s = props.getProperty("trace");
		if (s != null) {
			// Early trace option
			Vector<String> ret = new Vector<String>();
			traceCommand(s.split("\\s"), ret, ret);
			if (ret.size() > 0) {
				System.err.format("%s\n", join(ret));
			}
		}
		s = props.getProperty("debugger");
		if (s != null) {
			dbg = new StdioDebugger(props, this);
		}
	}

	private void setCpuSpeed(int spd) {
		cpuSpeed = spd;
		cpuCycle1ms = spd / 1000;
		long n = (long)1000000000 / spd;
		nanoSecCycle = (int)n;
	}

	private int getSpeed(String p, String s) {
		int c = 0;
		try {
			c = Integer.valueOf(s);
			if (c < 1000000 || c > 50000000) {
				System.err.format("%s: Invalid clock speed %d\n", p, c);
				c = 0;
			}
		} catch (Exception ee) {
			System.err.format("%s: %s\n", p, ee.getMessage());
			c = 0;
		}
		return c;
	}

	///////////////////////////////
	// BaseSystem implementation //
	public int sysClock() { return sysSpeed; }
	public int cpuClock() { return cpuSpeed; }
	public int extClock1() { return extSpeed1; }
	public int extClock2() { return extSpeed2; }
	public int extClock3() { return extSpeed3; }

	////////////////////////////////
	// Interruptor implementation //
	// TODO: more than 8 IRQs? Or N/A?
	public int registerINT(int irq) {
		int val = intRegistry[irq & 7]++;
		// TODO: check for overflow (32 bits max?)
		return val;
	}
	public void raiseINT(int irq, int src) {
		irq &= 7;
		intLines[irq] |= (1 << src);
		if ((intState & ~intMask) != 0) {
			cpu.setINTLine(true);
		}
	}
	public void lowerINT(int irq, int src) {
		irq &= 7;
		intLines[irq] &= ~(1 << src);
		if (intLines[irq] == 0) {
			intState &= ~(1 << irq);
			if ((intState & ~intMask) == 0) {
				cpu.setINTLine(false);
			}
		}
	}
	public void triggerNMI() {
		cpu.triggerNMI();
	}
	public void addClockListener(ClockListener lstn) {
		clks.add(lstn);
	}
	public void addTimeListener(TimeListener lstn) {
		times.add(lstn);
	}
	public void waitCPU() {
		addTicks(1);
	}

	// These must NOT be called from the thread...
	public void start() {
		stopped = false;
		if (running) {
			return;
		}
		running = true;
		Thread t = new Thread(this);
		t.setPriority(Thread.MAX_PRIORITY);
		t.start();
	}

	public void stop() {
		stopWait.drainPermits();
		if (!running) {
			return;
		}
		running = false;
		// This is safer than spinning, but still stalls the thread...
		try {
			stopWait.acquire();
		} catch (Exception ee) {}
	}

	private void reset() {
		cpu.reset();
		mem.reset();
		asci.reset();
		// anything else needs reset?
		backlogNs = 0;
	}

	private void addTicks(int ticks) {
		clock += ticks;
		for (ClockListener lstn : clks) {
			lstn.addTicks(ticks, clock);
		}
		int t = ticks * nanoSecCycle;
		for (TimeListener lstn : times) {
			lstn.addTime(t);
		}
	}

	////////////////////////////////
	/// Commander implementation ///
	public Vector<String> sendCommand(String cmd) {
		String[] args = cmd.split("\\s");
		Vector<String> ret = new Vector<String>();
		ret.add("ok");
		Vector<String> err = new Vector<String>();
		err.add("error");
		if (args.length < 1) {
			return ret;
		}
		if (args[0].equalsIgnoreCase("quit")) {
			// Release CPU, if held...
			stop();
			System.exit(0);
		}
		if (args[0].equalsIgnoreCase("trace") && args.length > 1) {
			if (!traceCommand(args, err, ret)) {
				return err;
			}
			return ret;
		}
		try {
			cpuLock.lock(); // might wait for CPU to finish 1mS
			if (args[0].equalsIgnoreCase("reset")) {
				reset();
				return ret;
			}
			if (args[0].equalsIgnoreCase("dump") && args.length > 1) {
				if (args[1].equalsIgnoreCase("core") && args.length > 2) {
					mem.dumpCore(args[2]);
				}
				if (args[1].equalsIgnoreCase("cpu")) {
					ret.add(cpu.dumpDebug());
					ret.add(disas.disas(cpu.getRegPC()) + "\n");
				}
				if (args[1].equalsIgnoreCase("page") && args.length > 2) {
					String s = dumpPage(args);
					if (s == null) {
						err.add("syntax");
						err.addAll(Arrays.asList(args));
						return err;
					}
					ret.add(s);
				}
				if (args[1].equalsIgnoreCase("mach")) {
					ret.add(dumpDebug());
				}
				// TODO: should be part of cpu?
				if (args[1].equalsIgnoreCase("asci")) {
					ret.add(asci.dumpDebug());
				}
				if (args[1].equalsIgnoreCase("dev") && args.length > 2) {
					for (IODevice dev : devs) {
						if (args[2].equals(dev.getDeviceName())) {
							ret.add(dev.dumpDebug());
							break;
						}
					}
				}
				return ret;
			}
			err.add("badcmd");
			err.add(cmd);
			return err;
		} finally {
			cpuLock.unlock();
		}
	}

	private boolean addDevice(IODevice dev) {
		int base = dev.getBaseAddress();
		int num = dev.getNumPorts();
		for (int x = 0; x < num; ++x) {
			if (ios.get(base + x) != null) {
				System.err.format("Conflicting I/O %02x (%02x)\n", base, num);
				return false;
			}
		}
		devs.add(dev);
		for (int x = 0; x < num; ++x) {
			ios.put(base + x, dev);
		}
		return true;
	}

	/////////////////////////////////////////
	/// Computer interface implementation ///
	public int peek8(int address) {
		int val = mem.read(address);
		return val;
	}

	public void poke8(int address, int value) {
		mem.write(address, value);
	}

	public int intrResp(Z80State.IntMode mode) {
		return 0xff;
	}

	public void retIntr(int opCode) {
	}

	public int inPort(int port) {
		int val = 0;
		port &= 0xff;
		IODevice dev = ios.get(port);
		if (dev == null) {
			//System.err.format("Undefined Input on port %02x\n", port);
		} else {
			val = dev.in(port);
		}
		return val;
	}

	private long lastT = 0;
	private long lastC = 0;
	public void outPort(int port, int value) {
		if ((port & 0xff) == 0xff) {
			// CPU timing debug
			long t = System.nanoTime();
			if (value == 0) {
				System.err.format("FF: %dc, %dt\n", clock - lastC, t - lastT);
			} else {
				System.err.format("FF: -----\n");
			}
			lastT = t;
			lastC = clock;
			return;
		}
		port &= 0xff;
		IODevice dev = ios.get(port);
		if (dev == null) {
			//System.err.format("Undefined Output on port %02x value %02x\n", port, value);
		} else {
			dev.out(port, value);
		}
	}
	public void changeSpeed(int mlt, int div) {
		int spd = (sysSpeed * mlt) / div;
		setCpuSpeed(spd);
		// notify all interested parties...
		asci.newSpeed();
		// TODO: how to fix execute() loop timing...
		// will catch up at next 1mS break.
	}

	// no longer called
	public void contendedStates(int address, int tstates) {
	}

	// no longer called
	public long getTStates() {
		return clock;
	}

	public void breakpoint() {
	}
	public void execDone() {
	}

	//////// Runnable /////////
	public void run() {
		String traceStr = "";
		int clk = 0;
		int limit = 0;
		while (running) {
			cpuLock.lock(); // might sleep waiting for external s/w
			limit += cpuCycle1ms;
			long t0 = System.nanoTime();
			int traced = 0; // assuming any tracing cancels 2mS accounting
			while (running && limit > 0) {
				int PC = cpu.getRegPC();
				boolean trace = tracing;
				if (!trace && (traceCycles > 0 ||
						(PC >= traceLow && PC < traceHigh))) {
					trace = true;
				}
				if (trace) {
					++traced;
					int ppc = cpu.phyAddr(PC);
					int SP = cpu.getRegSP();
					int psp = cpu.phyAddr(SP);
					traceStr = String.format("{%05d} %04x: %02x %02x %02x %02x " +
						": %02x %04x %04x %04x [%04x] %02x%02x",
						clock & 0xffff,
						PC, mem.read(ppc), mem.read(ppc + 1),
						mem.read(ppc + 2), mem.read(ppc + 3),
						cpu.getRegA(),
						cpu.getRegBC(),
						cpu.getRegDE(),
						cpu.getRegHL(),
						cpu.getRegSP(),
						mem.read(psp + 1), mem.read(psp));
				}
				clk = cpu.execute();
				if (clk < 0) {
					clk = -clk;
				} else if (trace) {
					System.err.format("%s {%d} %s\n", traceStr, clk,
						disas.disas(PC));
				}
				limit -= clk;
				if (traceCycles > 0) {
					traceCycles -= clk;
				}
				addTicks(clk);
			}
			cpuLock.unlock();
			if (!running) {
				break;
			}
			long t1 = System.nanoTime();
			if (traced == 0) {
				backlogNs += (1000000 - (t1 - t0));
				t0 = t1;
				if (backlogNs >= 100000) {
					try {
						Thread.sleep(backlogNs / 100000,
							(int)(backlogNs % 100000));
					} catch (Exception ee) {}
					t1 = System.nanoTime();
					backlogNs -= (t1 - t0);
				}
			}
			t0 = t1;
			// trigger 2mS interrupt...
		}
		stopped = true;
		stopWait.release();
	}

	public void startTracing(int cy) {
		if (cy > 0) {
			traceCycles = cy;
		} else {
			tracing = true;
		}
	}

	public void stopTracing() {
		traceLow = traceHigh = 0;
		traceCycles = 0;
		tracing = false;
	}

	private boolean traceCommand(String[] args, Vector<String> err, Vector<String> ret) {
		// TODO: do some level of mutexing?
		if (args[1].equalsIgnoreCase("on")) {
			startTracing(0);
		} else if (args[1].equalsIgnoreCase("off")) {
			stopTracing();
		} else if (args[1].equalsIgnoreCase("cycles") && args.length > 2) {
			try {
				traceCycles = Integer.valueOf(args[2]);
			} catch (Exception ee) {
				err.add(ee.getMessage());
				err.add(args[2]);
				return false;
			}
		} else if (args[1].equalsIgnoreCase("pc") && args.length > 2) {
			// TODO: this could be a nasty race condition...
			try {
				traceLow = Integer.valueOf(args[2], 16);
			} catch (Exception ee) {
				err.add(ee.getMessage());
				err.add(args[2]);
				return false;
			}
			if (args.length > 3) {
				try {
					traceHigh = Integer.valueOf(args[3], 16);
				} catch (Exception ee) {
					err.add(ee.getMessage());
					err.add(args[3]);
					return false;
				}
			} else {
				traceHigh = 0x10000;
			}
			if (traceLow >= traceHigh) {
				traceLow = traceHigh = 0;
			}
		} else {
			err.add("unsupported:");
			err.add(args[1]);
			return false;
		}
		return true;
	}

	private String join(Vector<String> vec) {
		if (vec.size() < 1) {
			return "";
		}
		String s = vec.get(0);
		for (int i = 1; i < vec.size(); ++i) {
			s += ' ' + vec.get(i);
		}
		return s;
	}

	// "dump page xxxx..."
	private String dumpPage(String[] args) {
		String str = "";
		int pg = 0;
		int bnk = 0;
		int i = 2;
		// TODO: allow physical addresses...
		try {
			pg = Integer.valueOf(args[i++], 16);
		} catch (Exception ee) {
			return ee.getMessage();
		}
		// these are paddrs
		int adr = cpu.phyAddr(pg << 8);
		int end = adr + 0x0100;
		while (adr < end) {
			str += String.format("%05x:", adr);
			for (int x = 0; x < 16; ++x) {
				str += String.format(" %02x", mem.read(adr + x));
			}
			str += "  ";
			for (int x = 0; x < 16; ++x) {
				int c = mem.read(adr + x);
				if (c < ' ' || c > '~') {
					c = '.';
				}
				str += String.format("%c", (char)c);
			}
			str += '\n';
			adr += 16;
		}
		return str;
	}

	public String dumpDebug() {
		String ret = String.format("System clock = %gMHz\n", (double)sysSpeed / 1e6);
		ret += String.format("CPU clock = %gMHz\n", (double)cpuSpeed / 1e6);
		ret += String.format("CKA0 clock = %gMHz\n", (double)extSpeed1 / 1e6);
		ret += String.format("CKA1 clock = %gMHz\n", (double)extSpeed2 / 1e6);
		ret += String.format("CKS clock = %gMHz\n", (double)extSpeed3 / 1e6);
		ret += String.format("Backlog = %d nS\n", backlogNs);
		ret += String.format("Tracing = %s\n", tracing);
		ret += String.format("Trace cycles = %d\n", traceCycles);
		ret += String.format("Trace PC = %04x %04x\n", traceLow, traceHigh);
		return ret;
	}
}
