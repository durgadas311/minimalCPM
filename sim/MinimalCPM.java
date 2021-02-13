// Copyright 2021 Douglas Miller <durgadas311@gmail.com>

import java.util.Vector;
import java.util.concurrent.Semaphore;
import java.util.concurrent.locks.ReentrantLock;
import java.util.Properties;
import java.io.*;

import z80core.*;

// TODO: Make hardware configurable...
// No I/O (outside Z180), no interrupts, ...
// A19 selects RAM/ROM...
public class MinimalCPM implements Computer, Runnable {
	private Z180 cpu;
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
	private Vector<ClockListener> clks;
	private Vector<TimeListener> times;
	private int cpuSpeed = 18432000;
	private int cpuCycle2ms = 36864;
	private int nanoSecCycle = 54;	// 54.25347222...
	private Z80Disassembler disas;
	private ReentrantLock cpuLock;

	private long backlogNs;

	public MinimalCPM(Properties props) {
		stopWait = new Semaphore(0);
		cpuLock = new ReentrantLock();
		clks = new Vector<ClockListener>();
		times = new Vector<TimeListener>();
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
		// Now build hardware...
		asci = new Z180ASCI(props);
		cpu = new Z180(this, asci);
		mem = new SimpleRAM_ROM(props);
		disas = new Z180DisassemblerMAC80(mem, cpu);

		s = props.getProperty("trace");
		if (s != null) {
			Vector<String> ret = new Vector<String>();
			traceCommand(s.split("\\s"), ret);
			if (ret.size() > 0) {
				System.err.format("%s\n", join(ret));
			}
		}
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
		return 0xff;
	}
	public void outPort(int port, int value) {
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
			limit += cpuCycle2ms;
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
				backlogNs += (2000000 - (t1 - t0));
				t0 = t1;
				if (backlogNs > 10000000) {
					try {
						Thread.sleep(10);
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

	private boolean traceCommand(String[] args, Vector<String> err) {
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
}
