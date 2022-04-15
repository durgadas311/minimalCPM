// Copyright (c) 2022 Douglas Miller <durgadas311@gmail.com>

import java.util.Arrays;
import java.util.Properties;
import java.util.Vector;
import java.awt.event.*;
import java.io.*;

public class RC2014_CF implements IODevice, Runnable {

	static final int adr_Data_c = 0;
	static final int adr_Error_c = 1;
	static final int adr_Feature_c = 1;
	static final int adr_SecCnt_c = 2;
	static final int adr_Sector_c = 3;
	static final int adr_CylLo_c = 4;
	static final int adr_CylHi_c = 5;
	static final int adr_Head_c = 6;
	static final int adr_Status_c = 7;
	static final int adr_Cmd_c = 7;
	// These are not accessible
	static final int adr_AltStatus_c = 14;
	static final int adr_DevCtl_c = 14;
	static final int adr_DrvAdr_c = 8; // TODO: where is this register?

	public static final int sts_Busy_c = 0x80;
	public static final int sts_Ready_c = 0x40;
	public static final int sts_WriteFault_c = 0x20;
	public static final int sts_DSC_c = 0x10;
	public static final int sts_Drq_c = 0x08;
	public static final int sts_Corr_c = 0x04;
	public static final int sts_Error_c = 0x01;

	public static final int err_BBK_c = 0x80;
	public static final int err_UNC_c = 0x40;
	public static final int err_MC_c = 0x20;
	public static final int err_IDNF_c = 0x10;
	public static final int err_MCR_c = 0x08;
	public static final int err_ABRT_c = 0x04;
	public static final int err_TK0NF_c = 0x02;
	public static final int err_AMNF_c = 0x01;

	// CF Command codes
	static final byte cmd_Recal_c = (byte)0x10;
	static final byte cmd_Read_c = (byte)0x20;
	static final byte cmd_ReadRetry_c = (byte)0x21;
	static final byte cmd_ReadV_c = (byte)0x40;	// implement?
	static final byte cmd_ReadVRetry_c = (byte)0x41;	// implement?
	static final byte cmd_Write_c = (byte)0x30;
	static final byte cmd_WriteRetry_c = (byte)0x31;
	static final byte cmd_WriteV_c = (byte)0x3c;
	static final byte cmd_Seek_c = (byte)0x70;
	static final byte cmd_Features_c = (byte)0xef;
	static final byte cmd_IdentDrv_c = (byte)0xec;
	static final byte cmd_SetParam_c = (byte)0x91;

	// State Machine
	// Read:
	//	cmd_Read_c	sts_Busy_c
	//			!sts_Busy_c, sts_Drq_c
	//	(read data)	...
	//			!sts_Drq_c
	//
	// Write:
	//	cmd_Write_c	sts_Busy_c
	//			sts_Drq_c
	//	(write data)	...
	//			!sts_Busy_c, !sts_Drq_c

	private int base;
	private enum State {
		IDLE,
		COMMAND,
		DATA_IN,
		DATA_OUT,
		SENSE,
		DRIVECB,
		STATUS,
	};
	private State curState;

	private Interruptor intr;
	private int driveSecLen;
	private boolean resetting;

	// mode COMMAND
	private byte[] cmdBuf = new byte[16];
	private byte curCmd;
	private byte feat;
	// mode DATA_IN/DATA_OUT
	private byte[] dataBuf;
	private int dataLength;
	private int dataIx;
	private long wrOff;
	private java.util.concurrent.LinkedBlockingDeque<Integer> cmds;

	private String name;

	private class CFCard {
		public RandomAccessFile driveFd;
		public String driveMedia;
		public long capacity;
		public int defCyl;
		public long defLBA;
		public int defSec = 16;
		public int defHead = 16;
		public int nCyl;
		public int nHead;
		public int nSec;
		public long nLBA;
	}

	private CFCard[] drives;
	private CFCard cd;

	RC2014_CF(Properties props, String lab, int base, Interruptor intr) {
		this.intr = intr;
		this.base = base;
		
		dataBuf = null;
		dataLength = 0;
		dataIx = 0;
		driveSecLen = 512;

		drives = new CFCard[2];
		cmds = new java.util.concurrent.LinkedBlockingDeque<Integer>();
		// Always show drive on front panel, even if not usable
		name = lab.toUpperCase();
		String pfx = lab.toLowerCase();
		String s = props.getProperty(pfx + "_port");
		// TODO: change port
		s = props.getProperty(pfx + "_intr");
		// TODO: change intr
		setupDrive(props, pfx, 0);

		dataBuf = new byte[driveSecLen + 4];	// space for ECC for "long" commands
		dataLength = driveSecLen;
		// default to drive 0 selected...
		selDrv(0);
		System.err.format("%s device at port %02x\n", name, base);
		Thread t = new Thread(this);
		t.start();
	}

	private void setupDrive(Properties props, String pfx, int ix) {
		CFCard cf = new CFCard();
		String s = props.getProperty(String.format("%s_drive%d", pfx, ix));
		if (s == null && ix == 0) {
			s = props.getProperty("cf_drive");
			if (s == null) {
				s = "64M";
			}
		}
		if (s == null) {
			return;
		}
		String[] ss = s.split("\\s", 2);
		if (ss[0].matches("[0-9]+[MmGg]")) {
			int i = ss[0].length() - 1;
			char f = Character.toUpperCase(ss[0].charAt(i));
			cf.capacity = Integer.valueOf(ss[0].substring(0, i)) *
				1024 * 1024;	// Megabytes at least...
			if (f == 'G') {
				cf.capacity *= 1024;
			}
		}
		if (ss.length > 1) {
			// Front panel name, not media name
			//name[ix] = ss[1];
		}
		cf.defCyl = (int)(cf.capacity / driveSecLen / cf.defSec / cf.defHead);
		if (cf.defCyl > 65535) {
			System.err.format("CF drive too large, reducing to 4G\n");
			cf.capacity = 4*1024*1024*1024;
			cf.defCyl = 32768;
		}
		cf.defLBA = cf.defCyl * cf.defHead * cf.defSec;
		s = props.getProperty(String.format("%s_disk%d", pfx, ix));
		if (s == null && ix == 0) {
			s = props.getProperty("cf_disk");
			if (s == null) {
				s = "CF";
			}
		}
		if (s == null) {
			s = String.format("CF-%d", ix);
		}
		cf.driveMedia = s;

		RandomAccessFile fd;
		try {
			fd = new RandomAccessFile(cf.driveMedia, "rw");
			if (fd.length() == 0) {
				// special case: new media - initialize it.
				System.err.format("Initializing new media %s as %dM\n",
					cf.driveMedia, cf.capacity / 1024 / 1024);
				fd.setLength(cf.capacity);
				setHeader(fd, cf.capacity);
			} else {
				// TODO: check for header?
				cf.capacity = fd.length() & ~0x07f; // drop "header" if present
				System.err.format("Mounted existing media %s as %dM\n",
						cf.driveMedia, cf.capacity / 1024 / 1024);
			}
		} catch (Exception ee) {
			System.err.format("RC2014: Unable to open media %s\n", cf.driveMedia);
			return;
		}
		cf.driveFd = fd;
		drives[ix] = cf;
	}

	private void setHeader(RandomAccessFile fd, long capacity) {
	}

	private int getCyl() {
		return ((cmdBuf[adr_CylHi_c] & 0xff) << 8) | (cmdBuf[adr_CylLo_c] & 0xff);
	}

	private int getSec() {
		return cmdBuf[adr_Sector_c] & 0xff;
	}

	private int getCount() {
		return cmdBuf[adr_SecCnt_c] & 0xff;
	}

	private int getHead() {
		return cmdBuf[adr_Head_c] & 0x0f;
	}

	private boolean isLBA() {
		return (cmdBuf[adr_Head_c] & 0x40) != 0;
	}

	private int getLUN(byte b) {
		return (b & 0x10) >> 4;
	}

	private int getLUN() {
		return getLUN(cmdBuf[adr_Head_c]);
	}

	private int getSSZ() {
		return driveSecLen;
	}

	private long getOff() {
		// TODO: support non-LBA mode...
		long off;
		if (isLBA()) {
			off = (getHead() << 24) |
				(getCyl() << 8) |
				getSec();
		} else {
			off = getCyl() * cd.nHead +
				getHead() * cd.nSec +
				getSec();
		}
		off *= driveSecLen;
		return off;
	}

	public int getBaseAddress() { return base; }
	public int getNumPorts() { return 8; }

	public synchronized int in(int port) {
		port &= 0x07;
		int val = cmdBuf[port] & 0xff;
		switch(port) {
		case adr_Data_c:
			// TODO: emulate 16->8 bit mux?
			getData();
			break;
		case adr_Error_c:
			// TODO: reset bits?
			break;
		case adr_Status_c:
			// TODO: need to ensure atomicity?
			synchronized(this) {
			val = cmdBuf[port] & 0xff;
			if ((val & sts_Busy_c) == 0) {
				cmdBuf[port] &= ~sts_Error_c;
			}
			}
			break;
		case adr_AltStatus_c:
			val = cmdBuf[adr_Status_c]; // no reset of bits
			break;
		case adr_DrvAdr_c:
			// TODO: implement this?
			// "1", nWTG, nHS3..0, nDS1..0
			break;
		}
		return val;
	}

	public void out(int port, int val) {
		port &= 0x07;
		// TODO: is synchronization needed here?
		if ((cmdBuf[adr_Status_c] & sts_Busy_c) != 0) {
			return;
		}
		switch(port) {
		case adr_DevCtl_c:
			// TODO: implement
			// nIEN: interrupts not supported by RC2014
			// SRST: disable device while "1"...
			if (resetting && (val & 0x04) == 0) {
				// This should not spin much, if any.
				while (cmds.size() > 0 && curCmd != 0);
				reset(); // TODO: too much?
			}
			resetting = ((val & 0x04) != 0);
			return;
		case adr_Feature_c:
			// anything?
			feat = (byte)val;
			return;
		case adr_Cmd_c:
			synchronized(this) {
			cmdBuf[adr_Status_c] |= sts_Busy_c;
			}
			cmdBuf[adr_Error_c] = 0;
			queueCmd(val);
			return;
		case adr_Data_c:
			// TODO: emulate 8->16 bit mux?
			putData(val);
			break;
		case adr_Head_c:
			selDrv(getLUN((byte)val));
			// rest is used later...
			break;
		}
		cmdBuf[port] = (byte)val;
	}

	public void reset() {
		cmds.clear();
		dataIx = 0;
		Arrays.fill(cmdBuf, (byte)0);
		curCmd = 0;
		selDrv(0);
	}

	private void selDrv(int ix) {
		cd = drives[ix];
		if (cd != null && cd.driveFd != null) {
			synchronized(this) {
			cmdBuf[adr_Status_c] |= sts_Ready_c;
			cmdBuf[adr_Status_c] |= sts_DSC_c;
			}
		} else {
			synchronized(this) {
			cmdBuf[adr_Status_c] &= ~sts_Ready_c;
			cmdBuf[adr_Status_c] &= ~sts_DSC_c;
			}
		}
	}

	private void putData(int val) {
		if (dataIx < dataLength) {
			dataBuf[dataIx++] = (byte)val;
			if (dataIx < dataLength) {
				synchronized(this) {
				cmdBuf[adr_Status_c] |= sts_Drq_c;
				}
			} else {
				processData();
			}
			return;
		}
		setDone();
	}

	private void incLBA() {
		++cmdBuf[adr_Sector_c];
		if (cmdBuf[adr_Sector_c] != 0) return;
		++cmdBuf[adr_CylLo_c];
		if (cmdBuf[adr_CylLo_c] != 0) return;
		++cmdBuf[adr_CylHi_c];
		if (cmdBuf[adr_CylHi_c] != 0) return;
		cmdBuf[adr_Head_c] = (byte)((cmdBuf[adr_Head_c] & 0xf0) |
			((cmdBuf[adr_Head_c] + 1) & 0x0f));
		// TODO: error if carry?
	}

	private void incCHS() {
		++cmdBuf[adr_Sector_c];
		if (cmdBuf[adr_Sector_c] < cd.nSec) return;
		cmdBuf[adr_Head_c] = (byte)((cmdBuf[adr_Head_c] & 0xf0) |
			((cmdBuf[adr_Head_c] + 1) & 0x0f));
		if (getHead() < cd.nHead) return;
		++cmdBuf[adr_CylLo_c];
		if (cmdBuf[adr_CylLo_c] != 0) return;
		++cmdBuf[adr_CylHi_c];
		// TODO: error if carry?
	}

	private void startData() {
		synchronized(this) {
		cmdBuf[adr_Status_c] &= ~sts_Busy_c;
		}
		getData();
	}

	private void getData() {
		if (dataIx < dataLength) {
			cmdBuf[adr_Data_c] = dataBuf[dataIx++];
			synchronized(this) {
			cmdBuf[adr_Status_c] |= sts_Drq_c;
			}
			return;
		}
		dataIx = 0;
		if (curCmd == cmd_IdentDrv_c) {
			setDone();
			return;
		}
		if (cmdBuf[adr_SecCnt_c] > 0) {
			--cmdBuf[adr_SecCnt_c];
			if (isLBA()) {
				incLBA();
			} else {
				incCHS();
			}
		}
		if (cmdBuf[adr_SecCnt_c] > 0) {
			processCmd();
			return;
		}
		setDone();
	}

	private void setDone() {
		synchronized(this) {
		cmdBuf[adr_Status_c] &= ~sts_Drq_c;
		cmdBuf[adr_Status_c] &= ~sts_Busy_c;
		}
	}

	private void setError(int err) {
		synchronized(this) {
		cmdBuf[adr_Error_c] |= (byte)err;
		cmdBuf[adr_Status_c] |= sts_Error_c;
		//setDone();
		cmdBuf[adr_Status_c] &= ~sts_Drq_c;
		cmdBuf[adr_Status_c] &= ~sts_Busy_c;
		}
	}

	private void dumpIO(String op, long off) {
		System.err.format("%s at %d (%d %d %d %d):",
			op, off, getLUN(), getCyl(), getHead(), getSec());
		for (int x = 0; x < 16; ++x) {
			System.err.format(" %02x", dataBuf[x]);
		}
		System.err.format("\n");
	}

	// 'idx' is *word* index...
	private void setWord(byte[] buf, int idx, int val) {
		idx <<= 1;	// byte index...
		buf[idx + 0] = (byte)val;
		buf[idx + 1] = (byte)(val >> 8);
	}

	private void identifyDrive() {
		dataIx = 0;
		Arrays.fill(dataBuf, (byte)0);
		setWord(dataBuf, 0, 0x848a);
		setWord(dataBuf, 1, cd.defCyl);
		setWord(dataBuf, 3, cd.defHead);
		setWord(dataBuf, 6, cd.defSec);
		setWord(dataBuf, 7, (int)(cd.defLBA >> 16));	// "vendor specific"...
		setWord(dataBuf, 8, (int)cd.defLBA);		// "vendor specific"...
		// drive s/n words 10-19...
		setWord(dataBuf, 22, 4);	// ECC bytes
		// f/w rev words 23-26...
		// model num/name words 27-46...
		// R/W multiple max word 47... supported?
		//setWord(dataBuf, 49, (int)0);	// capabilities...
		//setWord(dataBuf, 51, (int)0);	// PIO timing...
		//setWord(dataBuf, 53, (int)0);	// field validity...
		setWord(dataBuf, 54, cd.nCyl);
		setWord(dataBuf, 55, cd.nHead);
		setWord(dataBuf, 56, cd.nSec);
		setWord(dataBuf, 57, (int)cd.nLBA);
		setWord(dataBuf, 58, (int)(cd.nLBA >> 16));
		//setWord(dataBuf, 59, 0);	// multiple sector setting (??)
		setWord(dataBuf, 60, (int)cd.defLBA);
		setWord(dataBuf, 61, (int)(cd.defLBA >> 16));
		setWord(dataBuf, 80, 12);	// ATA-3 X3T13 2008D revision 7
		dataLength = driveSecLen;
		startData(); // prime first byte
	}

	private void queueCmd(int cmd) {
		cmds.add(cmd);
	}

	private void processCmd() {
		long off;
		long e;

		if (cd == null || cd.driveFd == null) {
			setError(err_ABRT_c);
			return;
		}
		switch (curCmd) {
		case cmd_Recal_c:
		case cmd_Seek_c:
			setDone();
			break;
		case cmd_IdentDrv_c:
			identifyDrive();
			break;
		case cmd_Features_c:
			// sub-command == adr_Feature_c,
			// params = adr_SecCnt_c, adr_Sector_c,
			//		adr_CylLo_c, adr_CylHi_c
			setDone();
			break;
		case cmd_SetParam_c:
			// TODO: allowed for CF?
			cd.nHead = getHead() + 1;
			cd.nSec = getSec();
			cd.nCyl = (int)(cd.capacity / driveSecLen /
				cd.nSec / cd.nHead);
			if (cd.nCyl > 65535) {
				cd.nCyl = 65535;
			}
			cd.nLBA = cd.nCyl * cd.nHead * cd.nSec;
			setDone();
			break;
		case cmd_Read_c:
		case cmd_ReadRetry_c:
			off = getOff();
			if (off >= cd.capacity) {
				setError(err_ABRT_c);
				break;
			}
			try {
				cd.driveFd.seek(off);
				// dataBuf includes (fake) ECC, must limit read()...
				e = cd.driveFd.read(dataBuf, 0, driveSecLen);
				if (e != driveSecLen) {
					setError(err_IDNF_c);
					break;
				}
			} catch (Exception ee) {
				setError(err_IDNF_c);
				break;
			}
			dataLength = driveSecLen;
			dataIx = 0;
			startData(); // prime first byte
			break;
		case cmd_Write_c:
		case cmd_WriteV_c:
		case cmd_WriteRetry_c:
			// Prepare for command... but must wait for data...
			wrOff = getOff();
			if (wrOff >= cd.capacity) {
				setError(err_ABRT_c);
				break;
			}
			dataLength = driveSecLen;
// TODO: support other commands?
//			if ((curCmd & cmd_Long_c) != 0) {
//				dataLength += 4;
//			}
			dataIx = 0;
			synchronized(this) {
			cmdBuf[adr_Status_c] |= sts_Drq_c;
			cmdBuf[adr_Status_c] &= ~sts_Busy_c;
			}
			break;
		default:
			setError(err_ABRT_c);
			break;
		}

	}

	private void processData() {
		long off;
		long e;

		switch (curCmd) {
		case cmd_Write_c:
		case cmd_WriteV_c:
		case cmd_WriteRetry_c:
			//off = getOff();
			off = wrOff;
			try {
				cd.driveFd.seek(off);
				// dataBuf includes (fake) ECC, must limit write()...
				cd.driveFd.write(dataBuf, 0, driveSecLen);
			} catch (Exception ee) {
				setError(err_IDNF_c);
				break;
			}
			dataIx = 0;
			if (cmdBuf[adr_SecCnt_c] > 0) {
				--cmdBuf[adr_SecCnt_c];
				wrOff += driveSecLen;
				if (isLBA()) {
					incLBA();
				} else {
					incCHS();
				}
			}
			if (cmdBuf[adr_SecCnt_c] > 0) {
				if (wrOff >= cd.capacity) {
					setError(err_ABRT_c);
					break;
				}
				synchronized(this) {
				cmdBuf[adr_Status_c] |= sts_Drq_c;
				}
				break;
			}
			setDone();
			break;
		default:
			break;
		}
	}

	public void run() {
		while (true) {
			int cmd = -1;
			try {
				cmd = cmds.take();
			} catch (Exception ee) {}
			if (cmd < 0) {
				System.err.format("%s thread exit!\n", name);
				break;
			}
			if (resetting) {
				continue;
			}
			try {
				Thread.sleep(1);
			} catch (Exception ee) {}
			if (resetting) {
				continue;
			}
			curCmd = (byte)cmd;
			processCmd();
		}
	}

	public String getDeviceName() { return name; }

	public String dumpDebug() {
		String ret = String.format(
			"[0] data    %02x\n" +
			"[1] error   %02x  %02x (feature)\n" +
			"[2] sec cnt %02x\n" +
			"[3] sector  %02x\n" +
			"[4] cyl lo  %02x\n" +
			"[5] cyl hi  %02x\n" +
			"[6] S/D/H   %02x\n" +
			"[7] status  %02x  %02x (cmd) (%d)\n" +
			"data index = %d  SRST = %s\n",
			cmdBuf[adr_Data_c] & 0xff,
			cmdBuf[adr_Error_c] & 0xff, feat & 0xff,
			cmdBuf[adr_SecCnt_c] & 0xff,
			cmdBuf[adr_Sector_c] & 0xff,
			cmdBuf[adr_CylLo_c] & 0xff,
			cmdBuf[adr_CylHi_c] & 0xff,
			cmdBuf[adr_Head_c] & 0xff,
			cmdBuf[adr_Status_c] & 0xff, curCmd & 0xff, cmds.size(),
			dataIx, resetting);
		return ret;
	}
}
