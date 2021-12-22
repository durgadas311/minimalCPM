// Copyright (c) 2018 Douglas Miller <durgadas311@gmail.com>

import java.util.Arrays;
import java.util.Properties;
import java.util.Vector;
import java.awt.*;
import java.awt.event.*;
import java.io.*;
import javax.swing.*;

// Init (power-on) sequence/protocol
// 1. wait 1mS after power-ok
// 2. (/SCS deasserted) send 8 bytes FF (send >= 74 clock cycles)
// 3. issue CMD0, if not IDLE repeat
// 4. issue CMD8, arg 0x000001aa (2.7-3.6V, test pattern "AA")
//    4.1. if ILL_CMD card=SD1
//    4.2. else receive 4 bytes,
//         4.2.1. if last bytes == 0x01aa card=SD2
//         4.2.2. else card invalid
// 5. issue ACMD41, with arg 0x40000000 (HCS) if SD2
//    5.1. if not READY, repeat (5)
// 6. if SD2, issue CMD58 and receive +4 bytes
//    6.1. if resp & 0xc0000000 == 0xc0000000, type=SDHC (power on, HCS)

// General issue command (get response) protocol:
//
// 1. assert /SCS
// 2. send FF until received FF (not busy)
// 3. send command (6 bytes)
// 4. send FF until received 0xxxxxxx (response)
//    4.1. if CMD58 or CMD8, receive 4 bytes

// Read/multi protocol:
// 1. issue command, get response
// 2. get error (0000xxxx) or [11111110], data block(512), [crc1],[crc2]
// 4. if single, deassert /SCS; done.
// 5. else offset+=512, read(), goto (2)

// Write/multi protocol:
// 1. issue command, get response
// 2. send [11111110], data block(512), [crc1],[crc2]
// 3. get data response [0000xxx1]
// 4. if single, deassert /SCS; done. 
// 5. else offset+=512, write(), goto (2)

// Multi-block termination:
// 1. issue CMD12 (STOP),

// SDSC = 8M-2G
// SDHC = 4G-32G
// SDXC = 64G-2T (not supported here)
// SDUC = 4T-128T (not supported here) - exceeds CSDv2

public class SDCard implements SPIDevice, TimeListener {

	// initialization command to get into SPI mode... (sts_Idle_c)
	public static final byte[] cmp_CMD0 = new byte[]
		{ 0b01000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, (byte)0b10010101 };
	// Older cards (< SD 2.0) respond "sts_IllCmd_c" to this:
	public static final byte[] cmp_CMD8 = new byte[]
		// VHS 2.7-3.6V, pattern AA
		{ 0b01001000, 0b00000000, 0b00000000, 0b00000001, (byte)0b10101010, (byte)0b10000111 };
	// Read OCR. May respond sts_IllCmd_c (???)
	public static final byte[] cmp_CMD58 = new byte[]
		{ 0b01111010, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b01110101 };

	// Standard response bits
	public static final int sts_Idle_c   = 0x0100;
	public static final int sts_EraRes_c = 0x0200;
	public static final int sts_IllCmd_c = 0x0400;
	public static final int sts_CRC_c    = 0x0800;
	public static final int sts_EraSeq_c = 0x1000;
	public static final int sts_Adr_c    = 0x2000;
	public static final int sts_Param_c  = 0x4000;
	// R2 status bits, plus above are shifted left 8...
	public static final int sts_Locked_c = 0x0001;	// card is locked
	public static final int sts_Failed_c = 0x0002;	// era | [un]lock failed
	public static final int sts_Error_c  = 0x0004;	// unspecified
	public static final int sts_CCErr_c  = 0x0008;	// Card Controller failed
	public static final int sts_ECCErr_c = 0x0010;	// Card ECC failed
	public static final int sts_WPErr_c  = 0x0020;	// Write Protect
	public static final int sts_EraPrm_c = 0x0040;	// Erase Parameter
	public static final int sts_Range_c  = 0x0080;	// out of range | csd ovrw

	// read data packet error bits
	public static final int rpk_Error_c  = 0x01;
	public static final int rpk_CCErr_c  = 0x02;
	public static final int rpk_ECCErr_c = 0x04;
	public static final int rpk_Range_c  = 0x08;
	public static final int rpk_OK_c     = 0b11111110; // followed by 512+2

	// write data response error bits
	public static final int wpk_OK_c     = 0b0000010100000000;
	public static final int wpk_CRCErr_c = 0b0000101100000000;
	public static final int wpk_Error_c  = 0b0000110100000000;

	// write data packet bits - followed by 512+2 (except stop)
	public static final int wpk_Single_c = 0b11111110; // rpk_OK_c
	public static final int wpk_Multi_c  = 0b11111100;
	public static final int wpk_Stop_c   = 0b11111101; // no data follows

	// SD Command codes
	static final byte cmd_GoIdle_c = (byte)0;
	static final byte cmd_Init_c = (byte)1;
	static final byte cmd_CheckV_c = (byte)8;
	static final byte cmd_ReadCSD_c = (byte)9;
	static final byte cmd_ReadCID_c = (byte)10;
	static final byte cmd_Stop_c = (byte)12;
	static final byte cmd_BlkLen_c = (byte)16;
	static final byte cmd_Read_c = (byte)17;
	static final byte cmd_ReadMulti_c = (byte)18;
	static final byte cmd_BlkCnt_c = (byte)23;
	static final byte cmd_Write_c = (byte)24;
	static final byte cmd_WriteMulti_c = (byte)25;
	static final byte cmd_Meta_c = (byte)55;
	static final byte cmd_ReadOCR_c = (byte)58;

	// Aux commands (prefixed by cmd_Meta_c)
	static final byte acmd_Status_c = (byte)13;
	static final byte acmd_WrCnt_c = (byte)22;	// TODO: num blks written OK
	static final byte acmd_EraCnt_c = (byte)23;
	static final byte acmd_Init_c = (byte)41;
	static final byte acmd_ReadSCR_c = (byte)51;


	private enum State {
		IDLE,
		COMMAND,
		DATA_IN,	// data into card, i.e. write to storage
		DATA_OUT,	// data out of card, i.e. read storage
		RESPONSE,
		DONE,		// requires /SCS off to exit
	};
	private State curState = State.IDLE;

	private RandomAccessFile driveFd;
	private int driveSecLen;
	private int sectorsPerTrack;
	private long capacity;
	private int driveCode;
	private int driveCnum;
	private String driveMedia;
	private int driveType;
	private long mediaSpt;
	private long mediaSsz;
	private long mediaCyl;
	private long mediaHead;
	private long mediaLat;

	// mode COMMAND
	private byte[] cmdBuf = new byte[6];	// command, [47:0]
	private int cmdIx;
	private int curCmd;
	private byte[] rspBuf = new byte[20];	// response, [7:0] opt: [31:0]
	private int rspIx;
	private int rspLen;	// mostly just "1"
	private int rspCnt;
	// mode DATA_IN/DATA_OUT
	private byte[] dataBuf;
	private int dataLength;
	private int dataIx;
	private int dataCnt;
	private int bsyCnt;
	private long rwOff;
	private int adr;
	private int ctl;
	private int spi;
	private int len;
	private int _miso;
	private int _mosi;

	// Timing for "power on"
	private long dead = 1000000; // nS after power-on that we're dead
	private long initCnt = 0;
	private int init = 74;

	// Registers
	private int sts;	//
	private int ocr;	// [31:0] operating conditions register
	private	byte[] cid;	// TODO: [127:0] card ID (UUID)
	private	int rca;	// [15:0] relative card address - n/u in SPI
	private	int dsr;	// [15:0] driver stage register
	private	byte[] csd;	// [127:0] card-specific data
	private	byte[] scr;	// [32:0] SD config register
	private	int csr;	// [32:0] card status register
	private	byte[] ssr;	// [511:0] SD status register
	private boolean sdc2;	// SDC v2?
	private boolean sdhc;	// SDHC
	private boolean idle;	// in pre-init phase?
	private boolean aux;	// CMD55 seen?

	private String name;
	private int index;
	private File dir;
	private JPanel pnl;

	public SDCard(Properties props, String id, int ix, int irq, Interruptor intr) {
		driveFd = null;
		driveSecLen = 0;
		sectorsPerTrack = 0;
		capacity = 0;
		driveCode = 0;
		driveCnum = 0;
		driveMedia = null;
		driveType = 0;
		dataBuf = null;
		dataLength = 0;
		dataIx = 0;

		intr.addTimeListener(this);
		dir = new File(System.getProperty("user.dir"));
		name = id;
		index = ix;
		cid = new byte[16];
		csd = new byte[16];
		scr = new byte[8];
		// SCR is fixed/constant
		scr[0] = (byte)0x02;	// structure version
		scr[1] = (byte)0x81;	// era hi, security, 1-bit bus
		scr[2] = (byte)0x00;
		scr[3] = (byte)0x0c;
		scr[4] = (byte)0x00;
		scr[5] = (byte)0x00;
		scr[6] = (byte)0x00;
		scr[7] = (byte)0x00;
		String media = null;
		capacity = 64*1024*1024;	// default 64M
		String s = props.getProperty(String.format("sd_card%d", ix));
		if (s != null) {
			// path [capacity [label]]
			String[] ss = s.split("\\s", 3);
			media = ss[0];
			if (ss.length > 1 && ss[1].matches("[0-9]+[MmGg]")) {
				int i = ss[1].length() - 1;
				char f = Character.toUpperCase(ss[1].charAt(i));
				capacity = Integer.valueOf(ss[1].substring(0, i)) *
					1024 * 1024;	// Megabytes at least...
				if (f == 'G') {
					capacity *= 1024;
				}
			}
			if (ss.length > 2) {
				// Front panel name, not media name
				name = ss[2];
			}
		}
		driveSecLen = 512;

		dataBuf = new byte[driveSecLen + 3];	// space for response and CRC
		dataLength = driveSecLen;

		if (media != null) {
			insertCard(new File(media));
		} else {
			insertCard(null);
		}
	}

	private void insertCard(File media) {
		// TODO: need to set capacity based on card...
		if (driveFd != null) {
			try {
				driveFd.close();
			} catch (Exception ee) {}
			driveFd = null;
		}
		if (media == null) {
			if (pnl != null) {
				pnl.setToolTipText("EMPTY");
			}
			return;
		}
		RandomAccessFile fd;
		try {
			fd = new RandomAccessFile(media, "rw");
			// TODO: allow/create header?
			if (fd.length() == 0) {
				// special case: new media - initialize it.
				System.err.format("Initializing new media %s as %dM\n",
							media, capacity / 1024 / 1024);
				fd.setLength(capacity);
				setHeader(fd, capacity);
			} else {
				capacity = fd.length() & ~127;
				System.err.format("Mounted existing media %s as %dM\n",
							media, capacity / 1024 / 1024);
				// TODO: verify header?
				chkHeader(fd);
			}
		} catch (Exception ee) {
			System.err.format("SDCard: Unable to open media %s\n", media);
			driveFd = null;
			if (pnl != null) {
				pnl.setToolTipText("EMPTY");
			}
			return;
		}
		if (pnl != null) {
			pnl.setToolTipText(media.getName());
		}
		driveMedia = media.getAbsolutePath();
		driveFd = fd;
		// literally a power-on...
		dead = 1000000;	// 1mS
		init = 74;
		internalReset();
	}

	private void setHeader(RandomAccessFile fd, long cap) throws Exception {
		// Append "header" to end of file...
		// Assumes header not already present!
		byte[] buf = new byte[128];
		Arrays.fill(cid, (byte)0); // TODO: generate CID
		cid[1] = 'Z'; cid[2] = 'Z';
		cid[3] = 'S'; cid[4] = 'I';
		cid[5] = 'M'; cid[6] = 'S'; cid[7] = 'D';
		setCSD(cap);
		// TODO: initialize header...
		// String hdr = String.format("%dc%dh%dz%dp%dl\n", ...
		// byte[] b = hdr.getBytes();
		// System.arraycopy(b, 0, buf, 0, b.length);
		System.arraycopy(csd, 0, buf, 128-32, 16);
		System.arraycopy(cid, 0, buf, 128-16, 16);
		long n = fd.length();
		fd.seek(n);
		fd.write(buf);
	}

	private void chkHeader(RandomAccessFile fd) throws Exception {
		byte[] buf = new byte[128];
		long n = fd.length() - 128;
		fd.seek(n);
		fd.read(buf);
		System.arraycopy(buf, 128-16, cid, 0, 16);
		System.arraycopy(buf, 128-32, csd, 0, 16);
		sdc2 = ((csd[0] & 0xc0) == 0x40);
		sdhc = true; // TODO: get real value
		ocr = 0;
		if (sdhc) {
			ocr |= 0x40000000;
		}
		ocr |= 0x80000000;	// power OK
		ocr |= 0x00ff8000;	// voltage range 2.7-3.6V
	}

	// TODO: what is entailed?
	private void internalReset() {
		// TODO: what gets cleared?
		sts = sts_Idle_c;
		idle = true;
	}

	private void setCSD(long cap) {
		ocr = 0;
		Arrays.fill(csd, (byte)0);
		long capval = (cap >> 9); // num 512B blocks
		if (capval > 4*1024*1024) { // 4M blocks = 2GB
			// SDHC... 4G-32G
			sdhc = true;
			ocr |= 0x40000000;	// SDHC (HCS=1)
			sdc2 = true;
			capval = (capval >> 10) - 1;
			csd[0] = (byte)0x40;	// CSD v2
			csd[1] = 0x0e;
			csd[2] = 0;
			csd[3] = 0x5a;
			csd[4] = 0x5b;
			csd[5] = 0x59;
			csd[6] = 0;
			csd[7] = (byte)(capval >> 16);
			csd[8] = (byte)(capval >> 8);
			csd[9] = (byte)capval;
			csd[10] = 0x7f;
			csd[11] = 0;
			csd[12] = 0x0a;
			csd[13] = 0x40;
			csd[14] = 0;
			csd[15] = (byte)0xff; // unknown CRC
		} else {
			// SDSC... 8M-2G
			sdhc = false;
			// TODO: never use CSDv1?
			////sdc2 = false;
			////int blkz = 9;	// 2^9 == 512
			////int mult = ??;
			////capval = (capval >> (blkz + mult - 7)) - 1
			////csd[0] = (byte)0;	// CSD v1
			////csd[5] = (byte)(blkz << 4);
			////csd[6] = (byte)(capval >> 10);
			////csd[7] = (byte)(capval >> 2);
			////csd[8] = (byte)(capval & 0x03);
			////csd[9] = (byte)(mult << 6);
			////csd[10] = (byte)(mult & 0x01);
			sdc2 = true;
			capval = ((capval >> 10) - 1) & 0x003fffff;
			csd[0] = (byte)0x40;	// CSD v2
			csd[1] = 0x0e;
			csd[2] = 0;
			csd[3] = 0x5a;
			csd[4] = 0x5b;
			csd[5] = 0x59;
			csd[6] = 0;
			csd[7] = (byte)(capval >> 16);
			csd[8] = (byte)(capval >> 8);
			csd[9] = (byte)capval;
			csd[10] = 0x7f;
			csd[11] = 0;
			csd[12] = 0x0a;
			csd[13] = 0x40;
			csd[14] = 0;
			csd[15] = (byte)0xff; // unknown CRC
		}
		// TODO: does this happen later? after 74 clocks? or?
		ocr |= 0x80000000;	// power OK
		ocr |= 0x00ff8000;	// voltage range 2.7-3.6V
	}

	public boolean status() { return (driveFd != null); }

	public int miso_() {
		_miso = _mosi;
		return _miso;
	}

	public int miso() {
		if (init > 0) return 0xff;
		if (spi < 0) return 0xff;
		int val = sdat(0xff);
		return val;
	}

	public void mosi(int val) {
		if (init > 0) return;
		if (spi < 0) return;
		sdat(val);
	}

	private int sdat(int val) {
		// TODO: factor BUSY into 0xff return...
		_miso = _mosi;
		//_mosi = 0xff;	// TODO: FF unless we are reading???
		_mosi = val;
		switch(curState) {
		case IDLE:
			if (bsyCnt > 0) {
				// TODO: need to reject commands, not ignore them...
				_mosi = 0x00; // "DO" is held low
				--bsyCnt;
				break;
			}
			if ((val & 0b11000000) == 0b01000000) {
				cmdIx = 0;
				cmdBuf[cmdIx++] = (byte)val;
				curState = State.COMMAND;
				break;
			}
			_mosi = 0xff; // "DO" is held high
			break;
		case DONE:
			// TODO: FF or response repeated?
			_mosi = 0xff; // "DO" is held high
			if (aux) {
				curState = State.IDLE;
			}
			break;
		case COMMAND:
			cmdBuf[cmdIx++] = (byte)val;
			if (cmdIx >= cmdBuf.length) {
				processCmd(); // must change state...
			}
			break;
		case DATA_IN:
			if (dataIx == 0) {
				if (val == wpk_Single_c) {
					// TODO: assert cmd_Write_c?
				} else if (val == wpk_Multi_c) {
					// TODO: assert cmd_WriteMulti_c?
				} else if (val == wpk_Stop_c) {
					// TODO: response?
					curState = State.DONE;
					break;
				} else {
					// TODO: abort command?
					break;
				}
			}
			dataBuf[dataIx++] = (byte)val;
			if (dataIx >= dataLength) {
				processData(); // must change state...
			}
			break;
		case DATA_OUT:
			if (dataCnt > 0) {
				--dataCnt;
				_mosi = 0xff;
				break;
			}
			_mosi = dataBuf[dataIx++] & 0xff;
			if (dataIx >= dataLength) {
				continueCmd(); // must change state...
			}
			break;
		case RESPONSE:
			if (rspCnt > 0) {
				--rspCnt;
				_mosi = 0xff;
				break;
			}
			_mosi = rspBuf[rspIx++] & 0xff;
			if (rspIx >= rspLen) {
				if (bsyCnt > 0) {
					curState = State.IDLE;
				} else if (curCmd == cmd_WriteMulti_c ||
						curCmd == cmd_Write_c) {
					// For writes, land here for both
					// command response and data response...
					// cmd_Write_c is cleared for data response.
					dataLength = 1 + driveSecLen + 2;
					dataIx = 0;
					curState = State.DATA_IN; // in to card
				} else if (curCmd == cmd_ReadMulti_c ||
						curCmd == cmd_Read_c) {
					continueCmd(); // must change state...
				} else if (curCmd == cmd_ReadCID_c) {
					setDataOut(rpk_OK_c, 1, cid);
				} else if (curCmd == cmd_ReadCSD_c) {
					setDataOut(rpk_OK_c, 1, csd);
				} else if (curCmd == acmd_ReadSCR_c) {
					setDataOut(rpk_OK_c, 1, scr);
				} else {
					curState = State.DONE;
				}
			}
			break;
		}
		return _miso;
	}

	public void sclk(int bits) {
		if (driveFd == null) return; // no card - we're not here
		if (dead <= 0 && init > 0) init -= bits;
	}

	public void scs(boolean on) {
		if (on) { // start xfer
			spi = 0;
			curState = State.IDLE;
			// must permit /SCS drop between cmd55 and aux-cmd
			//aux = false;
		} else { // terminate xfer
			if (curState != State.DONE) {
				System.err.format("/SCS during %s (%02x %02x)\n",
					curState.name(), curCmd, cmdBuf[0]);
			}
			// TODO: more actions to take?
			spi = -1;
		}
	}

	public void reset() {
		// no external RESET
	}

	private int getArg() {
		int arg = (((cmdBuf[1] & 0xff) << 24) |
			((cmdBuf[2] & 0xff) << 16) |
			((cmdBuf[3] & 0xff) << 8) |
			(cmdBuf[4] & 0xff));
		return arg;
	}

	private long getOff() {
		long off = (((cmdBuf[1] & 0xff) << 24) |
			((cmdBuf[2] & 0xff) << 16) |
			((cmdBuf[3] & 0xff) << 8) |
			(cmdBuf[4] & 0xff));
		// SDHC arg is in blocks, SDSC is in bytes...
		if (sdhc) {
			off *= driveSecLen;
		}
		return off;
	}

	private void setResponse(int rsp, int cnt) {
		rspCnt = cnt;	// "Ncr" delay before response appears
		rspLen = rspIx = 0;
		rspBuf[rspLen++] = (byte)(rsp >> 8);
		sts = rsp;
		curState = State.RESPONSE;
	}

	private void setR1b(int rsp, int cnt, int bsy) {
		setResponse(rsp, cnt);
		sts = rsp & 0x0ff; // override
		bsyCnt = bsy;
	}

	private void setR2(int rsp, int cnt) {
		setResponse(rsp, cnt);
		rspBuf[rspLen++] = (byte)rsp;
	}

	private void setResponse(int rsp, int cnt, int arg) {
		setResponse(rsp, cnt);
		rspBuf[rspLen++] = (byte)(arg >> 24);
		rspBuf[rspLen++] = (byte)(arg >> 16);
		rspBuf[rspLen++] = (byte)(arg >> 8);
		rspBuf[rspLen++] = (byte)arg;
	}

	private void setDataOut(int rsp, int cnt) {
		dataBuf[dataLength++] = (byte)rsp;
		dataCnt = cnt;
		curState = State.DATA_OUT;
	}

	// These are only one block...
	private void setDataOut(int rsp, int cnt, byte[] dat) {
		curCmd = 0;
		dataIx = dataLength = 0;
		setDataOut(rsp, cnt);
		int x = 0;
		while (dataLength < dataBuf.length && x < dat.length) {
			dataBuf[dataLength++] = dat[x++];
		}
		dataBuf[dataLength++] = (byte)0xff;	// CRC 1
		dataBuf[dataLength++] = (byte)0xff;	// CRC 2
	}

	private void processCmd() {
		long off;
		long e;

		int rsp = idle ? sts_Idle_c : 0;
		boolean _aux = aux;
		aux = false;
		int cmd = (cmdBuf[0] & 0b00111111);
		switch (cmd) {
		case cmd_GoIdle_c:
			// TODO: verify with cmp_CMD0?
			internalReset();
			setResponse(sts_Idle_c, 1);
			return;
		case cmd_CheckV_c:
			// TODO: verify with cmp_CMD8?
			if (sdc2) {
				// TODO: modify arg before echo?
				setResponse(rsp, 3, getArg());
			} else {
				setResponse(rsp | sts_IllCmd_c, 1);
			}
			return;
		case cmd_ReadOCR_c:
			// TODO: verify with cmp_CMD58?
			// TODO: match CCS with arg HCS?
			setResponse(rsp, 3, ocr);
			return;
		case cmd_Init_c:
			if (idle && initCnt <= 0) {
				initCnt = 100000000; // 100mS
			}
			setResponse(rsp, 1);
			return;
		case cmd_ReadCID_c:
			curCmd = cmd;
			setResponse(rsp, 3);
			return;
		case cmd_ReadCSD_c:
			curCmd = cmd;
			setResponse(rsp, 3);
			return;
		case cmd_Meta_c:
			aux = true; // is this even needed?
			setResponse(rsp, 1);
			return;
		case acmd_Status_c:
			if (!_aux) {
				setResponse(rsp | sts_IllCmd_c, 1);
			} else {
				setR2(sts, 1);
			}
		case acmd_EraCnt_c:
			if (!_aux) {
				setResponse(rsp | sts_IllCmd_c, 1);
			} else {
				// TODO: anything?
				setResponse(rsp, 3);
			}
			return;
		case acmd_Init_c:
			if (!_aux) {
				setResponse(rsp | sts_IllCmd_c, 1);
			} else {
				if (idle && initCnt <= 0) {
					initCnt = 100000000; // 100mS
				}
				setResponse(rsp, 3);
			}
			return;
		case acmd_ReadSCR_c:
			if (!_aux) {
				setResponse(rsp | sts_IllCmd_c, 1);
			} else {
				curCmd = cmd;
				setResponse(rsp, 1);
			}
			return;
		}
		if (idle) {
			setResponse(rsp | sts_IllCmd_c, 1);
			return;
		}
		switch (cmd) {
		case cmd_Stop_c:
			// TODO: what else?
			// cmdBuf[0] is no longer cmd_ReadMulti_c...
			// response type R1b... BUSY condition...
			curState = State.DONE;
			setR1b(rsp, 0, 5); // TODO: make busy async?
			break;
		case cmd_BlkLen_c:
			// TODO: not supported...
			setResponse(rsp, 1);
			break;
		case cmd_ReadMulti_c:
		case cmd_Read_c:
			curCmd = cmd;
			rwOff = getOff();
			if (rwOff >= capacity) {
				setResponse(sts_Param_c, 1);
				curCmd = 0; // cancel command
				break;
			}
			// actual read() done after response
			setResponse(rsp, 7);
			break;
		case cmd_WriteMulti_c:
		case cmd_Write_c:
			curCmd = cmd;
			// Prepare for command... but must wait for data...
			rwOff = getOff();
			setResponse(rsp, 1);
			break;
		default:
			setResponse(rsp | sts_IllCmd_c, 7);
			break;
		}
	}

	// Only called if Read or ReadMulti,
	// after reading and transmitting a block...
	// ...or first (command) response.
	private void continueCmd() {
		dataIx = 0;
		dataLength = 0;
		if (curCmd == 0) {
			curState = State.DONE;
			return;
		}
		int cmd	= (cmdBuf[0] & 0b00111111);
		if (cmd == cmd_Stop_c) {
			curCmd = 0;
			setResponse(0, 1);
			return;
		}
		if (curCmd != cmd_ReadMulti_c) {
			curCmd = 0; // only one block, then done
		}
		if (rwOff >= capacity) {
			setDataOut(rpk_Range_c, 1);
			curCmd = 0; // cancel command
			return;
		}
		try {
			driveFd.seek(rwOff);
			int e = driveFd.read(dataBuf, 1, driveSecLen);
			rwOff += driveSecLen;
			if (e != driveSecLen) {
				setDataOut(rpk_ECCErr_c, 1);
				curCmd = 0; // cancel command
				return;
			}
		} catch (Exception ee) {
			setDataOut(rpk_ECCErr_c, 1);
			curCmd = 0; // cancel command
			return;
		}
		setDataOut(rpk_OK_c, 7);
		dataLength += driveSecLen;
		dataLength += 2; // CRC
	}

	// received full buffer of data for write
	private void processData() {
		long off;
		long e;

		// responses seem to be expected immediately...
		if (rwOff >= capacity) {
			// TODO: is this the correct response?
			setR1b(wpk_Error_c | sts_Range_c, 0, 0);
			curCmd = 0; // TODO: cancel command?
			return;
		}
		if (curCmd != cmd_WriteMulti_c) {
			curCmd = 0; // done after response...
		}
		try {
			driveFd.seek(rwOff);
			// dataBuf includes (fake) CRC, must limit write()...
			driveFd.write(dataBuf, 1, driveSecLen);
			rwOff += driveSecLen;
		} catch (Exception ee) {
			// TODO: cancel command?
			setR1b(wpk_Error_c | sts_ECCErr_c, 0, 0);
			return;
		}
		// While not explicitly stated, description of
		// the Block Write protocol says that a BUSY contition
		// may follow the data response packet.
		setR1b(wpk_OK_c, 0, 10);	// make busy async?
	}

	private void chooseFile() {
	}

	public String getDeviceName() { return name; }

	public void addTime(int nsec) {
		if (dead > 0) dead -= nsec;
		if (initCnt > 0) {
			initCnt -= nsec;
			if (initCnt <= 0 ) {
				idle = false;
			}
		}
	}

	public String dumpDebug() {
		String ret = String.format("SPI %s %d\n", name, index);
		ret += String.format("dead=%d init=%d state=%s sdc2=%s sdhc=%s\n",
			dead, init, curState.name(), sdc2, sdhc);
		ret += String.format("CMD %02x %02x %02x %02x %02x %02x\n",
			cmdBuf[0] & 0xff, cmdBuf[1] & 0xff, cmdBuf[2] & 0xff,
			cmdBuf[3] & 0xff, cmdBuf[4] & 0xff, cmdBuf[5] & 0xff);
		ret += String.format("RSP %02x %02x %02x %02x %02x\n",
			rspBuf[0] & 0xff, rspBuf[1] & 0xff, rspBuf[2] & 0xff,
			rspBuf[3] & 0xff, rspBuf[4] & 0xff);
		ret += String.format("cmnd index = %d [%02x]\n", cmdIx, curCmd & 0xff);
		ret += String.format("data index = %d/%d (%d)\n",
						dataIx, dataLength, dataCnt);
		ret += String.format("resp index = %d/%d (%d, %d) %04x\n",
						rspIx, rspLen, rspCnt, bsyCnt, sts);
		ret += String.format("init count = %d idle=%s\n", initCnt, idle);
		return ret;
	}
}
