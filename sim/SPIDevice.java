// Copyright (c) 2018 Douglas Miller <durgadas311@gmail.com>

public interface SPIDevice {
	void reset();
	int miso_();	// without shift
	int miso();	// with shift
	void mosi(int value);	// with shift
	void sclk(int bits);
	boolean status();
	void scs(boolean on);
	String dumpDebug();
}
