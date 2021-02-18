// Copyright (c) 2021 Douglas Miller <durgadas311@gmail.com>

public interface BaseSystem {
	int cpuClock();	// CPU speed in Hz - can't exceed 'int'
	int extClock1(); // CKA0 clock in Hz
	int extClock2(); // CKA1 clock in Hz
	int extClock3(); // CKS clock in Hz
}
