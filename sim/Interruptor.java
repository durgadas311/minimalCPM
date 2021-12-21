// Copyright (c) 2021 Douglas Miller <durgadas311@gmail.com>

public interface Interruptor {
	int registerINT(int irq);
	void raiseINT(int irq, int src);
	void lowerINT(int irq, int src);
	void triggerNMI();
	void addClockListener(ClockListener lstn);
	void addTimeListener(TimeListener lstn);
	void waitCPU();
}
