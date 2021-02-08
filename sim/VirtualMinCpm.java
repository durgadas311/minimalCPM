// Copyright 2021 Douglas Miller <durgadas311@gmail.com>

import java.util.Properties;
import java.io.*;

public class VirtualMinCpm {
	static MinimalCPM vm;

	public static void main(String[] args) {
		Properties props = new Properties();
		String config = null;
		if (args.length > 0) {
			File f = new File(args[0]);
			if (f.exists()) {
				config = f.getAbsolutePath();
			}
		}
		if (config == null) {
			config = System.getenv("MINCPM_CONFIG");
			if (config == null) {
				config = "vmincpmrc";
				File f = new File(config);
				if (f.exists()) {
					config = f.getAbsolutePath();
				} else {
					config = System.getProperty("user.home") + "/." + config;
				}
			}
		}
		if (config != null) {
			try {
				FileInputStream cfg = new FileInputStream(config);
				props.setProperty("configuration", config);
				props.load(cfg);
				cfg.close();
			} catch(Exception ee) {
				config = null;
			}
		} else {
			props.setProperty("asci0_att", "StdioSerial");
			props.setProperty("ram_size", "64k");
			props.setProperty("rom_size", "8k");
		}
		vm = new MinimalCPM(props);
		vm.start();
	}
}
