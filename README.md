# Deriv-hs Benchmark

This is Haskell implementation of [Jon Harro]()'s [symbolic derivative benchmark](https://flyingfrogblog.blogspot.com/2017/12/does-reference-counting-really-use-less.html).
It's almost straight forward reimplementation of [OCaml](https://gist.github.com/jdh30/f3d90a65a7abc7c9faf5c0299b002db3)
version for Haskell. No fancy polymorphism, no Haskell specific optimizations.

## Benchmark Comparison with OCaml & Swift

Benchmark was done on my desktop Ryzen 1800x system with 32GB DDR4 2133MHz RAM running NixOS Linux.

```
$ lscpu
Architecture:        x86_64
CPU op-mode(s):      32-bit, 64-bit
Byte Order:          Little Endian
CPU(s):              16
On-line CPU(s) list: 0-15
Thread(s) per core:  2
Core(s) per socket:  8
Socket(s):           1
NUMA node(s):        1
Vendor ID:           AuthenticAMD
CPU family:          23
Model:               1
Model name:          AMD Ryzen 7 1800X Eight-Core Processor
Stepping:            1
CPU MHz:             3107.989
CPU max MHz:         3600.0000
CPU min MHz:         2200.0000
BogoMIPS:            7199.50
Virtualization:      AMD-V
L1d cache:           32K
L1i cache:           64K
L2 cache:            512K
L3 cache:            8192K
NUMA node0 CPU(s):   0-15
Flags:               fpu vme de pse tsc msr pae mce cx8 apic sep mtrr pge mca cmov pat pse36 clflush mmx fxsr sse sse2 ht syscall nx mmxext fxsr_opt pdpe1gb rdtscp lm constant_tsc rep_good nopl nonstop_tsc cpuid extd_apicid aperfmperf pni pclmulqdq monitor ssse3 fma cx16 sse4_1 sse4_2 movbe popcnt aes xsave avx f16c rdrand lahf_lm cmp_legacy svm extapic cr8_legacy abm sse4a misalignsse 3dnowprefetch osvw skinit wdt tce topoext perfctr_core perfctr_nb bpext perfctr_llc mwaitx cpb hw_pstate sme ssbd vmmcall fsgsbase bmi1 avx2 smep bmi2 rdseed adx smap clflushopt sha_ni xsaveopt xsavec xgetbv1 xsaves clzero irperf xsaveerptr arat npt lbrv svm_lock nrip_save tsc_scale vmcb_clean flushbyasid decodeassists pausefilter pfthreshold avic v_vmsave_vmload vgif overflow_recov succor smca

$ sudo lshw -short -C memory
H/W path                  Device           Class       Description
==================================================================
/0/0                                       memory      64KiB BIOS
/0/28                                      memory      32GiB System Memory
/0/28/0                                    memory      8GiB DIMM DDR4 Synchronous Unbuffered (Unregistered) 2133 MHz (0.5 ns)
/0/28/1                                    memory      8GiB DIMM DDR4 Synchronous Unbuffered (Unregistered) 2133 MHz (0.5 ns)
/0/28/2                                    memory      8GiB DIMM DDR4 Synchronous Unbuffered (Unregistered) 2133 MHz (0.5 ns)
/0/28/3                                    memory      8GiB DIMM DDR4 Synchronous Unbuffered (Unregistered) 2133 MHz (0.5 ns)
/0/2a                                      memory      768KiB L1 cache
/0/2b                                      memory      4MiB L2 cache
/0/2c                                      memory      16MiB L3 cache
```

Measurements are just simple output `time -v` with 10 nesting argument passed to binary via command line.

### Haskell

Compiled via GHC (without cabal or stack) with `-O2` optimization flag.

```
	Command being timed: "./Main 10"
	User time (seconds): 1.73
	System time (seconds): 0.11
	Percent of CPU this job got: 100%
	Elapsed (wall clock) time (h:mm:ss or m:ss): 0:01.84
	Average shared text size (kbytes): 0
	Average unshared data size (kbytes): 0
	Average stack size (kbytes): 0
	Average total size (kbytes): 0
	Maximum resident set size (kbytes): 432596
	Average resident set size (kbytes): 0
	Major (requiring I/O) page faults: 0
	Minor (reclaiming a frame) page faults: 107393
	Voluntary context switches: 1
	Involuntary context switches: 2
	Swaps: 0
	File system inputs: 0
	File system outputs: 0
	Socket messages sent: 0
	Socket messages received: 0
	Signals delivered: 0
	Page size (bytes): 4096
	Exit status: 0
```

### OCaml

Compiled using `ocamlopt` with `-O3` optimization flag.

```
	Command being timed: "./result/bin/deriv 10"
	User time (seconds): 1.51
	System time (seconds): 0.07
	Percent of CPU this job got: 99%
	Elapsed (wall clock) time (h:mm:ss or m:ss): 0:01.59
	Average shared text size (kbytes): 0
	Average unshared data size (kbytes): 0
	Average stack size (kbytes): 0
	Average total size (kbytes): 0
	Maximum resident set size (kbytes): 432756
	Average resident set size (kbytes): 0
	Major (requiring I/O) page faults: 0
	Minor (reclaiming a frame) page faults: 108169
	Voluntary context switches: 1
	Involuntary context switches: 2
	Swaps: 0
	File system inputs: 0
	File system outputs: 0
	Socket messages sent: 0
	Socket messages received: 0
	Signals delivered: 0
	Page size (bytes): 4096
	Exit status: 0
```

### Swift

Swift implementation is compiled using `swiftc` with `-O` optimization flag.

```
	Command being timed: "result/bin/deriv 10"
	User time (seconds): 6.51
	System time (seconds): 0.31
	Percent of CPU this job got: 99%
	Elapsed (wall clock) time (h:mm:ss or m:ss): 0:06.82
	Average shared text size (kbytes): 0
	Average unshared data size (kbytes): 0
	Average stack size (kbytes): 0
	Average total size (kbytes): 0
	Maximum resident set size (kbytes): 1574552
	Average resident set size (kbytes): 0
	Major (requiring I/O) page faults: 0
	Minor (reclaiming a frame) page faults: 391662
	Voluntary context switches: 1
	Involuntary context switches: 4
	Swaps: 0
	File system inputs: 0
	File system outputs: 0
	Socket messages sent: 0
	Socket messages received: 0
	Signals delivered: 0
	Page size (bytes): 4096
	Exit status: 0
```
