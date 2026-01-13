# Red Sudoku Solver

Status: **DEFERRED**

The Red language solver is currently deferred due to toolchain limitations on ARM64 architecture.

## Reason for Deferral

Red is currently in alpha and primarily supports 32-bit x86 (i386) on Linux. The benchmark environment is running on Ubuntu 24.04 ARM64. 

To run Red on 64-bit Linux, it typically requires 32-bit user-space libraries (`libc6:i386`, `libcurl4:i386`). However, these packages are not available in the Ubuntu ports repository for ARM64 architecture, making it impossible to install the necessary 32-bit support in the standard Docker container on ARM64 hosts (such as Apple Silicon Macs).

## Future Work

- Revisit when Red provides native ARM64 support or a 64-bit Linux binary.
- Explore using `qemu-user` for 32-bit x86 emulation if performance overhead is acceptable.
