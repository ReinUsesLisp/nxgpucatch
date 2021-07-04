## nxgpucatch
Test suite using misc features of a Nintendo Switch GPU.  
Useful for breaking emulators.

## How to build
First download through pacman the most recent packages for homebrew
Switch development. Then execute the following commands from a
Unix-like environment.
```sh
mkdir build
cd build
/opt/devkitpro/portlibs/switch/bin/aarch64-none-elf-cmake -G Ninja ..
ninja
```
Executing `export DEVKITPRO=/opt/devkitpro` is required if the environment
variable hasn't been set already.

To launch it press Y on hbmenu and execute
```sh
nxlink nxgpucatch.nro
```
