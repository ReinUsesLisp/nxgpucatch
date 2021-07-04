## nxgpucatch
Test suite using misc features of a Nintendo Switch GPU.  
Useful for breaking emulators.

## How to build
First download through pacman the most recent packages for homebrew
Switch development. Then execute the following commands from a
Unix-like enviornment.
```sh
mkdir build
cd build
/opt/devkitpro/portlibs/switch/bin/aarch64-none-elf-cmake -G Ninja ..
ninja
# Press Y on hbmenu and finally
nxlink nxgpucatch.nro
```
