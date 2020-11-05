**nxiload by Ben Daluz 2020**
-----------------------------
nxiload loads a Spectrum Next Layer 2 .nxi image file and displays it on screen

nxiload is compatible with nxi files of size 49,152 bytes (pixel data only, no palette)
using the standard Next palette and also nxi files of size 49664 bytes (pixel data prepended
with 512 bytes of palette data in %RRRGGGBB, %P000000B format) including a 9 bit custom palette
from the available 512 colours

**Installation**
-----------------------------
Copy /build/nxiload to /dot/nxiload on your Spectrum Next SD card

**Browser Integration**
-----------------------------
Run the included install.bas to set up file association for .nxi files in the Spectrum Next browser

**OR**
run the following command at the Spectrum Next command line after copying nxiload to /dot on your SD card

.associate -a NXI "<.nxiload |"

Once done, this will enable you to open .nxi files from the browser and nxiload will display the image

**Source**
-----------------------------
The included source can be built with sdcc using the included mnxi.py build script which utilises
Sol's Stupidly Simple Build System v.2.1.

mnxi.py calls c.bat lastly to copy the binaries to a target location. This can either be removed or
updated for your purposes