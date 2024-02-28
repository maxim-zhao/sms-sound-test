WLAZ80 = wla-z80.exe
WLALINK = wlalink.exe
BMP2TILE = bmp2tile.exe

# These are targets that aren't files
.PHONY: all clean default

# Disable built-in rules
.SUFFIXES:

default: sms-sound-test.sms

# WLA DX will tell us what files we need
auto.makefile: sms-sound-test.sms.asm
	$(WLAZ80) -M -t -o sms-sound-test.sms.o sms-sound-test.sms.asm > $@

-include auto.makefile

# We want to build the resources automatically
%: %.png
	$(BMP2TILE) $< -noremovedupes -savetiles $@

# And then we build it
%.o: 
	$(WLAZ80) -o $@ $<

sms-sound-test.sms: sms-sound-test.sms.o
	echo [objects] > linkfile
	echo $< >> linkfile
	$(WLALINK) -d -r -v -S linkfile $@

clean:
	del *.1bpp
	del sms-sound-test.sms
	del sms-sound-test.sms.o
	del auto.makefile