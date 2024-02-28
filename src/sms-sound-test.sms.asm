; This source code was reverse engineered from SMS Sound Test version 1.1, using Emulicious
; And then extended...

.memorymap
slotsize $8000
slot 0 $0000
slotsize $8000
defaultslot 0
.endme
.rombankmap
bankstotal 1
banksize $8000
banks 1
.endro

.enum $c000 export
RAM_VBlankWorkQueueIndex db
RAM_VBlankFlag db
RAM_StackPointerBackup dw
RAM_EffectiveInputs1 db
RAM_EffectiveInputs2 db
RAM_CurrentInputs db
RAM_PreviousInputs db
RAM_RepeatDelay_Up db
RAM_RepeatDelay_Down db
RAM_RepeatDelay_Left db
RAM_RepeatDelay_Right db
RAM_RepeatDelay_Button1 db
RAM_RepeatDelay_Button2 db
RAM_Tone1HalfWavelength dw
RAM_Tone2HalfWavelength dw
RAM_Tone3HalfWavelength dw
RAM_NoiseRate db
RAM_Tone1Attenuation db
RAM_Tone2Attenuation db
RAM_Tone3Attenuation db
RAM_NoiseAttenuation db
RAM_NoiseMode db
RAM_FunctionTablePointer dw
RAM_PauseFlag db
RAM_ScrollerText dw
RAM_ScrollerCounter db
RAM_ScrollerBuffer dsb 32
RAM_ScrollerBufferTerminator db ; set to $ff at all times
RAM_LastPSGModeWrite db
.ende

; ports
.define PORT_PSG $7f
.define PORT_VDPData $be
.define PORT_VDPAddress $bf
.define PORT_VDPRegister $bf

; input ports
.define PORT_VDPStatus $bf
.define PORT_IOPort1 $dc
.define PORT_IOPort2 $dd

.bank 0 slot 0
.org $0000

.asciitable
map ' ' to '~' = $20 ; Normal ASCII
.enda

.section "Boot" force
ResetPoint:
  jp Main
.ends

.section "Main loop" free
MainLoop:
  call WaitForVBlank
  call CheckInputs
  call RunFunctionFromTable
  call UpdateScroller
  ; Check for reset
  in a, (PORT_IOPort2)
  bit 4, a
  jp z, ResetPoint
  call UpdatePSG
  jp MainLoop
.ends

.org $38
.section "Interrupt hander" force
VDPInterruptHandler:
  ex af, af'
  exx
  ; Hander address is left in hl'
  jp (hl)

WaitForVBlank:
  ; Wait for flag to be zero
  ld a, $01
  ld (RAM_VBlankFlag), a
-:ld a, (RAM_VBlankFlag)
  or a
  jr nz, -
  ret
.ends

.org $66
.section "NMI hander" force
PauseHandler:
  push af
    ld a, (RAM_PauseFlag)
    xor 1
    ld (RAM_PauseFlag), a
  pop af
  retn
.ends

.section "Startup" free
Main:
  di
  im 1
  ld sp, $dff0

  ; Set VDP registers
  ld hl, VDPRegisterData
  ld b, _sizeof_VDPRegisterData
  ld c, PORT_VDPRegister
  otir

  ; Blank VRAM
  ; - Set VRAM address to 0
  sub a
  out (PORT_VDPAddress), a
  ld a, $40
  out (PORT_VDPAddress), a
  ; - Then emit $4000 zeroes
  ld c, $40
  sub a
  ld b, a
-:out (PORT_VDPData), a
  djnz -
  dec c
  jr nz, -

  ; Disable sprites
  ld a, $00
  out (PORT_VDPAddress), a
  ld a, $7f
  out (PORT_VDPAddress), a
  ld a, $d0
  out (PORT_VDPData), a

  ; Set font data address
  ld hl, FontData
  ld c, PORT_VDPAddress
  outi
  outi
  ; Then send all the data, padding from 1bpp to 4bpp
  ld de, _sizeof_FontData - 2
  inc d
  ld c, PORT_VDPData
  ld b, e
  sub a
-:outi
  out (c), a
  out (c), a
  out (c), a
  jr nz, -
  dec d
  jr nz, -

  ; Set the palette
  sub a
  out (PORT_VDPAddress), a
  ld a, $c0
  out (PORT_VDPAddress), a
  ld hl, PaletteData
  ld b, $20
  ld c, PORT_VDPData
  otir

  ; Load the tilemap
  ld a, $00
  out (PORT_VDPAddress), a
  ld a, $78
  out (PORT_VDPAddress), a
  ld hl, ScreenText
  ld b, $00 ; Counter for 256
  ld c, PORT_VDPData
  ld d, $06 ; 6 * 256 / 2 bytes get emitted
  ld a, $00 ; High byte
-:outi
  out (c), a
  djnz -
  dec d
  jr nz, -

  call InitPSGVariables
  call InitInputVariables
  call InitPauseAndMute
  call InitScroller
  ; Store VBlank handler in hl'
  ld hl, StartVBlank
  exx
  ld a, $10
  ld (RAM_VBlankWorkQueueIndex), a
  ; Turn on screen
  ld a, $e2
  out (PORT_VDPRegister), a
  ld a, $81
  out (PORT_VDPRegister), a
  ei
  jp MainLoop
.ends

.section "Scroller" free
InitScroller:
  ld hl, ScrollerText
  ld (RAM_ScrollerText), hl
  sub a
  ld (RAM_ScrollerCounter), a
  ld hl, RAM_ScrollerBuffer
  ld b, 32
-:
  ld (hl), a
  inc hl
  djnz -
  ; Write to RAM_ScrollerBufferTerminator
  ld a, $ff
  ld (hl), a
  ret

UpdateScroller:
  ; If we have a counter, decrement it; else get more data
  ld a, (RAM_ScrollerCounter)
  or a
  jr z, @NextData
  dec a
  ld (RAM_ScrollerCounter), a
  ret

@NextData:
  ld hl, (RAM_ScrollerText)
  ld a, (hl)
  inc hl
  ; High bit set means end of string
  bit 7, a
  jr nz, @EndOfString
  ; Regular text
  ld (RAM_ScrollerText), hl
  ; Scroll inside buffer
  ld hl, RAM_ScrollerBuffer+1
  ld de, RAM_ScrollerBuffer
  ld bc, 31
  ldir
  ; Write new char to the end
  ld (de), a
  ; Queue a full draw on the next frame
  ld de, $0016
  call SetVDPAddressToXYInDEInNextVBlank
  ld hl, RAM_ScrollerBuffer
  ld a, $08 ; Secondary palette
  call DrawString
  ret

@EndOfString:
  ; Get next byte
  ld a, (hl)
  or a
  jr z, @EndOfData
  ; Byte is a frame count to wait
  ld (RAM_ScrollerCounter), a
  inc hl
  ld (RAM_ScrollerText), hl
  ret

@EndOfData:
  call InitScroller
  ret
.ends

.section "Drawing routines" free
NibbleToHexAscii:
  ; Input: a = a nibble
  ; Putput: a = the ASCII char for that nibble in hex
  cp 10
  jp m, +
  add a, 'A'-10
  ret

+:
  add a, '0'
  ret

DrawOneDigitHex:
  push de
    ld de, VBlankWork_EmitWord
    call PushDEToVBlankWorkQueue
  pop de
  ld a, d
  and $0F
  call NibbleToHexAscii
  ld d, a
  call PushDEToVBlankWorkQueue
  ret

VBlankWork_EmitWord:
  pop de
  ld c, PORT_VDPData
  out (c), d
  out (c), e
  ret

DrawThreeDigitHex:
  push af
    push de
      ld de, VBlankWork_EmitThreeTilesWithAttributes
      call PushDEToVBlankWorkQueue
    pop bc
    ld a, b ; First digit
    call NibbleToHexAscii
    ld d, a
    ; Second digit
    ld a, c
    srl a
    srl a
    srl a
    srl a
    call NibbleToHexAscii
    ld e, a
    push bc
      call PushDEToVBlankWorkQueue
    pop bc
    ; Third digit
    ld a, c
    and $0f
    call NibbleToHexAscii
    ld d, a
  pop af
  ; Attributes
  ld e, a
  call PushDEToVBlankWorkQueue
  ret

VBlankWork_EmitThreeTilesWithAttributes:
  pop de
  pop hl
  ld c, PORT_VDPData
  out (c), d
  out (c), l
  out (c), e
  out (c), l
  out (c), h
  out (c), l
  ret

SetVDPAddressToXYInDEInNextVBlank:
  push de
    ld de, VBlankWork_SetVDPAddress
    call PushDEToVBlankWorkQueue
  pop de
  ; Compute VRAM address from x,y in d,e
  ld l, e
  ld h, $00
  add hl, hl
  add hl, hl
  add hl, hl
  add hl, hl
  add hl, hl
  add hl, hl
  ld a, d
  add a, a
  add a, l
  ld l, a
  ld de, $7800
  add hl, de
  ex de, hl
  call PushDEToVBlankWorkQueue
  ret

VBlankWork_SetVDPAddress:
  pop hl
  ld c, PORT_VDPAddress
  out (c), l
  out (c), h
  ret

DrawString:
  ; a = high byte for name table
  ; hl = string, terminated by $ff (or anything >127)
  push af
    push hl
      ld de, VBlankWork_WriteString
      call PushDEToVBlankWorkQueue
    pop de
    call PushDEToVBlankWorkQueue
  pop de
  call PushDEToVBlankWorkQueue
  ret

VBlankWork_WriteString:  
  pop hl
  pop de
  ld c, PORT_VDPData
-:
  bit 7, (hl)
  ret nz
  outi
  out (c), d
  jr -
    
StartVBlank:
  in a, (PORT_VDPStatus)
  ld de, EndVBlank
  call PushDEToVBlankWorkQueue
  ld (RAM_StackPointerBackup), sp
  ; Dispatch the queue
  ld sp, $c210
  ret

EndVBlank:
  ; Restore normal stack pointer
  ld hl, (RAM_StackPointerBackup)
  ld sp, hl
  ; Clear alt stack
  ld a, $10
  ld (RAM_VBlankWorkQueueIndex), a
  ; Save entry point for next VBlank
  ld hl, StartVBlank
  sub a
  ld (RAM_VBlankFlag), a
  ex af, af'
  exx
  ei
  reti

PushDEToVBlankWorkQueue:
  ; Point into alt stack area
  ld h, $c2
  ld a, (RAM_VBlankWorkQueueIndex)
  ld l, a
  ; Write de
  ld (hl), e
  inc l
  ld (hl), d
  inc l
  ; Save new pointer
  ld a, l
  ld (RAM_VBlankWorkQueueIndex), a
  ret
.ends

.section "Initialisation data" free
VDPRegisterData:
.db $04, $80
.db $00, $81
.db $ff, $82
.db $ff, $83
.db $ff, $84
.db $ff, $85
.db $00, $86
.db $00, $87
.db $00, $88
.db $00, $89
.db $ff, $8a

PaletteData:
.db $00 $3f $15 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 ; Palette 1: black, white, grey
.db $00 $0f $15 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 ; Palette 1: black, yellow, grey
.ends

.section "Screen text" free
ScreenText:
.ascstr "--------------------------------"
.ascstr "SMS Sound test (v1.2)           "
.ascstr "--------------------------------"
.ascstr "                                "
.ascstr "                                "
.ascstr "  PSG Zero                      "
.ascstr "    Freq: $3FF  -  Vol: $F      "
.ascstr "                                "
.ascstr "  PSG One:                      "
.ascstr "    Freq: $3FF  -  Vol: $F      "
.ascstr "                                "
.ascstr "  PSG Two:                      "
.ascstr "    Freq: $3FF  -  Vol: $F      "
.ascstr "                                "
.ascstr "  PSG Noise:                    "
.ascstr "    Type: White -  Vol: $F      "
.ascstr "    Freq: Low                   "
.ascstr "                                "
.ascstr "                                "
.ascstr "                                "
.ascstr "                                "
.ascstr "--------------------------------"
.ascstr "                                "
.ascstr "--------------------------------"
.ends

.section "Scroller text" free
.function Seconds(x) x*60

ScrollerText:
.ascstr "                                "
.ascstr " Sega Master System Sound Test  ", $ff, Seconds(3)
.ascstr " Version 1.2, February 28, 2024 ", $ff, Seconds(3)
.ascstr "  brought to you by Heliophobe  ", $ff, Seconds(3)
.ascstr "     (mrscreen@hotmail.com)     ", $ff, Seconds(3)
.ascstr "     with updates by Maxim      ", $ff, Seconds(3)
.ascstr "                                "
.ascstr "             Usage:             ", $ff, Seconds(3)
.ascstr "D-Pad: Highlight field to change", $ff, Seconds(3)
.ascstr " Button 1: decrease highlighted ", $ff, Seconds(3)
.ascstr " Button 2: increase highlighted ", $ff, Seconds(3)
.ascstr "Start(GG) or Pause : Toggle mute", $ff, Seconds(3)
.ascstr "       Reset (SMS): Guess!      ", $ff, Seconds(3)
.ascstr "                                "
.ascstr "Values are in native PSG format ", $ff, Seconds(3)
.ascstr " The rest you can figure out... ", $ff, Seconds(3)
.ascstr "Hopefully this may be of use to ", $ff, Seconds(3)
.ascstr "  Emulator authors, SMS coders, ", $ff, Seconds(3)
.ascstr "Hardware tinkerers, ruffians,etc", $ff, Seconds(3)
.ascstr "                                "
.ascstr "     Original release date:     ", $ff, Seconds(2)
.ascstr "      Valentine's Day 2001      ", $ff, Seconds(3)
.ascstr "   from www.kenseiden.com/sms   ", $ff, Seconds(3)
.ascstr "                Acknowledgements", $ff, Seconds(0.5)
.ascstr ".", $ff, Seconds(0.25)
.ascstr ".", $ff, Seconds(0.25)
.ascstr ".", $ff, Seconds(0.25)
.ascstr ".", $ff, Seconds(0.25)
.ascstr ".", $ff, Seconds(0.25)
.ascstr ".", $ff, Seconds(0.25)
.ascstr "                                "
.ascstr "Mike Gordon, testing on real SMS", $ff, Seconds(3)
.ascstr "(PS, never set bit 0 of VDP 0...", $ff, Seconds(3)
.ascstr "......................trust me!)", $ff, Seconds(3)
.ascstr "  Jason Starr for the concept.  ", $ff, Seconds(3)
.ascstr " Ville Helin for his assembler: ", $ff, Seconds(3)
.ascstr " WLA-DX.. much better than TASM ", $ff, Seconds(3)
.ascstr " (www.hut.fi/~vhelin/wla.html)  ", $ff, Seconds(3)
.ascstr "Ricardo Bittencourt for BRSMS...", $ff, Seconds(3)
.ascstr "more specifically, its debugger.", $ff, Seconds(3)
.ascstr "Neon Spiral Injector, for this..", $ff, Seconds(3)
.ascstr "     ...much abused font.       ", $ff, Seconds(3)
.ascstr "Where the hell are you, anyhow? ", $ff, Seconds(3)
.ascstr "Eric Quinn, whose version of the", $ff, Seconds(3)
.ascstr "       SMS ID Tag format        ", $ff, Seconds(3)
.ascstr "      is used in this ROM       ", $ff, Seconds(3)
.ascstr "   All the regulars on S8-Dev   ", $ff, Seconds(3)
.ascstr "  (www.smspower.org/dev/forum)  ", $ff, Seconds(3)
.ascstr "      ...and of course...       ", $ff, Seconds(3)
.ascstr "The obligatory greeting to Zoop!", $ff, Seconds(3)
.ascstr "            Hi Zoop.            ", $ff, Seconds(3)
.ascstr "   With that taken care of...   ", $ff, Seconds(3)
.ascstr "   I can end this scrolltext.   ", $ff, Seconds(3)
.ascstr "                                ", $ff, Seconds(2)
.ascstr "   --Nicolas Warren (Heliophobe)", $ff, Seconds(3)
.ascstr "                                ", $ff, Seconds(3)
.repeat 6
.ascstr "neverputsaltinyoureye"
.endr
.ascstr "wedochickenright"
.repeat 6
.ascstr "neverputsaltinyoureye"
.endr
.ascstr "maybejustanothercuportwoofcoffee"
.ascstr "dontwanttohavetoomuchcoffeeitgetsmeallwoundup"
.ascstr "neverputsaltinyoureye"
.ascstr "putsaltinyoureye"
.ascstr "alwaysputsaltinyoureye", $ff
.db $00 ; End of data
.ends

.section "Font" free
FontData:
.dw $4000
.incbin "font.1bpp" read $7ff ; Data is truncated by one byte
.ends

.section "PSG variables init" free
InitPSGVariables:
  ld hl, $03ff
  ld a, $0f
  ld (RAM_Tone1HalfWavelength), hl
  ld (RAM_Tone2HalfWavelength), hl
  ld (RAM_Tone3HalfWavelength), hl
  ld (RAM_Tone1Attenuation), a
  ld (RAM_Tone2Attenuation), a
  ld (RAM_Tone3Attenuation), a
  ld (RAM_NoiseAttenuation), a
  ld a, $02
  ld (RAM_NoiseRate), a
  ld a, $01
  ld (RAM_NoiseMode), a
  ld hl, FunctionsTable
  ld (RAM_FunctionTablePointer), hl
  ret
.ends

.section "Menu handling" free
RunFunctionFromTable:
  ld ix, (RAM_FunctionTablePointer)
  ; Read function address and RAM address
  ld l, (ix+0)
  ld h, (ix+1)
  ld e, (ix+2)
  ld d, (ix+3)
  ; Jump to function
  jp (hl)

Done:
  ret

; 1st entry of jump table from 177f (indexed by unknown)
HandleThreeDigits:
  push de
    ; Read coordinates
    ld d, (ix+12)
    ld e, (ix+13)
    call SetVDPAddressToXYInDEInNextVBlank
  pop hl
  push hl
    ; Read value from pointed RAM
    ld e, (hl)
    inc hl
    ld d, (hl)

    ; Check for buttons
    ld a, (RAM_EffectiveInputs1)
    bit 4, a
    jr z, +
    ; 1 = decrement
    dec de
+:  ; 2 = increment
    bit 5, a
    jr z, +
    inc de
+:  ; Check for overflow ($04xx)
    ld a, d
    cp $04
    jr nz, +
    ld de, $03ff
+:  ; Check for underflow ($ffxx)
    cp $ff
    jr nz, +
    ld de, $0000
+:
  pop hl
  ; Save to RAM
  ld (hl), e
  inc hl
  ld (hl), d
  ld a, $08 ; Second palette attribute
  ; Draw it
  call DrawThreeDigitHex

  ; Check for directions
  ld a, (RAM_EffectiveInputs2)
  and $0f
  jp z, Done
  call HandleDirection
  or a
  jp z, Done
  ; If we are moving away, re-draw the number in white
  ; Read x,y
  ld d, (ix+12)
  ld e, (ix+13)
  call SetVDPAddressToXYInDEInNextVBlank
  ; Read RAM pointer
  ld l, (ix+2)
  ld h, (ix+3)
  ld e, (hl)
  inc hl
  ld d, (hl)
  ld a, $00
  call DrawThreeDigitHex
  jp Done

HandleNoiseRate:
  push de
    ld d, (ix+12)
    ld e, (ix+13)
    call SetVDPAddressToXYInDEInNextVBlank
  pop hl
  ; Handle buttons changing the value
  ld d, (hl)
  ld a, (RAM_EffectiveInputs2)
  bit 4, a
  jr z, +
  dec d
+:bit 5, a
  jr z, +
  inc d
+:; Clamp to 0..3
  ld a, d
  and $03
  ld d, a
  ld (hl), d
  ld l, d
  ld h, $00
  add hl, hl
  ld de, NoiseDescriptions
  add hl, de
  ld e, (hl)
  inc hl
  ld d, (hl)
  push de
    call HandleDirection
    ld e, $00
    or a
    jr nz, +
    ld e, $08
+:  ld a, e
  pop hl
  ; Queue drawing for next vblank
  call DrawString
  jp Done

HandleNoiseType:
  push de
    ld d, (ix+12)
    ld e, (ix+13)
    call SetVDPAddressToXYInDEInNextVBlank
  pop hl
  ld a, (RAM_EffectiveInputs2)
  and %110000
  jr z, +
  ; Modify value (xor 1 for either button)
  ld a, (hl)
  and $01
  xor $01
  ld (hl), a
+:
  push hl
    call HandleDirection
    ld d, $00
    or a
    jr nz, +
    ld d, $08
+:pop hl
  ; Queue drawing the text for vblank
  ld a, (hl)
  ld hl, _Synch
  or a
  jr z, +
  ld hl, _White
+:
  ld a, d
  call DrawString
  jp Done

_Synch:
.ascstr "Synch",$ff
_White:
.ascstr "White",$ff

HandleSingleDigit:
  push de
    ld d, (ix+12)
    ld e, (ix+13)
    call SetVDPAddressToXYInDEInNextVBlank
  pop hl
  ; Read value
  ld d, (hl)
  ; Modify based on buttons
  ld a, (RAM_EffectiveInputs1)
  bit 4, a
  jr z, +
  dec d
+:bit 5, a
  jr z, +
  inc d
+:; Check for overflow from 0..f
  ld a, $FF
  cp d
  jr nz, +
  ld d, $00
+:ld a, $10
  cp d
  jr nz, +
  ld d, $0F
+:; Save back
  ld (hl), d
  ld e, $08
  call DrawOneDigitHex
  ld a, (RAM_EffectiveInputs2)
  and $0F
  jp z, Done
  call HandleDirection
  or a
  jp z, Done
  ; Redraw in white if leaving
  ld d, (ix+12)
  ld e, (ix+13)
  call SetVDPAddressToXYInDEInNextVBlank
  ld l, (ix+2)
  ld h, (ix+3)
  ld d, (hl)
  ld e, $00
  call DrawOneDigitHex
  jp Done

HandleDirection:
  ; Sets RAM_FunctionTablePointer to the next function data row if applicable
  ld a, (RAM_EffectiveInputs2)
  bit 0, a
  jr nz, @Up
  bit 1, a
  jr nz, @Down
  bit 2, a
  jr nz, @Left
  bit 3, a
  jr nz, @Right
  sub a
  ret

@Up:
  ld l, (ix+4)
  ld h, (ix+5)
  ld a, l
  or h
  ret z
  ld (RAM_FunctionTablePointer), hl
  ret

@Down:
  ld l, (ix+8)
  ld h, (ix+9)
  ld a, l
  or h
  ret z
  ld (RAM_FunctionTablePointer), hl
  ret

@Left:
  ld l, (ix+10)
  ld h, (ix+11)
  ld a, l
  or h
  ret z
  ld (RAM_FunctionTablePointer), hl
  ret

@Right:
  ld l, (ix+6)
  ld h, (ix+7)
  ld a, l
  or h
  ret z
  ld (RAM_FunctionTablePointer), hl
  ret

FunctionsTable:
;                        Function pointer  RAM address             Up                   Right                Down                 Left                 x,y
FunctionEntryTone1HW .dw HandleThreeDigits RAM_Tone1HalfWavelength $0000                FunctionEntryTone1At FunctionEntryTone2HW $0000                $060b
FunctionEntryTone2HW .dw HandleThreeDigits RAM_Tone2HalfWavelength FunctionEntryTone1HW FunctionEntryTone2At FunctionEntryTone3HW $0000                $090b
FunctionEntryTone3HW .dw HandleThreeDigits RAM_Tone3HalfWavelength FunctionEntryTone2HW FunctionEntryTone3At FunctionEntryNoiseTy $0000                $0c0b
FunctionEntryTone1At .dw HandleSingleDigit RAM_Tone1Attenuation    $0000                $0000                FunctionEntryTone2At FunctionEntryTone1HW $0619
FunctionEntryTone2At .dw HandleSingleDigit RAM_Tone2Attenuation    FunctionEntryTone1At $0000                FunctionEntryTone3At FunctionEntryTone2HW $0919
FunctionEntryTone3At .dw HandleSingleDigit RAM_Tone3Attenuation    FunctionEntryTone2At $0000                FunctionEntryNoiseAt FunctionEntryTone3HW $0c19
FunctionEntryNoiseTy .dw HandleNoiseType   RAM_NoiseMode           FunctionEntryTone3HW FunctionEntryNoiseAt FunctionEntryNoiseRt $0000                $0f0a
FunctionEntryNoiseRt .dw HandleNoiseRate   RAM_NoiseRate           FunctionEntryNoiseTy $0000                $0000                $0000                $100a
FunctionEntryNoiseAt .dw HandleSingleDigit RAM_NoiseAttenuation    FunctionEntryTone3At $0000                $0000                FunctionEntryNoiseTy $0f19


NoiseDescriptions:
.dw +, ++, +++, ++++
+:    .ascstr "High", $FF
++:   .ascstr "Med ", $FF
+++:  .ascstr "Low ", $FF
++++: .ascstr "Ch.2", $FF
.ends

.section "Input handling" free
InitInputVariables:
  ld a, $00
  ld (RAM_EffectiveInputs1), a
  ld (RAM_EffectiveInputs2), a
  ld (RAM_CurrentInputs), a
  ld (RAM_PreviousInputs), a
  ld (RAM_RepeatDelay_Up), a
  ld (RAM_RepeatDelay_Down), a
  ld (RAM_RepeatDelay_Left), a
  ld (RAM_RepeatDelay_Right), a
  ld (RAM_RepeatDelay_Button1), a
  ld (RAM_RepeatDelay_Button2), a
  ret

CheckInputs:
  ; Copy current to previous, and remember in c
  ld a, (RAM_CurrentInputs)
  ld (RAM_PreviousInputs), a
  ld c, a
  ; Read control port
  in a, (PORT_IOPort1)
  ; Invert so 1 = pressed
  xor $ff
  ; Mask out player 2 bits
  and $3f
  ; Save and remember in b
  ld (RAM_CurrentInputs), a
  ld b, a

  ld d, $00
  ld e, $00

  bit 0, b
  call nz, @Up
  bit 1, b
  call nz, @Down
  bit 2, b
  call nz, @Left
  bit 3, b
  call nz, @Right
  bit 4, b
  call nz, @Button1
  bit 5, b
  call nz, @Button2

  ; Store these for code to look at later
  ld a, d
  ld (RAM_EffectiveInputs1), a
  ld a, e
  ld (RAM_EffectiveInputs2), a
  ret

@Up:
  bit 0, c
  jr nz, +
  ; New press
  set 0, e
  set 0, d
  ld a, $20
  ld (RAM_RepeatDelay_Up), a
  ret

+:; Held
  ; Decrement counter
  ld a, (RAM_RepeatDelay_Up)
  dec a
  ld (RAM_RepeatDelay_Up), a
  jr nz, +
  ; Set bits only when counter expires
  set 0, e
  set 0, d
  ; Repeat faster
  ld a, $0a
  ld (RAM_RepeatDelay_Up), a
  ret

+:; Repeat delay has not expired
  ; Check for value >127
  and $f0
  ret nz
  ; If <127, then set bit in d on every 2nd frame
  ld a, (RAM_RepeatDelay_Up)
  and $01
  ret nz
  set 0, d
  ret

@Down:
  ; All the same but different bits and RAM variables
  bit 1, c
  jr nz, +
  set 1, e
  set 1, d
  ld a, $20
  ld (RAM_RepeatDelay_Down), a
  ret

+:
  ld a, (RAM_RepeatDelay_Down)
  dec a
  ld (RAM_RepeatDelay_Down), a
  jr nz, +
  set 1, e
  set 1, d
  ld a, $0a
  ld (RAM_RepeatDelay_Down), a
  ret

+:
  and $f0
  ret nz
  ld a, (RAM_RepeatDelay_Down)
  and $01
  ret nz
  set 1, d
  ret

@Left:
  bit 2, c
  jr nz, +
  set 2, e
  set 2, d
  ld a, $20
  ld (RAM_RepeatDelay_Left), a
  ret

+:
  ld a, (RAM_RepeatDelay_Left)
  dec a
  ld (RAM_RepeatDelay_Left), a
  jr nz, +
  set 2, e
  set 2, d
  ld a, $0a
  ld (RAM_RepeatDelay_Left), a
  ret

+:
  and $f0
  ret nz
  ld a, (RAM_RepeatDelay_Left)
  set 2, d
  ret

@Right:
  bit 3, c
  jr nz, +
  set 3, e
  set 3, d
  ld a, $20
  ld (RAM_RepeatDelay_Right), a
  ret

+:
  ld a, (RAM_RepeatDelay_Right)
  dec a
  ld (RAM_RepeatDelay_Right), a
  jr nz, +
  set 3, e
  set 3, d
  ld a, $0a
  ld (RAM_RepeatDelay_Right), a
  ret

+:
  and $f0
  ret nz
  ld a, (RAM_RepeatDelay_Right)
  and $01
  ret nz
  set 3, d
  ret

@Button1:
  bit 4, c
  jr nz, +
  set 4, e
  set 4, d
  ld a, $20
  ld (RAM_RepeatDelay_Button1), a
  ret

+:
  ld a, (RAM_RepeatDelay_Button1)
  dec a
  ld (RAM_RepeatDelay_Button1), a
  jr nz, +
  set 4, e
  set 4, d
  ld a, $06
  ld (RAM_RepeatDelay_Button1), a
  ret

+:
  and $f0
  ret nz
  ; Different here: we skip reading back from RAM so this does nothing and we "fast repeat" on every frame
  and $01
  ret nz
  set 4, d
  ret

@Button2:
  bit 5, c
  jr nz, +
  set 5, e
  set 5, d
  ld a, $20
  ld (RAM_RepeatDelay_Button2), a
  ret

+:
  ld a, (RAM_RepeatDelay_Button2)
  dec a
  ld (RAM_RepeatDelay_Button2), a
  jr nz, +
  set 5, e
  set 5, d
  ld a, $06
  ld (RAM_RepeatDelay_Button2), a
  ret

+:
  and $f0
  ret nz
  ld a, (RAM_RepeatDelay_Button2)
  and $01
  ret nz
  set 5, d
  ret
.ends

.section "Pause muting" free
InitPauseAndMute:
  ; Clear the pause flag
  ld a, $00
  ld (RAM_PauseFlag), a
  call MutePSG
  ret
.ends

.section "PSG update" free
MutePSG:
  ld h, $00 ; Channel
  ld l, $0f ; Attenuation
  ld de, $03ff ; Half wavelength
  call WritePSGTone
  ld h, $01
  ld l, $0f
  ld de, $03ff
  call WritePSGTone
  ld h, $02
  ld l, $0f
  ld de, $03ff
  call WritePSGTone
  ld h, $03
  ld l, $0f
  ld de, $03ff
  call WritePSGTone
  xor a
  ld (RAM_LastPSGModeWrite),a
  ret

UpdatePSG:
  ; If paused, mute everything
  ld a, (RAM_PauseFlag)
  bit 0, a
  jr nz, MutePSG
  ; Else emit values from RAM
  ld de, (RAM_Tone1HalfWavelength)
  ld h, $00
  ld a, (RAM_Tone1Attenuation)
  ld l, a
  call WritePSGTone
  ld de, (RAM_Tone2HalfWavelength)
  ld h, $01
  ld a, (RAM_Tone2Attenuation)
  ld l, a
  call WritePSGTone
  ld de, (RAM_Tone3HalfWavelength)
  ld h, $02
  ld a, (RAM_Tone3Attenuation)
  ld l, a
  call WritePSGTone
  ld a, (RAM_NoiseAttenuation)
  ld l, a
  ld a, (RAM_NoiseMode)
  ld h, a
  ld a, (RAM_NoiseRate)
  ld d, a
  call WritePSGNoise
  ret

WritePSGTone:
  ; h = channel
  ; l = attenuation
  ; de = half wavelength
  ; We want to emit
  ; 1. %1hh1llll <- set attenuation
  ld a, h
  add a, a
  add a, a
  add a, a
  add a, a
  add a, a
  ld h, a
  or %10010000
  or l
  out (PORT_PSG), a
  ; 2. %1hh0eeee <- low 4 bits of de, set half-wavelength low bits
  ld a, e
  and %1111
  or h
  or  %10000000
  and %11101111 ; <- Unnecessary, the bit is always 0
  out (PORT_PSG), a
  ; 3. %00ddeeee <- next 6 bits of de, set half-wavelength high bits
  srl d
  rr e
  srl d
  rr e
  srl d
  rr e
  srl d
  rr e
  ld a, e
  and $3f
  out (PORT_PSG), a
  ret

WritePSGNoise:
  ; h = mode (1 = periodic)
  ; l = attenuation
  ; d = rate (0..3)
  ; 1. %1111llll <- Attenuation for channel 3
  ld a, l
  and %00001111
  or  %11110000
  out (PORT_PSG), a
  ; 2. %11100hdd <- mode and rate
  ld a, h
  add a, a
  add a, a
  and %00000100
  or d
  or %11100000
  ; Write only if changed
  ld hl,RAM_LastPSGModeWrite
  cp (hl)
  ret z
  out (PORT_PSG), a
  ld (hl),a
  ret
.ends

.sdsctag 1.2, "SMS Sound Test", "Nicolas Warren (Heliophobe)", "Extended by Maxim"
