.segment "HEADER"
    .byte "NES"
    .byte $1a    
    .byte $02
    .byte $01
    .byte %00000000
    .byte $00
    .byte $00
    .byte $00
    .byte $00
    .byte $00, $00, $00, $00, $00

.segment "ZEROPAGE"

.segment "STARTUP"
;subroutines
VBlankWait:
    bit $2002
    bpl VBlankWait
    rts
;movement
MoveFwd:
    lda $0200, x
    clc
    adc #$01
    sta $0200, x
    rts

MoveBkd:
    lda $0200, x
    sec
    sbc #$01
    sta $0200, x
    rts

Move:
    cmp #$01
    bne :+
    jsr MoveFwd
    jmp :++
:
    cmp #$02
    bne :+
    jsr MoveBkd
:
    rts


RESET:
    sei
    cld
    
    ;stop apu
    ldx #$40
    stx $4017

    ;init stack register
    ldx #$ff
    txs

    ;set ppu registers to 0
    inx
    stx $2000
    stx $2001

    stx $4010

:
    bit $2002
    bpl :-

    ;clear memory
    txa
MEMCLR:
    sta $0000, x
    sta $0100, x
    sta $0300, x
    sta $0400, x
    sta $0500, x
    sta $0600, x
    sta $0700, x
    ;allocate sprite memory
    lda #$ff
    sta $0200, x
    lda #$00

    inx
    bne MEMCLR

:
    bit $2002
    bpl :-

    lda #$02
    sta $4014
    nop

    lda #$3f
    sta $2006
    lda #$00
    sta $2006

    ldx #$00
LoadPalette:
    lda PaletteData, x
    sta $2007
    inx
    cpx #$20
    bne LoadPalette

;load sprites to allocated sprite memory
    ldx #$00
LoadSprite:
    lda Sprites, x
    sta $0200, x
    inx
    cpx #$38
    bne LoadSprite

    cli

    lda #%10010000
    sta $2000

    lda #%00011110
    sta $2001

    lda #$00
    sta $000A

    ;init ball dir
    lda #$01
    sta $000B
    sta $000C

Loop:
    jmp Loop

NMI:
    ;latch controls
    lda #$01
    sta $4016
    lda #$00
    sta $4016

    ldx #$00
ControlsLoop:
    ldy #$00
    lda $4016
    and #%00000001
    beq :+
    ldy #$01
:
    sty $0000, x
    inx
    cpx #$08
    bne ControlsLoop

MainMenuLoop:
    ;test
    jmp MainGameLoop
;jump to level based on value of 0x000A
    ldx $000A
    cpx #$01
    beq MainGameLoop

;set level bit to 1 if start is pressed
    lda $0003
    cmp #$01
    bne :+
    lda #$01
    sta $000A
:
    rti

MainGameLoop:

    ;set direction variables
    lda #$00 ;reset to 0 each frame
    sta $0008 ;left bar
    sta $0009 ;right bar

    ;left bar control
    ;check A
    lda $0210
    cmp #$be
    beq :+
    lda $0005
    cmp #$01
    bne :+
    lda #$01
    sta $0008
    :
    ;check B
    lda $0210
    cmp #$08
    beq :+
    lda $0004
    cmp #$01
    bne :+
    lda #$02
    sta $0008
    :
    ;rught bar control
    ;check Up
    lda $0224
    cmp #$08
    beq :+
    lda $0001
    cmp #$01
    bne :+
    lda #$02
    sta $0009
    :
    ;check Down
    lda $0224
    cmp #$be
    beq :+
    lda $0000
    cmp #$01
    bne :+
    lda #$01
    sta $0009
    :

    ldy $0008
    ldx #$10
MoveBars:
    ;vertical
    tya
    jsr Move
    inx
    inx
    inx
    inx
    cpx #$24
    bne :+
    ldy $0009
:
    cpx #$38
    bne MoveBars

;owo(ball) movement
CheckWall:
    ;jmp EndHorChk
    ;check for collition with paddle
    ldx $0203
    cpx #$0f
    beq LeftPaddleCheck
    cpx #$dc
    beq RightPaddleCheck
    jmp EndHorChk

    ;assign value to check position of depending on the side
    ;0x0101 is temp value to compare
LeftPaddleCheck:
    ldy $0210
    ldx $0220
    jmp :+
RightPaddleCheck:
    ldy $0224
    ldx $0234
:
    ;check if the ball collides with the bar(ball being in same y range as the paddle)
    lda $0200
    adc #$0f
    sty $0101
    cmp $0101
    bcc EndHorChk ;as owo is higher than the paddle
    ;else check bottom end
    lda $0200
    sbc #$08
    stx $0101
    cmp $0101
    bcc :+
    jmp EndHorChk
:
    lda $000C
    cmp #$02
    bne :+    
    lda #$01
    sta $000C
    jmp EndHorChk
:
    lda #$02
    sta $000C
EndHorChk:
    ;clear value of 0x0101(maybe unnessesary)
    lda #$00
    sta $0101

    ;jmp EndVertChk
    ldx $0200
    cpx #$08
    beq :+
    cpx #$d4
    beq :+
    jmp EndVertChk


:
    lda $000B
    cmp #$02
    bne :+    
    lda #$01
    sta $000B
    jmp EndVertChk
:
    lda #$02
    sta $000B
EndVertChk:

    ldx #$00 
MoveOwOLoop:
    ;vertical
    lda $000B
    jsr Move
    inx
    inx
    inx
    ;horizontal
    lda $000C
    jsr Move
    inx
    cpx #$10
    bne MoveOwOLoop
    
    lda #$02
    sta $4014
    rti

PaletteData:
    .byte $0f,$00,$00,$25,$0f,$00,$00,$25,$0f,$00,$00,$25,$0f,$00,$00,$25
    .byte $0f,$14,$23,$25,$0f,$00,$00,$25,$0f,$00,$00,$25,$0f,$00,$00,$25

Sprites:
    ;OwO 0x00 - 0x0f
    ;middle height 78/80
    .byte $78,$00,$00,$76
    .byte $78,$00,$00,$82
    .byte $80,$01,$00,$78
    .byte $80,$01,$00,$80
    ;Left Bar 0x10 - 0x23
    .byte $68,$10,$00,$08
    .byte $70,$20,$00,$08
    .byte $78,$20,$00,$08
    .byte $80,$20,$00,$08
    .byte $88,$30,$00,$08
    ;right Bar 0x24 - 0x37
    .byte $68,$10,$00,$f0
    .byte $70,$20,$00,$f0
    .byte $78,$20,$00,$f0
    .byte $80,$20,$00,$f0
    .byte $88,$30,$00,$f0

.segment "VECTORS"
    .word NMI
    .word RESET

.segment "CHARS" 
    .incbin "../res/powong.chr"