screen segment at 0a000h
	dot label byte
screen ends

daten segment
unpuffer db 64000 dup (0)
daten ends

hilfe segment
hlfunp db 1535 dup (0)
hilfe ends

code segment
	assume cs:code,ds:code,es:code
entry:  mov dx,code
	mov ds,dx
	mov ax,0
	int 33h
	cmp ax,0ffffh
	jne gnm
	mov dx,offset mousetext
	mov ah,9
	int 21h
	mov ah,0
	int 16h
	cmp al,'J'
	jne gnm
gnm3:	mov mousie,1
	jmp gnm2
gnm:    cmp al,'j'
	je gnm3
	mov mousie,0
gnm2:	mov ah,3
	mov al,5
	mov bh,255
	mov bl,1fh
	int 16h
	call intro
	mov ax,0013h
	int 10h
	mov news,0
	mov si,0
	mov gxpos,0
	mov gypos,0
	mov glzeiger,0
	mov position1,16
	mov position2,10
	mov xpos1,7
	mov ypos1,1
	mov xpos2,1
	mov ypos2,1
	mov gxpos1,7
	mov gypos1,1
	mov gxpos2,1
	mov gypos2,1
	mov glevel[8100],7
	mov glevel[8101],1
	mov glevel[8102],1
	mov glevel[8103],1
	mov rand,0
	mov si,8100
entglcl1:
	mov al,glevel[si]
	mov glevel2[si],al
	inc si
	cmp si,8107
	jne entglcl1
	jmp read_colors
mousetext db 'In Ihrem System wurde eine Maus gefunden. Wollen Sie diese',10,13
		db 'im Marmoris-Editor benutzen ?  $'
xleft dw 0
yleft dw 0
xright dw 0
yright dw 0
xleft1 dw 252
yleft1 dw 12
xright1 dw 276
yright1 dw 30
xleft2 dw 36
yleft2 dw 12
xright2 dw 60
yright2 dw 30
farbe1	db 0
farbe2 db 0
d1farbe db 0
d2farbe db 0
position dw 0
position1 dw 0
position2 dw 0
xpos db 0
ypos db 0
ypos1 db 0
xpos1 db 0
ypos2 db 0
xpos2 db 0
spieler db 1
steinpos dw 0
zaehler db 0
anipos	db 0
deltext db '                                        ','$'
gldir dw 0
glzeiger dw 0
gly db 0
glx db 0
gly2 db 0
glx2 db 0
gxpos db 0
gypos db 0
gxpos1 db 0
gxpos2 db 0
gypos1 db 0
gypos2 db 0
glevel db 8106 dup (0)
glevel2 db 8106 dup (0)
screenpuffer db 8106 dup (?)
rand dw 0
glpos1	dw 0
tglpl db 0
switchstatus db 128 dup (?)
plp1	dw 0
plp2	dw 0
plc1	db 0
plc2	db 0
glmem1  dw 10000
glmem2	dw 10000
bvar	db 0
glxs1	db 0
glys1	db 0
glxs2	db 0
glys2	db 0
frominit db 0

include loadpic.asm
include loadstei.asm
include showstei.asm
include loadleve.asm
include editor.asm
include intro.asm
include fieps.asm         ;fiepsl und freq
include unpak.asm
hintergrund	proc
	mov si,0
delzerp22:mov cs:zerop[si],0
	inc si
	cmp si,765
	jne delzerp22
	setpalmac

	mov dateiname[4],'5'
	call unpak

outerl12:mov si,0
	mov bx,0
innerl12:mov al,lppuffer[si]
	cmp zerop[si],al
	je pmatched2
	inc zerop[si]
	mov bx,1
pmatched2:inc si
	cmp si,765
	jne innerl12
	push bx
	setpalmac
	pop bx
	mov ah,86h
	mov cx,0
	mov dx,300
	int 15h 	    ;delay
	cmp bx,0
	jne outerl12


	mov si,0
	mov dx,daten
	mov ds,dx
	mov dx,screen
	mov es,dx
hgrsd1: mov al,es:dot[si]
	mov ds:unpuffer[si],al
	inc si
	cmp si,64000
	jne hgrsd1
	mov dx,code
	mov ds,dx
	jmp endh1

	mov di,0			;alter Hintergrund...
	mov si,0
	mov steinpos,2560
hgl:	mov al,12
	call showstein
	inc si
	add steinpos,25
	cmp si,16
	jne hgl
	mov si,0
	add steinpos,5740
	inc di
        cmp di,10
	jne hgl
	cmp frominit,1
	je endhgr
endh1:
endhgr: mov frominit,0
	ret
hintergrund	endp

colors1 db 16 dup (?)
colors2 db 16 dup (?)
read_colors:
	mov bvar,26
	mov glmem1,10000
	mov glmem2,10000
	mov glflag,0
	call loadpicture
wfi:	tastloesch
	mov ah,0
	int 16h
	call loadsteine
	mov frominit,1
	call hintergrund
	mov dx,255
	call load_level
	mov zeigerposition,3848
	call zsichern
	call setborder
	jmp start

setborder	proc
	push si
	push di
	mov si,0
	mov al,bvar
setbloop1:
	mov glevel[si],al
	mov glevel2[si],al
	inc si
	cmp si,90
	jne setbloop1
	mov si,0
	mov di,0
setbloop2:
	mov glevel[si],al
	mov glevel2[si],al
	add si,90
	inc di
	cmp di,90
	jne setbloop2
	mov si,8010
	mov di,0
setbloop3:
	mov glevel[si],al
	mov glevel2[si],al
	inc si
	inc di
	cmp di,90
	jne setbloop3
	mov di,0
	dec si
setbloop4:
	mov glevel[si],al
	mov glevel2[si],al
	sub si,90
	inc di
	cmp di,90
	jne setbloop4
	pop di
	pop si
        mov dx,255
	call glcopy
	ret
setborder	endp

;urspr. delrow  proc
	mov ah,2
	mov bh,0
	mov dh,0
	mov dl,0
	int 10h
	mov dx,code
	mov ds,dx
	mov ah,9
	mov dx,offset deltext
	int 21h
	mov ah,2
	mov bh,0
	mov dx,0
	int 10h
        ret
;delrow  endp
delrow proc
	push dx
	push ax
	push si
	mov dx,screen
	mov es,dx
	mov si,0
	mov dx,daten
	mov ds,dx
drl1:	mov al,ds:unpuffer[si]
	mov es:dot[si],al
	inc si
	cmp si,2560
	jb drl1
	mov dx,code
	mov ds,dx
	mov ah,2
	mov bh,0
	mov dx,0
	mov bh,0
	int 10h
	pop si
	pop ax
	pop dx
	ret
delrow endp


zentrieren	proc
	cmp spieler,1
	jne zsp2
	cmp glmem1,10000
	je znor
	mov ax,glmem1
	mov glzeiger,ax
	mov dx,255
	call glcopy
	ret
zsp2:	cmp glmem2,10000
	je znor
	mov ax,glmem2
	mov glzeiger,ax
	mov dx,255
	call glcopy
	ret
znor:	cmp gypos,45
	jbe upper
	jmp lower
upper:	cmp gxpos,45
	jbe l1
	jmp r1
l1:     mov al,gxpos
	dec al
	mov glx,al
	mov al,gypos
	dec al
	mov gly,al
	mov position,10
	mov xleft,63	;36
	mov yleft,32	;12
	mov xpos,1
	mov ypos,1
	jmp cglcopy
r1:	mov al,gxpos
	sub al,7
	mov glx,al
	mov al,gypos
	dec al
	mov gly,al
	mov position,16
	mov xleft,225
	mov yleft,32
	mov xpos,7
	mov ypos,1
	jmp cglcopy
lower:	cmp gxpos,45
	jbe l2
	jmp r2
l2:	mov al,gxpos
	dec al
	mov glx,al
	mov al,gypos
	sub al,7
	mov gly,al
	mov position,64
	mov xpos,1
	mov ypos,7
	mov xleft,63
	mov yleft,152
	jmp cglcopy
r2:	mov al,gxpos
        sub al,7
	mov glx,al
	mov al,gypos
	sub al,7
        mov gly,al
	mov position,70
	mov xpos,7
	mov ypos,7
	mov xleft,225
	mov yleft,152
cglcopy:
	mov ax,0
	mov al,gly
	mov bx,90
	mul bx
	mov dx,0
	mov dl,glx
	add ax,dx
	mov glzeiger,ax
	mov dx,255
	call glcopy
	ret
zentrieren endp

glcopy	proc
	push si
	push di ;Ausschnitt weiterkopieren
	push ax
	push dx
	mov ax,steinpos
	push ax
	cmp spieler,1
	jne spn1
	mov tglpl,2
	jmp spn2
spn1:	mov tglpl,1
spn2:	cmp dx,255
	je glcl3
	cmp dx,256
	jne noglcl3
	jmp glcl3
noglcl3:
        mov si,glzeiger
	mov di,0
	mov ah,0
glcl1:  mov al,level[di]
	cmp al,13
	je tgl4
	cmp al,14
	je tgl4
	mov glevel[si],al
tgl4:	inc si
	inc di
	inc ah
	cmp ah,9
	jne glcl1
	add si,81
	mov ah,0
	cmp di,81
	jne glcl1
	cmp dx,254
	jne noeglc2
	jmp eglc2
noeglc2:
	cmp dx,253
	jne noeglc3
	jmp endglc
noeglc3:
        mov si,glzeiger
	add si,gldir
	mov glzeiger,si
glcl3:	mov si,glzeiger
	mov di,0
	mov ah,0
glcl2:  push ax
	push bx
	cmp spieler,2
	jne do2
	mov ax,0
	mov al,gypos1
	mov bx,90
	mul bx
	mov bx,0
	mov bl,gxpos1
	add ax,bx
	mov glpos1,ax
	jmp do3
do2:	mov ax,0
	mov bx,90
	mov al,gypos2
	mul bx
	mov bx,0
	mov bl,gxpos2
	add ax,bx
	mov glpos1,ax
do3:	pop bx
	pop ax

	mov al,glevel[si]
	mov level[di],al
	cmp glflag,1
	je tgl2
	cmp si,glpos1
	jne tgl2
	push ax
	mov ah,15
	sub ah,tglpl
	mov level[di],ah
	pop ax
tgl2:	inc di
        inc si
	inc ah
	cmp ah,9
	jne glcl2
	mov ah,0
	add si,81
	cmp di,81
	jne glcl2
	cmp dx,256
	je endglc
eglc2:	cmlshow
endglc: mov ax,0
	mov bx,0
	mov al,xpos1
	mov bx,27
	mul bx
	add ax,36
	mov xleft1,ax
	mov ax,0
	mov bx,0
	mov al,ypos1
	mov bx,20
	mul bx
	add ax,12
	mov yleft1,ax
	mov ax,0
	mov bx,0
	mov al,xpos2
	mov bx,27
	mul bx
	add ax,36
	mov xleft2,ax
	mov ax,0
	mov bx,0
	mov al,ypos2
	mov bx,20
	mul bx
	add ax,12
	mov yleft2,ax
	pop dx
	mov steinpos,dx
	pop dx
	pop ax
	pop di
	pop si
        ret
glcopy	endp				;und schluss mit dem spass

feldmark proc
	mov ax,yleft
	mov bx,320
	mul bx
	add ax,xleft
	mov steinpos,ax
	cmp spieler,1
	je spieler1mark
	mov al,13
	jmp showspieler
spieler1mark:
	mov al,14
showspieler:
        call showstein
	ret
feldmark endp

feldloesch proc
	push dx
	mov dx,code
	mov ds,dx
	pop dx
	mov ax,yleft
	mov bx,320
	mul bx
	add ax,xleft
	mov steinpos,ax
	mov di,position
	mov al,level[di]

	cmp al,13
	jae endfl
	call showstein
	ret
endfl:  cmp al,38
	jne no38
css:	call showstein
	ret
no38:	cmp al,39
	je css
	cmp al,26
	je css
	ret
feldloesch endp

set_spieler1 macro
	mov spieler,1
	mov ah,ypos1
        mov ypos,ah
	mov ah,xpos1
	mov xpos,ah
	mov ax,position1
	mov position,ax
	mov ax,xright1
	mov xright,ax
	mov ax,xleft1
	mov xleft,ax
	mov ax,yright1
	mov yright,ax
	mov ax,yleft1
	mov yleft,ax
	mov al,gypos1
	mov gypos,al
	mov al,gxpos1
	mov gxpos,al
endm

set_spieler2 macro
	mov spieler,2
	mov ah,ypos2
        mov ypos,ah
	mov ah,xpos2
	mov xpos,ah
	mov ax,position2
	mov position,ax
	mov ax,xright2
	mov xright,ax
	mov ax,xleft2
	mov xleft,ax
	mov ax,yright2
	mov yright,ax
	mov ax,yleft2
	mov yleft,ax
	mov al,gxpos2
	mov gxpos,al
	mov al,gypos2
	mov gypos,al
endm


write_spieler1 macro
	mov ah,ypos
        mov ypos1,ah
	mov ah,xpos
	mov xpos1,ah
	mov ax,position
	mov position1,ax
	mov ax,xright
	mov xright1,ax
	mov ax,xleft
	mov xleft1,ax
	mov ax,yright
	mov yright1,ax
	mov ax,yleft
	mov yleft1,ax
        mov al,gxpos
	mov gxpos1,al
	mov al,gypos
	mov gypos1,al
endm

write_spieler2 macro
	mov ah,ypos
        mov ypos2,ah
	mov ah,xpos
	mov xpos2,ah
	mov ax,position
	mov position2,ax
	mov ax,xright
	mov xright2,ax
	mov ax,xleft
	mov xleft2,ax
	mov ax,yright
	mov yright2,ax
	mov ax,yleft
	mov yleft2,ax
	mov al,gypos
	mov gypos2,al
	mov al,gxpos
	mov gxpos2,al
endm

include testnext.asm

cursor_hoch proc
	cmp spieler,1
	je spieler1
	set_spieler2
	jmp begin_hoch
spieler1:
	set_spieler1
begin_hoch:
	cmp ypos,1
	jne notmoveup
	jmp moveup
notmoveup:
	cmp ypos,0
	je end_ch
nmu2:   mov direction,-9
	call testnextfield
	cmp move,0
	jne ch_move
	jmp end_ch

ch_move: call feldloesch

	dec ypos
	dec gypos
        sub yleft,20
	sub yright,20
	sub position,9
end_ch: mov dx,40h
	mov es,dx
	mov ax,001eh
	mov es:001ah,ax
	mov es:001ch,ax
	cmp spieler,1
	je wspieler1
	write_spieler2
	jmp ch_end
wspieler1:
	write_spieler1
ch_end:
	ret
moveup: cmp gly,0
	jne noend_ch
	jmp nmu2
noend_ch:
	mov direction,-9
	mov rand,10
	call testnextfield
	cmp move,1
	je chw
	jmp end_ch
chw:
	mov gldir,-90
	dec gly
	dec gypos
	call glcopy
	mov move,0
	jmp end_ch
cursor_hoch endp

cursor_links proc
	cmp spieler,1
	je cl_spieler1
	set_spieler2
	jmp begin_cl
cl_spieler1: set_spieler1
begin_cl:
	cmp xpos,1
	jne cl_weiter1
	jmp movele
cl_weiter1:
	cmp xpos,0
	je end_cl
clw2:	mov direction,-1
	call testnextfield
	cmp move,1
	je cl_move
	jmp cl_end
cl_move:
        call feldloesch
        dec xpos
	dec gxpos
	dec position
	sub xright,27
	sub xleft,27
end_cl: cmp spieler,1
	je cl_wspieler1
	write_spieler2
	jmp cl_end
cl_wspieler1:
	write_spieler1
cl_end: mov ax,001eh
	mov dx,40h
	mov es,dx
	mov es:001ah,ax
	mov es:001ch,ax
        ret
movele: cmp glx,0
	jne noend_cl
	jmp clw2
noend_cl:
	mov rand,1
	mov direction,-1
	call testnextfield
	cmp move,1
	je wcl
	jmp end_cl
wcl:	mov gldir,-1
	dec glx
	dec gxpos
	call glcopy
	mov move,0
	jmp end_cl
cursor_links endp

cursor_rechts proc
	cmp spieler,1
	je cr_spieler1
	set_spieler2
	jmp begin_cr
cr_spieler1:
	set_spieler1
begin_cr:
	cmp xpos,7
	jne notmoveri
	jmp moveri
notmoveri:
	cmp xpos,8
	je end_cr
nmr2:	mov direction,1
	call testnextfield
	cmp move,1
	je cr_move
	jmp cr_end
cr_move:
        call feldloesch
        add xleft,27
	add xright,27
	inc position
	inc xpos
	inc gxpos
end_cr:
	cmp spieler,1
	je cr_wspieler1
	write_spieler2
	jmp cr_end
cr_wspieler1:
	write_spieler1
cr_end: mov dx,40h
	mov es,dx
	mov ax,001eh
	mov es:001ah,ax
	mov es:001ch,ax
        ret
moveri: cmp gxpos,88
	jne noend_cr
	jmp nmr2
noend_cr:
	mov rand,1
	mov direction,1
	call testnextfield
	cmp move,1
	je wcr
	jmp end_cr
wcr:	mov gldir,1
	inc glx
	inc gxpos
	call glcopy
	mov move,0
        jmp end_cr
cursor_rechts endp

cursor_unten proc
	cmp spieler,1
	je cu_spieler1
	set_spieler2
	jmp begin_cu
cu_spieler1:
	set_spieler1
begin_cu:
        cmp ypos,7
	jne notmovedn
	jmp movedn
notmovedn:
	cmp ypos,8
	je end_cu
nmd2:   mov direction,9
	call testnextfield
	cmp move,1
	je cu_move
	jmp end_cu
cu_move:
        call feldloesch

	add position,9
	inc ypos
	inc gypos
	add yleft,20
	add yright,20
end_cu: mov dx,40h
	mov es,dx
	mov ax,001eh
	mov es:001ah,ax
	mov es:001ch,ax
	cmp spieler,1
	je cu_wspieler1
	write_spieler2
	jmp cu_end
cu_wspieler1:
	write_spieler1
cu_end: ret
movedn: cmp gypos,88
	jne noend_cu
	jmp nmd2
noend_cu:
	mov rand,10
	mov direction,9
	call testnextfield
	cmp move,1
	je wcu
	jmp end_cu
wcu:	mov gldir,90
	inc gly
	inc gypos
	call glcopy
	mov move,0
	jmp end_cu
cursor_unten endp

start:

init:   mov glmem1,10000
	mov glmem2,10000
	call zwiederherstellen
	mov zeigerposition,3848
	call zsichern
	mov rowpos,3876
	mov zeigerdir,'up'
	mov rowdir,'up'
	mov endposition,62088
	mov rowpos2,0
	mov dx,zeigerposition
	mov steinpos,dx
	mov bx,36
	call zverk
	mov al,37
	call showstein
	mov key,0
	;mov ah,12h
	;mov bl,36h
	;mov al,1
	;int 10h
	set_spieler2
	call zentrieren
	write_spieler2
	set_spieler1
	call zentrieren
	write_spieler1
	;mov ah,12h
	;mov bl,36h
	;mov al,0
	;int 10h
gloop1:
	mov dx,40h
	mov es,dx
	mov ax,001eh
	mov es:001ah,ax
	mov es:001ch,ax
	call feldmark
	cmp news,1
	jne waitloop
	mov news,0
	jmp newstart
waitloop:
waitloop1:
        mov ah,0
	int 16h
zeichen_vorhanden:
	cmp ah,47h
	jne nw1
	jmp newstart
nw1:	cmp al,27
	jne noendmar
	jmp end_marmoris
noendmar:
	cmp al,13
	jne noem2
	jmp switchpl
noem2:	cmp ah,3bh
	jne nw01
	jmp loadalevel
nw01:   cmp ah,49h
	jne nw011
	call showmap
	jmp gloop1
nw011:	cmp ax,5200h
	jne nw02
	jmp editalevel
nw02:
nw2:	cmp al,0
	je nw21
	jmp gloop1
nw21:	cmp ah,48h
	jne w1
	call cursor_hoch
	jmp gloop1
w1:	cmp ah,4bh
	jne w2
	call cursor_links
	jmp gloop1
w2:	cmp ah,4dh
	jne w3
	call cursor_rechts
	jmp gloop1
w3:	cmp ah,50h
	je w31
	jmp gloop1
w31:	call cursor_unten
	jmp gloop1

newstart:
        mov glmem1,10000
	mov glmem2,10000
	mov dx,3876
	mov steinpos,dx
	mov si,0
copylevels:
	mov al,glevel2[si]
	mov glevel[si],al
	inc si
	cmp si,8106
	jne copylevels
	mov dx,255
	call glcopy
	mov dx,255
	call load_level
	mov ax,eballs
	mov balls,ax
	mov key,0
	jmp init

showmap proc
	push ax
	push bx
	push cx
	push dx
	push si
	push di
	push es
	push ds
	cmp mousie,1
	jne gsmnm
	mov ax,2
	int 33h
gsmnm:	mov ax,0
	mov al,gypos1
	mov bx,90
	mul bx
	mov bx,0
	mov bl,gxpos1
	add ax,bx
	mov plp1,ax
	mov si,ax
	mov al,glevel[si]
	mov plc1,al
	mov glevel[si],135
	mov ax,0
	mov al,gypos2
	mov bx,90
	mul bx
	mov bx,0
	mov bl,gxpos2
	add ax,bx
	mov plp2,ax
	mov si,ax
	mov al,glevel[si]
	mov plc2,al
	mov glevel[si],105
        mov dx,screen
	mov es,dx
	mov dx,code
	mov ds,dx
	mov di,16757	;3876
	mov si,0
	mov ah,0
	mov dh,0
sml1:	mov al,es:dot[di]
	mov ds:screenpuffer[si],al
	mov dl,glevel[si]
	cmp dl,4
	jne sml112
	mov dl,4
	jmp sml12
sml112: cmp dl,8
	jne sml11
	mov dl,70
	jmp sml12
sml11:  cmp dl,13
	jne sml121
        mov dl,110
	jmp sml12
sml121: cmp dl,14
        jne sml123
	mov dl,150
	jmp sml12
sml123: mov al,0
sml12:	mov es:dot[di],dl
	inc di
	inc si
	inc ah
	cmp ah,90
	jne sml1
	mov ah,0
	inc dh
	add di,230
	cmp dh,90
	jne sml1
	tastloesch
	mov ah,0
	int 16h
	mov dx,screen
	mov es,dx
	mov si,0
	mov di,16757
	mov ah,0
	mov dh,0
sml2:	mov al,ds:screenpuffer[si]
	mov es:dot[di],al
	inc di
	inc si
	inc ah
	cmp ah,90
	jne sml2
	mov ah,0
	inc dh
	add di,230
	cmp dh,90
	jne sml2
	mov al,plc1
	mov si,plp1
	mov glevel[si],al
	mov al,plc2
	mov si,plp2
	mov glevel[si],al
	cmp mousie,1
	jne gsmnm2
	mov ax,1
	int 33h
gsmnm2: pop ds
	pop es
	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	ret
showmap endp

switchpl:
	mov ax,0
        mov al,gly
	mov bx,90
	mul bx
	mov bx,0
	mov bl,glx
	add ax,bx
	cmp spieler,1
	jne setpl1
	jmp setpl2
setpl1: mov glmem2,ax
	mov dx,254
	call glcopy
	mov al,glx
	mov glxs2,al
	mov al,gly
	mov glys2,al
	write_spieler2
	set_spieler1
	mov al,glxs1
	mov glx,al
	mov al,glys1
	mov gly,al
	call zentrieren
	write_spieler1
jgl1:	jmp gloop1
setpl2: mov glmem1,ax
	mov dx,254
	call glcopy
	mov al,glx
	mov glxs1,al
	mov al,gly
	mov glys1,al
	write_spieler1
	set_spieler2
	mov al,glxs2
	mov glx,al
	mov al,glys2
	mov gly,al
	call zentrieren
        write_spieler2
jgl12:	jmp gloop1

loadalevel:
	call zwiederherstellen
	call delrow
	mov dx,0
	call load_level
	call delrow
	mov zeigerposition,3848
	call zsichern
	mov ax,eballs


	mov balls,ax
	mov glmem1,10000
	mov glmem2,10000
	jmp init
editalevel:
	call zwiederherstellen
	call delrow
	call editor
	call delrow
	mov zeigerposition,3848
	call zsichern
	jmp init
end_marmoris:
	mov dx,40h
	mov es,dx
	mov ax,es:001eh
	mov es:001ah,ax
	mov es:001ch,ax
	mov ax,0003h
	int 10h
	mov ah,4ch
	int 21h
code ends
	end entry
