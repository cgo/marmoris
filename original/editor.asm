mgxpos	dw 0
mgypos  dw 0

findgcoord      proc
	push es
	push dx
	push ax
	mov dx,code
	mov es,dx
        mov ax,0
	mov al,es:gly
	add ax,mypos
	mov mgypos,ax
	mov ax,0
	mov al,es:glx
	add ax,mxpos
	mov mgxpos,ax
        pop ax
	pop dx
	pop es
	ret
findgcoord	endp


mi      db 0
mal	db 0
mx	dw 0
my	dw 0
mypos	dw 0
mxpos	dw 0
mstein	db 0
mstein2 db 0
mset	db 0

mouseinstalled  proc
	mov dx,code
	mov ds,dx
	mov ax,0
	int 33h
	mov mi,1
	mov ax,1
	int 33h
	mov dx,code
	mov es,dx
	mov dx,offset mouseproc
	mov ax,0ch
	mov cx,1010b
	int 33h
	mov steinpos,295
	mov mal,1
emil1:  mov al,mal
	call showstein
	add steinpos,6400
	inc mal
	cmp mal,11
	jne emil1
	mov steinpos,10240
	mov al,38
	call showstein
	mov al,39
	add steinpos,6400
	call showstein
	mov al,11
	add steinpos,6400
	call showstein
	mov al,12
	add steinpos,6400
	call showstein

	add steinpos,6400
	mov al,13
	call showstein
	add steinpos,6400
	mov al,14
	call showstein
	ret
;*****************************
stb	proc
	cmp mx,550
	jbe nonotfield
	jmp notfield
nonotfield:
        cmp mx,70
	jae nonotfield2
	jmp notfield
nonotfield2:
        cmp my,192
	jbe nonotfield3
	jmp notfield
nonotfield3:
        cmp my,12
	jae infield
	jmp notfield
infield:
	mov cx,172
	mov mypos,8
myloop: cmp my,cx
	ja mgoty
	sub cx,20
	dec mypos
	jmp myloop
mgoty:	mov cx,500
	mov mxpos,8
mxloop: cmp mx,cx
	ja mgotx
	sub cx,54
	dec mxpos
	jmp mxloop
mgotx:  mov dx,code
	mov es,dx
	mov ax,mypos
	mov bx,6400
	mul bx
	mov es:steinpos,ax
	mov ax,mxpos
	mov bx,27
	mul bx
	add es:steinpos,ax
	add es:steinpos,3876

	mov mset,1
	mov ax,mypos
	mov bx,9
	mul bx
	add ax,mxpos
	mov si,ax
	mov al,mstein
	mov di,8100
	cmp al,13
	je mspla1
	mov di,8102
	cmp al,14
	jne mspla2
mspla1: call findgcoord
	mov al,byte ptr mgxpos
	mov ah,byte ptr mgypos
	mov es:glevel[di],al
	inc di
	mov es:glevel[di],ah
	jmp noslm1
mspla2: mov es:level[si],al
	mov es:level4[si],al
        mov ax,0
	mov al,es:gly
	mov bx,90
	mul bx
	mov bx,0
	mov bl,es:glx
	add ax,bx
	mov si,ax
	mov ax,mypos
	mov bx,90
	mul bx
	add ax,mxpos
	add si,ax
	mov al,mstein
	mov es:glevel[si],al
noslm1: mov al,mstein
	call showstein

	ret
notfield:
	mov mset,0
        cmp mx,590
	ja inauswahl
	jmp noiam
inauswahl:
        mov cx,20
	mov al,mstein2
	mov mstein2,1
coinauswahl:
	cmp my,cx
	jb m2goty
        add cx,20
	inc mstein2
	cmp mstein2,11
	jne coinauswahl
	mov mstein2,al
m2goty: ret
noiam:  cmp mx,54
	jb inauswahl2
	jmp endmouse
inauswahl2:
        mov cx,52
	mov al,mstein2
	mov mstein2,38
	cmp my,cx
	jae no13
	jmp endmouse
no13:   add cx,20
	mov mstein2,39
        cmp my,cx
	jae no14
	jmp endmouse
no14:   add cx,20
	mov mstein2,11
	cmp my,cx
	jae mno38
	jmp endmouse
mno38:	add cx,20
	mov mstein2,12
	cmp my,cx
	jae no39
	jmp endmouse
no39:   add cx,20
	mov mstein2,13
	cmp my,cx
	jae mno13
	jmp endmouse
mno13:	add cx,20
	mov mstein2,14
	cmp my,cx
	jae mno14
	jmp endmouse
mno14:	mov mstein2,al
endmouse:
        ret
stb	endp


mouseproc	proc far
	push ax
	push bx
	push cx
	push dx
	push es
	push ds
	push si
	push di

	push dx
	mov dx,code
	mov es,dx
	mov ds,dx
	pop dx
	cmp es:mfirst,1
	jne nomsch
	mov mstein2,1
nomsch: mov mx,cx
	mov my,dx
	cmp ax,1000b
	jne mndelete
	mov mstein,0
	jmp mterm
mndelete:
	mov al,mstein2
	mov mstein,al
mterm:	mov ax,2
	int 33h
	call stb
	mov ax,1
	int 33h
	mov dx,code
	mov es,dx
	mov es:mfirst,0

	pop di
	pop si
	pop ds
	pop es
	pop dx
	pop cx
	pop bx
	pop ax
	ret
mouseproc	endp

mouseinstalled	endp


editor  proc
editanf:
	mov mfirst,1
	cmp mousie,1
	jne emni2
	call mouseinstalled
emni2:	mov glflag,1
        mov dx,code
	mov ds,dx
	mov gxpos1,1
	mov gypos1,1
	mov glx,0
	mov gly,0
	mov glzeiger,0
	mov dx,255
	call glcopy
	mov plshown,0
	call zwiederherstellen
	mov steinpos,10303
	mov ypos,1
	mov xpos,1
	mov position,10
	mov xleft,63
	mov yleft,32
	jmp estart
mfirst	db 0
anzahlreq db 'Anzahl: ','$'
pfanz3	dw 0
pfanz	db 0
nameplease db 'Levelname: ','$'
elevelname  db 11 dup (0)
platz	db 2 dup(0)
lvl	db '.lvl',0
edummy	dw 0
eballs	dw 0
gotanz	db 0
plshown db 0
glflag	db 0
mousie  db 0

cmlshow macro
local cmls
	push cx
	push di
	push ax
	mov steinpos,3876
	mov di,0
	mov ch,0
	mov cl,0
cmls:	mov al,level[di]
	call showstein
	add steinpos,27
	inc di
	inc ch
	cmp ch,9
	jne cmls
	mov ch,0
	add steinpos,6157
	inc cl
	cmp cl,9
	jne cmls
	pop ax
	pop di
	pop cx
        endm

setfield	proc
	mov di,position
	cmp cl,0
	jne donttestonball
	cmp level[di],8
	jne donttestonball
	dec eballs
donttestonball:
	mov level4[di],cl
	mov level[di],cl

	cmp cl,8
	jne enoball
	inc eballs
enoball:cmp cl,13
	je sfend2
	cmp cl,14
	je sfend2
sfend:  mov al,gypos1
	mov ah,0
	mov bx,90
	mul bx
	mov bx,0
	mov bl,gxpos1
	add ax,bx
	mov si,ax
	mov glevel[si],cl
sfend2: ret
setfield	endp

efeldloesch proc
	mov al,ypos
	xor ah,ah
	mov bx,9
	mul bx
	add al,xpos
	mov position,ax
	call feldloesch
	ret
efeldloesch endp

gldarstellen proc
	push ax
	push bx
	push dx
	mov ax,0
	mov al,gly
	mov bx,90
	mul bx
	mov bx,0
	mov bl,glx
	add ax,bx
	mov glzeiger,ax
	mov dx,255
	call glcopy
	call girl
	pop dx
	pop bx
	pop ax
	ret
gldarstellen endp

girl proc
	mov al,cl
	push si
	call showstein
	pop si
	ret
girl endp

ecursorhoch	proc
	cmp al,'w'
	jne echn
	cmp gly,7
	jge e1
	ret
e1:
	sub gly,7
	sub gypos1,7
	call gldarstellen
	ret
echn:	cmp ypos,1
	jbe endech2
echn2:	call efeldloesch
	dec ypos
	dec gypos1
	sub yleft,20
	call setlevelkoordinates
endech: ret
endech2:
	cmp gly,0
	jne noendech
	cmp ypos,1
	je echn2
	ret
noendech:
        mov gldir,-90
	call glcopy
	dec gly
	dec gypos1
	mov move,0
	call girl
	jmp endech
ecursorhoch	endp

ecursorunten	proc
	cmp al,'y'
	jne ecun
	cmp gly,73
	jbe e2
	ret
e2:
	add gly,7
	add gypos1,7
	call gldarstellen
	ret
ecun:	cmp ypos,7
	jae endecu2
ecun2:	call efeldloesch
	inc ypos
	inc gypos1
	add yleft,20
	call setlevelkoordinates
endecu: ret
endecu2:
	cmp gly,81
	jne noendecu
	cmp ypos,7
	je ecun2
	ret
noendecu:
        mov gldir,90
	call glcopy
	inc gly
	inc gypos1
	mov move,0
	call girl
	jmp endecu
ecursorunten	endp

ecursorrechts	proc
	cmp al,'s'
	jne ecur
	cmp glx,73
	jbe e3
	ret
e3:
	add glx,7
	add gxpos1,7
	call gldarstellen
	ret
ecur:	cmp xpos,7
	jae endecr2
ecur2:	call efeldloesch
	inc xpos
	inc gxpos1
	add xleft,27
	call setlevelkoordinates
endecr: ret
endecr2:
	cmp glx,81
	jne noendecr
	cmp xpos,7
	je ecur2
	ret
noendecr:
        mov gldir,1
	call glcopy
	inc glx
	inc gxpos1
	mov move,0
	call girl
	jmp endecr
ecursorrechts	endp

ecursorlinks	proc
	cmp al,'a'
	jne ecul
	cmp glx,7
	jge e4
	ret
e4:
	sub glx,7
	sub gxpos1,7
	call gldarstellen
	ret
ecul:	cmp xpos,1
	jbe endecl2
ecul2:	call efeldloesch
	dec xpos
	dec gxpos1
	sub xleft,27
	call setlevelkoordinates
endecl: ret
endecl2:
	cmp glx,0
	jne noendecl
	cmp xpos,1
	je ecul2
	ret
noendecl:
        mov gldir,-1
	call glcopy
	dec glx
	dec gxpos1
	mov move,0
	call girl
	jmp endecl
ecursorlinks	endp

tastloesch macro
	push dx
	push ax
	mov dx,40
	mov es,dx
	mov ax,001eh
	mov es:001ah,ax
	mov es:001ch,ax
	pop ax
	pop dx
        endm

estart:
	mov ax,balls
	mov eballs,ax
	mov gotanz,0
	mov cl,1
	mov al,cl
	call showstein
	jmp egetloop1
rshift: cmp ax,7300h
	je decrement
	cmp ax,7400h
	jne egetloop1
	cmp cl,14
	jne incr2
	mov cl,37
incr2:  inc cl
	jmp nodec
decrement:
	cmp cl,1
	je is0
	cmp cl,38
	jne odec
	mov cl,15
	jmp odec
is0:    mov cl,39
	jmp nicht15
odec:	dec cl
nodec:
	cmp cl,40
	je ist15
	jmp nicht15
ist15:	mov cl,1
nicht15:
	mov al,cl
	call showstein
egetloop1:
	cmp mousie,1
	jne eegmio2
	mov ax,1
	int 33h
eegmio2:
        tastloesch
	mov ah,0
	int 16h
	cmp mousie,1
	jne eegmio
	push ax
	mov ax,2
	int 33h
	pop ax
eegmio: cmp ax,7300h
	je rshift
	cmp ax,7400h
	je rshift
notshowplayer:
	cmp ah,3ch
	jne notsaveityoufool
	jmp saveityoufool
notsaveityoufool:
	cmp ah,3dh
	jne nogetanzahl
	jmp getanzahl
nogetanzahl:
	cmp ah,48h
	jne en1
	call ecursorhoch
	jmp egetloop1
en1:	cmp al,'w'
	jne enoch
	call ecursorhoch
	jmp egetloop1
enoch:  cmp ah,50h
	jne eno1
	call ecursorunten
	jmp egetloop1
eno1:	cmp al,'y'
	jne enocu
	call ecursorunten
	jmp egetloop1
enocu:	cmp ah,4dh
	jne enocr1
	call ecursorrechts
	jmp egetloop1
enocr1: cmp al,'s'
	jne enocr
	call ecursorrechts
	jmp egetloop1
enocr:	cmp ah,4bh
	jne enoc1
	call ecursorlinks
	jmp egetloop1
enoc1:	cmp al,'a'
	jne enocl
	call ecursorlinks
	jmp egetloop1
enocl:  cmp ah,53h
	jne enodel
	jmp edeletelevel
enodel: cmp ah,4fh
	jne nichtendeeditor
	jmp endeeditor
nichtendeeditor:
	cmp ah,3bh
	jne nichtladen
	jmp eloadalevel
nichtladen:
	cmp ah,49h
	jne nichtladen2
	call showmap
	jmp egetloop1
nichtladen2:
	cmp ah,3eh
	jne nichtladen3
	cmp bvar,26
	je delbo
	mov bvar,26
	call setborder
	jmp egetloop1
delbo:	mov bvar,0
	call setborder
	jmp egetloop1
nichtladen3:
        cmp al,' '
	jne nee3
        call setfield
	call setlevelkoordinates
	cmp cl,13
	jne notplayerset
	jmp playerset
notplayerset:
        cmp cl,14
	jne nee2
	jmp playerset
nee3:	cmp al,13
	jne nee2
	push cx
	mov cl,0
	call setfield
	call setlevelkoordinates
	pop cx
nee2:	jmp egetloop1

setlevelkoordinates	proc
	mov al,xpos
	mov bl,ypos
	xor ah,ah
	xor bh,bh			;levelkoordinaten berechnen...
	mov edummy,ax
	push ax
	push bx
	mov ax,9
	mul bx
	add ax,edummy
	mov position,ax
	pop bx			;und weiter gehts...
	pop ax
	mov dx,27
	mul dx
	push ax
	mov ax,bx
	mov dx,20
	mul dx
	mov dx,320
	mul dx
	pop bx
	add ax,bx
	add ax,3876
	mov steinpos,ax
	mov al,cl
	call showstein
	ret
setlevelkoordinates	endp

getanzahl:
	push ax
	mov ah,9
	mov dx,offset anzahlreq
	int 21h
	tastloesch
	mov ah,1
	int 21h
	cmp al,13
	je unendlich
	sub al,48
	mov byte ptr glevel[8104],al
	tastloesch
	mov ah,1
	int 21h
	sub al,48
	mov byte ptr glevel[8105],al
endgetanzahl:
	pop ax
	tastloesch
	jmp endesave2
unendlich:
	mov byte ptr glevel[8104],20
	mov byte ptr glevel[8105],55
	jmp endgetanzahl

edeletelevel:
	push si
	push dx
	push cx
	mov dx,steinpos
	push dx
	mov si,0
	mov eballs,0
edl1:	mov level[si],0
	mov level2[si],0
	mov level3[si],0
	mov level4[si],0
	inc si
	cmp si,90
	jne edl1
	mov si,0
edl2:   mov glevel[si],0
	mov glevel2[si],0
	inc si
	cmp si,8106
	jne edl2
	mov dx,255
	call load_level
	mov glx,0
	mov gly,0
	mov gxpos1,1
	mov gypos1,1
	mov gypos2,1
	mov gxpos2,7
	mov xpos1,1
	mov ypos1,1
	mov xpos2,1
	mov ypos2,7
	mov glzeiger,0
	call setborder
	pop dx
	mov steinpos,dx
	pop cx
	pop dx
	pop si
	jmp egetloop1
playerset:
	mov dx,code
	mov ds,dx
        cmp cl,14
	je pl2set
	mov si,8100
pl1set: mov dh,gxpos1
	mov dl,gypos1
        mov glevel[si],dh
	inc si
	mov glevel[si],dl
	jmp egetloop1
pl2set: mov dh,gxpos1
	mov dl,gypos1
        mov si,8102
	jmp pl1set

eloadalevel:
	mov dx,steinpos
	push dx
	push cx
	call load_level
	pop cx
	pop dx
	mov steinpos,dx
	delrowmalanders
	jmp editanf

saveityoufool:
	push cx
	mov dx,steinpos
	push dx
	mov gldir,0
	call glcopy
	mov ah,9
	mov dx,offset nameplease
	int 21h
	mov si,0
sloop2: tastloesch
	mov ah,3
	mov bh,0
	int 10h
	cmp dl,19
	jbe sloop3
	dec dl
	mov ah,2
	int 10h
	push dx
	mov dl,' '
	int 21h
	pop dx
	int 10h
	mov si,8
sloop3: mov ah,1
	int 21h
	cmp al,8
	jne nbs
backspace:
        cmp si,0
	jne nsloop2
	mov ah,3
	mov bh,0
	int 10h
	inc dl
	mov ah,2
	int 10h
	jmp sloop2
nsloop2:
        mov ah,2
	mov dl,' '
	int 21h
	mov ah,3
	mov bh,0
	int 10h
	mov ah,2
	dec dl
	int 10h
	dec si
	jmp sloop2
nbs:	cmp al,27
	jne sloop4
	jmp endesave
sloop4: cmp al,13
	je namefertig
	cmp al,48
	jae mo65
	jmp sloop2
mo65:	cmp al,122
	jbe le122
	jmp sloop2
le122:  mov elevelname[si],al
	inc si
	jmp sloop2
namefertig:
	cmp elevelname,13
	je endesave
        mov di,0
nfertig2:
	mov al,lvl[di]
	mov elevelname[si],al
	inc si
	inc di
	cmp di,5
	jne nfertig2

	mov ah,60
	mov dx,offset elevelname
	mov cx,0
	int 21h
	mov bx,ax
	mov ah,64
	mov cx,8106
	mov dx,offset glevel
	int 21h
	mov ah,62
	int 21h
endesave:
        pop dx
	mov steinpos,dx
	pop cx
endesave2:
        delrowmalanders
	jmp egetloop1
endeeditor:
	cmp mousie,1
	jne endeenor1
	mov ax,2
	int 33h
	mov frominit,1
	call hintergrund
endeenor1:
        mov si,0
ecopyloop1:
	mov al,level4[si]
	mov level[si],al
	mov level2[si],al
	mov level3[si],al
	inc si
	cmp si,81
	jne ecopyloop1
	mov si,0
ecopyloop2:
	mov al,glevel[si]
	mov glevel2[si],al
	inc si
	cmp si,8106
	jne ecopyloop2
	mov glflag,0
	ret
editor	endp
