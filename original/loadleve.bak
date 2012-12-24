level  db 89 dup (?)
level2 db 89 dup (?)
level3 db 89 dup (?)
level4 db 89 dup (?)
dummy  db 0
dummy2 db 0
levelnames db 5000 dup (?)
levelname	db 14 dup (?)
llinput db 'Level:','$'
llerror db 'Fehler im Level ','$'
level1name db '*.lvl',0
levelpointer db 0
lnothing db 0
nthtext db 'Kein Level gefunden... $'
DTA	db 43 dup (0)
rowdel2 db '                        ','$'

delrowmalanders macro
	local ellds1
	push ax
	push dx
	push si
	mov dx,daten
	mov ds,dx
	mov dx,screen
	mov es,dx
	mov ax,0
	mov bh,0
	mov si,0
ellds1: mov bl,ds:unpuffer[si]
	mov es:dot[si],bl
	inc si
	inc ax
	cmp ax,293
	jne ellds1
	mov ax,0
	inc bh
	add si,27
	cmp bh,8
	jne ellds1
	mov dx,code
	mov ds,dx
	pop si
	pop dx
	pop ax
endm

tastl2	macro
	mov dx,40h
	mov es,dx
	mov ax,001eh
	mov es:001ah,ax
	mov es:001ch,ax
	endm

delrow2 proc
	push ax
	push bx
	push dx
	mov ah,2
	mov bh,0
	mov dh,0
	mov dl,6
	int 10h
	mov dx,offset rowdel2
	mov ah,9
	int 21h
	mov ah,2
	mov bh,0
	mov dh,0
	mov dl,6
	int 10h
	pop dx
	pop bx
	pop ax
	ret
delrow2 endp

load_level proc
	mov glmem1,10000
	mov glmem2,10000
	mov lnothing,0
	cmp dx,255
	jne notdislev
	jmp display_level_2
notdislev:
        mov ah,9
	mov dx,offset llinput
	int 21h
llnloop1:
	mov ah,26
	mov dx,offset DTA
	int 21h
	mov ah,78
	mov cx,0
	mov dx,offset level1name
	int 21h
	jc nothingfound
	mov di,0
neon2:  mov si,30
neon:	mov dl,DTA[si]
        inc si
        mov levelnames[di],dl
	inc di
	cmp dl,0
	je endofname
	jmp neon
endofname:
        mov ah,79
	int 21h
	jnc neon2
	jmp sthfound
nothingfound:
	mov dx,code
	mov ds,dx
	mov ah,2
	mov bh,0
	mov dx,0
	int 10h
	mov dx,offset nthtext
	mov ah,9
	int 21h
	tastl2
nthl1:  mov ah,1
	int 16h
	jz nthl1
	mov ah,2
	mov bh,0
	mov dx,0
	int 10h
	delrowmalanders
	mov ah,2
	mov bh,0
	mov dx,0
	int 10h
	ret
sthfound:
	mov levelnames[di],'$'
selectlevel:
	mov si,0
selloop2:
	mov di,si
	call delrow2
selloop1:
	mov dl,levelnames[si]
	cmp dl,'.'
	je nextname
	cmp dl,'$'
	je selectlevel
	mov ah,2
	int 21h
	inc si
	jmp selloop1
nextname:
	inc si
	inc si
	inc si
	inc si
	inc si
	tastl2
	mov ah,0
	int 16h
	cmp al,27
	jne notendlev
	call delrow
	mov ah,2
	mov bh,0
	mov dx,0
	int 10h
	ret
notendlev:
	cmp al,13
	jne notlsel
        jmp loadselected
notlsel:
	cmp al,' '
	jne nextname
	call delrow2
	jmp selloop2

loadselected:
	mov si,0
lscloop1:
        mov al,levelnames[di]
	mov levelname[si],al
	inc si
	inc di
	cmp al,0
	jne lscloop1
	tastl2
	mov dx,code
	mov ds,dx
	mov ah,61
	mov dx,offset levelname
	mov al,0
	int 21h
	jnc nolllerror
	jmp lllerror
nolllerror:
	mov bx,ax
	mov ah,63
	mov cx,8106
	mov dx,offset glevel
	int 21h
	jnc nollllerror
	jmp lllerror
nollllerror:
	mov ah,62
	int 21h
	mov al,glevel[8103]
	mov gypos2,al
	mov gly,al
	sub gly,4
	mov al,glevel[8102]
	mov gxpos2,al
	mov glx,al
	sub glx,4
	mov dx,255
	call glcopy
	mov al,glevel[8100]
	mov gxpos1,al
	mov al,glevel[8101]
	mov gypos1,al
	mov eballs,0
	mov si,0
copyloop2:
	mov al,glevel[si]
	mov glevel2[si],al
	cmp al,8
	jne llnoball
	inc eballs
llnoball:
	inc si
	cmp si,8106
	jne copyloop2
	mov di,0
copyloop1:
	mov al,level[di]
	mov level4[di],al
	mov level2[di],al
	mov level3[di],al
sobr2:	inc di
	cmp di,90
	jne copyloop1
	jmp display_level_2
lllerror:
	mov dl,al
	mov ah,2
	int 21h
        mov dx,offset llerror
	mov ah,9
	int 21h
	mov ah,4ch
	int 21h
display_level_2:
	mov dx,255
	call glcopy
	mov steinpos,3876
        mov di,0
	mov dummy,0
	mov dummy2,0
display_level:
	mov dx,code
	mov ds,dx
        mov al,level[di]
        call showstein
        inc di
	inc dummy
dl2:	mov al,dummy2
	xor ah,ah
	mov bx,6400
	mul bx
	add ax,3876
	push ax
	mov al,dummy
	xor ah,ah
	mov bx,27
	mul bx
	pop bx
	add ax,bx
	mov steinpos,ax
	cmp dummy,9
	jne display_level
	inc dummy2
	mov dummy,0
	cmp dummy2,9
	jne dl2
	mov dh,glevel[8100]
	mov gxpos2,dh
	mov dh,glevel[8101]
	mov gypos2,dh

        mov dx,eballs
	mov balls,dx
	mov dh,glevel[8102]
	mov gxpos1,dh
	mov glx,dh
	sub glx,4
	mov dh,glevel[8103]
	mov gypos1,dh
	mov gly,dh
	sub gly,4

	mov position1,41

	mov bl,5
	xor bh,bh
	mov ax,27
	mul bx
	add ax,36
	mov xleft1,ax
	mov bl,4
	xor bh,bh
	mov ax,20
	mul bx
	add ax,12
	mov yleft1,ax
	mov dh,glevel[8104]
	mov al,dh
	xor ah,ah
	mov bl,10
	mul bl
	add al,glevel[8105]
	mov pfanz,al
	mov ax,eballs
	mov balls,ax
	mov ah,2
	mov bh,0
	mov dx,0
	int 10h
	ret
load_level endp
