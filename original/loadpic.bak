dateiname db 'game3.pak',0
lppuffer db 765 dup (?)
zerop	db 765 dup (0)
handle dw 0
lpspeicher dw 0
palname db 'palette',0
lpdummy db 2
lpfehler db 'Bitte Marmoris-Diskette einlegen$'
loadpicture proc
lp1:
	mov dx,code
	mov ds,dx
	mov ah,61
	mov dx,offset palname
	mov al,0
	int 21h
	jc lpfehlerr
	mov handle,ax
	mov lpspeicher,0
	jmp lploop1
lpfehlerr:
	;call delrow
	mov ah,9
	mov dx,offset lpfehler
	int 21h
	mov dx,40h
	mov es,dx
	mov ax,es:001eh
	mov es:001ah,ax
	mov es:001ch,ax
lpfl:	mov ah,1
	int 16h
	jz lpfl
	jmp lp1
lploop1:
	mov bx,handle
	mov ah,63
	mov cx,765
	mov dx,offset lppuffer
	int 21h
	mov ah,62
	int 21h

	mov si,0
delzerp2:mov cs:zerop[si],0
	inc si
	cmp si,765
	jne delzerp2

setpalmac macro
	push ax
	push bx
	push cx
	push si
	push es
        mov ah,10h
	mov al,12h
	mov bx,0
	mov cx,256
	mov dx,code
	mov es,dx
	mov dx,offset zerop
	int 10h
	pop es
	pop si
	pop cx
	pop bx
	pop ax
endm
	setpalmac

	call unpak

	push cs
	pop ds
outerl1:mov si,0
	mov bx,0
innerl1:mov al,lppuffer[si]
	cmp zerop[si],al
	je pmatched
	inc zerop[si]
	mov bx,1
pmatched:inc si
	cmp si,765
	jne innerl1
	push bx
	setpalmac
	pop bx
	mov ah,86h
	mov cx,0
	mov dx,300
	int 15h 	    ;delay
	cmp bx,0
	jne outerl1

	mov dx,code
	mov ds,dx
	mov dx,screen
	mov es,dx
ende:   ret
loadpicture endp
