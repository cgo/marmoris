freq    dw 0
fiepsl  dw 0
fieps proc
fiepsstart:
s1:
	mov di,freq
	mov bx,3
	mov al,182
	out 43h,al
	mov dx,12h

	mov ax,di

	out 42h,al
	mov al,ah
	out 42h,al
	in al,61h
	mov ah,al
	or al,3
	out 61h,al
fiepswarte:
	mov cx,0
	mov dx,fiepsl
lab1:   mov ah,86h
	int 15h
	dec bx
	jnz fiepswarte

	mov al,ah
	out 61h,al
	ret
fieps endp


