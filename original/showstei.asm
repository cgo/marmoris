showstein proc
	mov dx,code
	mov ds,dx
	push di
	push si
	mov ah,0
        mov bx,475
	mul bx
        mov si,ax
        mov dx,screen
	mov es,dx
	mov di,steinpos
	mov dh,0
	mov dl,0
	jmp showloop1
showloop2:
	add di,295
	mov dl,0
	inc dh
showloop1:
        mov ah,ds:puffer0[si]
        mov es:dot[di],ah
	inc si
	inc di
	inc dl
	cmp dl,25
	jne showloop1
	cmp dh,18
	jne showloop2
	pop si
	pop di
        ret
showstein endp
