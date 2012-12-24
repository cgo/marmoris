puffer0 db 19950 dup (0)

steinname db 'steine.gme',0
loadsteine proc
	mov ah,61
	mov dx,code
	mov ds,dx
	mov dx,offset steinname
	mov al,0
	int 21h
	mov bx,ax
	mov ah,63
	mov cx,19950
	mov dx,offset puffer0
	int 21h
	mov ah,62
	int 21h
	ret
loadsteine endp


