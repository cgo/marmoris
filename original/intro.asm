intro   proc
	mov introzeiger,0
	jmp introstart
introtext db 'Marmoris',10,13,'$(c) 1991  Christian Gosch und Frederik Armknecht',10,13,'$PC-Grafik: Christian Gosch'
	db 10,13,'$Idee und Konzept: Frederik Armknecht und Christian Gosch'
	db 10,13,'$Amiga Version: Frederik Armknecht',10,13,'$PC Version: Christian Gosch'
	db 10,13,'$&'

introzeiger  dw 0
introy  db 0
introstart:
	mov dx,code
	mov ds,dx
	mov introy,0
	mov ax,0003h
	int 10h
	mov ah,1
	mov ch,1
	mov cl,0
	int 10h
	mov ah,10h
	mov al,10h
	mov bx,7
	mov ch,0
	mov dh,0
	mov cl,0
	int 10h
	mov si,0
	mov di,introzeiger
introloop1:     cmp introtext[di],'$'
	je ihabeanzahl
	inc si
	inc di
	jmp introloop1
ihabeanzahl:
	mov bx,80
	sub bx,si
	shr bx,1
	mov di,0
introloop2:  mov ah,2
	mov dl,' '
	int 21h
	inc di
	cmp di,bx
	jne introloop2

	mov di,introzeiger
introloop5:  mov ah,2
	mov dl,introtext[di]
	cmp dl,'$'
	je introtextaus
	int 21h
	inc di
	jmp introloop5
introtextaus:
	add introzeiger,si
	inc introzeiger
	inc di
	mov si,0
	cmp introtext[di],'&'
	jne introloop1




	mov cx,0
	mov dh,0
introloop3:     mov ah,10h
	mov al,10h
	mov bx,7
	int 10h
	inc cl
	mov si,2000
introloop6:     dec si
	cmp si,0
	jne introloop6
	cmp cl,50
	jne introloop3

	mov dx,40h
	mov es,dx
noiende:
	mov ax,es:001eh
	mov es:001ah,ax
	mov es:001ch,ax
	mov ah,0
	int 16h
	cmp al,27
	jne noiende

introloop7:
	mov ah,10h
	mov al,15h
	mov bx,7
	int 10h
	dec cl
	mov ah,10h
	mov al,10h
	mov bx,7
	int 10h
	mov si,2000
introloop8:     dec si
	cmp si,0
	jne introloop8
	cmp cl,0
	jne introloop7
iende:  ret
intro   endp
