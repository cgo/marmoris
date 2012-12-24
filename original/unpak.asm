upfehlertext db 'Bitte Marmoris-Diskette einlegen$'
unperror proc
	mov ah,02h
	mov bh,0
	mov dx,0
	int 10h
	mov dx,code
	mov ds,dx
	mov ah,12h
	mov bl,36h
	mov al,0
	int 10h
	mov ah,9
	mov dx,offset upfehlertext
	int 21h
	mov dx,40h
	mov es,dx
	mov ax,001eh
	mov es:001ah,ax
	mov es:001ch,ax
unperl1:
	mov ah,1
	int 16h
	jz unperl1
	mov ah,12h
	mov bl,36h
	mov al,1
	int 10h
	mov dx,screen
	mov es,dx
	ret
unperror endp

load_palette proc
beginloadpal:
        mov ah,61
	mov dx,offset palname
	mov al,0
	int 21h
	jc palloaderror
	mov bx,ax
	mov ah,63
	mov cx,765
	mov dx,offset lppuffer
	int 21h
	mov ah,62
	int 21h
        mov ax,1012h
	mov bx,0
	mov cx,256
	mov dx,daten
	mov es,dx
	mov dx,offset lppuffer
	int 10h
palloaderror:
	call unperror
	jmp beginloadpal
	ret
load_palette endp

unpak	proc
unpbegin:
        mov dx,screen
	mov es,dx
	mov ah,26
	mov dx,offset dta
	int 21h
	mov ah,78
	mov cx,0
	mov dx,offset dateiname
	int 21h
	mov ah,61
	mov al,0
	mov dx,offset dateiname
	int 21h
	jc upfehler
	jmp upnf
upfehler:
	call unperror
	jmp unpbegin
upnf:	mov bx,ax
	mov ah,63
	mov cx,word ptr dta[26]
	mov dx,daten
	mov ds,dx
	mov dx,offset unpuffer
	int 21h
	mov ah,62
	int 21h

	mov si,0
	mov di,0

searchpak:
	cmp ds:unpuffer[si],'P'
	jne nopak
	cmp ds:unpuffer[si+1],'a'
	jne nopak
	cmp ds:unpuffer[si+2],'K'
	je packed
nopak:  mov al,ds:unpuffer[si]
	mov es:dot[di],al
	inc si
	inc di
	cmp di,64000
	jne searchpak
	jmp endsearch
packed: add si,3
	mov al,ds:unpuffer[si]
	inc si
	mov dl,ds:unpuffer[si]
	inc si
	mov dh,ds:unpuffer[si]
	inc si
setblack:
        mov es:dot[di],al
updontch:
        dec dx
	inc di
	cmp di,64000
	je endsearch
	cmp dx,0
	jne setblack
	jmp searchpak
endsearch:
	mov dx,code
	mov ds,dx
	ret
unpak	endp
