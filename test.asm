; nasm -o out.o -f elf64 test.asm
; ld -melf_x86_64 -I/lib64/ld-linux-x86-64.so.2 -lpthread -luring -lrt -ldl -lm -lc -o out.elf out.o obj/debug/runtime.o obj/debug/std_io.o obj/debug/std_string.o
; ./out.elf

extern exit
extern brad_print
extern brad_init
extern brad_deinit

section .data

brad_str dq 14 
db "Hello World!", 10, 0

section .text

global _start
_start:
	call brad_init
	call brad_main
	call brad_deinit
	call exit

brad_main:
	mov qword [RSP - 16], RBX
	mov qword [rsp - 24], rbp
	sub RSP, 16
	mov RAX, 1
	mov RBX, 2
	mov RCX, RAX
	add RCX, RBX
	mov RAX, RCX


	mov rbp, rsp
	and rsp, 0xfffffffffffffff0
	lea rdi, [brad_str]
	call brad_print
	mov rsp, rbp

	add RSP, 16
	mov RBX, qword [RSP - 16]
	mov rbp, qword [rsp - 24]
	ret

section .note.GNU-stack