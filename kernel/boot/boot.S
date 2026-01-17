/* boot.S - Multiboot2 compliant kernel entry point */

.set MAGIC,    0xE85250D6          /* Multiboot2 magic number */
.set ARCH,     0                   /* i386 protected mode */
.set HEADER_LENGTH, header_end - header_start
.set CHECKSUM, -(MAGIC + ARCH + HEADER_LENGTH)

/* Multiboot2 header */
.section .multiboot
.align 8
header_start:
    .long MAGIC
    .long ARCH
    .long HEADER_LENGTH
    .long CHECKSUM

    /* End tag */
    .word 0    /* type */
    .word 0    /* flags */
    .long 8    /* size */
header_end:

/* Stack */
.section .bss
.align 16
stack_bottom:
    .skip 16384  /* 16 KiB stack */
stack_top:

/* Entry point */
.section .text
.global _start
.type _start, @function
_start:
    /* Set up stack */
    mov $stack_top, %esp

    /* Reset EFLAGS */
    pushl $0
    popf

    /* Push multiboot info pointer and magic number */
    push %ebx   /* Multiboot info structure pointer */
    push %eax   /* Multiboot magic number */

    /* Call kernel main */
    call kernel_main

    /* If kernel_main returns, halt */
    cli
.hang:
    hlt
    jmp .hang

.size _start, . - _start
