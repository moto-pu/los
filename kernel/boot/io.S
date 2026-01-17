/* io.S - I/O port access functions for Haskell FFI */

.section .text

/* uint8_t inb(uint16_t port) */
.global inb
.type inb, @function
inb:
    mov 4(%esp), %dx    /* port */
    xor %eax, %eax
    inb %dx, %al
    ret

/* void outb(uint16_t port, uint8_t value) */
.global outb
.type outb, @function
outb:
    mov 4(%esp), %dx    /* port */
    mov 8(%esp), %al    /* value */
    outb %al, %dx
    ret
