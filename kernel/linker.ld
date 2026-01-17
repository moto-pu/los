/* linker.ld - Kernel linker script */

ENTRY(_start)

SECTIONS
{
    /* Load kernel at 1MB (common for x86 kernels) */
    . = 1M;

    /* Multiboot header must come first */
    .multiboot ALIGN(8) :
    {
        *(.multiboot)
    }

    /* Text section (code) */
    .text ALIGN(4K) :
    {
        *(.text)
    }

    /* Read-only data */
    .rodata ALIGN(4K) :
    {
        *(.rodata)
    }

    /* Initialized data */
    .data ALIGN(4K) :
    {
        *(.data)
    }

    /* Uninitialized data (BSS) */
    .bss ALIGN(4K) :
    {
        *(COMMON)
        *(.bss)
    }

    /* Kernel end marker */
    _kernel_end = .;
}
