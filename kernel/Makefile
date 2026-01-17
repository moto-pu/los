# LOS Kernel Makefile
# Builds a bare-metal Haskell kernel for x86

# Cross-compiler settings
AS = i686-elf-as
LD = i686-elf-ld

# GHC settings for bare-metal (requires custom RTS)
GHC = ghc
GHCFLAGS = -O2 -fno-stack-protector -fPIC \
           -optc-nostdlib -optc-fno-builtin \
           -optl-nostdlib

# Source files
ASM_SOURCES = boot/boot.S boot/io.S
HS_SOURCES = $(wildcard src/Kernel/*.hs)

# Output
KERNEL = los-kernel
ISO = los.iso

# Object files
ASM_OBJECTS = $(ASM_SOURCES:.S=.o)

.PHONY: all clean iso run

all: $(KERNEL)

# Assemble boot code
boot/%.o: boot/%.S
	@echo "AS $<"
	@$(AS) $< -o $@

# Build Haskell kernel
# Note: This requires a custom GHC RTS for bare-metal
# For now, this is a placeholder - actual implementation requires
# significant work to port GHC RTS to bare-metal
kernel.o: $(HS_SOURCES)
	@echo "GHC (bare-metal) - requires custom RTS"
	@echo "This is a PoC structure. Full implementation pending."
	@touch $@

# Link kernel
$(KERNEL): $(ASM_OBJECTS) kernel.o linker.ld
	@echo "LD $@"
	@$(LD) -T linker.ld -o $@ $(ASM_OBJECTS) kernel.o 2>/dev/null || \
		echo "Note: Full linking requires GHC bare-metal RTS"

# Create bootable ISO
iso: $(KERNEL)
	@echo "Creating bootable ISO..."
	@mkdir -p isodir/boot/grub
	@cp $(KERNEL) isodir/boot/$(KERNEL) 2>/dev/null || \
		echo "Warning: Kernel binary not available"
	@cp boot/grub/grub.cfg isodir/boot/grub/
	@grub-mkrescue -o $(ISO) isodir 2>/dev/null || \
		echo "Note: grub-mkrescue not available"
	@rm -rf isodir

# Run in QEMU
run: iso
	qemu-system-i386 -cdrom $(ISO)

# Run with serial console output (for debugging)
run-serial: iso
	qemu-system-i386 -cdrom $(ISO) -serial stdio

# Clean build artifacts
clean:
	rm -f boot/*.o kernel.o $(KERNEL) $(ISO)
	rm -rf isodir

# Show help
help:
	@echo "LOS Kernel Build System"
	@echo ""
	@echo "Targets:"
	@echo "  all        - Build kernel (requires bare-metal GHC setup)"
	@echo "  iso        - Create bootable ISO"
	@echo "  run        - Run in QEMU"
	@echo "  run-serial - Run in QEMU with serial console"
	@echo "  clean      - Remove build artifacts"
	@echo ""
	@echo "Prerequisites:"
	@echo "  - i686-elf cross-compiler"
	@echo "  - GHC with bare-metal RTS (House-style)"
	@echo "  - GRUB and xorriso for ISO creation"
	@echo "  - QEMU for emulation"
