# VGA-EZ80: An experiment in eZ80 video signal generation

Check out this video of [60fps framebuffer scrolling](https://www.youtube.com/shorts/ev4wi_WDekA),
all done by the eZ80 CPU.

![256_color_text](pictures/PXL_20250428_text.jpg)

## What is this?

This is an experimental framebuffer video driver for the Agon
Light. The goal is to have 8-bit (256 colour RGB332) VGA output
directly from the main CPU (eZ80F92), as a more fun alternative
to the serial link display protocol of the VDP (esp32). This would
let programmers just draw pixels to ez80 memory, indulge in
cycle-counting optimisations, horizontal sync effects (like framebuffer
offsets per scanline for wibble-wobble effects), etc.

## How's that going to work?

It all hinges on the eZ80 `otirx` instruction, which is a bit
like `ldir` except that the output goes to a fixed IO port rather
than to another memory location. `otirx` takes 3 cycles per byte.
If you use a GPIO IO port, that means you can scan out 8-bit pixels
at CPU clock / 3. On Agon Light that means 18.432Hz / 3 = 6.14MHz
pixel clock.

That's a pretty low pixel clock for VGA timings. Sticking to standard 60Hz 640x480,
a 6.14MHz pixel clock gives you around 156 pixels per scanline. So
you get a super-wonky resolution of 156x480. In practice I implemented
double, triple, and quad-scanning. So more useful resolutions of
156x240, 156x160 and 156x120 were achieved. I also experimented with
15khz timings, which one of my monitors accepted. That gave 310 pixel
wide modes. Very nice.

## Timings

To follow VGA 64x480x60Hz timings, we have the following clock cycle
budget per scanline on the eZ80:

```
Total scanline:    586 cycles (31.792 us)
    hsync pulse:    71 cycles
    front porch:    35 cycles
    pixel data:    468 cycles
    back porch      12 cycles
```

## What about the wiring to a VGA port?

I used the following GPIOS:
GPIO D pin 7: hsync
GPIO D pin 6: vsync
GPIO C pin 0-7: pixel data (RGB332)

```
eZ80 GPIOD-PIN6 ------------------------ VGA vsync
eZ80 GPIOD-PIN7 ------------------------ VGA hsync

eZ80 GPIOC-PIN0 -----{R 1000Ω}---+------- VGA blue
                                |
eZ80 GPIOC-PIN1 -----{R 510Ω}---|

eZ80 GPIOC-PIN2 -----{R 2000Ω}--+------- VGA green
                                |
eZ80 GPIOC-PIN3 -----{R 1000Ω}---+
                                |
eZ80 GPIOC-PIN4 -----{R 510Ω}---+

eZ80 GPIOC-PIN5 -----{R 2000Ω}--+------- VGA red
                                |
eZ80 GPIOC-PIN6 -----{R 1000Ω}---+
                                |
eZ80 GPIOC-PIN7 -----{R 510Ω}---+
```

## The prototypes

### Without Interrupts

See [ideal31khz.asm](ideal31khz.asm).

The eZ80F92 has no DMA capabilities, so you need to hog the
CPU to do video scanout. I had 2 prototypes: one that simply ran in
a loop, using 100% CPU, to achieve ideal timing (within 0.03% of VGA
timings). Both my monitors could sync to this. Unfortunately you
can't run application code alongside this, so...

### With Interrupts

See [gpiovideo.asm](gpiovideo.asm) and [31khz.asm](31khz.asm).

The other prototype is interrupt-based. It used eZ80F92 PRT timers
to interrupt user code in order to output the sync signal, while 
the picture section of the scanout had to be done at 100% CPU since
there was no time left between sync and picture to really yield
to an application. This left less than 6% CPU for the user programs,
but that might still be more grunt than a 4MHz Z80 of bygone times.

The PRT interrupt handler is subject to 'random' jitter of up to
9 cycles due to the latency of completing the current instruction
before servicing the interrupt. Despite this, the VGA timing of
the interrupt-driven VGA driver is as precise as the no-interrupts
version, thanks to some jitter-correcting code on interrupt entry.

### More free CPU with 640x350

VGA standard 640x350@70Hz timings allow more time for application code,
since there are more blank scanlines in this resolution (25%).
I measured 15% CPU free in these modes.

## The project in images

Here we have the first Eureka! An image on screen!
![first_image](pictures/PXL_20250415_first_image.jpg)

Scanning out some real framebuffer data (actually it's the MOS ROM :) )
![rom_scanout](pictures/PXL_20250415_rom_scanout.jpg)

The resistor ladder is fully populated now. Here's all 256 colours:
![256_colours](pictures/PXL_20250418_256_colours.jpg)

Some text drawing routines in 256 colours:
![256_color_text](pictures/PXL_20250428_text.jpg)

Experiments in framebuffer-enabled MOS. Some programs don't like the framebuffer
sitting in RAM they expect to be free, hence the screen corruption and
crash dump :)
![mos_integration](pictures/PXL_20250428_mos_integration.jpg)

The completed wiring. My highschool electronics knowledge was enough to
avoid burning out the Agon.
![the_rats_nest](pictures/PXL_20250504_the_rats_nest.jpg)
