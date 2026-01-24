#!/bin/bash
# This works on agon via 'exec build.sh' and also on Linux :)

ez80asm gpiovideodriver.asm
ez80asm rst20_api_example.asm
ez80asm examples/saturn.asm
ez80asm examples/example-z80.asm
ez80asm examples/signaltest.asm
ez80asm examples/example-virtualfb.asm
