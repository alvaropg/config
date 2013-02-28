#!/bin/bash

export CFLAGS="-O2 -march=amdfam10 -mcx16 -mpopcnt -pipe"
export CXXFLAGS="${CFLAGS}"
export CHOST="x86_64-pc-linux-gnu"
export USE="3dnow 3dnowext mmx mmxext sse sse2 sse3 sse4a"
