all: client-fftw-cpp FastestFT

FastestFT:
	cabal build

client-fftw-cpp: client-fftw.cpp
	g++ client-fftw.cpp -O3 -lfftw3 -o client-fftw-cpp

bench:
	./dist/build/judge/judge -o bench.log -u bench.csv

.phony: FastestFT bench
