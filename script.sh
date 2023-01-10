#!/bin/bash

source /opt/intel/oneapi/setvars.sh

sed -i 's/integer, parameter :: n=480, m=480/integer, parameter :: n=160, m=160/g' poisson.f90


for i in $(seq 1 4)
do

	make clean

	export OMP_NUM_THREADS=$i

	make
	
	echo ''$i' Threads' | tee saida.out

	time ./poisson | tee saida.out

done
