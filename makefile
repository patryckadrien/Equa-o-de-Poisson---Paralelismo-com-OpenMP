FC = gfortran
FFLAGS = -O2 -fopenmp -fexpensive-optimizations -m64 -foptimize-register-move -funroll-loops -ffast-math -mtune=native -march=native
#FC = ifort
#FFLAGS = -O2 -qopenmp -mp1 -zero -xHOST -ipo -align 
#array64byte
#FFLAGS = -O2 -qopenmp -xW -align -fno-alias
OBJ = inicializa.o cond_ini.o cond_con.o sol_ex.o met_gs.o poisson.o

%.o: %.f90
	$(FC) $(FFLAGS) -c -o $@ $<

poisson: $(OBJ)
	$(FC) $(FFLAGS) -o poisson $(OBJ)

.PHONY:clean

clean:
	rm  poisson *.o *.mod

