FC=gfortran
FFLAGS=-O3 

%.o:%.f95
	$(FC) $(FFLAGS) -c -o $@ $<
	
poisson: inicializa.o cond_ini.o cond_con.o sol_ex.o met_gs.o poisson.o 
	$(FC) -o poisson inicializa.o cond_ini.o cond_con.o sol_ex.o met_gs.o poisson.o 

