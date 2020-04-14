main: main.f95 func_mod.o prec_mod.o
	gfortran $^ -o $@ 
func_mod.o: func_mod.f95 prec_mod.o
	gfortran $^ -c
prec_mod.o: prec_mod.f95
	gfortran $^ -c
clear:
	rm -f *.o *.mod 