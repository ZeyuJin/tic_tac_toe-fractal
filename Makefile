%:	%.f90
	gfortran -fcheck=bounds $< -o $*
