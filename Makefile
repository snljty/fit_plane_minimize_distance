# Makefile for fit_plane_minimize_distance

SHELL = cmd

FILENAME = fit_plane_minimize_distance
MODNAME = fit_plane_pca_module

FC = gfortran
FLINKER = $(FC)
ARCH = ar
ARCHFLAGS = -rsc
OPTS = -s
LAPACKROOT := C:\lapack-3.12.0
LIBPATH := -L $(LAPACKROOT)\lib
LIB = -l lapack -l blas

.PHONY: all
all: lib$(FILENAME).a $(MODNAME).mod

lib$(FILENAME).a: $(FILENAME).obj
	@echo Generating archive $@ from $^ ...
	$(ARCH) $(ARCHFLAGS) $@ $^

$(MODNAME).mod: $(FILENAME).obj

$(FILENAME).obj: $(FILENAME).f90
	@echo Compiling $@ from $< ...
	$(FC) -o $@ -c $< $(OPTS) -cpp

.PHONY: test
test: test.exe
	.\$<

test.exe: test.obj lib$(FILENAME).a
	@echo Linking $@ against $^ ...
	$(FLINKER) -o $@ $< -L . -l $(FILENAME) $(LIBPATH) $(LIB) $(OPTS) -static

test.obj: test.f90 $(MODNAME).mod
	@echo Compiling $@ from $< ...
	$(FC) -o $@ -c $< $(OPTS)

.PHONY: veryclean
veryclean: clean clean_test

.PHONY: clean
clean:
	-del /q lib$(FILENAME).a $(MODNAME).mod $(FILENAME).obj 2> NUL

.PHONY: clean_test
clean_test:
	-del /q test.exe test.obj 2> NUL

