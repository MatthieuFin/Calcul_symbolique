CC = ocamlc
CFLAGS =
LDFLAGS = gdnb.ml calc.ml
OBJECTS = gdnb.cmo gdnb.cmi calc.cmo calc.cmi
executable = calc

all : executable

executable : $(LDFLAGS) 
	$(CC) -o $(executable) $(LDFLAGS) 

gdnb.cmo : gdnb.cmi gdnb.ml
gdnb.cmi : gdnb.ml
calc.cmo : gdnb.cmo calc.ml
calc.cmi : gdnb.cmo calc.ml


clean :
# sous Linux
	rm -f $(executable) $(executable).cmo $(executable).cmi

#ocamlc -o calc gdnb.ml calc.ml
