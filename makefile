CC = ocamlc
CFLAGS =
LDFLAGS = gdnb.ml calc.ml
OBJECTS = gdnb.cmo gdnb.cmi calc.cmo calc.cmi
executable = calc

all : executable

executable : $(LDFLAGS) 
	$(CC) -o $(executable) $(LDFLAGS) 

gdnb.cmo : gdnb.cmi gdnb.ml
	ocamlc -c gdnb.ml
gdnb.cmi : gdnb.ml
	ocamlc -c gdnb.ml
calc.cmo : gdnb.cmo calc.ml
calc.cmi : gdnb.cmo calc.ml

doc/index.html :
	ocamldoc -html interfaces/gdnb.mli interfaces/entiermod.mli \
	interfaces/polynome.mli -d doc

doc : gdnb.cmo gdnb.cmi doc/index.html

clean :
# sous Linux
	rm -f $(executable) $(executable).cmo $(executable).cmi

cleandoc :
# sous Linux
	rm -f doc/*.html doc/*.css
