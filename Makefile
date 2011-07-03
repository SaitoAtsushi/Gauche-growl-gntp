GAUCHE = gosh
GAUCHE_LIB_DIR = /usr/local/gauche/share/gauche-0.9/0.9.1/lib

all:

install:
	cp -R src/growl/ $(GAUCHE_LIB_DIR)
