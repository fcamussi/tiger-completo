# Unix makefile for tigermain example

HOME=/usr/bin
MOSMLHOME=${HOME}
MOSMLTOOLS=camlrunm /usr/share/mosml/tools
MOSMLLEX=mosmllex
MOSMLYACC=mosmlyac -v

AS=sdasz80
CC=sdcc -mz80
CFLAGS= -g
MOSMLC=${MOSMLHOME}/mosmlc -c -liberal
MOSMLL=${MOSMLHOME}/mosmlc

# Unix
REMOVE=rm -f
MOVE=mv
EXEFILE=

# DOS
#REMOVE=del
#MOVE=move
#EXEFILE=.exe

.SUFFIXES :
.SUFFIXES : .sig .sml .ui .uo

GRALOBJS= tigerabs.uo tigergrm.uo tigerlex.uo tigermain.uo \
	tigernlin.uo tigerpp.uo tigerescap.uo tigertab.uo tigerseman.uo tigertemp.uo tigertree.uo \
	tigerframe.uo tigertrans.uo tigerit.uo tigerpila.uo tigertopsort.uo tigercanon.uo  \
	tigerassem.uo tigercodegen.uo tigergraph.uo tigercontrolflow.uo tigerliveness.uo tigercolor.uo \
    tigersugar.uo tigerspill.uo

all: tiger runtime.o

runtime.o: runtime.c
	$(AS) -o crt0.s
	$(CC) -c runtime.c

tiger: $(GRALOBJS) $(OBJSGEN)
	$(MOSMLL) -o tiger $(EXEFILE) tigermain.uo

tigergrm.sml tigergrm.sig: tigergrm.y 
	$(MOSMLYACC) tigergrm.y

tigerlex.sml: tigerlex.lex
	$(MOSMLLEX) tigerlex.lex

clean:
	$(REMOVE) Makefile.bak
	$(REMOVE) tigergrm.output
	$(REMOVE) tigergrm.sig
	$(REMOVE) tigergrm.sml
	$(REMOVE) tigerlex.sml
	$(REMOVE) tigermain
	$(REMOVE) *.ui
	$(REMOVE) *.uo
	$(REMOVE) errlist
	$(REMOVE) *.asm *.lst *.rel *.sym
	$(REMOVE) tiger

.sig.ui:
	$(MOSMLC) $<

.sml.uo:
	$(MOSMLC) $<

depend: tigerabs.sml tigergrm.sml tigerlex.sml tigermain.sml \
	tigernlin.sml tigerpp.sml
	$(REMOVE) Makefile.bak
	$(MOVE) Makefile Makefile.bak
	$(MOSMLTOOLS)/cutdeps < Makefile.bak > Makefile
	$(MOSMLTOOLS)/mosmldep >> Makefile

### DO NOT DELETE THIS LINE
tigertopsort.uo: tigertopsort.ui tigertab.ui tigertips.uo tigerabs.uo 
tigerpp.uo: tigerabs.uo 
tigerescap.ui: tigerabs.uo 
tigerframe.uo: tigerframe.ui tigertree.uo tigerassem.uo tigertemp.ui 
tigercontrolflow.ui: tigergraph.ui tigerassem.uo tigertemp.ui 
tigerlex.uo: tigergrm.ui tigernlin.uo 
tigercontrolflow.uo: tigercontrolflow.ui tigergraph.ui tigerassem.uo \
    tigertemp.ui tigersugar.uo 
tigermain.uo: tigerseman.ui tigercodegen.ui tigerescap.ui tigergrm.ui \
    tigerframe.ui tigercolor.ui tigercanon.ui tigerassem.uo tigerlex.uo \
    tigertrans.ui tigerpp.uo 
tigerliveness.uo: tigerliveness.ui tigercontrolflow.ui tigergraph.ui \
    tigertemp.ui tigersugar.uo 
tigerspill.ui: tigerframe.ui tigerassem.uo tigertemp.ui 
tigerescap.uo: tigerescap.ui tigertab.ui tigerabs.uo 
tigerseman.ui: tigerabs.uo 
tigertab.uo: tigertab.ui 
tigersugar.uo: tigergraph.ui tigertemp.ui 
tigercanon.uo: tigercanon.ui tigertree.uo tigertab.ui tigerframe.ui \
    tigerit.uo tigertemp.ui 
tigertree.uo: tigertemp.ui 
tigercodegen.uo: tigercodegen.ui tigertree.uo tigerframe.ui tigerit.uo \
    tigerassem.uo tigertemp.ui 
tigergrm.ui: tigerabs.uo 
tigersres.uo: tigertab.ui tigertips.uo tigertemp.ui tigerabs.uo \
    tigertrans.ui 
tigertemp.uo: tigertemp.ui 
tigerspill.uo: tigerspill.ui tigerframe.ui tigerassem.uo tigersugar.uo \
    tigertemp.ui 
tigergrm.uo: tigergrm.ui tigernlin.uo tigerabs.uo 
tigercolor.ui: tigerframe.ui tigerassem.uo tigertemp.ui 
tigercodegen.ui: tigertree.uo tigerframe.ui tigerassem.uo 
tigertrans.uo: tigertrans.ui tigertree.uo tigerpila.ui tigerframe.ui \
    tigerit.uo tigertemp.ui tigerabs.uo 
tigerliveness.ui: tigercontrolflow.ui tigergraph.ui tigertemp.ui 
tigergraph.uo: tigergraph.ui 
tigertopsort.ui: tigertab.ui tigertips.uo tigerabs.uo 
tigerpila.uo: tigerpila.ui 
tigerseman.uo: tigerseman.ui tigersres.uo tigertab.ui tigerpila.ui \
    tigertopsort.ui tigertemp.ui tigerabs.uo tigertrans.ui 
tigerassem.uo: tigertemp.ui 
tigertrans.ui: tigerframe.ui tigertemp.ui tigerabs.uo 
tigercanon.ui: tigertree.uo tigerframe.ui tigertemp.ui 
tigerframe.ui: tigertree.uo tigerassem.uo tigertemp.ui 
tigercolor.uo: tigercolor.ui tigercontrolflow.ui tigergraph.ui \
    tigerframe.ui tigerassem.uo tigersugar.uo tigertemp.ui tigerspill.ui \
    tigerliveness.ui 
tigerit.uo: tigertree.uo tigertab.ui 
