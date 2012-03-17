.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

.yrl.erl:
	erlc -W $<

ERL = erl -boot start_clean 

# Modules
MODS = qraph cds plotter \
	pcore pabi ibap qtransf qutils famdict \
	timer_wheel tester

all: compile

compile: ${MODS:%=%.beam}

# remove all the code

clean:	
	rm -rf *.beam erl_crash.dump
#	cd dir1; make clean
#	cd dir2; make clean  