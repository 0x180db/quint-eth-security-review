.PHONY: test, run, verify, repl

run:
	quint run --invariant=safetyOk main.qnt

test:
	quint test tests.qnt

verify:
	quint verify --temporal=livenessOk main.qnt

repl:
	quint -r main.qnt::main
