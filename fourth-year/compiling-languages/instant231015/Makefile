.PHONY: build 

build:
	cabal build insc_jvm 
	cabal build insc_llvm 
	cabal install  insc_jvm --installdir=. --overwrite-policy=always   
	cabal install  insc_llvm --installdir=. --overwrite-policy=always   
	

.PHONY: clean
clean:
	stack clean
