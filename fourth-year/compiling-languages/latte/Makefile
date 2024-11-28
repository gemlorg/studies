TARGET = ./latc_x86_64

.PHONY: build 

build:
	cabal install --installdir=. --overwrite-policy=always
	

.PHONY: test
test: 
	./test.sh $(TARGET)
.PHONY: clean
clean:
	cabal clean
	rm ./latc_x86_64
