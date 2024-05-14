.PHONY: build 

build:
	stack build
	stack install --local-bin-path .
	mv ./luna-exe ./interpreter

.PHONY: clean
clean:
	stack clean
