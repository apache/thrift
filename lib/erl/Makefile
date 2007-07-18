MODULES = lib # release

all clean:
	@for dir in $(MODULES); do \
		(cd $$dir; ${MAKE} $@); \
		if [ "$$?" -ne "0" ]; then ERROR=$$?; echo "Error Code $$ERROR"; exit $$ERROR; fi; \
	done

docs:
	(cd lib; ${MAKE} $@); \
	if [ "$$?" -ne "0" ]; then ERROR=$$?; echo "Error Code $$ERROR"; exit $$ERROR; fi; 

install:
	echo NO OP
