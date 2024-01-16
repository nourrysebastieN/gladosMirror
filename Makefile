STACK_PATH = $(shell stack path --local-install-root)

COMPILER_NAME = dawnc
VM_NAME = dawn

all:
	stack build
	cp $(STACK_PATH)/bin/dawnc-exe ./$(COMPILER_NAME)
	cp $(STACK_PATH)/bin/dawn-exe ./$(VM_NAME)

clean:
	stack clean

fclean:
	stack clean --full
	rm -f $(COMPILER_NAME)
	rm -f $(VM_NAME)

re: fclean all
