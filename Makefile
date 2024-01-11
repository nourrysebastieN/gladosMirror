STACK_PATH = $(shell stack path --local-install-root)

NAME = glados

all:
	stack build
	cp $(STACK_PATH)/bin/dawn-exe ./$(NAME)

clean:
	stack clean

fclean:
	stack clean --full
	rm -f $(NAME)

re: fclean all
