##
## EPITECH PROJECT, 2024
## Rush3
## File description:
## Rush3
##

NAME = glados-compiler-exe

all: $(NAME)

$(NAME):
	stack build --allow-different-user
	cp $(shell stack path --local-install-root)/bin/$(NAME) ./$(NAME)

clean:
	stack purge

fclean: clean
	rm -f $(NAME)

test_run:
	stack test

re: fclean all

.Phony: all clean fclean test_run re
