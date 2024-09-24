##
## EPITECH PROJECT, 2024
## Rush3
## File description:
## Rush3
##

NAME = my-lisp-interpreter-exe

all: $(NAME)

$(NAME):
	stack build
	cp $(shell stack path --local-install-root)/bin/my-lisp-interpreter-exe ./$(NAME)

clean:
	stack purge

fclean: clean
	rm -f $(NAME)

re: fclean all
