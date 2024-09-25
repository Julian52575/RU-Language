##
## EPITECH PROJECT, 2024
## Rush3
## File description:
## Rush3
##

NAME = my-lisp-interpreter-exe
COMPARE_SCRIPT = compareScheme.py
EXAMPLE_FOLDER = examples


all: $(NAME)

$(NAME):
	stack build
	cp $(shell stack path --local-install-root)/bin/my-lisp-interpreter-exe ./$(NAME)

clean:
	stack purge

fclean: clean
	rm -f $(NAME)

test_run:
	stack test
	./$(COMPARE_SCRIPT) $(EXAMPLE_FOLDER)

re: fclean all

.Phony: all clean fclean test_run re
