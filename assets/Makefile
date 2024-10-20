
NAME = ruCompiler

SRC = $(wildcard src/*.cpp)

all:
	g++ ${SRC} -o ${NAME}
clean:
fclean:
	rm ${NAME} || true
re: fclean all

.Phony: all clean fclean re
