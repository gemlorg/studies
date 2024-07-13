# Compiler and linker options
CC = gcc
CFLAGS = -Wall -Wextra -Wno-implicit-fallthrough  -std=c17 -O2
LDFLAGS =
# Source files
SRCS = game.c game_example.c
OBJS = $(SRCS:.c=.o)

# Target executable
TARGET = game

# Link object files into the target executable
$(TARGET): $(OBJS)
	$(CC) $(LDFLAGS) -o $@ $^


all: $(TARGET)

game.o: game.c game.h
game_example.o: game_example.c game.h


# Clean up object files and the target executable
clean:
	rm -f $(OBJS) $(TARGET)