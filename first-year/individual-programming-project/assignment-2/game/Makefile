# Compiler and linker options
CC = gcc
CFLAGS = -Wall -Wextra -Wno-implicit-fallthrough  -std=c17 -O2
LDFLAGS = -lncurses
# Source files
SRCS = game.c game_main.c
TEST_SRCS = game.c game_test_1.c 
TEST_OBJS = $(TEST_SRCS:.c=.o)
OBJS = $(SRCS:.c=.o)

# Target executable
TARGET = game

TEST = game_test

# Link object files into the target executable
$(TARGET): $(OBJS)
	$(CC) -o $@ $^  $(LDFLAGS)

# $(TEST): $(TEST_OBJS)
# 	$(CC) -o $@ $^  $(LDFLAGS) 


all: $(TARGET) $(TEST)

game.o: game.c game.h  
game_main.o: game_main.c game.h  
game_test_1.o: game_test_1.c game.h

.PHONY: clean
# Clean up object files and the target executable
clean:
	rm -f $(OBJS) $(TARGET)
