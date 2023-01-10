CC = g++
CFLAGS  = -w -g -lfreenect -lglut -lGL

TARGET = initkinect
SOURCES := $(shell find . -name '*.cpp')
all:
	$(CC) $(CFLAGS) -o $(TARGET) $(SOURCES)
clean:
	rm $(TARGET)
