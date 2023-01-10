CC = g++
CFLAGS  = -I"/usr/include/openrgb/include" -lX11

TARGET = rgb
SOURCES := $(shell find . -name '*.cpp')
all:
	$(CC) $(CFLAGS) -o $(TARGET) $(SOURCES) /usr/include/openrgb/build/liborgbsdk.a
clean:
	rm $(TARGET)
