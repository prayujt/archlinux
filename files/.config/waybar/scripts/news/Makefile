BIN := news
SRC := main.go
INSTALL_DIR ?= /usr/bin
DESTDIR ?=

.PHONY: run build install clean

run: build
	./$(BIN) 100 0.05

build: $(BIN)

$(BIN): $(SRC)
	go build -o $(BIN) $(SRC)

install: build
	install -d $(DESTDIR)$(INSTALL_DIR)
	install -m 0755 $(BIN) $(DESTDIR)$(INSTALL_DIR)/$(BIN)

clean:
	rm -f $(BIN)

