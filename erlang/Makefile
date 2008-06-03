
ERLC := erlc
ERLC_FLAGS := 

SRC_DIR := src
EBIN_DIR := ebin
EMULATOR := beam

ERL_SRC := $(wildcard $(SRC_DIR)/*.erl)
ERL_HDR := $(wildcard $(SRC_DIR)/*.hrl)
ERL_OBJ := $(ERL_SRC:$(SRC_DIR)/%.erl=$(EBIN_DIR)/%.$(EMULATOR))

build: $(ERL_OBJ)

run: build
	erl -noshell -pa $(EBIN_DIR) -s server start 2345

clean:
	-rm $(ERL_OBJ)

$(EBIN_DIR)/%.$(EMULATOR): $(SRC_DIR)/%.erl
	$(ERLC) $(ERLC_FLAGS) -o $(EBIN_DIR) $<

