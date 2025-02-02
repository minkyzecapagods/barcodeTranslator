# Nome do executável
EXECUTABLE = barcodeTranslator

# Diretórios
SRC_DIR = src
BUILD_DIR = build
OBJ_DIR = $(BUILD_DIR)/obj
BIN_DIR = $(BUILD_DIR)/bin

# Arquivos fonte
SRCS = $(wildcard $(SRC_DIR)/*.hs)
OBJS = $(patsubst $(SRC_DIR)/%.hs, $(OBJ_DIR)/%.o, $(SRCS))
HIS = $(patsubst $(SRC_DIR)/%.hs, $(OBJ_DIR)/%.hi, $(SRCS))

# Compilador Haskell
HC = ghc
HCFLAGS = -outputdir $(OBJ_DIR) -o $(BIN_DIR)/$(EXECUTABLE) -i$(SRC_DIR)

# Atalho na raiz
SYMLINK = $(EXECUTABLE)

# Regra principal
all: $(BIN_DIR)/$(EXECUTABLE) symlink

# Compila o executável
$(BIN_DIR)/$(EXECUTABLE): $(SRCS)
	@mkdir -p $(OBJ_DIR) $(BIN_DIR)
	$(HC) $(HCFLAGS) $(SRC_DIR)/Main.hs

# Cria um atalho na raiz para o executável
symlink:
	@ln -sf $(BIN_DIR)/$(EXECUTABLE) $(SYMLINK)

# Limpa tudo
clean:
	@rm -rf $(BUILD_DIR) $(SYMLINK)
	@echo "Todos os arquivos de build e o atalho foram removidos."

# Regra para garantir que os diretórios sejam criados
$(OBJ_DIR) $(BIN_DIR):
	@mkdir -p $@

# Regra padrão
.PHONY: all clean symlink
