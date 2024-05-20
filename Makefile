.PHONY: run run-ghci install-hlint install-apply-refact lint autofix-file

# Specify the local bin directory
LOCAL_BIN := .local/bin
HLINT := $(LOCAL_BIN)/hlint
APPLY_REFACT := $(LOCAL_BIN)/refactor

# Target to run the project
run:
	stack run

# Target to run the project in GHCi mode
run-ghci:
	stack ghci

# Target to install hlint only if it's not already installed
install-hlint: $(HLINT)

$(HLINT):
	@echo "Checking if hlint is installed at $(HLINT)"
	@if [ ! -f $(HLINT) ]; then \
		echo "Installing hlint..."; \
		mkdir -p $(LOCAL_BIN) && \
		stack install hlint --local-bin-path $(LOCAL_BIN); \
	else \
		echo "hlint is already installed."; \
	fi

# Target to install apply-refact only if it's not already installed
install-apply-refact: $(APPLY_REFACT)

$(APPLY_REFACT):
	@echo "Checking if refactor is installed at $(APPLY_REFACT)"
	@if [ ! -f $(APPLY_REFACT) ]; then \
		echo "Installing apply-refact..."; \
		mkdir -p $(LOCAL_BIN) && \
		stack install apply-refact --local-bin-path $(LOCAL_BIN); \
	else \
		echo "refactor is already installed."; \
	fi

# Target to automatically apply hlint suggestions to a specified file, depends on install-apply-refact
# e.g.: 
#	make autofix-file file=src/Mllib.hs
autofix-file: install-apply-refact
	@echo "Applying refactorings to $(file)"
	@$(HLINT) --refactor --refactor-options="--inplace" $(file)

# Target to automatically apply hlint suggestions to all Haskell files in the folder
autofix-all: install-apply-refact
	@echo "Applying refactorings to all Haskell files..."
	@find . -type f -name '*.hs' -exec $(MAKE) autofix-file file={} \;
