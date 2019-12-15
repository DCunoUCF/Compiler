CFLAGS ?= -g
COMPILER_SRC= \
	token.c \
	lexer.c \
	ast.c \
	datatype.c \
	parser.c \
	symtab.c \
  typechecker.c \
	codegen.c \
	compiler.c
VM_SRC= \
	vm_main.c \
	vm_impl.c
COMPILER_OBJ=$(COMPILER_SRC:%.c=%.o)
VM_OBJ=$(VM_SRC:%.c=%.o)
COMPILER=compiler
VM=vm

all: $(COMPILER) $(VM)

$(COMPILER): $(COMPILER_OBJ)
	$(CC) $(CFLAGS) -o $@ $^

$(VM): $(VM_OBJ)
	$(CC) $(CFLAGS) -o $@ $^

vm_main.c: vm_ops.c

lexer.o: lexer_reserved.c

parser.o: parser_tools.c ast.c ast.h

codegen_instr.h: ops.txt ops_to_macros.py
	cat ops.txt | python3 ops_to_macros.py > $@

codegen.o: codegen_tools.c codegen_instr.h

ast.c ast.h: ast.yml ast.py
	python3 ast.py

%.o: %.c
	$(CC) $(CFLAGS) -c $<

%.c: %.gperf
	gperf -N $* -t $^ > $@

clean:
	rm -rf $(COMPILER_OBJ) $(COMPILER) $(VM_OBJ) $(VM)

clobber: clean
	rm -rf vm_ops.c lexer_reserved.c.c ast.h ast.c codegen_instr.h

