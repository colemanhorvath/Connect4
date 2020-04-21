MODULES=game_mechanics command save
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test_mechanics.byte
TESTCOMMAND=test_command.byte
TESTSAVE=test_save.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test_mechanics:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)

test_command:
	$(OCAMLBUILD) -tag 'debug' $(TESTCOMMAND) && ./$(TESTCOMMAND)

test_save:
	$(OCAMLBUILD) -tag 'debug' $(TESTSAVE) && ./$(TESTSAVE)

#play:
#	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

# check:
# 	bash checkenv.sh && bash checktypes.sh
	
# finalcheck: check
# 	bash checkzip.sh
# 	bash finalcheck.sh

# zip:
# 	zip adventure.zip *.ml* *.json _tags Makefile
	
# docs: docs-public docs-private
	
# docs-public: build
# 	mkdir -p doc.public
# 	ocamlfind ocamldoc -I _build -package yojson,ANSITerminal \
# 		-html -stars -d doc.public $(MLIS)

# docs-private: build
# 	mkdir -p doc.private
# 	ocamlfind ocamldoc -I _build -package yojson,ANSITerminal \
# 		-html -stars -d doc.private \
# 		-inv-merge-ml-mli -m A $(MLIS) $(MLS)

# clean:
# 	ocamlbuild -clean
# 	rm -rf doc.public doc.private adventure.zip
