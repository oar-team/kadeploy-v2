doc: cleandoc
	@echo "Generating doc"
	@rdoc --exclude src/contrib --exclude test --include src --all --diagram --line-numbers --inline-source -A cattr_accessor=object

cleandoc:
	@rm -rf doc

mrproper: cleandoc
	@find . -name '*~' | xargs rm -f	
