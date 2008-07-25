doc: cleandoc
	@echo "Generating doc"
	@rdoc --exclude contrib --exclude test --all --diagram

cleandoc:
	@rm -rf doc

mrproper: cleandoc
	@find . -name '*~' | xargs rm -f	
