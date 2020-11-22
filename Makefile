.DEFAULT_GOAL: default
.PHONY: default install remove

PACKAGE := lambda-sup

default: install

install:
	raco pkg install --auto "$(PACKAGE)/"

setup:
	raco setup --pkgs "$(PACKAGE)"

remove:
	@raco pkg remove --auto $(PACKAGE) \
	  || (echo "\nConsider removing the dependent packages listed above by doing:" \
	           "\n    raco pkg remove --auto <package>" \
	           "\nYou can also force the removal to proceed by doing:" \
	           "\n    raco pkg remove --force $(PACKAGE)" \
	           "\nbut this is not recommended.\n" \
	           1>&2 ; \
	      exit 1)
