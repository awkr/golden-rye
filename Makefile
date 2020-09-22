# VERSION:=$(shell date "+%Y%m%d%H%M%s")
VERSION:=$(shell date "+%s")
PACKAGE_NAME:=gr-$(VERSION)
PACKAGE_DIR:=/tmp/$(PACKAGE_NAME)

pkg: $(PACKAGE_DIR)
	tar cvf ../$(PACKAGE_NAME).tar --exclude="*#" --exclude="*~" --exclude=".git" --exclude=".gitignore" --exclude="Makefile" --exclude="*package-template.el" -C $(PACKAGE_DIR)/.. $(PACKAGE_NAME)
	rm -rf $(PACKAGE_DIR)

$(PACKAGE_DIR):
	mkdir $@
	cp -r ./* $@
	gsed -re "s/VERSION/$(VERSION)/" $@/gr-package-template.el > $@/gr-pkg.el

clean:
	rm -f ../gr-*.tar

# end
