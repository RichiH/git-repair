PREFIX=/usr
CABAL?=cabal # set to "./Setup" if you lack a cabal program

build: Build/SysConfig.hs
	$(CABAL) build
	ln -sf dist/build/git-repair/git-repair git-repair
	@$(MAKE) tags >/dev/null 2>&1 &

Build/SysConfig.hs: configure.hs Build/TestConfig.hs Build/Configure.hs
	if [ "$(CABAL)" = ./Setup ]; then ghc --make Setup; fi
	$(CABAL) configure

install: build
	install -d $(DESTDIR)$(PREFIX)/bin
	install git-repair $(DESTDIR)$(PREFIX)/bin
	install -d $(DESTDIR)$(PREFIX)/share/man/man1
	install -m 0644 git-repair.1 $(DESTDIR)$(PREFIX)/share/man/man1

clean:
	rm -rf git-repair dist configure Build/SysConfig.hs Setup tags
	find -name \*.o -exec rm {} \;
	find -name \*.hi -exec rm {} \;

# Upload to hackage.
hackage: clean
	./Build/make-sdist.sh
	@cabal upload dist/*.tar.gz

# hothasktags chokes on some template haskell etc, so ignore errors
tags:
	find . | grep -v /.git/ | grep -v /tmp/ | grep -v /dist/ | grep -v /doc/ | egrep '\.hs$$' | xargs hothasktags > tags 2>/dev/null
