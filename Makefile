.PHONY: all clean
INSTALL_ROOT:=$(shell stack path --local-install-root)

all: js-build/install-root js-build/flux-cards.min.js

js-build/install-root: $(INSTALL_ROOT)
	mkdir -p js-build
	ln -sf $(INSTALL_ROOT) js-build/install-root

js-build/flux-cards.min.js: js-build/flux-cards.js
	closure-compiler --compilation_level=ADVANCED_OPTIMIZATIONS js-build/flux-cards.js > js-build/flux-cards.min.js

js-build/flux-cards.js: $(INSTALL_ROOT)/bin/flux-cards.jsexe/all.js
	mkdir -p js-build
	echo "(function(global,React,ReactDOM) {" > js-build/flux-cards.js
	cat $(INSTALL_ROOT)/bin/flux-cards.jsexe/all.js >> js-build/flux-cards.js
	echo "})(window, window['React'], window['ReactDOM']);" >> js-build/flux-cards.js
	sed -i 's/goog.provide.*//' js-build/flux-cards.js
	sed -i 's/goog.require.*//' js-build/flux-cards.js

clean:
	rm -rf js-build
