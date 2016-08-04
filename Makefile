.PHONY: all clean
INSTALL_ROOT:=$(shell stack path --local-install-root)

all: js-build/install-root js-build/fluxette.min.js

js-build/install-root: $(INSTALL_ROOT)
	mkdir -p js-build
	ln -sf $(INSTALL_ROOT) js-build/install-root

js-build/fluxette.min.js: js-build/fluxette.js
	closure-compiler --compilation_level=ADVANCED_OPTIMIZATIONS js-build/fluxette.js > js-build/fluxette.min.js

js-build/fluxette.js: $(INSTALL_ROOT)/bin/fluxette.jsexe/all.js
	mkdir -p js-build
	echo "(function(global,React,ReactDOM) {" > js-build/fluxette.js
	cat $(INSTALL_ROOT)/bin/fluxette.jsexe/all.js >> js-build/fluxette.js
	echo "})(window, window['React'], window['ReactDOM']);" >> js-build/fluxette.js
	sed -i 's/goog.provide.*//' js-build/fluxette.js
	sed -i 's/goog.require.*//' js-build/fluxette.js

clean:
	rm -rf js-build
