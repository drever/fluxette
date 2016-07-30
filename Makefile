.PHONY: all clean
INSTALL_ROOT:=$(shell stack path --local-install-root)

all: js-build/install-root js-build/flux-test.min.js

js-build/install-root: $(INSTALL_ROOT)
	mkdir -p js-build
	ln -sf $(INSTALL_ROOT) js-build/install-root

js-build/flux-test.min.js: js-build/flux-test.js
	closure-compiler --compilation_level=ADVANCED_OPTIMIZATIONS js-build/flux-test.js > js-build/flux-test.min.js

js-build/flux-test.js: $(INSTALL_ROOT)/bin/flux-test.jsexe/all.js
	mkdir -p js-build
	echo "(function(global,React,ReactDOM) {" > js-build/flux-test.js
	cat $(INSTALL_ROOT)/bin/flux-test.jsexe/all.js >> js-build/flux-test.js
	echo "})(window, window['React'], window['ReactDOM']);" >> js-build/flux-test.js
	sed -i 's/goog.provide.*//' js-build/flux-test.js
	sed -i 's/goog.require.*//' js-build/flux-test.js

clean:
	rm -rf js-build
