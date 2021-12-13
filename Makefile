
py_tests = test_fft_modrecur.py test_pmn.py
py_test_logs = $(patsubst test_%.py,test_%.py.log,%(py_tests))

test_%.py.log: test_%.py
	python3 $< | tee $@

%.html: %.fut hilite.css pandoc.css pandoc-pygmentize
	futhark literate $<
	pandoc -s --css hilite.css --filter ./pandoc-pygmentize $*.md -o $@ --metadata title=$* --css pandoc.css

futhark: 
	 git clone https://github.com/diku-dk/futhark && cd futhark && git checkout clean-ad && stack setup && stack build && stack install


pandoc-pygmentize:
	curl -LO https://gist.github.com/DoomHammer/22b54b207c903f4f868e/raw/5983feff7185060d793e3d46444ed81cc99d111d/pandoc-pygmentize
	chmod +x $@

pandoc.css:
	curl -LO https://gist.github.com/killercup/5917178/raw/40840de5352083adb2693dc742e9f75dbb18650f/pandoc.css

hilite.css:
	pygmentize -S default -f html > $@
