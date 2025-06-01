build:
	@mkdir -p _build
	cp assets/* _build
	@racket build.rkt
	sh _tmp.sh
	rm _tmp.sh
	rm *.dvi *.log

serve:
	python3 -m http.server -d _build
