build:
	@mkdir -p _build
	cp assets/* _build
	@racket build.rkt
	@sh .tmp.sh
	@rm .tmp.sh

serve:
	python3 -m http.server -d _build
