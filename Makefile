build:
	@mkdir -p _build
	cp assets/* _build
	@racket build.rkt

serve:
	python3 -m http.server -d _build
