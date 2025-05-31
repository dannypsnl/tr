build:
	@mkdir -p output
	cp assets/* output/
	racket build.rkt
	
serve:
	python3 -m http.server -d output
