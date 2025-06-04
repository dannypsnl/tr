build:
	@mkdir -p _build
	cp assets/* _build
	@racket build.rkt
	racket search.scrbl > _build/search.json
	racket rss.scrbl > _build/rss.xml

serve:
	python3 -m http.server -d _build
