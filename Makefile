build:
	@mkdir -p _build
	cp assets/* _build
	# we have to remove contexts/backlinks left last time, because we will only append new content to it
	@rm -f _tmp/*.context.scrbl
	@rm -f _tmp/*.backlinks.scrbl
	@racket build.rkt
	sh _tmp.sh
	rm -f _tmp.sh
	racket search.scrbl > _build/search.json

serve:
	python3 -m http.server -d _build
