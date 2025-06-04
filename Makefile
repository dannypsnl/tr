build:
	@mkdir -p _build
	cp -r assets/* _build
	@racket build.rkt
	racket search.scrbl > _build/search.json
	racket rss.scrbl > _build/rss.xml

serve:
	python3 -m http.server -d _build

deploy:
	tar -C _build/ -cvz . > site.tar.gz
	hut pages publish -d tr-notes.srht.site site.tar.gz
