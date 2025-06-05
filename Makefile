build:
	@raco tr build

serve:
	python3 -m http.server -d _build

deploy:
	tar -C _build/ -cvz . > site.tar.gz
	hut pages publish -d tr-notes.srht.site site.tar.gz
