build:
	racket xxx-0001.scrbl > output/xxx-0001.html
	racket xxx-0002.scrbl > output/xxx-0002.html
	
serve:
	python3 -m http.server -d output
