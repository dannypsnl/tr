build:
	@raco tr build

serve:
	python3 -m http.server -d _build
