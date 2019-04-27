all: src/main.ml

src/main.ml: src/mirage_config.ml
	cd src/ && mirage configure -f $<

