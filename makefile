.PHONY: slides openslides

all: slides slides.pdf openslides

slides.pdf: slides.tex
	pdflatex -shell-escape slides.tex  -o slides.pdf
	biber slides

openslides: slides.pdf
	rifle slides.pdf


