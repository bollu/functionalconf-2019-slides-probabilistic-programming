.PHONY: slides openslides

all: slides slides.pdf openslides

slides.pdf: slides.tex
	pdflatex -shell-escape slides.tex  -o slides.pdf

openslides: slides.pdf
	rifle slides.pdf


