.PHONY: slides

all: slides slides.pdf

slides: Main.lhs
	cabal v2-build

slides.pdf: slides Main.lhs
	pandoc Main.lhs -o slides.tex
	pdflatex -shell-escape slides.tex  -o slides.pdf

