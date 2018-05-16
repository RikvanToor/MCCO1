default: documentation

documentation:
	pandoc -f markdown                          \
	       -t latex                             \
				 -o ../documentation.pdf              \
				 --filter pandoc-crossref             \
				 --pdf-engine=xelatex                 \
				 documentation.md

clean:
	rm ../documentation.pdf
