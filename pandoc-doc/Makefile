doc.pdf: doc.md
	pandoc doc.md --filter ./graphviz.py -t latex -o doc.pdf

watch:
	while true; do $(MAKE) -q || $(MAKE); sleep 0.5; done
