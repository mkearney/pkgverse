.PHONY: all clean
all:
@echo "
---------------------
## Documenting...
---------------------
"
Rscript -e "devtools::document(roclets=c('rd', 'collate', 'namespace'))"
@echo "
---------------------
## Installing...
---------------------
"
R CMD INSTALL --no-multiarch --with-keep.source .
clean:
@echo "
---------------------
## Cleaning docs...
---------------------
"
rm man/*.Rd

