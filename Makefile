.PHONY: restore analysis audit test lint ci

restore:
	Rscript -e 'renv::restore(prompt = FALSE)'

analysis:
	R_USER_CONFIG_DIR=/tmp/distortions-r-config Rscript scripts/run_all.R

audit: analysis
	Rscript scripts/checks.R

test: audit
	Rscript tests/testthat.R

lint:
	Rscript -e 'lints <- c(lintr::lint_dir("scripts"), lintr::lint_dir("tests")); print(lints); quit(status = length(lints))'

ci: lint test
