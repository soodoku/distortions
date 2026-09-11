.PHONY: restore analysis audit test lint ci

restore:
	Rscript -e 'renv::restore(prompt = FALSE)'

analysis:
	R_USER_CONFIG_DIR=/tmp/distortions-r-config Rscript clean/05_run_all.R

audit: analysis
	Rscript clean/91_compare.R
	Rscript clean/92_full_audit.R

test: analysis
	Rscript tests/testthat.R

lint:
	Rscript -e 'lints <- c(lintr::lint_dir("clean"), lintr::lint_dir("tests")); print(lints); quit(status = length(lints))'

ci: lint test audit
