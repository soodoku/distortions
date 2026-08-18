.PHONY: analysis audit test lint ci

analysis:
	R_USER_CONFIG_DIR=/tmp/distortions-r-config Rscript --vanilla clean/05_run_all.R

audit: analysis
	Rscript --vanilla clean/91_compare.R
	Rscript --vanilla clean/92_full_audit.R

test: analysis
	Rscript --vanilla tests/testthat.R

lint:
	Rscript --vanilla -e 'lints <- c(lintr::lint_dir("clean"), lintr::lint_dir("tests")); print(lints); quit(status = length(lints))'

ci: lint test audit
