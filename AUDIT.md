# Audit: the domination sign issue and a corrected pipeline

An external note flagged that the domination rows of Table 2 in
"Deliberative Distortions?" appear to use the opposite sign reference from
Eq. 3 as printed. This document does three things, in order of importance:
(1) reproduces what was published from the committed code, establishing
provenance; (2) interrogates the published results — the corrections,
their diffs against the version of record, and the robustness of the
corrected inferences; and (3) records, clearly separated, issues confined
to unpublished outputs of the replication package — nothing in that third
part touches a number or figure in the paper, and fixes there are
completeness, not corrections. The revised pipeline is in `clean/`
(writing to `tabs_clean/` and `figs_clean/`); the original `scripts/`,
`tabs/`, and `figs/` are untouched so the provenance of the published
numbers stays verifiable.

## Summary

Eq. 3 (p. 1210) defines domination with the advantaged subgroup's initial
mean as the sign reference: D > 0 means the group moved toward the
advantaged subgroup's initial attitude. The published education, income,
and all-three rows of Table 2 come from code sections that use the
disadvantaged subgroup as the reference — the negation of Eq. 3. Gender
alone used the advantaged reference (via a file-naming slip), and its
value mixes two metrics (issue 3). The corrected values below are under
Eq. 3 as printed, keeping the paper's own analytic conventions — they are
what the original code computes once the reference is read the right way
around:

| Dimension | Published D (Db) | Corrected D (Db) | p(D) | p(Db vs .5) |
|---|---|---|---|---|
| Gender | +.008 (.464) | +.004 (.496) | .50 | .77 |
| Education | −.013 (.447) | +.013 (.535) | .010 | .004 |
| Income | .000 (.485) | −.001 (.499) | .88 | .90 |
| All three | −.015 (.466) | +.015 (.517) | .035 | .40 |

Four takeaways:

1. Education and all-three change sign, and the corrected D's remain
   statistically significant (a two-sided test is unaffected by a pure
   sign change): weak but significant domination rather than opposition.
   Education's Db is also significantly above .5 (.535, p = .004).
2. Gender ends up with nothing significant: corrected D is +.004
   (p = .50) and Db is .496 (p = .77). The published gender
   significances (p = .031 and .002) trace to the mixed-metric issue
   (issue 3), not to any real gender pattern.
3. The p. 1217 statement that Db is below .5 on every dimension no longer
   holds: corrected Db is at or above .5 everywhere except gender's .496,
   and the only significant frequency is education's — on the domination
   side.
4. The near-zero D's sit on top of two offsetting subgroup movements:
   the disadvantaged move toward the advantaged's initial position in a
   majority of pairs on every dimension, while the advantaged move the
   other way (details under Table 3 below).

The paper's headline conclusion — no routine or strong homogenization,
polarization, or domination — is unaffected; the corrected magnitudes are
as small as the published ones. What changes is the direction of the faint
domination pattern and some of the significance claims attached to it.
The homogenization and polarization columns are unchanged: the corrected
pipeline reproduces the published H = .013, Hb = .595, P = −.022,
Pb = .454. One robustness flag: two of the Db inferences are sensitive to
how no-movement ties are counted (education's and gender's) — see
"Errors vs. analytic conventions."

## How the published numbers map to the code

Each of `scripts/03a–03d` contains two mirror sections computing
`signed = (A2 − A1) · sign(ref_t1 − A1)` — movement toward that section's
reference subgroup. The section titles are inverted relative to the
algebra (the "Towards better educated" section uses `bettered == 0` as the
reference, so it measures movement toward the less educated), and the
downstream scripts read exactly one file per dimension:

| Dimension | Published row comes from | Reference used | Matches Eq. 3? |
|---|---|---|---|
| Education | `03a` "Towards better educated" → `04_table_4b_toward_highed.csv` | `bettered == 0` (disadvantaged) | No |
| Income | `03c` "Towards High Income" → `04_table_4c_toward_highinc.csv` | `highinc == 0` (disadvantaged) | No |
| All three | `03d` "Towards Triple" → `04_table_4d_toward_triple.csv` | `triple == 0` (disadvantaged) | No |
| Gender | `03b` "Towards Men" → `04_table_4a_toward_male.csv` | `female == 0` (advantaged) | Direction yes; value affected by issue 3 |

Evidence: rerunning the original scripts unmodified
(`clean/90_repro_original.R`) reproduces every committed table — and hence
every published value — to three or more decimals
(`tabs_clean/90_repro_manifest.csv`). The corrected pipeline agrees with
the original code's advantaged-referenced sections pair-for-pair across
~6,600 group-issue pairs; the only differences are exact-tie cases (see
the portability note) and missing-subgroup cases (issue 4).

## Diff to the published version

Checked against both the journal version of record (BJPS 52(3), 2022,
pp. 1205–1225, doi:10.1017/S0007123421000168 — Table 2 and Table 3 images
from the Cambridge article page) and the working paper at
gsood.com/research/papers/DeliberativeDistortions.pdf (tables on PDF
pp. 45–46, narrative on printed p. 17 / journal p. 1217). The two versions'
Tables 2 and 3 are cell-for-cell identical, including every s.e. and
p-value, so the diffs below apply to the version of record. Every printed
cell reproduces from the committed code to rounding, with one exception
noted under "Manuscript-only items." The Cambridge article page links no
correction notice for these tables (the 2024 erratum concerns a funding
acknowledgment).

### Table 2 (printed → corrected)

H and P columns are unchanged at printed precision. Domination columns:

| Cell | Gender | Education | Income | All 3 |
|---|---|---|---|---|
| D | .008 → .004 | −.013 → +.013 | .000 → −.001 | −.015 → +.015 |
| s.e.(D) | .004 → .005 | .005 → .005 | .005 → .005 | .007 → .007 |
| p(D) | .031 → .50 | .008 → .010 | .951 → .88 | .029 → .035 |
| Db | .464 → .496 | .447 → .535 | .485 → .499 | .466 → .517 |
| s.e.(Db) | .011 → .015 | .012 → .012 | .017 → .017 | .020 → .019 |
| p(Db) | .002 → .77 | .000 → .004 | .425 → .90 | .102 → .40 |

### Table 3, "Parsing Domination" (printed → corrected, Mean\* rows)

Table 3 inherits the same reference inversion as Table 2 — one error, not
a second one. Computing aM from the mirror section (negated) is a
legitimate shortcut, since dM and aM are mirror-image quantities; but with
the sections' references read the other way around, the two columns come
out exchanged for education, income, and all-three: education's printed
dM = .023 and aM = −.057 correspond to corrected aM = −.024 and
dM = .058, and likewise income (.036/−.043 ↔ −.037/.043) and all-three
(.066/−.031 ↔ −.062/.033). Gender's columns are not exchanged; its
printed dM reflects issue 3.

| Panel | D | dM | aM | Db | dMb | aMb |
|---|---|---|---|---|---|---|
| A. Gender | .008 → .004 | .027 → .039 | −.033 → −.034 | .464 → .496 | .495 → .562 | .440 → .363 |
| B. Education | −.013 → +.013 | .023 → .058 | −.057 → −.024 | .447 → .535 | .523 → .589 | .412 → .395 |
| C. Income | .000 → −.001 | .036 → .043 | −.043 → −.037 | .485 → .499 | .521 → .542 | .465 → .405 |
| D. All three | −.015 → +.015 | .066 → .033 | −.031 → −.062 | .466 → .517 | .495 → .552 | .455 → .304 |

D and Db above are on the paper's conventions (as in Table 2); dM, aM,
dMb, and aMb are from the revised pipeline — dM and dMb barely move
across conventions (the committed mirror files give dM within .003 of
these values), aMb is more sensitive (see the Notes).

The printed table's qualitative pattern — both subgroups converging
(dM > 0, aM < 0), with D their weighted net (per pair,
D ≈ w·aM + (1−w)·dM) — holds after correction too. What the correction
changes is which subgroup each magnitude and frequency belongs to, and
the dMb inference: corrected dM is positive and significant on every
dimension, with dMb between .54 and .59 (printed dMb p-values .56, .018,
.20, .82 become <.001, <.001, .009, .010).

Notes: the level of aMb (and to a lesser degree dMb) depends on the tie
convention (see "Errors vs. analytic conventions"); the transposition,
signs, and dM significance hold under either. Corrected aM/aMb standard
errors are not regenerated; corrected aM is the weighted mean of
−(advantaged movement toward the disadvantaged), matching the printed
Mean\* convention.

### Figure 3

Figure 1 (conceptual schematic) and Figure 2 (theory schematic) contain
no data. Figure 3 — the six-panel density figure — does, and its four
domination panels inherit the inversion: "Domination by better educated"
and "Domination by higher income" were drawn from the
disadvantaged-referenced files (their x-axes are mirror images of Eq. 3's
D), "Domination by men" was drawn from the doubled mixed-metric file
(issue 3; 4,952 values for 2,476 pairs), and "Domination by better
educated, higher income, men" was drawn from the advantaged-referenced
mirror — correctly oriented, but thereby on the opposite convention from
Table 2's published triple row. Because the densities are roughly
symmetric around zero the visual effect is subtle; regenerated panels are
in `figs_clean/`. The homogenization and polarization panels are
unaffected.

### Text passages affected

1. Journal p. 1217: "Db runs only from .447 to .485. No matter what the
   dimension, fewer than 50% of the group-issue pairs move toward the
   initial mean attitude of the advantaged… This is (weak) opposition,
   not domination." — Corrected Db runs .496 to .535; education's is
   significantly above .5.
2. Journal p. 1217: "The D's tell much the same tale" / "There is more
   opposition than domination." — Corrected D is positive on three of
   four dimensions, significantly so on education and all-three.
3. The parsing section (printed p. 20): "the disadvantaged and advantaged
   move toward each other … Second, the advantaged move slightly further
   toward the disadvantaged than vice versa (on all three dimensions,
   though not quite as far on the three combined), consistent with the
   slightly negative D's." — The first clause survives; the second
   reverses: corrected, the disadvantaged move farther on gender,
   education, and income, the advantaged farther only on all-three, and
   the D's are slightly positive.
4. The in-text correlation sentence (PDF p. 39): "the Hgj-Pgj correlation
   is only a modest .358, and the Hgj-Dgj and Dgj-Pgj correlations range
   only from −.062 to .105, averaging only .012." Two things here.
   Correcting D negates the D-involved correlations: the committed files
   give a range of −.106 to .053 (average ≈ 0); corrected, −.142 to .106
   (average −.02) — the "uniformly small" characterization is unchanged.
   Separately, the printed values themselves do not reproduce exactly
   from the committed files (which give H-P = .404, not .358), so this
   sentence, like the .018 p-value below, appears to be from an earlier
   vintage of the computation.
5. The abstract's "some faint … opposition (as opposed to domination)"
   would become faint domination for education and all-three.

### Manuscript-only items (independent of the code)

- Illustrative example, journal p. 1217: "If just two do so, Dgj = −.10" —
  two people shifting .1 in a group of 20 moves the mean by .01, so this
  should read −.010.
- Table 3 panel B prints p = .018 for education's dMb; the printed
  estimate and s.e. (.523, .015) imply p ≈ .13, and the committed code
  gives .108, so this cell appears to be a stale or transcription value.

## Unpublished outputs (nothing here touches the paper)

The replication package also writes outputs the paper does not report.
They change under the revised pipeline too; we record that here for
anyone re-running the package, not as corrections to the paper. Full
numbers: `clean/92_full_audit.R` / `tabs_clean/92_full_audit.csv`.

- **Normed tables**: education normed D −.025 → +.025, all-three −.033 →
  +.033; normed H .016 → .030 (issues 6, 7, 13).
- **Parsing regressions** (the exploratory `07_parsing_domination.R`, not
  the paper's Table 3 and not reported in the text): the education
  dM-slope changes from +.120 to −.108, income from +.047 (p = .11) to
  −.196 (p < .001), and the all-three regressor is redefined (issue 8).
  Slopes on D itself stay insignificant everywhere.
- **Precision-weighted rows** of the SE CSVs (issue 9) are not reported
  in the paper.
- **Unchanged anywhere:** Table 1, `att_change.csv`, all H/P headline
  numbers and their SEs and p-values, and the appendix composition plots
  (`density_p_*.png`, from script `08`).

## Issue inventory

Each item states its verified reach. In short: issues 1 and 3 drive every
published-number change (Tables 2 and 3, Figure 3's domination panels, and
the text claims above); issue 2 changes no number but explains why gender
is the direction exception; issues 4 and 5 move published values below
their printed precision; 6–9 and 13 affect unpublished or secondary
outputs only; 10–12 affect reproducibility only. Two things checked and
deliberately *not* listed as errors: the floating-point sensitivity of
exact-tie pairs (portability note below) and the tie condition at
`03c_dom_income.R:69`/`03d_dom_men_income_ed.R:71` that keys on
`t1_disgrp` rather than `t1_grp` — with both subgroups present,
`t1_grp = w · t1_disgrp` (w > 0), so the two conditions are analytically
equivalent; in the data they select the same pairs everywhere but one
float-level case (24 subgroup-tie pairs checked, 1 bitwise divergence).

### A. Issues bearing on published results

1. **Sign reference inverted vs Eq. 3** for education, income, and
   all-three (`03a_dom_educ.R:66`, `03c_dom_income.R:57`,
   `03d_dom_men_income_ed.R:59`: the section feeding the published table
   subtracts the disadvantaged subgroup's t1 mean). Reach: the education,
   income, and all-three columns of Tables 2 and 3, two Figure 3 panels,
   and the direction claims in the text. Fixed structurally in
   `clean/02_domination.R` (`signed_move()` always takes the advantaged
   mean as reference).
2. **Gender file-naming slip** (`03b_dom_gender.R:138` vs `246`): "Towards
   Men" writes `03_dom_fem_by_group_issue.csv` and vice versa. Reach:
   changes no number by itself — it explains why gender alone ended up
   Eq. 3-consistent in direction (the male-referenced output carried the
   file name downstream reads). Notably, `03b`'s section titles match
   what its code computes, while `03a/03c/03d`'s are inverted — consistent
   with one underlying direction mix-up rather than a second error.
3. **Gender double-append** (`03b_dom_gender.R:62–66` and `85–89`): the
   "Towards Men" section appends an older metric (raw attitude change and
   an absolute-distance frequency) *and* the signed Eq. 3 metric to the
   same accumulators, so the committed pair file has 4,952 rows for 2,476
   pairs and the published gender row averages the two: the raw-change
   half has weighted mean +.012, the signed half +.003, and the published
   D is their blend, +.008 (p = .031). Reach: the published gender column
   of Tables 2 and 3 (the pure Eq. 3 values are D = +.004, p = .50 and
   Db = .496, p = .77 — nothing significant), the gender SEs (computed
   over the doubled mixed file: D SE .0037 vs .0054 corrected), the
   gender Figure 3 panel, and the gender correlations.
4. **`goji::nona` maps missing to 0** (used on subgroup means): a subgroup
   whose members all lack a valid response on an index gets a reference
   mean of 0 rather than being treated as missing. Reach: 1 spurious pair
   in the file behind the published education row (changing its weighted
   D by < .00001 and Db by .00007) and 7 more across the unpublished
   mirror files. A real coding error with no effect at published
   precision.
5. **The synthetic Mean row is included in the Weighted Mean**
   (`02:83–84`, `03a:127–128`, and the same pattern in every table).
   Reach: every published weighted row, at the third to fourth decimal
   (published Hb .5950 vs .5956 without it; adding the Mean row back to
   the revised pipeline reproduces the committed values, see
   `91_compare.R`).

### B. Issues confined to unpublished outputs or reproducibility

Nothing below changes a number in the paper; fixes in `clean/` are
completeness. The one partial exception is issue 12, whose input choices
surface in published Figure 3 (covered in the Figure 3 note above).

6. **`homoex_normed` appended to the wrong accumulator**
   (`02_hom_pol_table_2_3.R:58`): built on `polarex_normed`. Reach:
   unpublished normed-H output only.
7. **Normed values recycled from the first index** (`02:57–58`): the
   normed expressions divide the accumulated `polarex`/`homoex` vectors by
   per-index group data, so R recycles index 1's values. Reach:
   unpublished normed columns only (the domination scripts' normed
   columns use per-index values and are fine).
8. **`pt3` mis-specified** (`07_parsing_domination.R:42`):
   `mean(highinc == 0 & highinc == 0 & female == 1)` repeats income and
   omits `bettered == 0`. Reach: the exploratory `07` regressions only —
   the paper's parsing section reports Table 3's dM/aM decomposition, not
   these regressions, so nothing published is affected.
9. **Precision-weighted SEs over the wrong rows** (`05b_dom_se.R:63–70`
    and the three other 21-poll blocks): point estimates use rows 1:21,
    the SEs sum rows 1:11. The 11-poll blocks also order the
    Mean/Weighted-Mean rows differently from the 21-poll blocks. Reach:
    the "Precision Wtd Mean" rows of the SE CSVs, which the paper does
    not report.
10. **`10_run_all.R` omits `03d`**, though `05b` and `07` read its
    outputs, so a fresh run of the runner depends on previously generated
    files. Reach: reproducibility only.
11. **Stray NA rows** from pre-allocating 12 rows for 11-poll dimensions
    (`03c:31–32`, `03d:33–34`, `03d:138–139`; `03c:136` vs `137` disagree
    within one section). Reach: cosmetic rows in the CSVs, worked around
    by `05b`.
12. **`06_figs.R` no longer runs** (`06_figs.R:495` references
    `poll_name`, which the mirror-section pair files don't carry; plus
    ggplot2 API drift), and its domination panels drew on the
    disadvantaged-referenced education/income files, the gender file from
    issue 3, and — unlike Table 2 — the advantaged-referenced triple
    mirror. Reach: published Figure 3's domination panels (see the
    Figure 3 note above) and reproducibility. Replaced by
    `clean/06_figs.R`.
13. **Normed homogenization divides by `.5 − t1sd`** (`02:58`), but a
    small group's sample sd can reach or exceed .5. Reach: 4 of 2,480
    pairs, one of which makes the normed-H aggregate non-finite when
    recomputed — confined to an unpublished column that issues 6–7
    already affect. The revised pipeline treats those pairs as undefined.

## Errors vs. analytic conventions

The inventory above lists places where the code does something other than
what the paper describes. Separately, any implementation of Eq. 3 makes a
few analytic choices where the paper's implicit choice was reasonable and
an alternative is equally defensible. These are robustness matters, and
none changes a sign or significance conclusion:

- **No-movement ties.** The paper's code drops pairs whose group mean did
  not move; an alternative keeps them in Db's denominator as "not
  domination." Both are defensible readings of a frequency of movement,
  and the level stakes are small: tied pairs are 2–5% of the data (123 of
  2,436 education, 113 of 2,476 gender, 30 of 1,161 income, 22 of 994
  all-three), so the choice moves Db by .02–.03 and D by less than .001.
  The Summary table uses the paper's own convention; the ties-counted
  variant gives Db (p): gender .467 (.026), education .518 (.153), income
  .493 (.662), all-three .511 (.594). Two frequency inferences are
  therefore convention-sensitive: education's Db is significantly above
  .5 under the paper's convention but not under the alternative, and
  gender's Db is significantly below .5 under the alternative but not
  under the paper's. The D conclusions and all sign conclusions are
  stable across conventions; we would not lean on either
  convention-sensitive frequency inference. `clean/91_compare.R` prints
  the full sensitivity.
- **Aggregation** keeps the published convention: per-poll means of
  group-issue values, then a weighted mean over poll rows with weight
  ngroups × nindices (minus the Mean-row inclusion, issue 6).
- **SEs** keep the published approach: per poll `lmer(y ~ (1 | issue))`
  intercept SEs, aggregated by fixed-effect meta-analysis (`fe_meta()`,
  algebraically identical to `rmeta::meta.summaries(method = "fixed")`).
- **Missing subgroup means** make a pair NA rather than an attitude of 0  (issue 4), and normed values with non-positive denominators are NA
  rather than infinite (issue 13).

Portability note (not an error, and a nice-to-have rather than a needed
change): the package reproduces exactly under its documented environment
(R 3.6.3/x86 per the README sessionInfo). On other hardware or R versions,
the strict `<`/`>`/`== 0` comparisons on differences of means computed
from different row subsets can score an analytically tied pair as 0 in one
floating-point environment and as the full movement in another; about 10
of ~7,000 pairs differ between the documented environment and arm64/R 4.6,
moving weighted means in the fourth decimal (at most a rounding boundary
in the third). The revised pipeline uses a 1e-12 tolerance so the same
pairs are ties everywhere; no conclusion depends on this either way.

Safety guards (defensive coding with no effect on any number in this
data): the revised pipeline also guards the attitude-normed denominators
(`1 − t1`, `t1`) against boundary group means, propagates NA through the
normed helpers when the underlying movement is missing, and skips a
poll-index in which no group has members of both subgroups. None of these
conditions occurs in this dataset; the guards exist so the code fails
safely on other data, not because they change results here.

Verification: the corrected pipeline was independently reviewed twice
(statistical semantics; code and data handling). An independent
reimplementation of the income dimension matched to full precision,
`fe_meta` matched `rmeta` exactly, `unique_id` join keys were shown to be
collision-free, and a fresh scratch-directory rerun regenerated every
output identically.

## Running the pipeline

```
Rscript clean/90_repro_original.R   # originals, unmodified, in a scratch copy
Rscript clean/05_run_all.R          # corrected tables + figures
Rscript clean/91_compare.R          # headline comparison + verification checks
Rscript clean/92_full_audit.R       # table-by-table change inventory
```

- **90** (~2 min) copies `data/` and `scripts/` to a scratch directory,
  runs the original scripts unmodified (inserting the omitted `03d`), and
  value-compares all 40 regenerated CSVs against the committed `tabs/`
  (`tabs_clean/90_repro_manifest.csv`). Expected: every numeric script
  runs (`06_figs.R` fails, issue 12) and every table matches except the
  ~10 exact-tie cells (portability note).
- **05** (~1 min) writes the 27 corrected tables to `tabs_clean/` and the
  28 corrected figures to `figs_clean/`.
- **91** prints the published vs committed vs corrected comparison and
  runs the verification checks (pair-level equivalence, exact mirror
  negation, Mean-row decomposition, tie sensitivity); all should PASS.
- **92** prints and writes the change inventory summarized above.

Requirements: R 4.6.0 with the packages pinned in `renv.lock` — run
`renv::restore()` from the project root (this covers both the revised
pipeline and the original-scripts rerun, including the GitHub-installed
goji). The lockfile pins package versions, not floating-point behavior;
the ~10 exact-tie cells in the repro comparison are the portability note
above, not a package-version effect.
