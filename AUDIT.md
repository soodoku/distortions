# Correction and validation report

## Verdict

The published evidence supports a narrower conclusion than the article states.
Across the observed deliberative polls, average signed homogenization,
directional polarization, and domination are small. The corrected domination
results do not support a general tendency toward opposition. Education points
toward modest domination, while gender and income are near zero. The combined
estimate is positive but sensitive to the small-cluster inference method.

These estimates describe 21 selected deliberative polls. The design has no
untreated comparison and does not identify deliberation, working memory, or any
specific design feature as the cause of the observed changes.

## Analysis universe

The published inventories mix several versions of the data. The corrected
pipeline uses the following explicit universe:

| Quantity | Published descriptions | Corrected value |
|---|---:|---:|
| Participant records | 5,736 or 6,084 | 6,084 raw records |
| Exact duplicate records | Not reported | 217 |
| Distinct analysis participants | Not reported | 5,867 |
| Small groups | 372 or 397 | 397 |
| Policy indices | 134 or 139 | 129 |
| Group-index pairs | 2,601 | 2,480 |

All 217 duplicates occur in the US Presidential Primaries poll. Each duplicate
has the same poll, case identifier, group, covariates, and responses as its
retained record. Removing them does not change any group mean beyond numerical
precision, but it corrects participant counts and sample standard deviations.

The original code uses stale poll-level `numindices` values as weights. The
corrected analysis gives one unit of weight to each valid group-index pair and
records an outcome-specific denominator.

## Corrected Table 2

The point estimate is the mean over actual valid group-index pairs. The primary
standard error uses CR2 clustered by deliberative poll with Satterthwaite
degrees of freedom. The wild-cluster column reports a null-imposed bootstrap
with 99,999 draws. Frequencies are tested against .5; signed means against zero.

| Construct | Estimate | SE | 95% CI | CR2 p | Wild p | N |
|---|---:|---:|---:|---:|---:|---:|
| H | .01285 | .00311 | [.00617, .01953] | .0010 | .0010 | 2,480 |
| Hb | .58589 | .02022 | [.54247, .62930] | .0008 | .0004 | 2,480 |
| P | -.02221 | .00838 | [-.04019, -.00423] | .0191 | .0215 | 2,431 |
| Pb | .43974 | .04068 | [.35245, .52702] | .1608 | .1839 | 2,431 |
| Gender D | .00175 | .00352 | [-.00580, .00931] | .6260 | .6305 | 2,436 |
| Gender Db | .47126 | .01420 | [.44080, .50173] | .0626 | .0636 | 2,436 |
| Education D | .01004 | .00313 | [.00332, .01676] | .0063 | .0009 | 2,385 |
| Education Db | .52327 | .01715 | [.48650, .56004] | .1962 | .1986 | 2,385 |
| Income D | .00151 | .00419 | [-.00829, .01130] | .7292 | .7277 | 1,144 |
| Income Db | .50524 | .01753 | [.46424, .54625] | .7730 | .7647 | 1,144 |
| Combined D | .01285 | .00594 | [-.00100, .02670] | .0646 | .0414 | 983 |
| Combined Db | .51882 | .01528 | [.48317, .55447] | .2553 | .2399 | 983 |

The paper defines `Db = 1` when `D > 0` and `Db = 0` when `D <= 0`.
Accordingly, genuine no-movement cases remain in the denominator as zero.
Cases where the initial group mean equals the initial advantaged mean have no
defined direction under Equation 3 and are excluded rather than recoded zero.

Across the 12 primary tests, Holm correction retains H and Hb. The wild-cluster
Holm correction also retains education D. Benjamini and Hochberg correction for
the CR2 p-values retains H, Hb, and education D. These adjustments do not
support a broad opposition claim.

## Corrected Table 3

`dM` is disadvantaged-member movement toward the advantaged reference. `aM`
uses the same domination direction for advantaged members, so negative values
mean movement toward disadvantaged members. Frequencies use the same signs and
count defined zero movement as false.

| Dimension | D | dM | aM | Db | dMb | aMb |
|---|---:|---:|---:|---:|---:|---:|
| Gender | .00175 | .03947 | -.03665 | .47126 | .57160 | .36192 |
| Education | .01004 | .05224 | -.03011 | .52327 | .59346 | .39045 |
| Income | .00151 | .04899 | -.03256 | .50524 | .56731 | .41259 |
| Combined | .01285 | .02945 | -.06413 | .51882 | .55239 | .30319 |

The subgroup estimates show movement toward one another. They do not imply that
the group mean generally moves away from the advantaged reference. The
advantaged subgroup often moves farther toward the disadvantaged subgroup than
the disadvantaged subgroup moves toward the advantaged subgroup, while the
unequal subgroup shares leave the whole-group D near zero or modestly positive.

## Other corrected statements

- The H and P correlation is .4043 over all 2,431 valid common pairs. The
  published .358 is a listwise-selected correlation conditioned on all original
  domination columns being observed.
- The combined disadvantaged share means female or lower education or lower
  income. The corrected combined D regression is essentially flat. Predicted D
  is .01282 at a disadvantaged share of .20, .01285 at .80, and .01286 at .95;
  the slope p-value is .999.
- Mean absolute net attitude change is .08941, not .092. Mean gross change is
  .20266, which still rounds to .203.
- If two of twenty people move by .1, the group mean moves by .010, not .10.
- Table 2 contains 12 estimates, not ten. Four have unadjusted CR2 p-values
  below .05 under the corrected primary inference.
- H is not mathematically bounded by .5 when calculated with the sample standard
  deviation. The published range statement is false for small groups.
- The paper's directional P is movement away from the midpoint, not conventional
  change in extremity. The correction retains that construct name explicitly.

## Inference and scope

The article states that it uses Huber-White standard errors clustered by policy
index. The published code instead fits a per-poll random-intercept model by
issue and combines poll estimates with a fixed-effect meta-analysis.

The corrected primary analysis clusters by poll. This absorbs repeated groups,
repeated issues, and other shared poll-level conditions. A two-way group and
issue covariance estimate is retained as a sensitivity analysis in the
generated Table 2 file, but it assumes that cells within a poll are independent
when they share neither group nor issue. That assumption is too strong for the
primary analysis.

The 21 polls, and the 11 polls with income measures, were not randomly sampled.
The intervals therefore describe an exchangeable-poll sensitivity model. They
do not turn the selected polls into a probability sample or establish a causal
effect of deliberation.

## Reproduction and provenance

Run the complete correction from the repository root:

```sh
make ci
```

The authoritative outputs are:

- `tabs_clean/02_table_2_corrected.csv`
- `tabs_clean/03_table_3_corrected.csv`
- `tabs_clean/05_attitude_change.csv`
- `tabs_clean/05_corr_hpd.csv`
- `tabs_clean/07_parsing_domination.csv`
- `tabs_clean/99_validation.csv`
- `figs_clean/figure_manifest.csv`
- `provenance/claims.csv`, `values.csv`, `artifacts.csv`, and `checks.csv`

The repository has no editable manuscript source. The published PDF cannot be
rebuilt locally, so the journal article remains immutable. The claim ledger
identifies each journal location that requires an erratum or revised article.
The original `scripts/`, `tabs/`, and `figs/` remain unchanged as historical
provenance.
