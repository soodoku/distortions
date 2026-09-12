# Out-of-sample deliberation study

This study asks whether the corrected paper's homogenization, polarization, and
domination patterns recur in previously unused Deliberative Polls, and whether
comparable changes occur in other deliberative formats. The original analysis is
fixed at commit `0e8f3867288acfe95285bff21a6bb8949d1ec9f5`.

## Protocol recorded before new outcome calculations

Recorded September 11, 2026. Public papers and source documentation have already
been read, and variable availability and sample sizes inspected. This is a
retrospective analysis plan, not a blinded preregistration. Independent design
review identified participant overlap, changing wave membership, missing group
identifiers, and sparse independent events as principal risks.

Search public material available through September 11, 2026: Stanford's project
catalogue, Dataverse, OSF, ICPSR, national archives, research-team websites, and
deliberation registries. Search variants of deliberation, deliberative poll,
mini-public, citizens' assembly/jury, discussion experiment, and polarization;
follow references and citing studies from reviews and eligible studies. Record
searches and dispositions, including inaccessible sources. Discovery remains open
until these searches and two successive citation rounds yield no new eligible
studies. No author requests will be sent without authorization.

Eligibility is specific to group, item, episode, and metric. Require actual
interaction groups, structured discussion of reasons/evidence, private individual
ratings before and after the episode, and documented comparable instruments.
Do not substitute treatment arms, venues, or collective verdicts for discussion
groups or private ratings. Ordinary non-deliberating panels are outside scope.
Exclude the original 21 polls and record repeated publications and cohorts.

The direct replication uses available responses at each wave. The extension's
primary sample uses the same respondents at both waves, item by item; both rules
are reported for every included event. Use the last pre-deliberation and first
post-deliberation measurement; report intervals and separate later follow-ups.
No outcome, group-identity, or demographic imputation. Normalize by documented
instrument endpoints, never observed sample extrema. Keep policy attitudes,
affective ratings, and other judgments in separate outcome strata. Rankings and
nominal choices do not acquire cardinal scores merely to enter this analysis.

H is sample SD before minus sample SD after. Directional P is movement away from
the initial group mean's direction relative to the scale midpoint; also report
change in absolute distance from the midpoint. Primary P requires a defensible
midpoint. D is movement of the whole-group mean toward the initial advantaged
subgroup mean. Reuse the corrected paper's movement function: reference ties are
undefined, genuine zero movement remains zero. Require two observed respondents
per wave for H and both demographic categories for D. Report a minimum-five
respondent sensitivity. Recompute D's means and reference on the eligible sample.

Gender, education, income, and combined advantage follow the paper where source
definitions permit: male, above-median education, above-median income, and the
intersection of those three. Calculate socioeconomic thresholds among participating
delegates before outcome-specific filtering; report ties and missing categories.
Context and measurement must support each classification. Proxy socioeconomic
measures and additional dimensions remain separate exploratory outcomes.

Report events first, separating Deliberative Polls from other formats. Pool only
comparable outcome strata, weighting each valid group-item-episode equally.
Sensitivity: equal independent-family weight, then equal event weight within
family; leave one family out. Link known overlapping participants and rounds in
one event family. Use CR2/Satterthwaite at that level, reporting effective degrees
of freedom and largest-family share. With fewer than two families, non-estimable
covariance, or effective df below four, report estimates without inferential
claims. Display intervals rather than significance stars; no confirmatory claims
are planned. Any subsequent confirmatory tests require a stated family and Holm
adjustment.

Fieldwork date, baseline/post timing, publication date, and source-version access
date are separate fields. Older unused studies are out of sample but not later
temporal replications. Format and date comparisons describe the observed corpus;
within-group change does not by itself identify deliberation's causal effect,
subgroup influence, or a mechanism. Discuss material interpretation changes with
the author before revising manuscript claims.

## Validation and delivery

One R entry point will obtain version-checked sources, prepare eligible data,
calculate outcomes, and write tables. Source-specific transformations remain
small, explicit R scripts; shared metric definitions stay in the original helper
file. The source register and item dictionary document joins, codes, exclusions,
and unresolved definitions. Downloaded files are cached, not duplicated in Git.

Validate source sample counts, unique keys, documented bounds and missing codes,
join cardinality, genuine group identity, overlap, row-order invariance, and
hand-calculated H/P/D examples. Run the original checks and independent review.
Deliver a PR with source dispositions, sample flows, event results and comparison
to the corrected paper. Public-data access gaps remain explicit, not silently
treated as evidence of absent effects or as completed replication.
