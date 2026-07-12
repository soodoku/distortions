"
Deliberative Distortions -- clean pipeline
Shared functions
"

suppressPackageStartupMessages({
    library(dplyr)
    library(tidyr)
    library(purrr)
    library(tibble)
})

# NaN (empty mean) -> NA, so a pair whose reference cannot be computed is
# treated as missing. (goji::nona, used by the original scripts, maps
# NA/NaN to 0; see issue 4 in AUDIT.md.)
nona <- function(x) {
    x[is.nan(x)] <- NA
    x
}

# Eq. 3: signed movement of an entity from t1 to t2 toward a reference point.
# Positive = toward the reference; sign(0) = 0 covers both tie cases (no
# movement, or reference equal to the entity's t1 mean). Differences below
# eps count as ties so that analytically equal means, computed from
# different row subsets, score the same in every floating-point environment
# (see the portability note in AUDIT.md).
signed_move <- function(t1, t2, ref_t1, eps = 1e-12) {
    gap <- if_else(abs(ref_t1 - t1) < eps, 0, ref_t1 - t1)
    move <- if_else(abs(t2 - t1) < eps, 0, t2 - t1)
    move * sign(gap)
}

# Normed movement, same convention as the original scripts: divide by the
# headroom (1 - t1) when the entity moved up, by t1 when it moved down.
# No .default: an NA comparison must stay NA, not become 0. A boundary t1
# (headroom 0) makes the normed value undefined, not infinite.
normed_move <- function(signed, t1, t2) {
    out <- case_when(is.na(signed) ~ NA,
                     t2 > t1 ~ signed / (1 - t1),
                     t2 < t1 ~ signed / t1,
                     t2 == t1 ~ 0)
    if_else(is.finite(out), out, NA)
}

# One row per small group for one poll x attitude index: group, advantaged,
# and disadvantaged subgroup means at t1/t2, plus the four signed movements.
# adv is a logical over smdata rows. Groups without at least one advantaged
# and one disadvantaged member are dropped (as in the original scripts).
dom_pairs_index <- function(smdata, t1var, t2var, adv) {
    means <- tibble(group_id = as.character(smdata$pollgroup),
                    adv = adv,
                    t1 = smdata[[t1var]],
                    t2 = smdata[[t2var]]) |>
        summarise(across(c(t1, t2), \(x) nona(mean(x, na.rm = TRUE))),
                  .by = c(group_id, adv))

    stopifnot(!anyNA(adv))
    both_sides <- means |>
        count(group_id) |>
        filter(n == 2)
    if (nrow(both_sides) == 0) return(NULL)

    grp <- tibble(group_id = as.character(smdata$pollgroup),
                  t1 = smdata[[t1var]],
                  t2 = smdata[[t2var]]) |>
        summarise(across(c(t1, t2), \(x) nona(mean(x, na.rm = TRUE))),
                  .by = group_id) |>
        rename(grp_t1 = t1, grp_t2 = t2)

    means |>
        semi_join(both_sides, by = "group_id") |>
        pivot_wider(names_from = adv, values_from = c(t1, t2)) |>
        rename(adv_t1 = t1_TRUE, dis_t1 = t1_FALSE,
               adv_t2 = t2_TRUE, dis_t2 = t2_FALSE) |>
        left_join(grp, by = "group_id") |>
        arrange(group_id) |>
        mutate(ext_grp     = signed_move(grp_t1, grp_t2, adv_t1),  # Eq. 3: D
               ext_dis     = signed_move(dis_t1, dis_t2, adv_t1),  # disadv. toward adv.
               ext_grp_mir = signed_move(grp_t1, grp_t2, dis_t1),  # mirror: toward disadv.
               ext_adv_mir = signed_move(adv_t1, adv_t2, dis_t1),  # adv. toward disadv.
               ext_grp_normed = normed_move(ext_grp, grp_t1, grp_t2),
               ext_dis_normed = normed_move(ext_dis, dis_t1, dis_t2))
}

# Mean and Weighted Mean rows computed over the poll rows only; the Mean row
# is not part of the data the Weighted Mean averages over. Assumes the first
# column is pollname and every other column is numeric.
add_summary_rows <- function(res, weight_cols = c("ngroups", "nindices")) {
    vals <- res |>
        select(-pollname) |>
        mutate(across(everything(), as.numeric))
    w <- vals[[weight_cols[1]]] * vals[[weight_cols[2]]]
    bind_rows(
        res,
        vals |>
            summarise(across(everything(), \(x) mean(x, na.rm = TRUE))) |>
            mutate(pollname = "Mean"),
        vals |>
            summarise(across(everything(),
                             \(x) weighted.mean(x, w = w, na.rm = TRUE))) |>
            mutate(pollname = "Weighted Mean (By Indices and Groups)"))
}

# Fixed-effect meta-analytic mean: with external weights the standard error is
# sqrt(sum(w^2 se^2)) / sum(w); with w = 1/se^2 this reduces to
# sqrt(1 / sum(1/se^2)) (what rmeta::meta.summaries, method = "fixed", returns).
fe_meta <- function(x, se, w = 1 / se^2) {
    est <- weighted.mean(x, w)
    se_est <- sqrt(sum(w^2 * se^2)) / sum(w)
    list(est = est, se = se_est, p = 2 * pnorm(-abs(est / se_est)))
}

load_dp_data <- function() {
    list(dpdat = read.csv("data/polardata.csv"),
         att_indices = read.csv("data/poll_indices.csv"))
}
