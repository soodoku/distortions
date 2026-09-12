# Input: one row per participant, group, item, and deliberative episode.
score_groups <- function(data, membership = c("paired", "available")) {
  membership <- match.arg(membership)
  key <- c("event_id", "episode_id", "group_id", "item_id", "participant_id")
  stopifnot(
    !anyDuplicated(data[key]), !anyNA(data[key]),
    all(is.na(data$t1) | between(data$t1, 0, 1)),
    all(is.na(data$t2) | between(data$t2, 0, 1))
  )
  data <- data |>
    arrange(across(all_of(key))) |>
    mutate(paired = !is.na(t1) & !is.na(t2))
  if (membership == "paired") {
    data <- data |> mutate(across(c(t1, t2), \(x) if_else(paired, x, NA_real_)))
  }

  group_keys <- c("event_id", "episode_id", "group_id", "item_id", "construct", "midpoint")
  hp <- data |>
    summarise(
      n_pre = sum(!is.na(t1)), n_post = sum(!is.na(t2)),
      n_paired = sum(paired),
      mean_pre = na_if(mean(t1, na.rm = TRUE), NaN),
      mean_post = na_if(mean(t2, na.rm = TRUE), NaN),
      sd_pre = sd(t1, na.rm = TRUE), sd_post = sd(t2, na.rm = TRUE),
      .by = all_of(group_keys)
    ) |>
    mutate(
      h = sd_pre - sd_post,
      p = -signed_move(mean_pre, mean_post, midpoint),
      p_absolute = abs(mean_post - midpoint) - abs(mean_pre - midpoint)
    ) |>
    pivot_longer(c(h, p, p_absolute), names_to = "metric", values_to = "estimate") |>
    select(all_of(group_keys), metric, estimate, n_pre, n_post, n_paired)

  domination <- data |>
    pivot_longer(c(gender, education, income, combined),
      names_to = "dimension", values_to = "advantaged"
    ) |>
    filter(!is.na(advantaged)) |>
    group_by(event_id, episode_id, item_id, construct, midpoint, dimension) |>
    group_modify(\(d, keys) {
      counts <- d |>
        summarise(
          n_pre = sum(!is.na(t1)), n_post = sum(!is.na(t2)),
          n_paired = sum(paired),
          both_pre = n_distinct(advantaged[!is.na(t1)]) == 2L,
          both_post = n_distinct(advantaged[!is.na(t2)]) == 2L,
          .by = group_id
        )
      means <- dom_pairs_index(mutate(d, pollgroup = group_id), "t1", "t2", d$advantaged)
      if (is.null(means)) {
        return(tibble())
      }
      means |>
        select(group_id, estimate = ext_grp) |>
        left_join(counts, by = "group_id", relationship = "one-to-one") |>
        mutate(estimate = if_else(both_pre & both_post, estimate, NA_real_)) |>
        select(-both_pre, -both_post)
    }) |>
    ungroup()

  if (nrow(domination) > 0) {
    domination <- domination |> mutate(metric = paste0("d_", dimension)) |> select(-dimension)
  }
  bind_rows(hp, domination) |>
    mutate(membership = membership, positive = movement_frequency(estimate)) |>
    arrange(event_id, episode_id, group_id, item_id, metric)
}
