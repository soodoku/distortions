"
Deliberative Distortions -- clean pipeline
Figures: density and by-poll boxplots of polarization, homogenization, and
domination (all four dimensions), plus Anglo/Other and Online/F2F splits.

Replaces scripts/06_figs.R, which no longer runs (it reads a mirror pair
file that lacks a poll_name column) and drew its domination panels from a
mix of differently-referenced files (issue 12 in AUDIT.md). All panels here
use the Eq. 3 (advantaged-referenced) pair files in tabs_clean/.
"

source("clean/00_functions.R")
library(ggplot2)

anglo <- c(
  "UK EU", "UK Health", "UK Monarchy", "UK General Election",
  "UK Crime", "Central Power & Light", "San Mateo, CA",
  "West Texas Utilities", "National Issues Convention",
  "By the People: National", "Southwestern Electric Power",
  "By the People: Health and Education",
  "By the People 2004 US General Election",
  "By the People 2004 US Presidential Primaries",
  "New Haven, CT", "National Issues Convention 2"
)

online <- c(
  "By the People: National",
  "By the People 2004 US General Election",
  "By the People 2004 US Presidential Primaries",
  "By the People: Health and Education",
  "National Issues Convention"
)

fig_theme <- theme_bw() +
  theme(
    axis.text = element_text(size = 7),
    axis.ticks = element_blank(),
    panel.grid.major.x = element_line(color = "#eeeeee"),
    panel.grid.major.y = element_line(
      color = "#dddddd",
      linetype = "dotted"
    ),
    panel.grid.minor = element_blank(),
    plot.margin = unit(rep(.5, 4), "cm"),
    panel.border = element_blank(),
    legend.title = element_blank()
  )

density_fig <- function(d, lab, file) {
  p <- ggplot(d, aes(value)) +
    geom_histogram(aes(y = after_stat(density)),
      bins = 40,
      fill = "grey75", color = "white", linewidth = .2
    ) +
    geom_vline(xintercept = 0, linewidth = .35) +
    labs(x = lab, y = "Density") +
    fig_theme
  ggsave(file, p, width = 3.3, height = 3)
}

poll_boxplot_fig <- function(d, file) {
  size <- if (n_distinct(d$poll_name) > 15) 7 else 5
  p <- ggplot(d, aes(poll_name, value)) +
    geom_boxplot() +
    coord_flip() +
    labs(x = NULL, y = NULL) +
    fig_theme
  ggsave(file, p, width = size, height = size)
}

split_density_fig <- function(d, split, lab, file) {
  p <- ggplot(d, aes(value,
    fill = .data[[split]], color = .data[[split]],
    group = .data[[split]]
  )) +
    geom_density(aes(y = after_stat(scaled)), alpha = .1) +
    scale_fill_grey() +
    scale_color_grey() +
    geom_vline(xintercept = 0, linewidth = .35) +
    labs(x = lab, y = "Scaled density") +
    fig_theme
  ggsave(file, p, width = 5, height = 5)
}

split_boxplot_fig <- function(d, split, lab, file) {
  p <- ggplot(d, aes(.data[[split]], value)) +
    geom_boxplot() +
    coord_flip() +
    labs(x = NULL, y = lab) +
    fig_theme
  ggsave(file, p, width = 5, height = 5)
}

hp <- read.csv("tabs_clean/03_hom_pol_by_group_issue.csv")

dom_pairs <- function(dim_name) {
  read.csv(sprintf("tabs_clean/03_dom_%s_by_group_issue.csv", dim_name)) |>
    rename(value = ext_grp)
}

sets <- list(
  polarization = list(
    d = hp |> rename(value = polarex),
    lab = expression(P["gj"]), dom = FALSE
  ),
  homogenization = list(
    d = hp |> rename(value = homoex),
    lab = expression(H["gj"]), dom = FALSE
  ),
  dom_gender = list(d = dom_pairs("gender"), lab = expression(D["gj"]), dom = TRUE),
  dom_educ = list(d = dom_pairs("educ"), lab = expression(D["gj"]), dom = TRUE),
  dom_income = list(d = dom_pairs("income"), lab = expression(D["gj"]), dom = TRUE),
  dom_triple = list(d = dom_pairs("triple"), lab = expression(D["gj"]), dom = TRUE)
)

dir.create("figs_clean", showWarnings = FALSE)
manifest <- list()

for (name in names(sets)) {
  s <- sets[[name]]
  d <- s$d |>
    filter(!is.na(value)) |>
    mutate(
      anglo = if_else(poll_name %in% anglo, "Anglo", "Other"),
      online = if_else(poll_name %in% online, "Online", "F2F")
    )

  density_fig(d, s$lab, sprintf("figs_clean/density_%s.png", name))
  poll_boxplot_fig(d, sprintf("figs_clean/boxplot_%s.png", name))
  split_density_fig(
    d, "anglo", s$lab,
    sprintf("figs_clean/density_anglo_%s.png", name)
  )
  split_density_fig(
    d, "online", s$lab,
    sprintf("figs_clean/density_online_f2f_%s.png", name)
  )
  if (s$dom) {
    split_boxplot_fig(
      d, "anglo", s$lab,
      sprintf("figs_clean/boxplot_anglo_%s.png", name)
    )
  }
  manifest[[name]] <- tibble(
    construct = name,
    pair_file = if (s$dom) {
      sprintf("tabs_clean/03_%s_by_group_issue.csv", name)
    } else {
      "tabs_clean/03_hom_pol_by_group_issue.csv"
    },
    value_column = if (s$dom) "ext_grp" else if (name == "polarization") "polarex" else "homoex",
    n_pairs = nrow(d),
    density_figure = sprintf("figs_clean/density_%s.png", name),
    poll_figure = sprintf("figs_clean/boxplot_%s.png", name),
    anglo_figure = sprintf("figs_clean/density_anglo_%s.png", name),
    mode_figure = sprintf("figs_clean/density_online_f2f_%s.png", name)
  )
}

write.csv(list_rbind(manifest), "figs_clean/figure_manifest.csv", row.names = FALSE)
