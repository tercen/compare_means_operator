suppressPackageStartupMessages({
  library(tercen)
  library(tercenApi)
  library(dplyr)
  library(rstatix)
})

ctx = tercenCtx()

if(length(ctx$colors) < 1) stop("A color factor is required.")

method <- ctx$op.value('method', as.character, 'one.way')
collapse_str <- c("one.way" = " + ", "two.way" = " * ")[method]

col_names <- unlist(ctx$colors)
fixed_effect <- paste0(
  paste0("`", col_names, "`"),
  collapse = collapse_str
)

## If labels, add random effect:
lab_names <- unlist(ctx$labels)
if(!is.null(lab_names)) {
  random_effect <- paste0(" + Error(", paste0("`", lab_names, "`"), "/(", fixed_effect, "))")
  form <- formula(paste0(paste0(".y ~ ", fixed_effect), random_effect))
} else {
  form <- formula(paste0(".y ~ ", fixed_effect))
}

df <- ctx$select(c(".ci", ".ri", ".y", col_names, lab_names)) %>%
  group_by(.ci, .ri) %>%
  anova_test(., form) %>%
  as_tibble() %>%
  rename(significance = !!sym("p<.05")) %>%
  ctx$addNamespace() %>%
  ctx$save()
