suppressPackageStartupMessages({
  library(tercen)
  library(tercenApi)
  library(dplyr)
  library(rstatix)
  library(ggpubr)
  library(svglite)
  library(tim)
})

ctx = tercenCtx()

equal_variances <- ctx$op.value('equal_variances', as.logical, FALSE)
comparison <- ctx$op.value('comparison', as.character, 'pairwise')
reference_index <- ctx$op.value('reference_index', as.double, 1)
plot_type <- ctx$op.value('plot_type', as.character, "png")

pval_label <- ctx$op.value('pval_label', as.character, "p.signif")

plot_width <- ctx$op.value('plot_width', as.double, 800)
plot_height <- ctx$op.value('plot_height', as.double, 600)


if(equal_variances) {
  global_method <- 'anova'
  post_hoc_method <- 't.test'
} else {
  global_method <- 'kruskal.test'
  post_hoc_method <- 'wilcox.test'
}

if(length(ctx$xAxis) == 0 & length(ctx$colors) == 0) {
  stop("At least one color or x-axis factor must be provided.")
}

if(length(ctx$xAxis) == 0) {
  xAxis <- ctx$colors[[1]]
  xAxis_str <- xAxis
} else {
  xAxis <- ctx$xAxis
  xAxis_str <- ".x"
}

if(length(ctx$colors) == 0) {
  col_names <- xAxis_str# ctx$xAxis
} else {
  col_names <- unlist(ctx$colors)[1]
}

form <- formula(paste0(".y ~ .x"))

df <- ctx$select(unique(c(".ci", ".ri", ".y", xAxis_str, col_names))) %>%
  group_by(.ci, .ri) 
# 
# df <- df %>% group_by_at(c(".ci", ".ri", xAxis_str, col_names)) %>%
#   count() %>%
#   filter(n > 2) %>%
#   ungroup() %>%
#   select(.ci, .ri) %>%
#   inner_join(df, c(".ci", ".ri")) %>%
#   group_by(.ci, .ri) 

if(col_names != unlist(xAxis) & length(ctx$colors) > 0) {
  df$.x <- paste(df$.x, df[[col_names]], sep = "_")
  df <- arrange(df, .x)
}
if(!ctx$hasXAxis) {
  df$.x <- df[[col_names]]
}

df1 <- compare_means(form, df, group.by = c(".ci", ".ri"), method = global_method)
df2 <- compare_means(form, df, group.by = c(".ci", ".ri"), method = post_hoc_method)

df_out <- full_join(df1, df2) %>% 
  select(-.y.) %>%
  ctx$addNamespace()

ref.group <- NULL
combs <- NULL

if(comparison %in% c("pairwise", "groups")) {
  combs <- combn(c(unique(df[[col_names]])), 2, simplify = FALSE)
  if(col_names != unlist(xAxis)) {
    combs <- combn(c(unique(df$.x)), 2, simplify = FALSE)
  }
  if(comparison == "groups") {
    idx <- unlist(lapply(combs, function(x) {
      vc <- unlist(lapply(strsplit(x, "_"), "[[", 1))
      length(unique(vc)) == 1
    }))
    combs <- combs[idx]
  }
} else if(comparison == "all") {
  ref.group <- ".all."
} else {
  ref.group <- unique(df$.x)[reference_index]
}

do.plot <- function(df) {
  uniq <- unique(df$.x)
  idx <- unlist(lapply(combs, function(x) all(x %in% uniq)))
  
  p <- ggboxplot(
    df,
    x = ".x",
    y = ".y",
    color = col_names,
    palette = "npg", 
    add = "jitter",
    facet.by = c(".ri", ".ci")
  ) + 
    stat_compare_means(
      label = pval_label,
      method = post_hoc_method,
      comparisons = combs[idx],
      ref.group = ref.group
    )
  
  p <- p +
    stat_compare_means(
      label.y = layer_scales(p)$y$range$range[2] * 1.1,
      method = global_method
    ) +
    theme_minimal(base_family = "sans") +
    labs(x = xAxis[[1]], y = ctx$yAxis[[1]], color = col_names)
  
  if(plot_type ==  "svg2") {
    type <- "svg"
    device <- svg
    
  } else {
    type <- plot_type
    device <- NULL
  }
  
  fname <- tim::save_plot(
    p,
    type = type,
    width = plot_width,
    height = plot_height,
    units = "px",
    dpi = 144,
    device = device
  ) 
  tibble(filename = fname) %>%
    mutate(.ci = df$.ci[1], .ri = df$.ri[1])
}

plts <- df %>% 
  group_split(.ci, .ri) %>%
  lapply(do.plot) %>%
  bind_rows()

join_png <- tim::plot_file_to_df(plts$filename, filename = paste0("Boxplot.", plot_type)) %>% 
  bind_cols(plts %>% select(.ci, .ri)) %>%
  ctx$addNamespace() # %>%
# as_relation()

# join_res <- df_out %>%
#   as_relation() %>%
#   {if(ctx$cnames[[1]] != "") left_join_relation(., ctx$crelation, ".ci", ctx$crelation$rids)  else .} %>%
#   {if(ctx$rnames[[1]] != "") left_join_relation(., ctx$rrelation, ".ri", ctx$rrelation$rids)  else .} %>%
#   left_join_relation(join_png, list(), list()) %>%
#   as_join_operator(list(), list())
# 
# join_res %>%
#   save_relation(ctx)

ctx$save(list(df_out, join_png))
