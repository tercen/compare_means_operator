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

if(length(ctx$colors) < 1) stop("A color factor is required.")

equal_variances <- ctx$op.value('equal_variances', as.logical, FALSE)
comparison <- ctx$op.value('comparison', as.character, 'pairwise')
reference_index <- ctx$op.value('reference_index', as.double, 1)
plot_type <- ctx$op.value('plot_type', as.character, "png")

if(equal_variances) {
  global_method <- 'anova'
  post_hoc_method <- 't.test'
} else {
  global_method <- 'kruskal.test'
  post_hoc_method <- 'wilcox.test'
}


col_names <- unlist(ctx$colors)[1]
fixed_effect <- paste0(
  paste0("`", col_names, "`"),
  collapse = " + "
)

form <- formula(paste0(".y ~ ", fixed_effect))

df <- ctx$select(c(".ci", ".ri", ".y", ".x", col_names)) %>%
  group_by(.ci, .ri) 

df1 <- compare_means(form, df, group.by = c(".ci", ".ri"), method = global_method)
df2 <- compare_means(form, df, group.by = c(".ci", ".ri"), method = post_hoc_method)

df_out <- full_join(df1, df2) %>% 
  select(-.y.) %>%
  ctx$addNamespace()

p <- ggboxplot(
  df,
  x = ".x",
  y = ".y",
  color = col_names,
  palette = "npg", 
  add = "jitter"
)

ref.group <- NULL
combs <- NULL
if(comparison == "pairwise") {
  ref.group
  combs <- combn(unique(df$condition), 2, simplify = FALSE)
  
} else if(comparison == "all") {
  ref.group <- ".all."
} else {
  ref.group <- unique(df[[col_names]])[reference_index]
}


p <- p + 
  stat_compare_means(
    # aes(label = "..p.signif.."),
    label = "p.signif",
    method = post_hoc_method,
    comparisons = combs,
    ref.group = ref.group,
  )

p <- p +
  stat_compare_means(
    label.y = layer_scales(p)$y$range$range[2] * 1.1,
    method = global_method
  ) +
  theme_minimal()

plt_file <- tim::save_plot(
  p,
  type = plot_type,
  width = 600,
  height = 600,
  units = "px",
  dpi = 144
)

join_png <- tim::plot_file_to_df(plt_file, filename = paste0("Boxplot.", plot_type)) %>% 
  ctx$addNamespace() %>%
  as_relation()

join_res <- df_out %>%
  as_relation() %>%
  left_join_relation(ctx$crelation, ".ci", ctx$crelation$rids) %>%
  left_join_relation(ctx$rrelation, ".ri", ctx$rrelation$rids) %>%
  left_join_relation(join_png, list(), list()) %>%
  as_join_operator(list(), list())

join_res %>%
  save_relation(ctx)
