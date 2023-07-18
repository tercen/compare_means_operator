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

set.seed(123)

equal_variances <- ctx$op.value('equal_variances', as.logical, FALSE)
comparison <- ctx$op.value('comparison', as.character, 'pairwise')
reference_index <- ctx$op.value('reference_index', as.double, 1)
plot_type <- ctx$op.value('plot_type', as.character, "png")

displayed.aggregate <- ctx$op.value('displayed.aggregate', as.character, "mean_ci")
displayed.aggregate <- c(gsub("_.*", "", displayed.aggregate), displayed.aggregate)
displayed.box <- ctx$op.value('displayed.box', as.character, "none")
if(displayed.box != "none") {
  displayed.aggregate <- c(displayed.box, displayed.aggregate)
}
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

cval <- ctx$cselect() %>%
  mutate(.ci = seq_len(nrow(.)) - 1L)
rval <- ctx$rselect() %>%
  mutate(.ri = seq_len(nrow(.)) - 1L)
cnames <- ctx$cnames
if(cnames[[1]] == "") cnames <- ".all"
rnames <- ctx$rnames
if(rnames[[1]] == "") rnames <- ".all"
if(cnames[[1]] == "" & rnames[[1]] == "") {
  cnames <- ".all.x"
  rnames <- ".all.y"
}

df <- ctx$select(unique(c(".ci", ".ri", ".y", xAxis_str, col_names))) %>% 
  left_join(cval, ".ci") %>%
  left_join(rval, ".ri") %>%
  group_by(.ci, .ri) 

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
  
  plot.title <- df %>% head(1) %>%
    select(matches(unlist(c(cnames, rnames)))) %>%
    select(!contains(".all")) %>%
    mutate(across(everything(), ~ paste0(cur_column(), " = ", .x))) %>%
    tidyr::unite(title, everything(), sep = "; ")

  p <- ggdotplot(
    df,
    x = ".x",
    y = ".y",
    color = col_names,
    palette = "npg", 
    add = displayed.aggregate,
    add.params = list(color = "black", size = 0.5),
    error.plot = "errorbar"
  ) + 
    stat_compare_means(
      label = pval_label,
      method = post_hoc_method,
      comparisons = combs[idx],
      ref.group = ref.group,
      step.increase = 0.2
    )
  
  p <- p +
    stat_compare_means(
      label.y = layer_scales(p)$y$range$range[2] * 1.1,
      method = global_method
    ) +
    theme_classic(base_family = "sans") +
    labs(title = plot.title[[1]], x = xAxis[[1]], y = ctx$yAxis[[1]], color = col_names)

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

plts <- suppressWarnings(suppressMessages({
  if(ctx$cnames[[1]] == "" & ctx$rnames[[1]] == "") {
    df %>%
      do.plot() %>%
      bind_rows()
  } else {
    df %>%
      group_split(.ci, .ri) %>%
      lapply(do.plot) %>%
      bind_rows()
  }
}))

plot_file_to_df <- function (file_path, filename = NULL)  {
  if (is.null(filename)) 
    filename <- basename(file_path)
  type <- tools::file_ext(filename)
  mimetype <- sapply(type, function(x) switch(x, png = "image/png",
                                              svg = "image/svg+xml",
                                              svg2 = "image/svg+xml",
                                              pdf = "application/pdf", "unknown"))
  output_str <- sapply(file_path, function(x) {
    base64enc::base64encode(readBin(x, "raw", file.info(x)[1, 
                                                           "size"]), "txt")
  })
  df <- tibble::tibble(filename = filename, mimetype = mimetype, .content = output_str)
  return(df)
}

join_png <- plot_file_to_df(
  plts$filename,
  filename = paste0("Chart", paste0("_ci", plts$.ci, "_ri", plts$.ri, "."), plot_type)
) %>% 
  bind_cols(plts %>% select(.ci, .ri)) %>%
  ctx$addNamespace()

ctx$save(list(df_out, join_png))
