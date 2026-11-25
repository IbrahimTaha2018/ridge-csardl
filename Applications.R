# =============================================================================
#  RIDGE-REGULARIZED CS-ARDL  —  
# =============================================================================
# =============================================================================
#  1. CONFIGURATION
# =============================================================================
SEL_DIR <- Sys.getenv(**********)
SEL_DIR <- gsub("\\\\", "/", SEL_DIR)
OUT_DIR <- file.path(SEL_DIR, "Summary")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)


DATASETS <- list(
  list(label     = "Cigarette demand",
       file      = "P03_Cigarette_US_states",
       vars      = c(ln_price = "Own price", ln_ndi = "Income", ln_pimin = "Neighboring price"),
       refs      = c(
         "Baltagi, B. H., & Levin, D. (1992). Cigarette taxation: Raising revenues and reducing consumption. Structural Change and Economic Dynamics, 3(2), 321-335.",
         "Baltagi, B. H., Griffin, J. M., & Xiong, W. (2000). To pool or not to pool: Homogeneous versus heterogeneous estimators applied to cigarette demand. Review of Economics and Statistics, 82(1), 117-126. https://doi.org/10.1162/003465300558551"),
       short_ref = "Baltagi and Levin (1992); Baltagi et al. (2000)"),
  list(label     = "US house prices",
       file      = "P04_House_prices_US_states",
       vars      = c(ln_income = "Income", ln_pop = "Population", intrate = "Real interest rate"),
       refs      = c(
         "Holly, S., Pesaran, M. H., & Yamagata, T. (2010). A spatio-temporal model of house prices in the USA. Journal of Econometrics, 158(1), 160-173. https://doi.org/10.1016/j.jeconom.2010.03.040",
         "Millo, G. (2015). Narrow replication of 'A spatio-temporal model of house prices in the USA' using R. Journal of Applied Econometrics, 30(4), 703-704. https://doi.org/10.1002/jae.2424"),
       short_ref = "Holly et al. (2010)"),
  list(label     = "US manufacturing R&D",
       file      = "P06_RD_spillovers_USA",
       vars      = c(lnl = "Labor", lnk = "Physical capital", lnrd = "R&D capital"),
       refs      = c(
         "Eberhardt, M., Helmers, C., & Strauss, H. (2013). Do spillovers matter when estimating private returns to R&D? Review of Economics and Statistics, 95(2), 436-448. https://doi.org/10.1162/REST_a_00272"),
       short_ref = "Eberhardt et al. (2013)"),
  list(label     = "EU-15 production",
       file      = "P12_PWT_EU15",
       vars      = c(ln_rnna = "Capital", ln_emp = "Employment", ln_hc = "Human capital"),
       refs      = c(
         "Feenstra, R. C., Inklaar, R., & Timmer, M. P. (2015). The next generation of the Penn World Table. American Economic Review, 105(10), 3150-3182. https://doi.org/10.1257/aer.20130954",
         "Groningen Growth and Development Centre (2023). Penn World Table version 10.01. DataverseNL. https://doi.org/10.34894/QT5BCC"),
       short_ref = "Feenstra et al. (2015)")
)

DPI        <- 1200
ALPHA_TEST <- 0.05          # level of the Wilcoxon star in Figure 3
SAVE_PDF   <- FALSE         # also write vector PDF copies of the figures

for (d in DATASETS) {
  f <- file.path(SEL_DIR, paste0(d$file, c(".xlsx", "_RidgeCSARDL_Results.xlsx")))
  if (any(!file.exists(f))) stop("File(s) not found:\n", paste(f[!file.exists(f)], collapse = "\n"))
}

# --- estimators and classes -----------------------------------------------------
EST_LEVELS <- c("CS_OLS", "R_HK", "R_HKB", "R_KL", "R_CN", "R_DF", "D_CN", "D_DF")
EST_SINGLE <- c("R_HK", "R_HKB", "R_KL", "R_CN", "R_DF")
EST_DUAL   <- c("D_CN", "D_DF")
CLASS_SETS <- list(Single = c("CS_OLS", EST_SINGLE), Dual = c("CS_OLS", EST_DUAL))
EST_COLORS <- c(CS_OLS = "#1B1B1B", R_HK = "#8D99AE", R_HKB = "#B15928", R_KL = "#2A9D8F",
                R_CN = "#E63946", R_DF = "#6A3D9A", D_CN = "#F4A261", D_DF = "#1D6996")
NON_OLS    <- setdiff(EST_LEVELS, "CS_OLS")
DS_COLORS  <- c("#1D3557", "#E63946", "#2A9D8F", "#7B2CBF")
INK        <- "#1B2A4A"
DS_LEVELS  <- vapply(DATASETS, `[[`, character(1), "label")
names(DS_COLORS) <- DS_LEVELS

# =============================================================================
#  2. THEME AND HELPERS
# =============================================================================
theme_app <- function(base_size = 15, angle_x = 0) {
  theme_bw(base_size = base_size) +
    theme(
      axis.text        = element_text(color = "black", size = base_size - 2),
      axis.text.x      = element_text(angle = angle_x, hjust = if (angle_x) 1 else 0.5,
                                      vjust = if (angle_x) 1 else 0.5),
      axis.text.y      = element_text(face = "bold"),
      axis.title       = element_text(color = "black", face = "bold", size = base_size + 1),
      axis.title.x     = element_text(margin = ggplot2::margin(t = 8)),
      axis.title.y     = element_text(margin = ggplot2::margin(r = 8)),
      strip.text       = element_text(face = "bold", size = base_size - 1, color = "white",
                                      margin = ggplot2::margin(5, 4, 5, 4)),
      strip.background = element_rect(fill = INK, color = NA),
      legend.position  = "bottom",
      legend.title     = element_blank(),
      legend.text      = element_text(size = base_size - 1,
                                      margin = ggplot2::margin(l = 3, r = 14)),
      legend.key.width = unit(1.2, "cm"),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.35),
      panel.grid.minor = element_blank(),
      panel.border     = element_rect(color = "gray55", linewidth = 0.5, fill = NA),
      panel.spacing    = unit(12, "pt"),
      plot.subtitle    = element_text(face = "bold", size = base_size, hjust = 0,
                                      margin = ggplot2::margin(b = 6)),
      plot.margin      = ggplot2::margin(10, 14, 10, 10),
      plot.background  = element_rect(fill = "white", color = NA)
    )
}

save_fig <- function(obj, name, width, height) {
  dev <- if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else "png"
  ggsave(file.path(OUT_DIR, paste0(name, ".png")), obj, width = width, height = height,
         units = "in", dpi = DPI, bg = "white", device = dev, limitsize = FALSE)
  if (SAVE_PDF) ggsave(file.path(OUT_DIR, paste0(name, ".pdf")), obj, width = width, height = height,
                       units = "in", device = cairo_pdf, bg = "white")
  message("Saved: ", name)
}

white_bg <- function(g) {
  cowplot::ggdraw(g) + theme(plot.background = element_rect(fill = "white", colour = NA))
}

## value label on a white box without border (the border argument was renamed in ggplot2 4.0)
white_label <- function(...) {
  if (utils::packageVersion("ggplot2") >= "4.0.0") {
    geom_label(..., fill = "white", linewidth = 0, label.padding = grid::unit(0.12, "lines"))
  } else {
    geom_label(..., fill = "white", label.size = 0, label.padding = grid::unit(0.12, "lines"))
  }
}

as_num  <- function(x) suppressWarnings(as.numeric(as.character(x)))
kv      <- function(df, key) {                      # value of a Field/Value or Item/Value sheet
  hit <- which(trimws(as.character(df[[1]])) == key)
  if (length(hit)) df[[2]][hit[1]] else NA
}
fct_ds  <- function(x) factor(x, levels = DS_LEVELS)
fct_est <- function(x) factor(x, levels = EST_LEVELS)
fmt_num <- function(x, d = 3) ifelse(is.finite(x), formatC(x, format = "f", digits = d), "")
stars   <- function(p) ifelse(!is.finite(p), "", ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.10, "*", ""))))

# =============================================================================
#  3. READ THE DATA AND RESULT WORKBOOKS
# =============================================================================
info_l <- mg_l <- pr_l <- st_l <- cn_l <- cd_l <- list()

for (d in DATASETS) {
  dfile <- file.path(SEL_DIR, paste0(d$file, ".xlsx"))
  rfile <- file.path(SEL_DIR, paste0(d$file, "_RidgeCSARDL_Results.xlsx"))
  rd <- read_xlsx(dfile, sheet = "README")
  ov <- read_xlsx(rfile, sheet = "Overview")
  mc <- read_xlsx(rfile, sheet = "Multicollinearity")
  ucn <- read_xlsx(rfile, sheet = "Unit_Collinearity")
  cds <- read_xlsx(rfile, sheet = "CD_Test_Series")

  N  <- as_num(kv(mc, "Units N")); Tn <- as_num(kv(mc, "Periods T"))
  yname <- as.character(kv(ov, "Response"))
  lab_of <- function(v) ifelse(v %in% names(d$vars), d$vars[v], ifelse(v == yname, "Response", v))
  pkg <- as.character(kv(rd, "Source_package")); ver <- as.character(kv(rd, "Source_package_version"))
  obj <- as.character(kv(rd, "Source_object"))

  info_l[[d$label]] <- tibble(
    Dataset    = d$label,
    Domain     = as.character(kv(rd, "Domain")),
    Unit       = as.character(kv(rd, "Unit of the panel")),
    Response   = sprintf("%s (%s)", as.character(kv(rd, "Response_description")), yname),
    Regressors = paste(sprintf("%s (%s)", d$vars, names(d$vars)), collapse = "; "),
    N = N, T = Tn, Span = as.character(kv(rd, "Time span")),
    p_T        = as_num(kv(mc, "CSA lags p_T")),
    Unit_df    = as_num(kv(mc, "Residual df per unit equation")),
    CD_y       = as_num(cds$CD_levels[1]),
    CN_full    = as_num(kv(mc, "Unit level: median condition number, full design")),
    CN_x_cond  = as_num(kv(mc, "Unit level: median condition number, regressor block given CSAs")),
    Max_VIF    = as_num(kv(mc, "Pooled: largest VIF")),
    Transformation = as.character(kv(rd, "Transformation")),
    Original_source = d$short_ref,
    R_source   = sprintf("R package %s (version %s), dataset %s", pkg, ver, obj),
    pkg = pkg, ver = ver, obj = obj)

  cn_l[[d$label]] <- bind_rows(
    tibble(Dataset = d$label, Block = "Whole unit design", CN = as_num(ucn$CN_full)),
    tibble(Dataset = d$label, Block = "Regressors given the CSAs", CN = as_num(ucn$CN_x_cond)))

  cd_l[[d$label]] <- tibble(Dataset = d$label, Variable = lab_of(as.character(cds$Variable)),
                            Role = ifelse(as.character(cds$Variable) == yname, "Response", "Regressor"),
                            CD = as_num(cds$CD_levels), p = as_num(cds$p_levels),
                            Order = seq_len(nrow(cds)))

  mg <- read_xlsx(rfile, sheet = "MG_Estimates")
  mg_l[[d$label]] <- mg |>
    filter(Type != "Short-run") |>
    transmute(Dataset = d$label, Estimator = as.character(Estimator), Type = as.character(Type),
              Variable = as.character(Variable),
              Parameter = ifelse(Type == "Long-run", paste0("Long-run: ", lab_of(Variable)), "Error correction"),
              Order = ifelse(Type == "Long-run", match(Variable, names(d$vars)), 99),
              Estimate = as_num(Estimate), SE = as_num(SE), p = as_num(p),
              CI_low = as_num(CI_low), CI_high = as_num(CI_high), N_used = as_num(N_used))

  pr <- read_xlsx(rfile, sheet = "Prediction")
  if (!"Ties_with_CS_OLS_percent" %in% names(pr)) pr$Ties_with_CS_OLS_percent <- NA
  pr_l[[d$label]] <- pr |>
    transmute(Dataset = d$label, Estimator = as.character(Estimator),
              MSFE = as_num(MSFE), MAFE = as_num(MAFE), RE = as_num(RE_vs_CS_OLS),
              Beats = as_num(Beats_CS_OLS_percent), Ties = as_num(Ties_with_CS_OLS_percent),
              Wilcoxon_p = as_num(Wilcoxon_p_vs_CS_OLS),
              Origins = as_num(sub(".*expanding window, ([0-9]+) .*", "\\1", kv(ov, "Out-of-sample design"))))

  st <- read_xlsx(rfile, sheet = "Stability")
  st_l[[d$label]] <- st |>
    transmute(Dataset = d$label, Estimator = as.character(Estimator),
              CU_var = as_num(Total_cross_unit_variance_LR),
              Win_var = as_num(Total_variance_MG_LR_over_windows),
              Sign_dis = as_num(Sign_disagreements_LR),
              ECT_bad = as_num(Units_ECT_outside_minus2_0)) |>
    mutate(CU_rel = CU_var / CU_var[Estimator == "CS_OLS"],
           Win_rel = Win_var / Win_var[Estimator == "CS_OLS"])
}

info <- bind_rows(info_l) |> mutate(Dataset = fct_ds(Dataset))
mgs  <- bind_rows(mg_l)   |> mutate(Dataset = fct_ds(Dataset), Estimator = fct_est(Estimator))
pred <- bind_rows(pr_l)   |> mutate(Dataset = fct_ds(Dataset), Estimator = fct_est(Estimator))
stab <- bind_rows(st_l)   |> mutate(Dataset = fct_ds(Dataset), Estimator = fct_est(Estimator))
cns  <- bind_rows(cn_l)   |> mutate(Dataset = fct_ds(Dataset),
                                    Block = factor(Block, levels = c("Whole unit design", "Regressors given the CSAs")))
cdt  <- bind_rows(cd_l)   |> mutate(Dataset = fct_ds(Dataset))

strip_lab   <- sprintf("%s\n(N = %d, T = %d)", info$Dataset, info$N, info$T)
DS_LABELLER <- as_labeller(setNames(strip_lab, as.character(info$Dataset)))

# =============================================================================
#  4. TABLES
# =============================================================================
tab1 <- info |>
  arrange(Dataset) |>
  transmute(Dataset, Domain, `Cross-section unit` = Unit, Response, Regressors,
            N, T, `Time span` = Span, `CSA lags` = p_T, `Residual df per unit` = Unit_df,
            `Pesaran CD of y` = round(CD_y, 2),
            `Median CN, whole unit design` = round(CN_full, 1),
            `Median CN, regressors given CSAs` = round(CN_x_cond, 2),
            `Pooled max VIF` = round(Max_VIF, 2),
            `Original source` = Original_source, `Data obtained from` = R_source)

## estimates as "estimate (standard error) stars"; rows = dataset x parameter, columns = estimators
tab2 <- mgs |>
  mutate(Cell = sprintf("%s (%s)%s", fmt_num(Estimate), fmt_num(SE), stars(p))) |>
  arrange(Dataset, Order, Estimator) |>
  select(Dataset, Parameter, Order, Estimator, Cell) |>
  pivot_wider(names_from = Estimator, values_from = Cell) |>
  arrange(Dataset, Order) |>
  select(-Order)

tab3 <- pred |>
  left_join(stab, by = c("Dataset", "Estimator")) |>
  arrange(Dataset, Estimator) |>
  transmute(Dataset, Estimator,
            Class = ifelse(Estimator == "CS_OLS", "Benchmark", ifelse(Estimator %in% EST_DUAL, "Dual-penalty", "Single-penalty")),
            MSFE = signif(MSFE, 4), MAFE = signif(MAFE, 4),
            `RE vs CS_OLS` = round(RE, 3),
            `Predictions beating CS_OLS (percent)` = round(Beats, 1),
            `Ties with CS_OLS (percent)` = round(Ties, 1),
            `Wilcoxon p vs CS_OLS` = signif(Wilcoxon_p, 3),
            `Cross-unit variance of LR (relative to CS_OLS)` = round(CU_rel, 3),
            `Variance of MG LR across windows (relative to CS_OLS)` = round(Win_rel, 3),
            `LR sign disagreements` = Sign_dis,
            `Units without valid error correction` = ECT_bad)

## best value within each dataset and class: direction per column
BEST3 <- c(MSFE = "min", MAFE = "min", `RE vs CS_OLS` = "max",
           `Predictions beating CS_OLS (percent)` = "max",
           `Cross-unit variance of LR (relative to CS_OLS)` = "min",
           `Variance of MG LR across windows (relative to CS_OLS)` = "min",
           `LR sign disagreements` = "min", `Units without valid error correction` = "min")

sources <- bind_rows(lapply(DATASETS, function(d) {
  i <- info[info$Dataset == d$label, ]
  tibble(Dataset = d$label,
         Type = c(rep("Original source", length(d$refs)), "R distribution"),
         Reference = c(d$refs, sprintf(
           "%s, dataset %s (R package version %s). https://CRAN.R-project.org/package=%s",
           i$pkg, i$obj, i$ver, i$pkg)))
}))
sources$Dataset <- fct_ds(sources$Dataset)

notes <- tibble(Note = c(
  "All results: RidgeCSARDL_Application_v1_2.R; CS-ARDL(1,1) in error-correction form estimated unit by unit and averaged (Mean Group).",
  "CSA lags = lag order of the cross-sectional averages, floor(T^(1/3)) lowered only if a unit equation would keep fewer than 3 residual degrees of freedom.",
  "CN = condition number sqrt(lambda_max / lambda_min) of the unit-scaled columns after partialling out the intercept and y(t-1); median over the units.",
  "Table 2: Mean Group estimate (standard error); *** p < 0.01, ** p < 0.05, * p < 0.10, t test with N - 1 degrees of freedom.",
  "RE vs CS_OLS = MSFE of CS_OLS divided by that of the estimator (values above 1 favour the estimator); expanding-window one-step-ahead predictions of dy.",
  "In the prediction step the contemporaneous average of y leaves out the predicted unit, so its own outcome is never used.",
  "Predictions beating CS_OLS = percent of unit-period predictions with a smaller absolute error than CS_OLS; ties (agreement to 1e-6 of the CS_OLS RMSFE) are counted separately.",
  "Wilcoxon p = paired signed-rank test of the squared-error differences against CS_OLS (ties excluded).",
  "Cross-unit variance of LR = sum over regressors of the variance of the unit long-run estimates across units, relative to CS_OLS.",
  "Variance of MG LR across windows = sum over regressors of the variance of the Mean Group long-run estimates across the expanding windows, relative to CS_OLS.",
  "Units without valid error correction = units whose ECT estimate lies outside (-2, 0).",
  "Green cells mark the best value within each dataset and class: {CS_OLS + single-penalty} and {CS_OLS + dual-penalty}."))

## ---- write the workbook ------------------------------------------------------
NAVY_FILL <- "1B2A4A"; GREEN_FILL <- "C6EFCE"; GREEN_FONT <- "006100"

add_tab <- function(wb, sheet, df, best = NULL, group = "Dataset", widths = NULL) {
  df_out <- as.data.frame(lapply(df, function(z) if (is.factor(z)) as.character(z) else z),
                          check.names = FALSE)
  wb <- wb$add_worksheet(sheet)
  wb <- wb$add_data(sheet = sheet, x = df_out, na.strings = "")
  nc <- ncol(df_out); nr <- nrow(df_out)
  wb <- wb$add_fill(sheet = sheet, dims = wb_dims(rows = 1, cols = seq_len(nc)),
                    color = wb_color(NAVY_FILL))
  wb <- wb$add_font(sheet = sheet, dims = wb_dims(rows = 1, cols = seq_len(nc)),
                    color = wb_color("FFFFFF"), bold = TRUE)
  wb <- wb$add_cell_style(sheet = sheet, dims = wb_dims(rows = 1, cols = seq_len(nc)),
                          wrap_text = TRUE, vertical = "center", horizontal = "center")
  if (!is.null(best)) {
    for (g in unique(df_out[[group]])) for (cs in CLASS_SETS) {
      rows <- which(df_out[[group]] == g & df_out$Estimator %in% cs)
      for (cn in names(best)) {
        v <- df_out[[cn]][rows]
        if (all(!is.finite(v))) next
        target <- if (best[[cn]] == "min") min(v, na.rm = TRUE) else max(v, na.rm = TRUE)
        hit <- rows[which(abs(v - target) <= 1e-12 * max(1, abs(target)))]
        col <- match(cn, names(df_out))
        for (h in hit) {
          wb <- wb$add_fill(sheet = sheet, dims = wb_dims(rows = h + 1, cols = col),
                            color = wb_color(GREEN_FILL))
          wb <- wb$add_font(sheet = sheet, dims = wb_dims(rows = h + 1, cols = col),
                            color = wb_color(GREEN_FONT), bold = TRUE)
        }
      }
    }
  }
  wb <- wb$set_col_widths(sheet = sheet, cols = seq_len(nc),
                          widths = if (is.null(widths)) "auto" else widths)
  wb <- wb$freeze_pane(sheet = sheet, first_row = TRUE)
  wb
}

wb <- wb_workbook()
wb <- add_tab(wb, "Table1_Datasets", tab1,
              widths = c(22, 24, 30, 40, 60, 5, 5, 13, 8, 10, 10, 12, 12, 10, 34, 50))
wb <- add_tab(wb, "Table2_Estimates", tab2, widths = c(22, 32, rep(19, length(EST_LEVELS))))
wb <- add_tab(wb, "Table3_Performance", tab3, best = BEST3,
              widths = c(22, 10, 15, 11, 11, 10, 14, 12, 12, 16, 18, 12, 14))
wb <- add_tab(wb, "Sources", sources, widths = c(22, 16, 160))
wb <- add_tab(wb, "Notes", notes, widths = 170)
xlsx_file <- file.path(OUT_DIR, "RidgeCSARDL_Applications_Summary_Tables.xlsx")
wb$save(xlsx_file, overwrite = TRUE)
message("Saved: ", basename(xlsx_file))

# =============================================================================
#  FIGURE 1 — CROSS-SECTIONAL DEPENDENCE AND COLLINEARITY OF THE UNIT DESIGNS
# =============================================================================
cdt <- cdt |> group_by(Dataset) |>
  mutate(Variable = factor(paste(Dataset, Variable, sep = "::"),
                           levels = paste(Dataset, Variable, sep = "::")[order(Order)])) |>
  ungroup()
f1a <- ggplot(cdt, aes(Variable, CD, fill = Role)) +
  geom_hline(yintercept = 1.96, colour = "grey25", linewidth = 0.6, linetype = "22") +
  geom_col(width = 0.66, colour = "grey25", linewidth = 0.2) +
  geom_text(aes(label = sprintf("%.1f", CD), vjust = ifelse(CD >= 0, -0.35, 1.25)), size = 4.3) +
  facet_wrap(~ Dataset, nrow = 1, scales = "free", labeller = DS_LABELLER) +
  scale_x_discrete(labels = function(x) sub(".*::", "", x)) +
  scale_fill_manual(values = c(Response = "#1D3557", Regressor = "#7FA7C9")) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.14))) +
  labs(subtitle = "A.  Pesaran CD statistic of every series in levels (dashed: 1.96)",
       x = NULL, y = "CD statistic") +
  theme_app(angle_x = 30)

cn_lab <- cns |> group_by(Dataset, Block) |>
  summarise(med = median(CN), top = max(CN), .groups = "drop")
f1b <- ggplot(cns, aes(Block, CN, fill = Block)) +
  geom_hline(yintercept = 30, colour = "grey25", linewidth = 0.6, linetype = "22") +
  geom_boxplot(width = 0.55, outlier.size = 1.1, outlier.alpha = 0.6, colour = "grey20",
               linewidth = 0.45) +
  geom_text(data = cn_lab, aes(Block, top, label = sprintf("%.1f", med)), inherit.aes = FALSE,
            vjust = -0.6, size = 4.3) +
  facet_wrap(~ Dataset, nrow = 1, labeller = DS_LABELLER) +
  scale_fill_manual(values = c(`Whole unit design` = "#E63946", `Regressors given the CSAs` = "#2A9D8F")) +
  scale_x_discrete(labels = c(`Whole unit design` = "Whole design", `Regressors given the CSAs` = "Regressors | CSAs")) +
  scale_y_log10(expand = expansion(mult = c(0.05, 0.16))) +
  labs(subtitle = "B.  Unit-level condition numbers, log scale (dashed: 30; label: median over the units)",
       x = NULL, y = "Condition number") +
  theme_app()

fig1 <- gridExtra::arrangeGrob(f1a, f1b, ncol = 1, heights = c(1, 1))
save_fig(white_bg(fig1), "Fig_App1_Dependence_Collinearity", 16, 12)

# =============================================================================
#  FIGURE 2 — MEAN GROUP LONG-RUN AND ERROR-CORRECTION ESTIMATES
# =============================================================================
fp <- mgs |>
  arrange(Dataset, Order) |>
  mutate(Panel = paste0(Dataset, "\n", Parameter),
         Panel = factor(Panel, levels = unique(Panel)),
         Est_rev = factor(Estimator, levels = rev(EST_LEVELS)),
         lab = fmt_num(Estimate),
         lab_x = ifelse(is.finite(CI_high), CI_high, Estimate))
n_par <- max(table(as.character(fp$Dataset)) / length(EST_LEVELS))

f2 <- ggplot(fp) +
  geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.5, linetype = "22") +
  geom_segment(aes(x = CI_low, xend = CI_high, y = Est_rev, yend = Est_rev, colour = Estimator),
               linewidth = 1.2, na.rm = TRUE) +
  geom_point(aes(Estimate, Est_rev, fill = Estimator), shape = 21, size = 3.4, colour = "black", stroke = 0.6) +
  white_label(aes(x = lab_x, y = Est_rev, label = lab), hjust = -0.1, size = 3.7, fontface = "bold") +
  facet_wrap(~ Panel, ncol = n_par, scales = "free_x") +
  scale_colour_manual(values = EST_COLORS) + scale_fill_manual(values = EST_COLORS) +
  scale_x_continuous(expand = expansion(mult = c(0.06, 0.45)), breaks = scales::breaks_pretty(n = 4)) +
  labs(x = "Mean Group estimate with 95 percent confidence interval (label = estimate)", y = NULL) +
  theme_app(base_size = 14) + theme(legend.position = "none", strip.text = element_text(size = 12.5, lineheight = 0.95))
save_fig(white_bg(f2), "Fig_App2_MG_Estimates", 17, 17)

# =============================================================================
#  FIGURE 3 — OUT-OF-SAMPLE PREDICTION AND STABILITY RELATIVE TO CS_OLS
# =============================================================================
gain <- pred |> filter(Estimator %in% NON_OLS) |>
  mutate(Estimator = factor(Estimator, levels = NON_OLS),
         Star = ifelse(is.finite(Wilcoxon_p) & Wilcoxon_p < ALPHA_TEST & RE > 1, "*", ""),
         Label = paste0(sprintf("%.2f", RE), Star))
class_sep <- geom_vline(xintercept = length(EST_SINGLE) + 0.5, colour = "grey55", linewidth = 0.5, linetype = "dotted")

f3a <- ggplot(gain, aes(Estimator, RE, fill = Estimator)) +
  geom_hline(yintercept = 1, colour = "grey25", linewidth = 0.6, linetype = "22") +
  geom_col(width = 0.68, colour = "grey25", linewidth = 0.2) +
  class_sep +
  geom_text(aes(label = Label), vjust = -0.35, size = 4.1) +
  facet_wrap(~ Dataset, nrow = 1, scales = "free_y", labeller = DS_LABELLER) +
  scale_fill_manual(values = EST_COLORS[NON_OLS]) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.14))) +
  labs(subtitle = "A.  Relative efficiency, MSFE(CS_OLS) / MSFE(estimator)  (dashed: 1; * Wilcoxon p < 0.05 in favour)",
       x = NULL, y = "Relative efficiency") +
  theme_app(angle_x = 30) + theme(legend.position = "none")

rel <- stab |> filter(Estimator %in% NON_OLS) |>
  mutate(Estimator = factor(Estimator, levels = NON_OLS)) |>
  select(Dataset, Estimator, `Across units` = CU_rel, `Across windows` = Win_rel) |>
  pivot_longer(c(`Across units`, `Across windows`), names_to = "Kind", values_to = "Ratio") |>
  filter(is.finite(Ratio), Ratio > 0)

## variance relative to CS_OLS on a log scale: stems start at 1, so gains (below 1) and
## losses (above 1) are drawn symmetrically and no single value squashes the panel
lolli <- function(kind, subtitle) {
  dd <- rel |> filter(Kind == kind) |>
    mutate(lab = ifelse(Ratio >= 10, sprintf("%.1f", Ratio), ifelse(Ratio >= 0.01, sprintf("%.2f", Ratio), sprintf("%.3f", Ratio))),
           vj = ifelse(Ratio >= 1, -0.9, 1.9))
  ggplot(dd, aes(Estimator, Ratio, colour = Estimator)) +
    geom_hline(yintercept = 1, colour = "grey25", linewidth = 0.6, linetype = "22") +
    geom_segment(aes(xend = Estimator, y = 1, yend = Ratio), linewidth = 1.6) +
    geom_point(aes(fill = Estimator), shape = 21, colour = "grey15", size = 4.2, stroke = 0.5) +
    class_sep +
    geom_text(aes(label = lab, vjust = vj), colour = "black", size = 4.1) +
    facet_wrap(~ Dataset, nrow = 1, scales = "free_y", labeller = DS_LABELLER) +
    scale_colour_manual(values = EST_COLORS[NON_OLS], guide = "none") +
    scale_fill_manual(values = EST_COLORS[NON_OLS]) +
    scale_y_log10(labels = function(x) format(x, drop0trailing = TRUE, scientific = FALSE),
                  expand = expansion(mult = c(0.16, 0.16))) +
    labs(subtitle = subtitle, x = NULL, y = "Variance relative to CS_OLS") +
    theme_app(angle_x = 30)
}
f3b <- lolli("Across units", "B.  Cross-unit variance of the unit long-run estimates relative to CS_OLS, log scale (below 1 = more stable)") +
  theme(legend.position = "none")
f3c <- lolli("Across windows", "C.  Variance of the Mean Group long-run estimates across the windows relative to CS_OLS, log scale") +
  guides(fill = guide_legend(nrow = 1))

leg3 <- cowplot::get_legend(f3c)
fig3 <- gridExtra::arrangeGrob(f3a, f3b, f3c + theme(legend.position = "none"), leg3,
                               ncol = 1, heights = c(1, 1, 1, 0.12))
save_fig(white_bg(fig3), "Fig_App3_Prediction_Stability", 16, 16)

# =============================================================================
#  CONSOLE SUMMARY
# =============================================================================
old_w <- options(width = 200)
cat("\n================ RIDGE CS-ARDL APPLICATIONS: SUMMARY ================\n")
cat("\nTable 1. Panels, dependence and collinearity\n")
print(as.data.frame(tab1[, c("Dataset", "N", "T", "CSA lags", "Residual df per unit", "Pesaran CD of y",
                             "Median CN, whole unit design", "Median CN, regressors given CSAs", "Pooled max VIF")]),
      row.names = FALSE)
cat("\nTable 2. Mean Group long-run and error-correction estimates\n")
print(as.data.frame(tab2), row.names = FALSE)
cat("\nTable 3. Prediction and stability (main criteria)\n")
print(as.data.frame(tab3[, c("Dataset", "Estimator", "RE vs CS_OLS", "Predictions beating CS_OLS (percent)",
                             "Wilcoxon p vs CS_OLS", "Cross-unit variance of LR (relative to CS_OLS)",
                             "Variance of MG LR across windows (relative to CS_OLS)")]), row.names = FALSE)
for (i in seq_len(nrow(sources))) cat(sprintf("  [%s] %s\n", sources$Dataset[i], sources$Reference[i]))
cat("\nOutput written to: ", OUT_DIR, "\n", sep = "")
options(old_w)
