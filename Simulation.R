################################################################################
#   RIDGE-REGULARIZED CS-ARDL                                                 #
#   Author: Ibrahim M. Taha                                                    #
################################################################################

# ------------------------------------------------------------------------------
# 0.1 Run controls
# ------------------------------------------------------------------------------
N_REPS       <- 2000        
                            
MASTER_SEED  <- 20260705    
GRAPH_DPI    <- 1200        
EXCEL_FILE   <- "Ridge_CSARDL_Simulation_Results.xlsx"
EXCEL_ROB    <- "Ridge_CSARDL_Robustness_Results.xlsx"
RUN_ROBUSTNESS <- TRUE      
USE_PARALLEL <- TRUE        
                            
                            
N_CORES      <- max(1, parallel::detectCores() - 1)
WORKDIR_OVERRIDE <- NULL    

# ------------------------------------------------------------------------------
# 0.2 CHANGING FACTORS  
# ------------------------------------------------------------------------------
N_grid     <- c(5, 10, 15)         
T_grid     <- c(20, 30, 40, 70)     
                                    
rho_grid   <- c(0.80, 0.90, 0.99, 0.999)  
                                   
                                    
slope_configs <- list(            
  Equal   = list(beta0 = c(1.00, 1.00), beta1 = c(0.50, 0.50)),
  Unequal = list(beta0 = c(1.00, 0.40), beta1 = c(0.50, 0.20))
)                                   
                                
                                   

# ------------------------------------------------------------------------------
# 0.3 ROBUSTNESS DESIGN  
# ------------------------------------------------------------------------------
rob_baseline <- list(N = 10, T_len = 30, Slope = "Equal")
rob_rho_grid <- rho_grid
rob_settings <- data.frame(
  Setting   = c("Baseline", "High noise", "Slow adjustment",
                "Persistent factors", "Three factors"),
  sig2_mult = c(1, 4, 1, 1, 1),                  # multiplier on sigma_ei^2
  phi_lo    = c(0.20, 0.20, 0.60, 0.20, 0.20),   # phi_i ~ U(phi_lo, phi_hi);
  phi_hi    = c(0.60, 0.60, 0.80, 0.60, 0.60),   # 0.80 keeps true long-run
                                                 # effects inside the cap
  rho_f     = c(0.5, 0.5, 0.5, 0.9, 0.5),        # factor AR(1) persistence
  m_factors = c(2L, 2L, 2L, 2L, 3L),             # 3 = k_x + 1, the boundary
  stringsAsFactors = FALSE                       # of the CCE rank condition
)

# ------------------------------------------------------------------------------
# 0.4 FIXED FACTORS
# ------------------------------------------------------------------------------
fixed_pars <- list(
  k_x        = 2L,              
  m_factors  = 2L,             
                                 
  rho_f      = 0.5,             
  rho_v      = 0.5,             
  phi_lo     = 0.20,             
  phi_hi     = 0.60,
  beta0_sd   = 0.10,             
  beta1_sd   = 0.10,           
  load_lo    = 0.5,            
  load_hi    = 1.5,           
  sig2_lo    = 0.5,             
  sig2_hi    = 1.5,
  sig2_mult  = 1,               
  burn       = 50L,              
  ect_trim   = 0.02,            
  theta_cap  = 25               
)

# ------------------------------------------------------------------------------
# 0.5 Estimators and their comparison classes
# ------------------------------------------------------------------------------
est_benchmark <- "CS_OLS"
est_single    <- c("R_HK", "R_HKB", "R_KL", "R_CN", "R_DF")
est_dual      <- c("D_CN", "D_DF")
est_novel     <- c("R_CN", "R_DF", "D_CN", "D_DF")


################################################################################
#   1.  Setup
################################################################################

required_packages <- c("dplyr", "tidyr", "tibble", "openxlsx",
                       "ggplot2", "gridExtra", "scales", "grid")
new_packages <- required_packages[!(required_packages %in%
                                    installed.packages()[, "Package"])]
if (length(new_packages)) install.packages(new_packages, dependencies = TRUE)
invisible(lapply(required_packages, library, character.only = TRUE))

lag  <- dplyr::lag      # guard against masking of dplyr verbs
lead <- dplyr::lead

get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)                 # (1) Rscript
  fa <- grep("^--file=", args, value = TRUE)
  if (length(fa)) return(dirname(normalizePath(sub("^--file=", "", fa[1]))))
  if (requireNamespace("rstudioapi", quietly = TRUE) &&     # (2) RStudio
      rstudioapi::isAvailable()) {
    p <- tryCatch(rstudioapi::getActiveDocumentContext()$path,
                  error = function(e) "")
    if (nzchar(p)) return(dirname(normalizePath(p)))
  }
  for (fr in rev(sys.frames())) {                           # (3) source()
    if (!is.null(fr$ofile)) return(dirname(normalizePath(fr$ofile)))
  }
  getwd()
}
WORKDIR <- if (is.null(WORKDIR_OVERRIDE)) get_script_dir() else WORKDIR_OVERRIDE
if (!dir.exists(WORKDIR)) dir.create(WORKDIR, recursive = TRUE)
setwd(WORKDIR)
dir.create("graphs", showWarnings = FALSE)

t_start <- Sys.time()
cat("Run started:", format(t_start), "\n")


################################################################################
#   2.  Scenario grids and factor documentation
################################################################################

for (sl in slope_configs) {
  stopifnot(length(sl$beta0) == fixed_pars$k_x, length(sl$beta1) == fixed_pars$k_x)
}
stopifnot(rob_baseline$Slope %in% names(slope_configs))

est_names <- c(est_benchmark, est_single, est_dual)
n_est     <- length(est_names)
est_class <- setNames(c("Benchmark", rep("Single-penalty", length(est_single)),
                        rep("Dual-penalty", length(est_dual))), est_names)

# ---- MAIN design ---------------------------------------------------------------
changing_factors <- expand.grid(N = N_grid, T_len = T_grid, rho_x = rho_grid,
                                Slope = names(slope_configs),
                                KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE) %>%
  arrange(match(Slope, names(slope_configs)), N, T_len, rho_x) %>%
  mutate(Scenario_ID = sprintf("S%02d", row_number()),
         p_T = floor(T_len^(1/3)),
         Design = "Main", Setting = "Baseline",
         sig2_mult = fixed_pars$sig2_mult,
         phi_lo = fixed_pars$phi_lo, phi_hi = fixed_pars$phi_hi,
         rho_f = fixed_pars$rho_f, m_factors = fixed_pars$m_factors) %>%
  select(Scenario_ID, N, T_len, rho_x, Slope, p_T, Design, Setting,
         sig2_mult, phi_lo, phi_hi, rho_f, m_factors)

# ---- ROBUSTNESS design ----------------------------------------------------------
robust_factors <- expand.grid(Setting = rob_settings$Setting, rho_x = rob_rho_grid,
                              KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE) %>%
  left_join(rob_settings, by = "Setting") %>%
  arrange(match(Setting, rob_settings$Setting), rho_x) %>%
  mutate(Scenario_ID = sprintf("R%02d", row_number()),
         N = rob_baseline$N, T_len = rob_baseline$T_len,
         Slope = rob_baseline$Slope,
         p_T = floor(T_len^(1/3)), Design = "Robustness") %>%
  select(Scenario_ID, N, T_len, rho_x, Slope, p_T, Design, Setting,
         sig2_mult, phi_lo, phi_hi, rho_f, m_factors)

all_scen <- if (RUN_ROBUSTNESS) bind_rows(changing_factors, robust_factors) else
  changing_factors

# ---- documentation tables ------------------------------------------------------
vec <- function(v) paste0("(", paste(sprintf("%.2f", v), collapse = ", "), ")")
changing_levels <- data.frame(
  Factor = c("N (cross-section units)", "T (time-series length)",
             "rho_x (x idiosyncratic correlation)",
             paste0("Slope configuration: ", names(slope_configs))),
  Levels = c(paste(N_grid, collapse = ", "), paste(T_grid, collapse = ", "),
             paste(rho_grid, collapse = ", "),
             vapply(slope_configs, function(s)
               sprintf("beta0 means %s, beta1 means %s", vec(s$beta0), vec(s$beta1)),
               character(1))),
  stringsAsFactors = FALSE
)

fp <- fixed_pars
fixed_factors <- data.frame(
  Factor = c("k_x (regressors)", "m (common factors)", "rho_f (factor AR)",
             "rho_v (x idiosyncratic AR)", "phi_i", "beta0_ij, beta1_ij",
             "gamma_i (y loadings)", "Gamma_i (x loadings)", "sigma_e_i^2",
             "Fixed effects c_yi, c_xi", "Burn-in", "CSA lag rule p_T",
             "CSA variables", "Penalty structure", "Long-run trim rule",
             "Winsorization of theta_hat_ij", "Replications per scenario",
             "Master seed"),
  Value = c(fp$k_x, fp$m_factors, fp$rho_f, fp$rho_v,
            sprintf("U(%.2f, %.2f)", fp$phi_lo, fp$phi_hi),
            sprintf("N(slope mean_j, %.2f^2) and N(slope mean_j, %.2f^2)",
                    fp$beta0_sd, fp$beta1_sd),
            sprintf("U(%.1f, %.1f)", fp$load_lo, fp$load_hi),
            sprintf("U(%.1f, %.1f)", fp$load_lo, fp$load_hi),
            sprintf("U(%.1f, %.1f)", fp$sig2_lo, fp$sig2_hi),
            "N(0, 1)", paste(fp$burn, "periods"), "floor(T^(1/3))",
            "y_bar and x_bar's: lags 0..p_T",
            "Single R_*: ridge on CSA block only; Dual D_*: k_S on x-block + k_P on CSA; intercept & ECT never penalized",
            sprintf("Unit dropped from long-run MG if |ECT_hat_i| < %.2f (all estimators)", fp$ect_trim),
            sprintf("|theta_hat_ij| capped at %g (all estimators)", fp$theta_cap),
            N_REPS, MASTER_SEED),
  stringsAsFactors = FALSE
)
rm(fp)

robust_doc <- data.frame(
  Item = c("Baseline cell", "rho_x levels", rob_settings$Setting),
  Description = c(
    sprintf("N = %d, T = %d, %s slopes; all other factors as in the main design",
            rob_baseline$N, rob_baseline$T_len, rob_baseline$Slope),
    paste(rob_rho_grid, collapse = ", "),
    sprintf("sigma_ei^2 multiplier = %g; phi_i ~ U(%.2f, %.2f); rho_f = %.1f; m = %d",
            rob_settings$sig2_mult, rob_settings$phi_lo, rob_settings$phi_hi,
            rob_settings$rho_f, rob_settings$m_factors)),
  stringsAsFactors = FALSE
)

cat("\n===== CHANGING FACTORS (main design:", nrow(changing_factors), "scenarios) =====\n")
print(changing_levels, right = FALSE)
cat("\n===== FIXED FACTORS =====\n"); print(fixed_factors, right = FALSE)
if (RUN_ROBUSTNESS) {
  cat("\n===== ROBUSTNESS DESIGN (", nrow(robust_factors), "scenarios) =====\n")
  print(robust_doc, right = FALSE)
}

# scenario-specific parameter list passed to the DGP
scen_fp <- function(sc) {
  fp <- fixed_pars
  fp$beta0_mean <- slope_configs[[sc$Slope]]$beta0
  fp$beta1_mean <- slope_configs[[sc$Slope]]$beta1
  fp$rho_f      <- sc$rho_f
  fp$m_factors  <- as.integer(sc$m_factors)
  fp$sig2_mult  <- sc$sig2_mult
  fp$phi_lo     <- sc$phi_lo
  fp$phi_hi     <- sc$phi_hi
  fp
}


################################################################################
#   3.  Data-generating process 
################################################################################

generate_panel <- function(N, T_len, rho_x, fp) {

  Ttot <- T_len + fp$burn
  kx   <- fp$k_x
  m    <- fp$m_factors

  f <- matrix(0, Ttot, m)
  innov_f <- matrix(rnorm(Ttot * m, 0, sqrt(1 - fp$rho_f^2)), Ttot, m)
  for (t in 2:Ttot) f[t, ] <- fp$rho_f * f[t - 1, ] + innov_f[t, ]

  phi   <- runif(N, fp$phi_lo, fp$phi_hi)
  beta0 <- matrix(rnorm(N * kx, rep(fp$beta0_mean, each = N), fp$beta0_sd), N, kx)
  beta1 <- matrix(rnorm(N * kx, rep(fp$beta1_mean, each = N), fp$beta1_sd), N, kx)
  gam   <- matrix(runif(N * m, fp$load_lo, fp$load_hi), N, m)
  sig_e <- sqrt(fp$sig2_mult * runif(N, fp$sig2_lo, fp$sig2_hi))
  c_y   <- rnorm(N); c_x <- matrix(rnorm(N * kx), N, kx)

  Sig_v <- matrix(rho_x, kx, kx); diag(Sig_v) <- 1
  Cv    <- chol(Sig_v)

  Y <- matrix(0, N, Ttot)
  X <- array(0, dim = c(N, Ttot, kx))

  for (i in 1:N) {
    Gam_i <- matrix(runif(m * kx, fp$load_lo, fp$load_hi), m, kx)
    e_raw <- matrix(rnorm(Ttot * kx), Ttot, kx) %*% Cv
    v     <- matrix(0, Ttot, kx)
    for (t in 2:Ttot) v[t, ] <- fp$rho_v * v[t - 1, ] + sqrt(1 - fp$rho_v^2) * e_raw[t, ]
    Xi <- matrix(c_x[i, ], Ttot, kx, byrow = TRUE) + f %*% Gam_i + v
    X[i, , ] <- Xi

    eps <- rnorm(Ttot, 0, sig_e[i])
    fy  <- as.vector(f %*% gam[i, ])
    for (t in 2:Ttot) {
      Y[i, t] <- c_y[i] + phi[i] * Y[i, t - 1] +
        sum(beta0[i, ] * Xi[t, ]) + sum(beta1[i, ] * Xi[t - 1, ]) +
        fy[t] + eps[t]
    }
  }

  keep <- (fp$burn + 1):Ttot
  list(
    Y = Y[, keep, drop = FALSE],
    X = X[, keep, , drop = FALSE],
    theta_true = (beta0 + beta1) / (1 - phi),   # N x kx realized targets
    sr_true    = beta0,
    ect_true   = phi - 1
  )
}


################################################################################
#   4.  Ridge parameter (k) selectors
################################################################################


k_df_solver <- function(lam, T_eff) {
  # [NOVEL R_DF] effective degrees of freedom of the CSA block matched to
  # ceil(T_eff^(1/3)) + 1, the same T^(1/3) order as the CSA lag rule
  df_star <- ceiling(T_eff^(1/3)) + 1
  p <- length(lam)
  if (df_star >= p) return(1e-8 * lam[1])
  f <- function(kk) sum(lam / (lam + kk)) - df_star
  uniroot(f, lower = 1e-12 * lam[1], upper = 1e12 * lam[1], tol = 1e-10)$root
}

compute_targeted_specs <- function(lam, alpha, s2, T_eff) {
  p  <- length(lam)
  a2 <- pmax(alpha^2, 1e-12)
  if (p < 2) return(list(CS_OLS = list(type = "ridge", k = 0)))
  kappa <- lam[1] / lam[p]
  hkb   <- p * s2 / sum(a2)
  k_kl  <- p * s2 / (2 * sum(a2) + s2 * sum(1 / lam))   # Kibria-Lukman (2020)
  k_kl  <- min(max(k_kl, 1e-8), 1e6)
  clip  <- function(k) min(max(k, 0), 1e6)
  list(
    CS_OLS = list(type = "ridge", k = 0),
    R_HK   = list(type = "ridge", k = clip(s2 / max(a2))),
    R_HKB  = list(type = "ridge", k = clip(hkb)),
    R_KL   = list(type = "kl",    k = k_kl),
    R_CN   = list(type = "ridge", k = clip(hkb * max(log(kappa), 1e-4))),
    R_DF   = list(type = "ridge", k = clip(k_df_solver(lam, T_eff)))
  )
}

cn_rule <- function(lam_g, alpha_g, s2) {
  # condition-adaptive rule k = HKB * ln(kappa) on an arbitrary block
  okg <- lam_g > 1e-10 * max(lam_g[1], 1)
  lg  <- lam_g[okg]
  ag2 <- pmax(alpha_g[okg]^2, 1e-12)
  pg  <- length(lg)
  if (pg < 2) return(1e-8)
  kap <- lg[1] / lg[pg]
  min(max((pg * s2 / sum(ag2)) * max(log(kap), 1e-4), 1e-8), 1e6)
}


################################################################################
#   5.  Unit-level CS-ARDL estimation: single-penalty (A) and dual-penalty (B)
################################################################################


fit_unit_all <- function(dy, y_l1, Xl1, dX, CSA, kx, fp) {

  T_eff <- length(dy)
  coef_mat  <- matrix(NA_real_, n_est, 1 + 2 * kx,
                      dimnames = list(est_names,
                                      c("ECT", paste0("B", 1:kx), paste0("b", 1:kx))))
  resid_mat <- matrix(NA_real_, n_est, T_eff, dimnames = list(est_names, NULL))
  k_used    <- setNames(rep(NA_real_, n_est), est_names)

  ## ===== (A) SINGLE-PENALTY architecture: ridge on CSA block only ==================
  XU <- cbind(Intercept = 1, ECT = y_l1, Xl1, dX)
  XP <- CSA

  qU    <- qr(XU)
  ystar <- qr.resid(qU, dy)
  XPs   <- qr.resid(qU, XP)

  nrm  <- sqrt(colSums(XPs^2))
  keep <- nrm > 1e-8 * max(nrm, 1)
  S    <- sweep(XPs[, keep, drop = FALSE], 2, nrm[keep], "/")

  eg  <- eigen(crossprod(S), symmetric = TRUE)
  lam <- pmax(eg$values, 0)
  V   <- eg$vectors
  cv  <- crossprod(V, crossprod(S, ystar))

  tol   <- 1e-10 * max(lam[1], 1)
  ok    <- lam > tol
  alpha <- ifelse(ok, cv / lam, 0)

  bS0   <- V %*% alpha
  res0  <- ystar - as.vector(S %*% bS0)
  p_pen <- sum(ok)
  df    <- max(T_eff - ncol(XU) - p_pen, 1)
  s2    <- sum(res0^2) / df

  specs <- compute_targeted_specs(lam[ok], alpha[ok], s2, T_eff)

  for (e in names(specs)) {
    kk  <- specs[[e]]$k
    fac <- if (specs[[e]]$type == "kl") {
      (lam - kk) / (lam + kk)
    } else if (kk == 0) {
      rep(1, length(lam))
    } else {
      lam / (lam + kk)
    }
    bS <- V %*% (alpha * fac)
    bP <- numeric(ncol(XP)); bP[keep] <- as.vector(bS) / nrm[keep]
    bU <- qr.coef(qU, dy - as.vector(XP %*% bP))
    bU[is.na(bU)] <- 0
    fit <- as.vector(XU %*% bU) + as.vector(XP %*% bP)

    coef_mat[e, "ECT"] <- bU["ECT"]
    coef_mat[e, paste0("B", 1:kx)] <- bU[colnames(Xl1)]
    coef_mat[e, paste0("b", 1:kx)] <- bU[colnames(dX)]
    resid_mat[e, ] <- dy - fit
    k_used[e] <- kk
  }

  ## ===== (B) DUAL-PENALTY architecture ========================================
  XU0 <- cbind(Intercept = 1, ECT = y_l1)
  XS  <- cbind(Xl1, dX)
  q0  <- qr(XU0)
  y0  <- qr.resid(q0, dy)
  Ss  <- qr.resid(q0, XS)
  Ps  <- qr.resid(q0, XP)

  nS <- sqrt(colSums(Ss^2)); keepS <- nS > 1e-8 * max(nS, 1)
  nP <- sqrt(colSums(Ps^2)); keepP <- nP > 1e-8 * max(nP, 1)
  Zs <- sweep(Ss[, keepS, drop = FALSE], 2, nS[keepS], "/")
  Zp <- sweep(Ps[, keepP, drop = FALSE], 2, nP[keepP], "/")
  Z  <- cbind(Zs, Zp)
  pS <- ncol(Zs); pP <- ncol(Zp)

  ZtZ  <- crossprod(Z)
  Zty  <- crossprod(Z, y0)
  egZ  <- eigen(ZtZ, symmetric = TRUE)
  lamZ <- pmax(egZ$values, 0)
  okZ  <- lamZ > 1e-10 * max(lamZ[1], 1)
  b0   <- egZ$vectors %*% ifelse(okZ, crossprod(egZ$vectors, Zty) / lamZ, 0)

  egS <- eigen(crossprod(Zs), symmetric = TRUE)
  egP <- eigen(crossprod(Zp), symmetric = TRUE)
  aS  <- crossprod(egS$vectors, b0[seq_len(pS), , drop = FALSE])
  aP  <- crossprod(egP$vectors, b0[pS + seq_len(pP), , drop = FALSE])

  lamS <- pmax(egS$values, 0)
  lamP <- pmax(egP$values, 0)
  okP  <- lamP > 1e-10 * max(lamP[1], 1)

  kS_cn <- cn_rule(lamS, aS, s2)
  kP_cn <- cn_rule(lamP, aP, s2)
  kP_df <- k_df_solver(lamP[okP], T_eff)

  dual_specs <- list(D_CN = c(kS = kS_cn, kP = kP_cn),
                     D_DF = c(kS = kS_cn, kP = kP_df))

  for (e in names(dual_specs)) {
    ks <- dual_specs[[e]]
    Kd <- diag(c(rep(ks["kS"], pS), rep(ks["kP"], pP)), nrow = pS + pP)
    bstd <- tryCatch(
      solve(ZtZ + Kd, Zty),
      error = function(err)
        solve(ZtZ + Kd + diag(1e-8 * max(lamZ[1], 1), pS + pP), Zty))

    bSfull <- setNames(numeric(ncol(XS)), colnames(XS))
    bSfull[keepS] <- bstd[seq_len(pS)] / nS[keepS]
    bPfull <- numeric(ncol(XP))
    bPfull[keepP] <- bstd[pS + seq_len(pP)] / nP[keepP]

    bU0 <- qr.coef(q0, dy - as.vector(XS %*% bSfull) - as.vector(XP %*% bPfull))
    bU0[is.na(bU0)] <- 0
    fit <- as.vector(XU0 %*% bU0) + as.vector(XS %*% bSfull) +
           as.vector(XP %*% bPfull)

    coef_mat[e, "ECT"] <- bU0["ECT"]
    coef_mat[e, paste0("B", 1:kx)] <- bSfull[colnames(Xl1)]
    coef_mat[e, paste0("b", 1:kx)] <- bSfull[colnames(dX)]
    resid_mat[e, ] <- dy - fit
    k_used[e] <- ks["kP"]
  }


  cn_of <- function(l) { l <- l[l > 1e-12 * max(l[1], 1)]; sqrt(l[1] / l[length(l)]) }
  XSc  <- qr.resid(qr(cbind(XU0, XP)), XS)
  nC   <- sqrt(colSums(XSc^2)); keepC <- nC > 1e-8 * max(nC, 1)
  lamC <- eigen(crossprod(sweep(XSc[, keepC, drop = FALSE], 2, nC[keepC], "/")),
                symmetric = TRUE, only.values = TRUE)$values
  diag_cn <- c(CN_full   = cn_of(lamZ),        # whole regressor set (x + CSA)
               CN_x      = cn_of(lamS),        # x-block, CSA not partialled
               CN_x_cond = cn_of(pmax(lamC, 0)),  # x-block given the CSAs
               CN_CSA    = cn_of(lamP))        # CSA block alone

  list(coef = coef_mat, resid = resid_mat, k = k_used, cn = diag_cn)
}


################################################################################
#   6.  One replication: build panel, run all units, Mean-Group aggregation
################################################################################

pesaran_cd <- function(res_mat) {
  N <- nrow(res_mat); Tn <- ncol(res_mat)
  if (N < 3) return(NA_real_)
  R <- suppressWarnings(cor(t(res_mat)))
  sqrt(2 * Tn / (N * (N - 1))) * sum(R[upper.tri(R)])
}

run_one_rep <- function(N, T_len, rho_x, fp) {

  dat <- generate_panel(N, T_len, rho_x, fp)
  kx  <- fp$k_x
  p_T <- floor(T_len^(1/3))

  ybar <- colMeans(dat$Y)
  xbar <- apply(dat$X, c(2, 3), mean)
  csa_all <- cbind(ybar, xbar)
  colnames(csa_all) <- c("ybar", paste0("x", 1:kx, "bar"))

  t_idx <- (p_T + 1):T_len
  T_eff <- length(t_idx)

  CSA <- do.call(cbind, lapply(0:p_T, function(L) {
    out <- csa_all[t_idx - L, , drop = FALSE]
    colnames(out) <- paste0(colnames(csa_all), "_l", L)
    out
  }))

  coef_arr <- array(NA_real_, dim = c(n_est, N, 1 + 2 * kx))
  res_arr  <- array(NA_real_, dim = c(n_est, N, T_eff))
  k_mat    <- matrix(NA_real_, n_est, N)
  cn_mat   <- matrix(NA_real_, N, 4)

  for (i in 1:N) {
    yi <- dat$Y[i, ]; Xi <- dat$X[i, , ]
    dy   <- yi[t_idx] - yi[t_idx - 1]
    y_l1 <- yi[t_idx - 1]
    Xl1  <- Xi[t_idx - 1, , drop = FALSE]; colnames(Xl1) <- paste0("x", 1:kx, "_l1")
    dX   <- Xi[t_idx, , drop = FALSE] - Xi[t_idx - 1, , drop = FALSE]
    colnames(dX) <- paste0("dx", 1:kx)

    fu <- fit_unit_all(dy, y_l1, Xl1, dX, CSA, kx, fp)
    coef_arr[, i, ] <- fu$coef
    res_arr[, i, ]  <- fu$resid
    k_mat[, i]      <- fu$k
    cn_mat[i, ]     <- fu$cn
  }
  cn_rep <- colMeans(cn_mat)

  theta_tgt <- colMeans(dat$theta_true)
  sr_tgt    <- colMeans(dat$sr_true)
  ect_tgt   <- mean(dat$ect_true)

  out <- vector("list", n_est)
  for (e in seq_len(n_est)) {
    ect_i <- coef_arr[e, , 1]
    B_i   <- coef_arr[e, , 2:(1 + kx), drop = TRUE]
    b_i   <- coef_arr[e, , (2 + kx):(1 + 2 * kx), drop = TRUE]
    if (kx == 1) { B_i <- matrix(B_i, ncol = 1); b_i <- matrix(b_i, ncol = 1) }

    theta_i <- -B_i / ect_i
    bad     <- abs(ect_i) < fp$ect_trim
    theta_i[bad, ] <- NA
    trim_share <- mean(bad, na.rm = TRUE)
    wins_share <- if (all(is.na(theta_i))) NA_real_ else
      mean(abs(theta_i) >= fp$theta_cap, na.rm = TRUE)
    theta_i <- pmin(pmax(theta_i, -fp$theta_cap), fp$theta_cap)

    nv <- colSums(!is.na(theta_i))
    theta_mg <- colMeans(theta_i, na.rm = TRUE)
    theta_se <- apply(theta_i, 2, sd, na.rm = TRUE) / sqrt(pmax(nv, 1))
    crit  <- qt(0.975, df = pmax(nv - 1, 1))        # t with N - 1 df (MG s.e.)
    cover <- as.numeric(abs(theta_mg - theta_tgt) <= crit * theta_se)
    cover[nv < 3] <- NA
    theta_mg[nv < 3] <- NA

    out[[e]] <- data.frame(
      Estimator = est_names[e],
      th_err1 = theta_mg[1] - theta_tgt[1],
      th_err2 = theta_mg[2] - theta_tgt[2],
      sr_err1 = mean(b_i[, 1], na.rm = TRUE) - sr_tgt[1],
      sr_err2 = mean(b_i[, 2], na.rm = TRUE) - sr_tgt[2],
      ect_err = mean(ect_i, na.rm = TRUE) - ect_tgt,
      cover1  = cover[1], cover2 = cover[2],
      CD      = pesaran_cd(res_arr[e, , ]),
      k_mean  = mean(k_mat[e, ], na.rm = TRUE),
      Wins_share = wins_share, Trim_share = trim_share,
      CN_full = cn_rep[1], CN_x = cn_rep[2], CN_x_cond = cn_rep[3],
      CN_CSA = cn_rep[4]
    )
  }
  bind_rows(out)
}


################################################################################
#   7.  Monte Carlo loop over all scenarios (serial or PSOCK parallel)
################################################################################

run_scenario <- function(s) {
  sc  <- all_scen[s, ]
  fp  <- scen_fp(sc)
  off <- if (sc$Design == "Robustness") 5e7 else 0   # separate seed streams
  res <- vector("list", N_REPS)
  for (r in seq_len(N_REPS)) {
    set.seed(MASTER_SEED + off + s * 100000 + r)      # reproducible cell-by-cell
    rr <- run_one_rep(sc$N, sc$T_len, sc$rho_x, fp)
    rr$Scenario_ID <- sc$Scenario_ID; rr$Design <- sc$Design
    rr$Setting <- sc$Setting; rr$N <- sc$N; rr$T_len <- sc$T_len
    rr$rho_x <- sc$rho_x; rr$Slope <- sc$Slope; rr$rho_f <- sc$rho_f
    rr$m_factors <- sc$m_factors
    rr$rep <- r
    res[[r]] <- rr
  }
  bind_rows(res)
}

n_scen <- nrow(all_scen)
if (USE_PARALLEL && N_CORES > 1) {
  cat(sprintf("\nRunning %d scenarios x %d replications on %d PSOCK workers...\n",
              n_scen, N_REPS, N_CORES))
  cl <- parallel::makePSOCKcluster(N_CORES)
  parallel::clusterEvalQ(cl, suppressPackageStartupMessages(library(dplyr)))
  parallel::clusterExport(cl, c("generate_panel", "k_df_solver",
                                "compute_targeted_specs", "cn_rule",
                                "fit_unit_all", "pesaran_cd", "run_one_rep",
                                "run_scenario", "scen_fp", "fixed_pars",
                                "slope_configs", "est_names", "n_est",
                                "all_scen", "N_REPS", "MASTER_SEED"))
  rep_results <- parallel::parLapplyLB(cl, seq_len(n_scen), run_scenario)
  parallel::stopCluster(cl)
} else {
  rep_results <- vector("list", n_scen)
  for (s in seq_len(n_scen)) {
    sc <- all_scen[s, ]
    cat(sprintf("[%s] %s : N=%d, T=%d, rho_x=%.2f, %s slopes, %s\n",
                format(Sys.time(), "%H:%M:%S"), sc$Scenario_ID, sc$N, sc$T_len,
                sc$rho_x, sc$Slope, sc$Setting))
    rep_results[[s]] <- run_scenario(s)
  }
}

rep_df <- bind_rows(rep_results)
saveRDS(rep_df, "results_replevel.rds")
cat("\nReplication-level results saved: results_replevel.rds (",
    nrow(rep_df), "rows )\n")


################################################################################
#   8.  Aggregation across replications
################################################################################

key_cols <- c("Design", "Scenario_ID", "Setting", "N", "T_len", "rho_x",
              "Slope", "rho_f", "m_factors")

agg_all <- rep_df %>%
  group_by(across(all_of(key_cols)), Estimator) %>%
  summarise(
    Bias_LR   = mean(c(th_err1, th_err2), na.rm = TRUE),
    RMSE_LR   = sqrt(mean(c(th_err1^2, th_err2^2), na.rm = TRUE)),
    MAE_LR    = mean(abs(c(th_err1, th_err2)), na.rm = TRUE),
    Cover_LR  = mean(c(cover1, cover2), na.rm = TRUE),
    Bias_SR   = mean(c(sr_err1, sr_err2), na.rm = TRUE),
    RMSE_SR   = sqrt(mean(c(sr_err1^2, sr_err2^2), na.rm = TRUE)),
    Bias_ECT  = mean(ect_err, na.rm = TRUE),
    RMSE_ECT  = sqrt(mean(ect_err^2, na.rm = TRUE)),
    CD_Reject = mean(abs(CD) > 1.96, na.rm = TRUE),
    CD_mean   = mean(CD, na.rm = TRUE),
    Wins_pct  = 100 * mean(Wins_share, na.rm = TRUE),
    Trim_pct  = 100 * mean(Trim_share, na.rm = TRUE),
    k_mean    = mean(k_mean, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Estimator = factor(Estimator, levels = est_names)) %>%
  arrange(Scenario_ID, Estimator)

# collinearity diagnostics are design properties (identical for all estimators)
cn_all <- rep_df %>%
  filter(Estimator == est_benchmark) %>%
  group_by(across(all_of(key_cols))) %>%
  summarise(CN_full_median = median(CN_full), CN_x_median = median(CN_x),
            CN_x_cond_median = median(CN_x_cond), CN_CSA_median = median(CN_CSA),
            Share_CN_full_above_30 = mean(CN_full > 30),
            Share_CN_x_cond_above_30 = mean(CN_x_cond > 30), .groups = "drop") %>%
  arrange(Scenario_ID)

# RMSE ratio vs the benchmark (< 1 = estimator beats CS_OLS)
rel_all <- agg_all %>%
  select(all_of(key_cols), Estimator, RMSE_LR, RMSE_SR, RMSE_ECT) %>%
  group_by(Scenario_ID) %>%
  mutate(across(c(RMSE_LR, RMSE_SR, RMSE_ECT),
                ~ .x / .x[Estimator == est_benchmark])) %>%
  ungroup()

beats_summary <- function(rel_d, by) {
  rel_d %>%
    filter(Estimator != est_benchmark) %>%
    group_by(Estimator, across(all_of(by))) %>%
    summarise(Scenarios = n(),
              Pct_beat_LR  = 100 * mean(RMSE_LR  < 1),
              Pct_beat_SR  = 100 * mean(RMSE_SR  < 1),
              Pct_beat_ECT = 100 * mean(RMSE_ECT < 1),
              Median_ratio_LR = median(RMSE_LR), Worst_ratio_LR = max(RMSE_LR),
              .groups = "drop") %>%
    arrange(across(all_of(by)), Estimator)
}

main_id <- c("Scenario_ID", "N", "T_len", "rho_x", "Slope")
rob_id  <- c("Scenario_ID", "Setting", "N", "T_len", "rho_x")

agg      <- filter(agg_all, Design == "Main")
rel_long <- filter(rel_all, Design == "Main")
beats_main <- beats_summary(rel_long, c("Slope", "rho_x"))

cat("\n===== MAIN: percent of scenarios in which each estimator beats CS_OLS =====\n")
print(as.data.frame(beats_main), digits = 3)

if (RUN_ROBUSTNESS) {
  agg_rob   <- filter(agg_all, Design == "Robustness")
  rel_rob   <- filter(rel_all, Design == "Robustness")
  beats_rob <- beats_summary(rel_rob, c("Setting", "rho_x"))
  cat("\n===== ROBUSTNESS: long-run RMSE ratio vs CS_OLS =====\n")
  print(as.data.frame(rel_rob %>% select(Setting, rho_x, Estimator, RMSE_LR) %>%
                        pivot_wider(names_from = Estimator, values_from = RMSE_LR)),
        digits = 3)
}


################################################################################
#   9.  Excel export
################################################################################

criteria_spec <- list(
  RMSE_LongRun     = list(col = "RMSE_LR",   rule = "min"),
  Bias_LongRun     = list(col = "Bias_LR",   rule = "min_abs"),
  MAE_LongRun      = list(col = "MAE_LR",    rule = "min"),
  Coverage_LongRun = list(col = "Cover_LR",  rule = "target95"),
  RMSE_ShortRun    = list(col = "RMSE_SR",   rule = "min"),
  Bias_ShortRun    = list(col = "Bias_SR",   rule = "min_abs"),
  RMSE_ECT         = list(col = "RMSE_ECT",  rule = "min"),
  Bias_ECT         = list(col = "Bias_ECT",  rule = "min_abs")
)

class_sets <- list(Single = c(est_benchmark, est_single),
                   Dual   = c(est_benchmark, est_dual))

pick_best <- function(x, rule) {
  score <- switch(rule,
                  min      = x,
                  min_abs  = abs(x),
                  target95 = abs(x - 0.95),
                  target05 = abs(x - 0.05))
  if (all(is.na(score))) return(integer(0))
  which(score <= min(score, na.rm = TRUE) + 1e-12)
}

wide_tab <- function(colname, data, id_cols) {
  data %>%
    select(all_of(id_cols), Estimator, value = all_of(colname)) %>%
    pivot_wider(names_from = Estimator, values_from = value) %>%
    select(all_of(id_cols), all_of(est_names)) %>%
    arrange(Scenario_ID)
}

style_header <- createStyle(textDecoration = "bold", fgFill = "#0D3B66",
                            fontColour = "#FFFFFF", halign = "center",
                            border = "TopBottomLeftRight")
style_best   <- createStyle(fgFill = "#C6EFCE", fontColour = "#006100",
                            textDecoration = "bold")
style_num    <- createStyle(halign = "center", numFmt = "0.0000")

readme_tab <- function(design_note) {
  data.frame(Item = c(
    "Study", "Design", "Model", "Estimation window", "Penalty structure",
    "CS_OLS", "R_HK", "R_HKB", "R_KL", "R_CN [novel]", "R_DF [novel]",
    "D_CN [novel dual]", "D_DF [novel dual]", "k_mean column note",
    "Evaluation targets", "Trim rule", "Winsorization",
    "Best-method rule per sheet", "Coverage interval", "Collinearity sheet",
    "CD_Diagnostic sheet", "Winsorized_Share sheet", "Beats_OLS sheet",
    "Replications", "Date"),
    Description = c(
      "Monte Carlo comparison of ridge-regularized CS-ARDL estimators with classical CS-ARDL",
      design_note,
      "Heterogeneous panel ARDL(1,1) with AR(1) common factors in y and x (Chudik-Pesaran style DGP)",
      "ECM form; CSA block = lags 0..floor(T^(1/3)) of (ybar, x1bar, x2bar)",
      "Single-penalty R_*: ridge on CSA block only. Dual-penalty D_*: k_S on x-block and k_P on CSA block. Intercept and ECT never penalized",
      "Classical CS-ARDL by OLS (= Gaussian maximum likelihood of the unit regression); k = 0",
      "Hoerl-Kennard (1970): k = s2 / max(alpha_j^2); CSA-block penalty",
      "Hoerl-Kennard-Baldwin (1975): k = p*s2 / sum(alpha_j^2); CSA-block penalty",
      "Kibria-Lukman (2020): ((l-k)/(l+k))*alpha with k = p*s2/(2*sum(a2)+s2*sum(1/l)); CSA-block penalty",
      "Condition-adaptive: k = HKB * ln(kappa), kappa = lambda_max/lambda_min of CSA block",
      "Effective-DF-matched: k solves sum lambda/(lambda+k) = ceil(T_eff^(1/3)) + 1 on the CSA block",
      "k_S = HKB_S*ln(kappa_S) on structural x-block and k_P = HKB_P*ln(kappa_P) on CSA block",
      "k_P = effective-DF-matched on CSA block, k_S = HKB_S*ln(kappa_S) on x-block",
      "For dual estimators k_mean is the CSA-block k_P; for R_KL it is the KL parameter",
      "Realized cross-sectional means: theta_bar (long-run), beta0_bar (short-run), mean(phi)-1 (ECT)",
      sprintf("Unit excluded from long-run MG when |ECT_hat_i| < %.2f (identical for all estimators)", fixed_pars$ect_trim),
      sprintf("theta_hat_ij winsorized at +/-%g (identical for all estimators)", fixed_pars$theta_cap),
      "GREEN = best within class: {CS_OLS + single-penalty} and {CS_OLS + dual-penalty}. RMSE/MAE minimum; Bias minimum absolute; Coverage closest to 0.95; ties all highlighted",
      "MG estimate +/- t(0.975, N_used - 1) x MG standard error (Pesaran-Smith 1995 MG variance)",
      "Median condition numbers CN = sqrt(lambda_max/lambda_min) of unit-scaled regressors after partialling intercept and ECT. CN_x_cond also partials out the CSA block (collinearity the estimator faces). CN > 30 = severe collinearity (Belsley, Kuh & Welsch 1980)",
      "Mean Pesaran CD statistic and rejection rate of residuals. Descriptive only, not ranked: the CD test is distorted for residuals of CSA-augmented regressions (Juodis & Reese 2022, JBES 40(3), 1191-1203)",
      "Percent of unit long-run estimates winsorized at +/- the cap, and percent of units trimmed for |ECT_hat| below the trim value. Winsorization truncates the largest errors, so it favours the estimator winsorized most",
      "Percent of scenarios in which each estimator has lower RMSE than CS_OLS",
      as.character(N_REPS),
      format(Sys.Date())
    ))
}

write_results_workbook <- function(file, agg_d, rel_d, cn_d, beats_d, id_cols,
                                   factor_tables, design_note) {
  wb  <- createWorkbook()
  nid <- length(id_cols)

  addWorksheet(wb, "README")
  writeData(wb, "README", readme_tab(design_note), headerStyle = style_header)
  setColWidths(wb, "README", cols = 1:2, widths = c(28, 120))

  addWorksheet(wb, "Factors")
  row <- 1
  for (nm in names(factor_tables)) {
    writeData(wb, "Factors", data.frame(Section = nm), startRow = row, colNames = FALSE)
    writeData(wb, "Factors", factor_tables[[nm]], startRow = row + 1,
              headerStyle = style_header)
    row <- row + nrow(factor_tables[[nm]]) + 4
  }
  setColWidths(wb, "Factors", cols = 1:12, widths = c(36, 16, rep(12, 10)))

  cn_out <- cn_d %>% select(all_of(id_cols), CN_full_median, CN_x_median,
                            CN_x_cond_median, CN_CSA_median,
                            Share_CN_full_above_30, Share_CN_x_cond_above_30)
  addWorksheet(wb, "Collinearity")
  writeData(wb, "Collinearity", cn_out, headerStyle = style_header)
  addStyle(wb, "Collinearity", createStyle(halign = "center", numFmt = "0.00"),
           rows = 2:(nrow(cn_out) + 1), cols = (nid + 1):(nid + 6), gridExpand = TRUE)
  setColWidths(wb, "Collinearity", cols = 1:(nid + 6), widths = "auto")

  # descriptive two-block sheets (no ranking, no highlighting)
  write_diag_sheet <- function(sheet, blocks, fmt) {
    addWorksheet(wb, sheet)
    row <- 1
    for (nm in names(blocks)) {
      tab <- wide_tab(blocks[[nm]], agg_d, id_cols)
      writeData(wb, sheet, data.frame(Section = nm), startRow = row, colNames = FALSE)
      writeData(wb, sheet, tab, startRow = row + 1, headerStyle = style_header)
      addStyle(wb, sheet, createStyle(halign = "center", numFmt = fmt),
               rows = (row + 2):(row + 1 + nrow(tab)), cols = (nid + 1):(nid + n_est),
               gridExpand = TRUE)
      row <- row + nrow(tab) + 4
    }
    setColWidths(wb, sheet, cols = 1:(nid + n_est), widths = "auto")
  }
  write_diag_sheet("CD_Diagnostic",
                   list("MEAN PESARAN CD STATISTIC OF RESIDUALS (descriptive)" = "CD_mean",
                        "REJECTION RATE |CD| > 1.96 (descriptive, not a performance criterion)" = "CD_Reject"),
                   "0.000")
  write_diag_sheet("Winsorized_Share",
                   list("PERCENT OF UNIT LONG-RUN ESTIMATES WINSORIZED" = "Wins_pct",
                        "PERCENT OF UNITS TRIMMED FOR SMALL |ECT|" = "Trim_pct"),
                   "0.00")

  wins <- matrix(0L, n_est, length(criteria_spec) + 1,
                 dimnames = list(est_names, c(names(criteria_spec), "RelRMSE_LongRun")))

  write_sheet <- function(sheet, tab, rule) {
    best <- lapply(class_sets, function(cs) {
      lapply(seq_len(nrow(tab)), function(r) {
        idx <- pick_best(as.numeric(tab[r, cs]), rule)
        match(cs[idx], est_names)
      })
    })
    lab <- function(ii) if (length(ii)) paste(est_names[ii], collapse = " / ") else NA_character_
    tab$Best_Single <- vapply(best$Single, lab, "")
    tab$Best_Dual   <- vapply(best$Dual, lab, "")

    addWorksheet(wb, sheet)
    writeData(wb, sheet, tab, headerStyle = style_header)
    addStyle(wb, sheet, style_num, rows = 2:(nrow(tab) + 1),
             cols = (nid + 1):(nid + n_est), gridExpand = TRUE, stack = TRUE)
    for (r in seq_len(nrow(tab))) {
      for (bi in unique(c(best$Single[[r]], best$Dual[[r]]))) {
        addStyle(wb, sheet, style_best, rows = r + 1, cols = nid + bi, stack = TRUE)
        wins[bi, sheet] <<- wins[bi, sheet] + 1L
      }
    }
    freezePane(wb, sheet, firstActiveRow = 2, firstActiveCol = nid + 1)
    setColWidths(wb, sheet, cols = 1:(nid + n_est + 2), widths = "auto")
  }

  for (sheet in names(criteria_spec)) {
    write_sheet(sheet, wide_tab(criteria_spec[[sheet]]$col, agg_d, id_cols),
                criteria_spec[[sheet]]$rule)
  }
  write_sheet("RelRMSE_LongRun", wide_tab("RMSE_LR", rel_d, id_cols), "min")

  addWorksheet(wb, "Beats_OLS")
  writeData(wb, "Beats_OLS", beats_d, headerStyle = style_header)
  nb <- ncol(beats_d)
  addStyle(wb, "Beats_OLS", createStyle(halign = "center", numFmt = "0.000"),
           rows = 2:(nrow(beats_d) + 1), cols = (nb - 4):nb, gridExpand = TRUE)
  setColWidths(wb, "Beats_OLS", cols = 1:nb, widths = "auto")

  addWorksheet(wb, "Wins_Summary")
  wins_df <- data.frame(Estimator = rownames(wins), Class = est_class[rownames(wins)],
                        wins, row.names = NULL, check.names = FALSE)
  wins_df$Total_Wins <- rowSums(wins)
  writeData(wb, "Wins_Summary", wins_df, headerStyle = style_header)
  for (cs in names(class_sets)) {
    rows_cs <- match(class_sets[[cs]], wins_df$Estimator)
    for (cc in 3:ncol(wins_df)) {
      br <- rows_cs[which.max(wins_df[[cc]][rows_cs])]
      addStyle(wb, "Wins_Summary", style_best, rows = br + 1, cols = cc, stack = TRUE)
    }
  }
  setColWidths(wb, "Wins_Summary", cols = 1:ncol(wins_df), widths = "auto")

  saveWorkbook(wb, file, overwrite = TRUE)
  cat("Excel file written:", file.path(WORKDIR, file), "\n")
}

write_results_workbook(
  EXCEL_FILE, agg, rel_long, filter(cn_all, Design == "Main"), beats_main, main_id,
  factor_tables = list(
    "CHANGING FACTORS (levels)" = changing_levels,
    "MAIN SCENARIOS" = select(changing_factors, Scenario_ID, N, T_len, rho_x,
                              Slope, p_T),
    "FIXED FACTORS" = fixed_factors),
  design_note = sprintf("MAIN design: full factorial over N, T, rho_x and slope configuration (%d scenarios)",
                        nrow(changing_factors)))

if (RUN_ROBUSTNESS) {
  write_results_workbook(
    EXCEL_ROB, agg_rob, rel_rob, filter(cn_all, Design == "Robustness"), beats_rob,
    rob_id,
    factor_tables = list(
      "ROBUSTNESS DESIGN" = robust_doc,
      "ROBUSTNESS SCENARIOS" = select(robust_factors, Scenario_ID, Setting, N, T_len,
                                      rho_x, Slope, sig2_mult, phi_lo, phi_hi, rho_f, m_factors),
      "FIXED FACTORS (unless changed above)" = fixed_factors),
    design_note = "ROBUSTNESS design: one factor changed at a time around a baseline cell, across rho_x")
}


################################################################################
#   10. Graphs
################################################################################

theme_sim <- theme_minimal(base_size = 14) +
  theme(
    text             = element_text(color = "black", size = 14),
    axis.title       = element_text(face = "bold", size = 14, color = "black"),
    axis.text        = element_text(size = 12, color = "black"),
    plot.title       = element_text(face = "bold", size = 15, color = "black", hjust = 0.5),
    plot.subtitle    = element_text(size = 13, color = "black", hjust = 0.5),
    legend.position  = "bottom",
    legend.text      = element_text(size = 11, color = "black"),
    legend.title     = element_text(face = "bold", size = 12, color = "black"),
    strip.text       = element_text(face = "bold", size = 12, color = "black"),
    panel.grid.minor = element_blank(),
    panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.5),
    plot.margin      = ggplot2::margin(8, 8, 8, 8)
  )

est_colors <- c(CS_OLS = "#000000", R_HK = "#7F7F7F", R_HKB = "#B15928",
                R_KL = "#2A9D8F", R_CN = "#E63946", R_DF = "#6A3D9A",
                D_CN = "#E76F51", D_DF = "#1B6B93")
est_shapes <- c(CS_OLS = 16, R_HK = 17, R_HKB = 15, R_KL = 4,
                R_CN = 18, R_DF = 6, D_CN = 8, D_DF = 3)

save_png <- function(file, plot, w = 14, h = 14)
  ggsave(file.path("graphs", file), plot, width = w, height = h,
         dpi = GRAPH_DPI, bg = "white")

config_levels <- paste(names(slope_configs), "slopes")
add_config <- function(d) mutate(d, Config = factor(paste(Slope, "slopes"),
                                                    levels = config_levels))

agg_plot <- agg %>%
  add_config() %>%
  mutate(N_lab   = factor(paste0("N = ", N), levels = paste0("N = ", N_grid)),
         rho_lab = paste0("rho[x] == ", rho_x),
         T_fac   = factor(T_len),
         absBias_LR = abs(Bias_LR))

line_plot <- function(data, ycol, ylab, title, subtitle, hline = NULL) {
  p <- ggplot(data, aes(x = T_fac, y = .data[[ycol]],
                        color = Estimator, shape = Estimator, group = Estimator)) +
    geom_line(linewidth = 0.9) + geom_point(size = 2.8, stroke = 1) +
    facet_grid(rho_lab ~ N_lab, labeller = labeller(rho_lab = label_parsed)) +
    scale_color_manual(values = est_colors) +
    scale_shape_manual(values = est_shapes) +
    labs(title = title, subtitle = subtitle, x = "T (time-series length)", y = ylab,
         color = "Estimator", shape = "Estimator") +
    theme_sim
  if (!is.null(hline))
    p <- p + geom_hline(yintercept = hline, linetype = "dashed", color = "red")
  p
}

line_specs <- list(
  list(file = "01_rmse_longrun", col = "RMSE_LR",
       ylab = "RMSE of MG long-run coefficients",
       title = "Long-Run RMSE by Estimator across N, T and Regressor Correlation"),
  list(file = "02_absbias_longrun", col = "absBias_LR",
       ylab = "|Bias| of MG long-run coefficients",
       title = "Absolute Bias of MG Long-Run Coefficients"),
  list(file = "03_rmse_ect", col = "RMSE_ECT",
       ylab = "RMSE of MG error-correction coefficient",
       title = "ECT RMSE by Estimator across N, T and Regressor Correlation"),
  list(file = "04_coverage_longrun", col = "Cover_LR", hline = 0.95,
       ylab = "Empirical coverage of nominal 95% MG interval",
       title = "Coverage of 95% Long-Run MG Confidence Intervals"),
  list(file = "05_mean_cd_diagnostic", col = "CD_mean", hline = 0,
       ylab = "Mean Pesaran CD statistic of residuals",
       title = "Residual CD Statistic (descriptive diagnostic, not a performance criterion)")
)

# one graph per criterion and per slope configuration
for (sl in names(slope_configs)) {
  dsub <- filter(agg_plot, Slope == sl)
  sub  <- sprintf("%s slopes across the two regressors", sl)
  tag  <- sl
  for (ls in line_specs) {
    save_png(sprintf("%s_%s.png", ls$file, tag),
             line_plot(dsub, ls$col, ls$ylab, ls$title, sub, ls$hline))
  }

  # relative long-run RMSE heatmap for this combination
  rel_sub <- rel_long %>% filter(Slope == sl) %>%
    mutate(Estimator = factor(Estimator, levels = est_names))
  g6 <- ggplot(rel_sub, aes(x = Estimator, y = Scenario_ID, fill = RMSE_LR)) +
    geom_tile(color = "white", linewidth = 0.4) +
    geom_text(aes(label = sprintf("%.2f", RMSE_LR)), size = 3, color = "black") +
    scale_fill_gradient2(low = "#2E7D32", mid = "#FFFFFF", high = "#C62828",
                         midpoint = 1, name = "RMSE ratio\nvs CS_OLS") +
    labs(title = "Relative Long-Run RMSE (values below 1 favour the ridge estimator)",
         subtitle = sub, x = "Estimator", y = "Scenario") +
    theme_sim + theme(panel.grid = element_blank())
  save_png(sprintf("06_relative_rmse_heatmap_%s.png", tag), g6, w = 12,
           h = max(10, 0.3 * nrow(rel_sub) / n_est + 3))
}

# relative RMSE vs rho_x for each slope configuration, averaged over N and T
rel_rho <- rel_long %>%
  filter(Estimator != est_benchmark) %>%
  group_by(Slope, rho_x, Estimator) %>%
  summarise(across(c(RMSE_LR, RMSE_SR, RMSE_ECT), mean), .groups = "drop") %>%
  add_config() %>%
  pivot_longer(c(RMSE_LR, RMSE_SR, RMSE_ECT), names_to = "Parameter",
               values_to = "Ratio") %>%
  mutate(Parameter = case_match(Parameter, "RMSE_LR" ~ "Long-run",
                                "RMSE_SR" ~ "Short-run", "RMSE_ECT" ~ "ECT"),
         Parameter = factor(Parameter, levels = c("Long-run", "Short-run", "ECT")),
         rho_fac = factor(rho_x))
g8 <- ggplot(rel_rho, aes(x = rho_fac, y = Ratio, color = Estimator,
                          shape = Estimator, group = Estimator)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  geom_line(linewidth = 0.9) + geom_point(size = 2.8, stroke = 1) +
  facet_grid(Parameter ~ Config, scales = "free_y") +
  scale_color_manual(values = est_colors) +
  scale_shape_manual(values = est_shapes) +
  labs(title = "RMSE Relative to CS_OLS by Regressor Correlation and Slope Configuration",
       subtitle = "Averaged over N and T; values below the dashed line favour the ridge estimator",
       x = expression(bold(rho[x])), y = "RMSE ratio vs CS_OLS",
       color = "Estimator", shape = "Estimator") +
  theme_sim
save_png("08_relative_rmse_by_rho_slope.png", g8, w = 13, h = 11)

# replication-level errors in the hardest cells (smallest N and T, largest
# rho_x), shown for each slope configuration
hard <- rep_df %>%
  filter(Design == "Main", N == min(N_grid), T_len == min(T_grid),
         rho_x == max(rho_grid)) %>%
  mutate(Estimator = factor(Estimator, levels = est_names),
         Slope = factor(paste(Slope, "slopes"),
                        levels = paste(names(slope_configs), "slopes")),
         th_err = (th_err1 + th_err2) / 2)

box_plot <- function(ycol, title) {
  ggplot(hard, aes(x = Estimator, y = .data[[ycol]], fill = Estimator)) +
    geom_boxplot(outlier.size = 0.6, show.legend = FALSE) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    facet_wrap(~ Slope, nrow = 1) +
    scale_fill_manual(values = est_colors) +
    labs(title = title, x = NULL, y = "Error") +
    theme_sim + theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
g7 <- gridExtra::arrangeGrob(
  box_plot("th_err", "Long-Run MG Error"), box_plot("ect_err", "ECT MG Error"),
  ncol = 1,
  top = grid::textGrob(
    sprintf("Replication-Level Errors in the Hardest Cells (N=%d, T=%d, rho_x=%.2f)",
            min(N_grid), min(T_grid), max(rho_grid)),
    gp = grid::gpar(fontface = "bold", fontsize = 15)))
save_png("07_error_boxplots_hardest.png", g7, h = 12)

# robustness design: relative RMSE vs rho_x by setting
if (RUN_ROBUSTNESS) {
  rob_plot <- rel_rob %>%
    filter(Estimator != est_benchmark) %>%
    pivot_longer(c(RMSE_LR, RMSE_SR, RMSE_ECT), names_to = "Parameter",
                 values_to = "Ratio") %>%
    mutate(Parameter = case_match(Parameter, "RMSE_LR" ~ "Long-run",
                                  "RMSE_SR" ~ "Short-run", "RMSE_ECT" ~ "ECT"),
           Parameter = factor(Parameter, levels = c("Long-run", "Short-run", "ECT")),
           Setting = factor(Setting, levels = rob_settings$Setting),
           Estimator = factor(Estimator, levels = est_names),
           rho_fac = factor(rho_x))
  g9 <- ggplot(rob_plot, aes(x = rho_fac, y = Ratio, color = Estimator,
                             shape = Estimator, group = Estimator)) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
    geom_line(linewidth = 0.9) + geom_point(size = 2.8, stroke = 1) +
    facet_grid(Parameter ~ Setting, scales = "free_y") +
    scale_color_manual(values = est_colors) +
    scale_shape_manual(values = est_shapes) +
    labs(title = "Robustness: RMSE Relative to CS_OLS around the Baseline Cell",
         subtitle = sprintf("Baseline cell N = %d, T = %d, %s slopes",
                            rob_baseline$N, rob_baseline$T_len, rob_baseline$Slope),
         x = expression(bold(rho[x])), y = "RMSE ratio vs CS_OLS",
         color = "Estimator", shape = "Estimator") +
    theme_sim
  save_png("09_robustness_relative_rmse.png", g9, w = 16, h = 11)
}

cat("\nGraphs written to:", file.path(WORKDIR, "graphs"), "\n")
if (file.exists("Rplots.pdf")) invisible(file.remove("Rplots.pdf"))
cat("\nTotal elapsed:",
    format(round(difftime(Sys.time(), t_start, units = "mins"), 2)), "\n")

