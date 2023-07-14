function(x, truth)
x <- glm_predictions

sdm_metric_impl(truth, estimate, metric, thr) {
  TPR <- TNR <- JACCARD <- SORENSEN <- threshold <- FPB <- TSS <- NULL
  if (any(
    !(thr[is.na(suppressWarnings(as.numeric(thr)))]) %in% c(
      "lpt",
      "max_sens_spec",
      "equal_sens_spec",
      "sensitivity",
      "max_jaccard",
      "max_sorensen",
      "max_fpb"
    )
  )) {
    stop("'thr' Argument is not valid!")
  }

  if (is.null(thr)) {
    thr <- c(
      "lpt",
      "max_sens_spec",
      "max_kappa",
      "equal_sens_spec",
      "sensitivity",
      "max_jaccard",
      "max_sorensen",
      "max_fpb"
    )
  }

  if (any(thr %in% "sensitivity") &&
      !any(names(thr) %in% "sens")) {
    thr <- c(thr, sens = 0.9)
  }

  np <- length(p)
  na <- length(a)
  if (na == 0 | np == 0) {
    stop("cannot evaluate a model without absence and presence data that are not NA")
  }

  # Threshold breaks:
  if (length(p) > 1000) {
    tr <- as.vector(stats::quantile(p, 0:1000 / 1000))
  } else {
    tr <- p
  }
  if (length(a) > 1000) {
    tr <- c(tr, as.vector(stats::quantile(a, 0:1000 / 1000)))
  } else {
    tr <- c(tr, a)
  }
  tr <- sort(unique(round(tr, 8)))

  #add the desired threshold if we are using sensitivity
  if (any(thr %in% "sensitivity")) {
    tr <- c(tr, as.numeric(thr["sens"]))
    tr <- sort(tr)
  }

  res <- matrix(ncol = 4, nrow = length(tr))
  colnames(res) <- c("tp", "fp", "fn", "tn")
  # Confusion Matrix
  for (i in 1:length(tr)) {
    res[i, 1] <- length(p[p >= tr[i]]) # a  true positives
    res[i, 2] <- length(a[a >= tr[i]]) # b  false positives
    res[i, 3] <- length(p[p < tr[i]]) # c  false negatives
    res[i, 4] <- length(a[a < tr[i]]) # d  true negatives
  }
  res <- data.frame(res)

  # Performance metrics for evry given threshold
  performance <- dplyr::tibble(
    threshold = tr,
    n_presences = np,
    n_absences = na,
    TPR = res$tp / (res$tp + res$fn),
    TNR = res$tn / (res$tn + res$fp),
    SORENSEN = 2 * res$tp / (res$fn + (2 * res$tp) + res$fp),
    JACCARD = res$tp / (res$fn + res$tp + res$fp),
    FPB = 2 * JACCARD,
    OR = (1 - TPR),
    TSS = (TPR + TNR) - 1
  )

  # compute AUC
  R <- sum(rank(c(p, a))[1:np]) - (np * (np + 1) / 2)
  performance <- performance %>% dplyr::mutate(AUC = R / (as.numeric(na) * as.numeric(np)))

  # compute BOYCE
  if (is.null(bg)) {
    performance <- performance %>% dplyr::mutate(BOYCE = flexsdm:::boyce(pres = p, contrast = c(p, a)))
  } else {
    performance <- performance %>% dplyr::mutate(BOYCE = flexsdm:::boyce(pres = p, contrast = c(p, bg)))
  }

  # compute IMAE
  real <- c(rep(1, length(p)), rep(0, length(a)))
  pred <- c(p, a)
  performance <- performance %>% dplyr::mutate(IMAE = 1 - (sum(abs(real - pred)) / length(pred)))



  # Thresholds
  thresholds <- list()

  thresholds$max_sorensen <-
    max(performance %>% dplyr::filter(SORENSEN == max(SORENSEN)) %>% dplyr::pull(threshold))

  thresholds$max_jaccard <-
    max(performance %>% dplyr::filter(SORENSEN == max(SORENSEN)) %>% dplyr::pull(threshold))

  thresholds$max_fpb <-
    max(performance %>% dplyr::filter(FPB == max(FPB)) %>% dplyr::pull(threshold))

  thresholds$max_sens_spec <-
    max(performance %>% dplyr::filter(TSS == max(TSS)) %>% dplyr::pull(threshold))

  thresholds$equal_sens_spec <-
    performance$threshold[which(abs(
      performance$TPR - performance$TNR
    ) ==
      min(abs(performance$TPR - performance$TNR)))] %>% max()

  suppressWarnings(thresholds$lpt <- max(performance$threshold[performance$TPR == 1]))

  if (any(thr == "sensitivity")) {
    thresholds$sensitivity <- performance$threshold[which(abs(
      performance$TPR - as.numeric(thr["sens"])
    ) ==
      min(abs(performance$TPR - as.numeric(thr["sens"]))))] %>% max()
  }


  thresholds <- dplyr::bind_cols(thresholds)

  thr_table <- dplyr::tibble(threshold = names(thresholds), thr_value = unlist(thresholds))
  thr_table <- dplyr::left_join(thr_table, performance, by = c("thr_value" = "threshold"))

  # Return final result
  result <- thr_table
  if (!is.null(thr)) {
    result <- result %>%
      dplyr::filter(threshold %in% thr)
  }


}
