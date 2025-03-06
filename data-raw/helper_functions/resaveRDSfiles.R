#' Resave RDS files with a specified compression method
#'
#' This function resaves RDS files with a specified compression method. If
#' compress is `auto`, the function will choose the best compression method
#' based on the file size.
#'
#' @param paths A character vector of file paths to resave.
#' @param compress A character string specifying the compression method. One of
#'   `"auto"`, `"gzip"`, `"bzip2"`, or `"xz"`. Default is `"auto"`. If
#'   `compress` is `"auto"`, the function will choose the best compression
#'   method based on the file size.
#' @param version he workspace format version to use. NULL specifies the current
#'   default version (3). The only other supported value is 2, the default from
#'   R 1.4.0 to R 3.5.0.
#' @export
#'

resaveRDSfiles <- function(paths, compress = c("auto", "gzip", "bzip2", "xz"),
                           version = NULL) {
  stop("This function does not work yet")
  if (length(paths) == 1L && dir.exists(paths)) {
    paths <- Sys.glob(c(file.path(paths, "*.rds")))
  }
  compress <- match.arg(compress)

  if (is.null(version)) {
    version <- 3L
  }
  for (p in paths) {
    this_obj <- readRDS(p)
    if (compress == "auto") {
      f1 <- tempfile()
      saveRDS(this_obj, file = f1, version = version)
      f2 <- tempfile()
      save(this_obj, file = f2, compress = "bzip2", version = version)
      ss <- file.size(c(f1, f2)) * c(0.9, 1)
      names(ss) <- c(f1, f2)
      if (ss[1L] > 10240) {
        f3 <- tempfile()
        save(this_obj,
          file = f3,
          compress = "xz", version = version
        )
        ss <- c(ss, file.size(f3))
        names(ss) <- c(f1, f2, f3)
      }
      nm <- names(ss)
      ind <- which.min(ss)
      file.copy(nm[ind], p, overwrite = TRUE)
      unlink(nm)
    } else {
      saveRDS(this_obj,
        file = p, compress = compress,
        version = ver
      )
    }
  }
}
