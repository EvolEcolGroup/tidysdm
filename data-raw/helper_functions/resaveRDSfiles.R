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

resaveRDSfiles <- function(paths, compress = c("auto", "gzip", "bzip2", "xz", "zstd"),
                           version = NULL) {
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
      compress_options <- c("gzip", "bzip2", "xz", "zstd")
      compress_files <- c()
      # we try all compression options and pick the smallest one
      for (i_compress in compress_options)
      {
        f_temp <- tempfile()
        saveRDS(this_obj,
          file = f_temp,
          compress = i_compress,
          version = version
        )
        compress_files <- c(compress_files, f_temp)
      }
      file_sizes <- file.size(compress_files)
      which_min <- which.min(file_sizes)
      file.copy(compress_files[which_min], p, overwrite = TRUE)
      unlink(compress_files)
    } else {
      saveRDS(this_obj,
        file = p, compress = compress,
        version = ver
      )
    }
  }
}
