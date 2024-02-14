#' Multivariate environmental similarity surfaces (MESS)
#' 
#' Compute multivariate environmental similarity surfaces (MESS), as described
#' by Elith et al., 2010. This function is a modified version of `mess` in 
#' package `predicts`, with a method added to work on [`terra::SpatRasterDataset`].
#' 
#' \code{v} can be obtained for a set of points using
#' [terra::extract()] .
#' 
#' @param x [`terra::SpatRaster`], [`terra::SpatRasterDataset`] or [`data.frame`]
#' @param v matrix or data.frame containing the reference values; each column
#' should correspond to one layer of the [`terra::SpatRaster`] object.
#' @param .col the column containing the presences (optional). If specified,
#' it is excluded from the clamping.
#' @param filename character. Output filename (optional)
#' @param ... additional arguments as for [terra::writeRaster()]
#' @return a [`terra::SpatRaster`] (data.frame) with
#' the MESS values.
#' @author Jean-Pierre Rossi, Robert Hijmans, Paulo van Breugel, Andrea Manica
#' @references Elith J., M. Kearney M., and S. Phillips, 2010. The art of
#' modelling range-shifting species. Methods in Ecology and Evolution
#' 1:330-342. c("\\Sexpr[results=rd]{tools:::Rd_expr_doi(\"#1\")}",
#' "10.1111/j.2041-210X.2010.00036.x")\Sexpr{tools:::Rd_expr_doi("10.1111/j.2041-210X.2010.00036.x")}
 

# author: Jean-Pierre Rossi <jean-pierre.rossi@supagro.inra.fr>
# modifications by Robert Hijmans and Paulo van Breugel
# rewritten for predicts by RH
# adapted for tidysdm by AM

setGeneric("mess_predictors", function(x, training, ...) 
  standardGeneric("mess_predictors") )

setMethod("mess_predictors", signature(x="SpatRaster"), 
	function(x, training, .col, filename="", ...) {
	  # remove the class column if it is present
	  .col <- rlang::enquo(.col) %>%
	    rlang::quo_get_expr() %>%
	    rlang::as_string()
	  if (.col!=""){
	    training <- training %>% dplyr::select(-dplyr::one_of(.col))
	  }
	  # remove locations in training if they are present
	  training <- training %>% sf::st_drop_geometry()
	  # check that all variables are present in the raster
	  if (!setequal(names(training),names(x))){
	    stop("`x` and `training` should contain the same variables")
	  }
	  
		training <- stats::na.omit(training)
		if (nrow(training) < 2) {
			stop("insufficient number of reference points")
		}
    
		out <- terra::rast(x)
		nl <- terra::nlyr(x)
		nms <- paste0(names(x), "_mess")
		terra::readStart(x)
		on.exit(terra::readStop(x))
		if (nl == 1) {
			names(out) <- "mess"
			b <- terra::writeStart(out, filename, ...)
			for (i in 1:b$n) {		
				vv <- terra::readValues(x, b$row[i], b$nrows[i])
				p <- .messi(vv, training)
				terra::writeValues(out, p, b$row[i], b$nrows[i])
			}
		} else {
				terra::nlyr(out) <- 1
				names(out) <- "mess"
				b <- terra::writeStart(out, filename, ...)
				for (i in 1:b$n) {
					vv <- terra::readValues(x, b$row[i], b$nrows[i], mat=TRUE)
					vv <- sapply(1:ncol(training), function(i) .messi(vv[,i], training[,i]))
					suppressWarnings(m <- apply(vv, 1, min, na.rm=TRUE))
					m[!is.finite(m)] <- NA
					terra::writeValues(out, m, b$row[i], b$nrows[i])
			}
		}
		terra::writeStop(out)
		out
	}	
)

setMethod("mess_predictors", signature(x="data.frame"), 
	function(x, training, .col) {
	  # remove the class column if it is present
	  .col <- rlang::enquo(.col) %>%
	    rlang::quo_get_expr() %>%
	    rlang::as_string()
	  if (.col!=""){
	    training <- training %>% dplyr::select(-dplyr::one_of(.col))
	  }
	  # remove locations in training if they are present
	  training <- training %>% sf::st_drop_geometry()
	  x <- x %>% sf::st_drop_geometry()
	  # check that all variables are present in the raster
	  if (!setequal(names(training),names(x))){
	    stop("`x` and `training` should contain the same variables")
	  }
	  
	  training <- stats::na.omit(training)
	  if (nrow(training) < 2) {
	    stop("insufficient number of reference points")
	  }
	  
		if (ncol(x) == 1) {
			data.frame(mess=.messi(x, training))
		} else {
			x <- sapply(1:ncol(x), function(i) .messi(x[,i], training[,i]))
			rmess <- apply(x, 1, min, na.rm=TRUE)
				data.frame(mess=rmess)
		}	
	}
)

setMethod("mess_predictors", signature(x="SpatRasterDataset"), 
	function(x, training, .col) {
	  # remove the class column if it is present
	  .col <- rlang::enquo(.col) %>%
	    rlang::quo_get_expr() %>%
	    rlang::as_string()
	  if (.col!=""){
	    training <- training %>% dplyr::select(-dplyr::one_of(.col))
	  }
	  
  # check this is a valid region_series
  # TODO
  # cycle over the time steps
  for (i_time in pastclim::time_bp(x)){
    # get a slice
    
    # apply mess to the slice
    
  }
  # now combine layers into a raster

	}
)


.messi <- function(p, v) {
  if (inherits(v, "data.frame")){
    v <- v %>% dplyr::pull()
  }
	v <- sort(v)
	f <- 100 * findInterval(p, v) / length(v)
	minv <- v[1]
	maxv <- v[length(v)]
	res <- 2*f 
	f[is.na(f)] <- -99
	i <- f>50 & f<100
	res[i] <- 200-res[i]

	i <- f==0 
	res[i] <- 100*(p[i]-minv)/(maxv-minv)
	i <- f==100
	res[i] <- 100*(maxv-p[i])/(maxv-minv)
	res
}

.messix <- function(p,v) {
# a little bit different, no negative values.
	a <- stats::ecdf(v)(p)
	a[a>0.5] <- 1-a[a>0.5]
	200 * a
}


