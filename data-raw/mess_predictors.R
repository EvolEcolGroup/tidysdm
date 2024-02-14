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
#' @param filename character. Output filename (optional)
#' @param ... additional arguments as for {terra::writeRaster}
#' @return [`terra::SpatRaster`] (or data.frame) with layers (columns) corresponding to
#' the input layers and an additional layer with the mess values (if
#' \code{full=TRUE} and \code{nlyr(x) > 1}) or a [`terra::SpatRaster`] (data.frame) with
#' the MESS values (if \code{full=FALSE}).
#' @author Jean-Pierre Rossi, Robert Hijmans, Paulo van Breugel, Andrea Manica
#' @references Elith J., M. Kearney M., and S. Phillips, 2010. The art of
#' modelling range-shifting species. Methods in Ecology and Evolution
#' 1:330-342. c("\\Sexpr[results=rd]{tools:::Rd_expr_doi(\"#1\")}",
#' "10.1111/j.2041-210X.2010.00036.x")\Sexpr{tools:::Rd_expr_doi("10.1111/j.2041-210X.2010.00036.x")}
#' @examples
#' 
#' 
#' set.seed(9)
#' r <- rast(ncol=10, nrow=10)
#' r1 <- setValues(r, (1:ncell(r))/10 + rnorm(ncell(r)))
#' r2 <- setValues(r, (1:ncell(r))/10 + rnorm(ncell(r)))
#' r3 <- setValues(r, (1:ncell(r))/10 + rnorm(ncell(r)))
#' s <- c(r1,r2,r3)
#' names(s) <- c('a', 'b', 'c')
#' xy <- cbind(rep(c(10,30,50), 3), rep(c(10,30,50), each=3))
#' refpt <- extract(s, xy)
#' 
#' ms <- mess(s, refpt, full=TRUE)
#' plot(ms)
#' 
#' \dontrun{
#' filename <- paste0(system.file(package="predicts"), "/ex/bradypus.csv")
#' bradypus <- read.table(filename, header=TRUE, sep=',')
#' bradypus <- bradypus[,2:3]
#' 
#' predfile <- paste0(system.file(package="predicts"), "/ex/bio.tif")
#' predictors <- rast(predfile)
#' reference_points <- extract(predictors, bradypus, ID=FALSE)
#' mss <- mess(x=predictors, v=reference_points, full=TRUE)
#' 
#' breaks <- c(-500, -50, -25, -5, 0, 5, 25, 50, 100)
#' fcol <- colorRampPalette(c("blue", "beige", "red"))
#' plot(mss[[10]], breaks=breaks, col=fcol(9), plg=list(x="bottomleft"))
#' }
#' 
#' 

# author: Jean-Pierre Rossi <jean-pierre.rossi@supagro.inra.fr>
# modifications by Robert Hijmans and Paulo van Breugel
# rewritten for predicts by RH
# adapted for tidysdm by AM


setMethod("mess_predictors", signature(x="SpatRaster"), 
	function(x, v, filename="", ...) {

		v <- stats::na.omit(v)
		if (nrow(v) < 2) {
			stop("insufficient number of reference points")
		}
		stopifnot(NCOL(v) == nlyr(x))

		out <- rast(x)
		nl <- nlyr(x)
		nms <- paste0(names(x), "_mess")
		readStart(x)
		on.exit(readStop(x))
		if (nl == 1) {
			names(out) <- "mess"
			b <- writeStart(out, filename, ...)
			for (i in 1:b$n) {		
				vv <- terra::readValues(x, b$row[i], b$nrows[i])
				p <- .messi(vv, v)
				terra::writeValues(out, p, b$row[i], b$nrows[i])
			}
		} else {
			if (full) {
				nlyr(out) <- nl+1
				names(out) <- c(nms, "mess")
				b <- writeStart(out, filename, ...)
				for (i in 1:b$n) {
					vv <- terra::readValues(x, b$row[i], b$nrows[i], mat=TRUE)
					vv <- sapply(1:ncol(v), function(i) .messi(vv[,i], v[,i]))
					suppressWarnings(m <- apply(vv, 1, min, na.rm=TRUE))
					m[!is.finite(m)] <- NA
					terra::writeValues(out, cbind(vv, m), b$row[i], b$nrows[i])
				}
			} else {			
				nlyr(out) <- 1
				names(out) <- "mess"
				b <- writeStart(out, filename, ...)
				for (i in 1:b$n) {
					vv <- terra::readValues(x, b$row[i], b$nrows[i], mat=TRUE)
					vv <- sapply(1:ncol(v), function(i) .messi(vv[,i], v[,i]))
					suppressWarnings(m <- apply(vv, 1, min, na.rm=TRUE))
					m[!is.finite(m)] <- NA
					terra::writeValues(out, m, b$row[i], b$nrows[i])
				}
			}
		}
		writeStop(out)
		out
	}	
)

setMethod("mess_predictors", signature(x="data.frame"), 
	function(x, v, full=FALSE) {
		if (ncol(x) == 1) {
			data.frame(mess=.messi(x, v))
		} else {
			x <- sapply(1:ncol(x), function(i) .messi(x[,i], v[,i]))
			rmess <- apply(x, 1, min, na.rm=TRUE)
			if (full) {
				out <- data.frame(x, rmess)
				nms <- paste0(names(x), "_mess")
				names(out) <- c(nms, "mess")
				out
			} else {
				data.frame(mess=rmess)
			}
		}	
	}
)

setMethod("mess_predictors", signature(x="SpatRasterDataset"), 
	function(x, v, full=FALSE) {
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


