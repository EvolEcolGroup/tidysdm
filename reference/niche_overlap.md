# Compute overlap metrics of the two niches

This function computes overlap metrics between two rasters. It currently
implements Schoener's D and the inverse I of Hellinger's distance.

## Usage

``` r
niche_overlap(x, y, method = c("Schoener", "Hellinger"))
```

## Arguments

- x:

  a
  [terra::SpatRaster](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  with a single layer

- y:

  a
  [terra::SpatRaster](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  with a single layer

- method:

  a string (or vector of strings) taking values "Schoener" and
  "Hellinger"

## Value

a list of overlap metrics, with slots *D* and *I* (depending on
`method`)

## Details

Note that Hellinger's distance is normalised by dividing by square root
of 2 (which is the correct asymptote for Hellinger's D), rather than the
incorrect 2 used originally in Warren et al (2008), based on the Erratum
for that paper.

## References

Warren, D.L., Glor, R.E. & Turelli M. (2008) Environmental niche
equivalency versus conservativism: quantitative approaches to niche
evolution. Evolution 62: 2868-2883
