vignette_names <- c("a0_tidysdm_overview", "a1_palaeodata_application",
                    "a2_tidymodels_additions","a3_troubleshooting")

if (any(!file.exists(paste0("vignettes/",vignette_names,".Rmd.orig")))){
  file.rename(from = paste0("vignettes/",vignette_names,".Rmd"),
              to = paste0("vignettes/",vignette_names,".Rmd.orig"))
}

for (this_vignette_prefix in vignette_names){
  this_vignette <- paste0(this_vignette_prefix,".Rmd")
  # Pre-compile vignette
  knitr::knit(paste0("vignettes/",this_vignette,".orig"),paste0("vignettes/",this_vignette))
  
  # remove file path such that vignettes will build with figures
  replace <- readLines(paste0("vignettes/",this_vignette))
  replace <- gsub("!\\[([^]]+)\\]", "![]", replace)
  replace <- gsub("](figure/", paste0("](figure_",this_vignette_prefix,"/"),
                  replace, fixed=TRUE)
  fileConn <- file(paste0("vignettes/",this_vignette))
  writeLines(replace, fileConn)
  close(fileConn)
  
  if (!dir.exists(paste0("vignettes/figure_",this_vignette_prefix))){
    dir.create(paste0("vignettes/figure_",this_vignette_prefix))
  }
  # move resource figures to vignette
  resources <-
    list.files("figure/", pattern = ".png$", full.names = TRUE)
  file.copy(from = resources,
            to = paste0("vignettes/figure_",this_vignette_prefix),
            overwrite = TRUE)
  # delete the figure directory 
  unlink("figure/",recursive = TRUE)
}




#library("devtools")
#build_vignettes()


