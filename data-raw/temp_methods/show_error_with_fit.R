foo<-workflow(
  preprocessor = lacerta_rec_uncor,
  spec = sdm_spec_glm()
)

fit(foo,lacerta_thin)


lacerta_rec_all <- recipe(lacerta_thin, formula=class~.)
lacerta_rec_uncor <- lacerta_rec_all %>% step_select(all_of(c("class","bio05","bio06", "bio13", "bio15")))
lacerta_rec_log <- recipe(lacerta_thin, formula=class~.) %>% step_log(bio19)


lacerta_rec_uncor <- lacerta_rec_all %>% step_rm(all_of(c("bio05","bio06", "bio13", "bio15")))
