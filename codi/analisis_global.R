


####################################################  -----------------------------
# analisis_OT<-TRUE
# mostra_test=TRUE


# 1. Lectura  ----------------
rm(list = ls())
source("codi/global_metplus.R")
arguments_render(analisis_OT=F,mostra_test=F)

rmarkdown::render(input="./codi/1_LECTURA_METPLUS.Rmd",
                  params = list(analisis_OT= analisis_OT,mostra=mostra))


# 2. Preparació ---------------------
rm(list = ls())
gc()
source("codi/global_metplus.R")
arguments_render(analisis_OT=F,mostra_test=F)
rmarkdown::render(input="./codi/2_PREPARACIO_METPLUS.Rmd",
                  params =  list(analisis_OT= analisis_OT,mostra=mostra))


# 3. RESULTATS -------------------------
rm(list = ls())
gc()
source("codi/global_metplus.R")
arguments_render(analisis_OT=F,mostra_test=F)

rmarkdown::render(input="./codi/3_RESULTATS_METPLUS.Rmd",output_file=nom_output,
                  params = list(
                    analisis_OT= analisis_OT,
                    mostra=mostra,
                    analisis=tipoanalisis))

#################################################### ----------------------------

