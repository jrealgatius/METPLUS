# 1. Inicialització de parametres           -----------------------------

rm(list=ls())
gc()

#   SOURCE
# CArrego funcions -------------------
link_source<-paste0("https://github.com/jrealgatius/Stat_codis/blob/master/funcions_propies.R","?raw=T")
devtools::source_url(link_source)


# Parametres -------------------
Nmostra=Inf
# Parametre discontinuitat/stop tractament:
gap_dies<-92
# Parametre d'analisis OT (TRUE) / ITT (FALSE) 
analisis_OT<-F
#
fitxersortida<-here::here("dades/preparades","BD_METPLUS_V5.rds")
# Conductor cataleg 
fitxer_cataleg<-"cataleg_met.xls"
# Conductor variables
conductor_variables<-"variables_metplus.xls"

# 2. Generació del nom del fitxer  -----------
nom_fitxer<-"3_RESULTATS_METPLUS"
if (Nmostra!=Inf) nom_fitxer<-paste0(nom_fitxer,"N",Nmostra)

if (analisis_OT) {
  nom_fitxer<-paste0(nom_fitxer,"OT.html")
  output_Rdata<-"Output_metplusOT.RData"
  tipoanalisis="On Treatment"
  } else {
  nom_fitxer<-paste0(nom_fitxer,"ITT.html")
  output_Rdata<-"Output_metplusITT.RData"
  tipoanalisis="Intention to treat (ITT)"}



# 3. Execució de codi -------------
source("./codi/1_LECTURA_METPLUS.R")

source("./codi/2_PREPARACIO_METPLUS.R")

rmarkdown::render(input="./codi/3_RESULTATS_METPLUS.Rmd",output_file=nom_fitxer,params = list(arxiu_Rdata=output_Rdata,analisis=tipoanalisis))
 

