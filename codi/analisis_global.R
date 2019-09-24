

# 0. Inicialització de parametres           -----------------------------
# N test mostra a seleccionar  (Nmostra=Inf)
# Nmostra=Inf  # Seria tota la mostra
Nmostra=300
# Parametre discontinuitat/stop tractament:
gap_dies<-61
# Parametre d'analisis OT / No OT 
analisis_OT<-T


# Generació del nom del fitxer

nom_fitxer<-"3_RESULTATS_METPLUS"

if (Nmostra!=Inf) nom_fitxer<-paste0(nom_fitxer,".N",Nmostra)
if (analisis_OT) nom_fitxer<-paste0(nom_fitxer,".OT.html")
if (analisis_OT==F) nom_fitxer<-paste0(nom_fitxer,".ITT.html")




source("codi/1_LECTURA_METPLUS.R")
source("codi/2_PREPARACIO_METPLUS.R")

rmarkdown::render("codi/3_RESULTATS_METPLUS.Rmd",output_file=nom_fitxer)


