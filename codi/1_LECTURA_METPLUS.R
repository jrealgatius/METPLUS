#                 FASE 1: LECTURA                     --------------------
#
#i)                  
#1a)                    [Lectura]
#ib)                    [Generació data index]
#ic)                    [Generació de Cohorts]
#id)                    [Agregació de variables]

# 0. Directori de treball / càrrega de funcions    --------------
#
# Lectura de dades     

memory.size(max=130685)
#
#   SOURCE

rm(list=ls())


link_source<-paste0("https://github.com/jrealgatius/Stat_codis/blob/master/funcions_propies.R","?raw=T")
devtools::source_url(link_source)


# directori.arrel<-c("C:/Users/Jordi/Google Drive", 
#                    "C:/Users/usuari/Google Drive",
#                    "C:/Users/43728088M/Google Drive",
#                    "C:/Users/jreal/Google Drive",
#                    "D:/Google Drive",
#                    "G:/Google Drive",
#                    "E:/Google Drive")

# library(dplyr)
# directori.arrel[file.exists(directori.arrel)] %>% 
#   file.path("Stat_codis/funcions_propies.R") %>% 
#   source()

# #  DIRECTORI DE TREBALL              #
# #  setwd en directori de treball 
# 
# "CIBERDEM/GEDAPS/METPLUS/SIDIAP" %>% 
#   directori_treball(directori.arrel)


# Llista de fitxers :

# 0. Inicialització de parametres           -----------------------------

# N test mostra a seleccionar  (Nmostra=Inf)

# Nmostra=Inf  # Seria tota la mostra
Nmostra=Inf

# Conductor cataleg 
fitxer_cataleg<-"cataleg_met.xls"

# fitxersortida
fitxersortida<-here::here("dades/preparades","BD_METPLUS_V4.rds")

# 1. Lectura de Fitxers  --------------------------

# llistadefitxers<- list.files(pattern = ".rds") 


# "METPLUS_entregable_poblacio_20181126_190346.rds"
# 
# "METPLUS_entregable_diagnostics_20181126_190346.rds"
# "METPLUS_entregable_cmbdh_diagnostics_20181126_190346.rds"
# "METPLUS_entregable_cmbdh_procediments_20181126_190346.rds"
# "METPLUS_entregable_reaccions_adverses_20181126_190346.rds"
# 
# "METPLUS_entregable_farmacs_facturats_20181126_190346.rds"
# "METPLUS_entregable_farmacs_prescrits_20181126_190346.rds"
# 
# "METPLUS_entregable_tabaquisme_20181126_190346.rds"
# "METPLUS_entregable_variables_cliniques_20181128_175605.rds"
# "METPLUS_entregable_variables_analitiques_20181126_190346.rds"
# 
# "METPLUS_entregable_cataleg_20181128_175605.rds"

# CATALEG<-readRDS("METPLUS_entregable_cataleg_20181128_175605.rds")

# CATALEG<-readRDS("METPLUS_entregable_cataleg_20181128_175605.rds")

### Arreglar Cataleg: Substituir espais en blanc
# CATALEG<-CATALEG %>% mutate(agr= stringr::str_replace(agr," ","")) %>% as.data.table()
### Escriure el Cataleg
# write.xlsx(CATALEG, "cataleg.xlsx")


conductor_variables<-read_excel(fitxer_cataleg,col_types = "text")


LLEGIR.PACIENTS<-function(n=Nmostra) {
  readRDS("dades/SIDIAP" %>% here::here("METPLUS_entregable_poblacio_20181126_190346.rds")) %>% as_tibble() %>% head(n)}

LLEGIR.PROBLEMES<-function(n=Nmostra) {
  readRDS("dades/SIDIAP" %>% here::here("METPLUS_entregable_diagnostics_20181126_190346.rds"))%>% as_tibble() %>% head(n)}

LLEGIR.CMBDH<-function(n=Nmostra) {
  readRDS("dades/SIDIAP" %>% here::here("METPLUS_entregable_cmbdh_diagnostics_20181126_190346.rds"))%>% as_tibble() %>% head(n)}

LLEGIR.PROC<-function(n=Nmostra) {
  readRDS("dades/SIDIAP" %>% here::here("METPLUS_entregable_cmbdh_procediments_20181126_190346.rds"))%>% as_tibble() %>% head(n)}

LLEGIR.REACCIONS_ADV<-function(n=Nmostra) {
  readRDS("dades/SIDIAP" %>% here::here("METPLUS_entregable_reaccions_adverses_20181126_190346.rds"))%>% as_tibble() %>% head(n) }

LLEGIR.TABAC<-function(n=Nmostra) {
  readRDS("dades/SIDIAP" %>% here::here("METPLUS_entregable_tabaquisme_20181126_190346.rds"))%>% as_tibble() %>% head(n) }

LLEGIR.FX.FACTURATS<-function(n=Nmostra) {
  readRDS("dades/SIDIAP" %>% here::here("METPLUS_entregable_farmacs_facturats_20181126_190346.rds"))%>% as_tibble() %>% head(n) }

LLEGIR.FX.PRESCRITS<-function(n=Nmostra) {
  readRDS("dades/SIDIAP" %>% here::here("METPLUS_entregable_farmacs_prescrits_20181126_190346.rds"))%>% as_tibble() %>% head(n) }

LLEGIR.VARIABLES<-function(n=Nmostra) {
  readRDS("dades/SIDIAP" %>% here::here("METPLUS_entregable_variables_cliniques_20181128_175605.rds"))%>% as_tibble() %>% head(n) }

LLEGIR.CLINIQUES<-function(n=Nmostra) {
  readRDS("dades/SIDIAP" %>% here::here("METPLUS_entregable_variables_analitiques_20181126_190346.rds"))%>% as_tibble() %>% head(n) }

LLEGIR.PROB_EXTRA<-function(n=Nmostra) {
  fread("dades/SIDIAP" %>% here::here("METPLUS_entregable_diagnostics_extra_20190607_174921.txt"))%>% as_tibble() %>% head(n) }


# 1.1.Lectura de PACIENTS   --------------


dt_pacients<-Inf %>% LLEGIR.PACIENTS()


# 1.2.Lectura de PROBLEMES DE SALUT -------------------
PROBLEMES<-Nmostra %>% LLEGIR.PROBLEMES()
CMBDH<-Nmostra %>% LLEGIR.CMBDH()
CMBDH.PROC<-Nmostra %>% LLEGIR.PROC()
PROB_EXTRA<-Nmostra %>% LLEGIR.PROB_EXTRA()

PROBLEMES<-PROBLEMES %>% select(idp,cod,dat) 
CMBDH<-CMBDH %>% select(idp,cod,dat) 
CMBDH.PROC<-CMBDH.PROC %>% select(idp,cod,dat) %>% mutate(cod=as.character(cod))
PROB_EXTRA<-PROB_EXTRA %>% select(idp,cod,dat)


### Juntar totes les bdades de problemes de salut 
PROBLEMES_total<-
  PROBLEMES %>% 
  rbind(CMBDH) %>% 
  rbind(CMBDH.PROC) %>% 
  rbind(PROB_EXTRA)

rm(PROBLEMES)
rm(CMBDH)
rm(CMBDH.PROC)
rm(PROB_EXTRA)


# 2. Generació de data index         ---------------------------

# 2.1. Llegeixo facturacions / prescripcions  --------------

FX.FACTURATS<-Nmostra %>% LLEGIR.FX.FACTURATS()
FX.PRESCRITS<-Nmostra %>% LLEGIR.FX.PRESCRITS()

# 2.3. Fusionar ambdues fonts de dades (Prescripcions + facturacions) I formatejar  ----------------

# Formatejar fitxers FACTURACIO + PRESCRIPCIONS (fusionar)
FX.PRESCRITS<-
  FX.PRESCRITS %>%
  select(idp, cod, dat, dbaixa)%>%
  mutate(env= ceiling(lubridate::time_length(lubridate::interval(lubridate::ymd(dat), lubridate::ymd(dbaixa)) ,"month")) )%>% 
  mutate(dat=as.integer(substr(dat,1,6))) %>% 
  select(idp,cod,dat,env)

# Fusionar fitxers
FX.FACTURATS_PRESCRITS<-
  FX.FACTURATS %>% 
  select(idp,cod,env,dat) %>%
  rbind(FX.PRESCRITS)

# Alliberar memoria 
gc()
# rm(FX.FACTURATS,FX.PRESCRITS)
  
# 2.4.- Agregar: Generar data de facturació/prescripció dels farmacs d'estudi (IDPP4/ISGLT2/SU) ---------------

# Seleccionar la primera data de prescripció per cada tipus de farmac #
# Agregar per agafar primera facturació / dispensació per grup
farmacs_grups<-agregar_facturacio(dt=FX.FACTURATS_PRESCRITS,
                                  finestra.dies = c(-Inf,+Inf),camp_agregador="GRUP",dt.agregadors = conductor_variables,bd.dindex = "20171231",prefix="FD.",agregar_data=T)


# Genero data index per farmac  
# 2.5. Assigno a grup aquella data mínima de cada farmac ---------------
# Genero grups 

# load("metplus_test.Rdata")
# save.image("metplus_test.Rdata")

farmacs_grups<-
  farmacs_grups %>% 
  group_by(idp) %>% 
  mutate (data_index=min(c(FD.IDPP4,FD.iSGLT2,FD.SU),na.rm = T)) %>% 
  ungroup(idp) %>% 
  mutate(grup=case_when(data_index==FD.IDPP4~"IDPP4",
                   data_index==FD.iSGLT2~"ISGLT2",
                   data_index==FD.SU~"SU"
                   ))

dt_grups<-farmacs_grups %>% select(idp,dtindex=data_index,FD.IDPP4,FD.iSGLT2,FD.SU,grup)
dt_index<-dt_grups %>% select(idp,dtindex)


# 3. Agregació de variables en data index  -------------------

# 3.1.Agrego en data index MET (Any previ) ----------------------

dt_fx.met<-agregar_facturacio(dt=FX.FACTURATS_PRESCRITS,finestra.dies=c(-365,0),dt.agregadors=conductor_variables,bd.dindex=dt_index,prefix="F.",camp_agregador="MET", agregar_data=F)

# 3.2.Agrego en data index altres farmacs (Any previ) ---------------------

dt_fx.AD<-agregar_facturacio(dt=FX.FACTURATS_PRESCRITS,finestra.dies=c(-365,0),dt.agregadors=conductor_variables,bd.dindex=dt_index,prefix="F.",camp_agregador="agr", agregar_data=F)

# 3.3. Canvis de tractament grup d'estudi POST    ------------------- 

dt_fx.canvis<-agregar_facturacio(dt=FX.FACTURATS,finestra.dies=c(+1,+730),dt.agregadors=conductor_variables,bd.dindex=dt_index,prefix="CANVITX.",camp_agregador="GRUP", agregar_data=T)

# 3.4. Agregació de Efectes adversos (tractaments) POST 
REACCIONS_ADV<-Nmostra %>% LLEGIR.REACCIONS_ADV() %>%  mutate(dbaixa="20191212")

dt_fx_Radversos<-agregar_prescripcions(dt=REACCIONS_ADV,bd.dindex=dt_index,dt.agregadors = conductor_variables,finestra.dies = c(1,+730),prefix="RADV.",camp_agregador="REAC_ADV_FARMACO",agregar_data=T)

# 3.5. Agregació de problemes basals I   -----------------
dt_problemes<-agregar_problemes(dt=PROBLEMES_total,bd.dindex=dt_index,dt.agregadors=conductor_variables,finestra.dies=c(-Inf,0),prefix="DG.",camp_agregador="agr") 

# 3.6. Agregació de problemes basals II  -----------------
dt_problemes_NIV2<-agregar_problemes(dt=PROBLEMES_total,bd.dindex=dt_index,dt.agregadors=conductor_variables,finestra.dies=c(-Inf,0),prefix="DG.",camp_agregador="agr2") 

# 3.7. Agregació de events post (+24 MESOS)  -----------------
dt_events<-agregar_problemes(dt=PROBLEMES_total,bd.dindex=dt_index,dt.agregadors=conductor_variables,finestra.dies=c(1,730),prefix="EV.",camp_agregador="EVENT_DG") 

rm(PROBLEMES_total)
# 3.8.Lectura + Agregació de variables basals  ---------------------
VARIABLES<-Nmostra %>% LLEGIR.VARIABLES() %>% select(idp,cod=agr,dat,val)
dt_variables<-agregar_analitiques(dt=VARIABLES,bd.dindex=dt_index,finestra.dies=c(-365,0)) 

# 3.9.Agregació de variables seguiment (3-24 mesos+-: 90-730 dies)  ---------------------
dt_variables324m<-agregar_analitiques(dt=VARIABLES,bd.dindex=dt_index,finestra.dies=c(90,730),sufix = c(".valor324m",".dies324m"))

# 3.10.Agregació de variables seguiment (24 mesos+-3m: 639-821 dies)  ---------------------
dt_variables24m<-agregar_analitiques(dt=VARIABLES,bd.dindex=dt_index,finestra.dies=c(639,821),sufix = c(".valor24m",".dies24m")) 

# 3.11.Agregació de variables seguiment (12 mesos+-3m: 274-456 dies)  ---------------------
dt_variables12m<-agregar_analitiques(dt=VARIABLES,bd.dindex=dt_index,finestra.dies=c(274,456),sufix = c(".valor12m",".dies12m")) 

# 3.12.Agregació de variables seguiment (6 mesos+-3m: 90-273 dies)  ---------------------
dt_variables6m<-agregar_analitiques(dt=VARIABLES,bd.dindex=dt_index,finestra.dies=c(90,273),sufix = c(".valor6m",".dies6m")) 

rm(VARIABLES)

# 3.13.Lectura i agregació de clíniques basals  ---------------------
CLINIQUES<-Nmostra %>% LLEGIR.CLINIQUES() %>% select(idp,cod=agr,dat,val)
dt_cliniques<-agregar_analitiques(dt=CLINIQUES,bd.dindex=dt_index,finestra.dies=c(-365,0)) 

# 3.14.Agregació de clíniques seguiment (3-24 mesos+-: 90-730 dies)  ---------------------
dt_cliniques324m<-agregar_analitiques(dt=CLINIQUES,bd.dindex=dt_index,finestra.dies=c(90,730),sufix = c(".valor324m",".dies324m")) 

# 3.15.Agregació de clíniques seguiment (24 mesos+-3m: 639-821 dies)  ---------------------
dt_cliniques24m<-agregar_analitiques(dt=CLINIQUES,bd.dindex=dt_index,finestra.dies=c(639,821),sufix = c(".valor24m",".dies24m")) 

# 3.16.Agregació de clíniques seguiment (12 mesos+-3m: 274-456 dies)  ---------------------
dt_cliniques12m<-agregar_analitiques(dt=CLINIQUES,bd.dindex=dt_index,finestra.dies=c(274,456),sufix = c(".valor12m",".dies12m")) 

# 3.17.Agregació de clíniques seguiment (6 mesos+-3m: 90-273 dies)  ---------------------
dt_cliniques6m<-agregar_analitiques(dt=CLINIQUES,bd.dindex=dt_index,finestra.dies=c(90,273),sufix = c(".valor6m",".dies6m")) 
rm(CLINIQUES)

# 3.19.Agregació de tabac  ---------------------
TABAC<-Nmostra %>% LLEGIR.TABAC %>% mutate(cod="tab")
dt_tabac<-agregar_analitiques(dt=TABAC,bd.dindex =dt_index,finestra.dies=c(-Inf,0))
rm(TABAC)


dt_grups<-dt_grups %>% left_join(dt_pacients,by="idp")


# 4. Formatar data--> numeric tots els camps dtindex pre fusió ------------------
formatar_dtindex<- function(dades) dades<-dades %>% mutate(dtindex=as.numeric(dtindex))

dt_grups<-formatar_dtindex(dt_grups)
dt_fx.met<-formatar_dtindex(dt_fx.met)
dt_fx.AD<-formatar_dtindex(dt_fx.AD)
dt_fx.canvis<-formatar_dtindex(dt_fx.canvis)

dt_problemes<-formatar_dtindex(dt_problemes)
dt_problemes_NIV2<-formatar_dtindex(dt_problemes_NIV2)

dt_variables<-formatar_dtindex(dt_variables)
dt_cliniques<-formatar_dtindex(dt_cliniques)
dt_tabac<-formatar_dtindex(dt_tabac)

dt_cliniques6m<-formatar_dtindex(dt_cliniques6m)
dt_cliniques12m<-formatar_dtindex(dt_cliniques12m)
dt_cliniques24m<-formatar_dtindex(dt_cliniques24m)
dt_cliniques324m<-formatar_dtindex(dt_cliniques324m)

dt_variables6m<-formatar_dtindex(dt_variables6m)
dt_variables12m<-formatar_dtindex(dt_variables12m)
dt_variables24m<-formatar_dtindex(dt_variables24m)
dt_variables324m<-formatar_dtindex(dt_variables324m)

dt_events<-formatar_dtindex(dt_events)
dt_fx_Radversos<-formatar_dtindex(dt_fx_Radversos)

# 5.Fusió total --------------------- 

BDTOTAL<-dt_grups %>% 
  left_join(dt_fx.met, by=c("idp","dtindex")) %>% 
  left_join(dt_fx.AD, by=c("idp","dtindex")) %>% 
  left_join(dt_fx.canvis, by=c("idp","dtindex")) %>% 
  left_join(dt_problemes, by=c("idp","dtindex")) %>%
  left_join(dt_problemes_NIV2, by=c("idp","dtindex")) %>% 
  left_join(dt_variables, by=c("idp","dtindex")) %>% 
  left_join(dt_cliniques, by=c("idp","dtindex")) %>% 
  left_join(dt_tabac, by=c("idp","dtindex")) %>% 
  left_join(dt_cliniques6m, by=c("idp","dtindex")) %>% 
  left_join(dt_cliniques12m, by=c("idp","dtindex")) %>% 
  left_join(dt_cliniques24m, by=c("idp","dtindex")) %>% 
  left_join(dt_cliniques324m, by=c("idp","dtindex")) %>% 
  left_join(dt_variables6m, by=c("idp","dtindex")) %>% 
  left_join(dt_variables12m, by=c("idp","dtindex")) %>% 
  left_join(dt_variables24m, by=c("idp","dtindex")) %>% 
  left_join(dt_variables324m, by=c("idp","dtindex")) %>%  
  left_join(dt_events, by=c("idp","dtindex")) %>% 
  left_join(dt_fx_Radversos, by=c("idp","dtindex"))


write.csv(names(BDTOTAL),file="variables.csv")

saveRDS(BDTOTAL,fitxersortida)


##                  Fi d'analisis         ------------------------------------

