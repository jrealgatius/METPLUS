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

# Funcions 
link_source<-paste0("https://github.com/jrealgatius/Stat_codis/blob/master/funcions_propies.R","?raw=T")
devtools::source_url(link_source)

# Llista de fitxers :
# 0. Inicialització de parametres           -----------------------------
# N test mostra a seleccionar  (Nmostra=Inf)

# Nmostra=Inf  # Seria tota la mostra
Nmostra=100000

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

# Copia de prescripcions dels grups d¡'estudi
FX.PRESCRITS_GRUPS<-FX.PRESCRITS %>% 
  dplyr::semi_join(conductor_variables %>% filter(GRUP=="IDPP4" | GRUP=="iSGLT2" | GRUP=="SU"),by="cod") %>% 
  dplyr::distinct(idp,cod,dat,dbaixa) 

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


# 2.6. Generar data de final de tractament iniciat (havent eliminat discontinuitats de N dies (gap_dies=60))


# 2.6.1. Tenint en compte facturació + dispensació 

# Formatar Fitxer amb: dataindex (date), idp, grup(farmac), dat(date),datafi(date)

FX.FACTURATS_PRESCRITS_GRUPS<-
  dt_index %>% 
  left_join(FX.FACTURATS_PRESCRITS,by="idp") %>% 
  dplyr::semi_join(conductor_variables %>% filter(GRUP=="IDPP4" | GRUP=="iSGLT2" | GRUP=="SU"),by="cod") %>% 
  dplyr::left_join(select(conductor_variables,cod,GRUP),by="cod") %>% 
  mutate(dat=lubridate::ymd(paste0(as.character(dat),"15")),datafi=dat+(30*env))

# Eliminar gaps i solapaments per grups agregar_solapaments_gaps()
gap_dies<-160
# farmacs_dt_sense_gaps<-agregar_solapaments_gaps(dt=FX.FACTURATS_PRESCRITS_GRUPS,id="idp",datainici = "dat",datafinal="datafi",gap=gap_dies)

farmacs_dt_sense_gaps<-FX.FACTURATS_PRESCRITS_GRUPS %>% 
  split(.$GRUP) %>% 
  map(~agregar_solapaments_gaps(dt=.x,id="idp",datainici = "dat",datafinal="datafi",gap=gap_dies)) %>% 
  map_df(~bind_rows(.),.id="GRUP")

# Verificar solapamenta 
set.seed(125)
MAP_ggplot_univariant(farmacs_dt_sense_gaps %>% filter(GRUP=="IDPP4"),datainicial = "dat",datafinal = "datafi",id="idp",Nmostra = 10)

# Capturar primera datafi per pacient i grup serà el primer stop d'aquell tractament 
farmacs_STOP<-
  farmacs_dt_sense_gaps %>% 
  group_by(idp,GRUP) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(idp,GRUP,datafi) %>% 
  spread(GRUP,datafi)



# 2.6.2. Tenint en compte només facturació

# 3. Agregació de variables en data index  -------------------
# 3.1.Agrego en data index MET (Any previ) ----------------------
dt_fx.met<-agregar_facturacio(dt=FX.FACTURATS_PRESCRITS,finestra.dies=c(-365,0),dt.agregadors=conductor_variables,bd.dindex=dt_index,prefix="F.",camp_agregador="MET", agregar_data=F)

# 3.2.Agrego en data index altres farmacs (Any previ) ---------------------
dt_fx.AD<-agregar_facturacio(dt=FX.FACTURATS_PRESCRITS,finestra.dies=c(-365,0),dt.agregadors=conductor_variables,bd.dindex=dt_index,prefix="F.",camp_agregador="agr", agregar_data=F)

# 3.3. Canvis de tractament grup d'estudi POST  (1-24 mesos)  ------------------- 
dt_fx.canvis<-agregar_facturacio(dt=FX.FACTURATS,finestra.dies=c(+1,+730),dt.agregadors=conductor_variables,bd.dindex=dt_index,prefix="CANVITX.",camp_agregador="GRUP", agregar_data=T)

# 3.4. Agregació de nombre d'envasos facturats POST    ------------------- 
dt_fx.Nenvas<-agregar_facturacio(dt=FX.FACTURATS,finestra.dies=c(0,+730),dt.agregadors=conductor_variables,bd.dindex=dt_index,prefix="NenvasTX.",camp_agregador="GRUP", agregar_data=F)

# 3.5. Agregació de temps en dies de prescripcio POST (+24 mesos)    ------------------- 
dt_fx.temps<-agregar_prescripcions(dt=FX.PRESCRITS_GRUPS,finestra.dies=c(0,+730),dt.agregadors=conductor_variables,bd.dindex=dt_index,prefix="tempsTX.",camp_agregador="GRUP", agregar_data=F)

# 3.6. Agregació de Efectes adversos (tractaments) POST 
REACCIONS_ADV<-Nmostra %>% LLEGIR.REACCIONS_ADV() %>%  mutate(dbaixa="20191212")

dt_fx_Radversos<-agregar_prescripcions(dt=REACCIONS_ADV,bd.dindex=dt_index,dt.agregadors = conductor_variables,finestra.dies = c(1,+730),prefix="RADV.",camp_agregador="REAC_ADV_FARMACO",agregar_data=T)

# 3.7. Agregació de problemes basals I   -----------------
dt_problemes<-agregar_problemes(dt=PROBLEMES_total,bd.dindex=dt_index,dt.agregadors=conductor_variables,finestra.dies=c(-Inf,0),prefix="DG.",camp_agregador="agr") 

# 3.8. Agregació de problemes basals II  -----------------
dt_problemes_NIV2<-agregar_problemes(dt=PROBLEMES_total,bd.dindex=dt_index,dt.agregadors=conductor_variables,finestra.dies=c(-Inf,0),prefix="DG.",camp_agregador="agr2") 

# 3.9. Agregació de events post (+24 MESOS)  -----------------
dt_events<-agregar_problemes(dt=PROBLEMES_total,bd.dindex=dt_index,dt.agregadors=conductor_variables,finestra.dies=c(1,730),prefix="EV.",camp_agregador="EVENT_DG") 

rm(PROBLEMES_total)
# 3.10.Lectura + Agregació de variables basals  ---------------------
VARIABLES<-Nmostra %>% LLEGIR.VARIABLES() %>% select(idp,cod=agr,dat,val)
dt_variables<-agregar_analitiques(dt=VARIABLES,bd.dindex=dt_index,finestra.dies=c(-365,0)) 

# 3.11.Agregació de variables seguiment (3-24 mesos+-: 90-730 dies)  ---------------------
dt_variables324m<-agregar_analitiques(dt=VARIABLES,bd.dindex=dt_index,finestra.dies=c(90,730),sufix = c(".valor324m",".dies324m"))

# 3.12.Agregació de variables seguiment (24 mesos+-3m: 639-821 dies)  ---------------------
dt_variables24m<-agregar_analitiques(dt=VARIABLES,bd.dindex=dt_index,finestra.dies=c(639,821),sufix = c(".valor24m",".dies24m")) 

# 3.13.Agregació de variables seguiment (12 mesos+-3m: 274-456 dies)  ---------------------
dt_variables12m<-agregar_analitiques(dt=VARIABLES,bd.dindex=dt_index,finestra.dies=c(274,456),sufix = c(".valor12m",".dies12m")) 

# 3.14.Agregació de variables seguiment (6 mesos+-3m: 90-273 dies)  ---------------------
dt_variables6m<-agregar_analitiques(dt=VARIABLES,bd.dindex=dt_index,finestra.dies=c(90,273),sufix = c(".valor6m",".dies6m")) 

rm(VARIABLES)

# 3.15.Lectura i agregació de clíniques basals  ---------------------
CLINIQUES<-Nmostra %>% LLEGIR.CLINIQUES() %>% select(idp,cod=agr,dat,val)
dt_cliniques<-agregar_analitiques(dt=CLINIQUES,bd.dindex=dt_index,finestra.dies=c(-365,0)) 

# 3.16.Agregació de clíniques seguiment (3-24 mesos+-: 90-730 dies)  ---------------------
dt_cliniques324m<-agregar_analitiques(dt=CLINIQUES,bd.dindex=dt_index,finestra.dies=c(90,730),sufix = c(".valor324m",".dies324m")) 

# 3.17.Agregació de clíniques seguiment (24 mesos+-3m: 639-821 dies)  ---------------------
dt_cliniques24m<-agregar_analitiques(dt=CLINIQUES,bd.dindex=dt_index,finestra.dies=c(639,821),sufix = c(".valor24m",".dies24m")) 

# 3.18.Agregació de clíniques seguiment (12 mesos+-3m: 274-456 dies)  ---------------------
dt_cliniques12m<-agregar_analitiques(dt=CLINIQUES,bd.dindex=dt_index,finestra.dies=c(274,456),sufix = c(".valor12m",".dies12m")) 

# 3.19.Agregació de clíniques seguiment (6 mesos+-3m: 90-273 dies)  ---------------------
dt_cliniques6m<-agregar_analitiques(dt=CLINIQUES,bd.dindex=dt_index,finestra.dies=c(90,273),sufix = c(".valor6m",".dies6m")) 
rm(CLINIQUES)

# 3.20.Agregació de tabac  ---------------------
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
dt_fx.Nenvas<-formatar_dtindex(dt_fx.Nenvas)
dt_fx.temps<-formatar_dtindex(dt_fx.temps)

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
  left_join(dt_fx.Nenvas, by=c("idp","dtindex")) %>% 
  left_join(dt_fx.temps, by=c("idp","dtindex")) %>% 
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


write.csv2(names(BDTOTAL),file="variables.csv")

saveRDS(BDTOTAL,fitxersortida)


##                  Fi d'analisis         ------------------------------------

