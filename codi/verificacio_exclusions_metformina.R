
# 1. Inicialització de parametres           -----------------------------

rm(list=ls())
gc()

#   SOURCE
# CArrego funcions -------------------
link_source<-paste0("https://github.com/jrealgatius/Stat_codis/blob/master/funcions_propies.R","?raw=T")
devtools::source_url(link_source)

# Parametre discontinuitat/stop tractament:
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


# llegir dades 
conductor_variables<-read_excel(fitxer_cataleg,col_types = "text")

LLEGIR.FX.FACTURATS<-function(n=Nmostra) {
  readRDS("dades/SIDIAP" %>% here::here("METPLUS_entregable_farmacs_facturats_20181126_190346.rds"))%>% as_tibble() %>% head(n) }

LLEGIR.FX.PRESCRITS<-function(n=Nmostra) {
  readRDS("dades/SIDIAP" %>% here::here("METPLUS_entregable_farmacs_prescrits_20181126_190346.rds"))%>% as_tibble() %>% head(n) }



# 2.1. Llegeixo facturacions / prescripcions  --------------

FX.FACTURATS<-Nmostra %>% LLEGIR.FX.FACTURATS()
FX.PRESCRITS<-Nmostra %>% LLEGIR.FX.PRESCRITS() %>% filter(dat!=dbaixa) # elimino els que tenen 0 dies de prescripció


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

# 0. Lectura de dades          -----------------------------

# fitxersortida
fitxer_entrada<-fitxersortida

# Lectura dades
dades<-readRDS(fitxer_entrada)  

#   dtindex ---> data --------------

dades<-dades %>% mutate(dtindex=as_date(dtindex))


#   Recode farmacs basals ---------

# NA --> 0 (Else=1) (No hi ha 0)
dades<-dades %>% mutate_at(vars(starts_with("F.")), 
                           funs(ifelse(is.na(.) | 0,0,1))) 
#   Inclusió 1 (METformina)  -------
dades<-dades %>% mutate(inclusio_met=ifelse(F.MET | F.METF,1,0))


# Filtre dades met  ----------------
dades_NMET<-dades %>% filter(inclusio_met==0)
ids_nomet<-dades_NMET %>% select(idp,dtindex,grup)
ids_nomet<-ids_nomet %>% mutate(datainicial=dtindex,datafinal=dtindex)

# Selecciono conductor met   ---------------
conductor_variables_met<-conductor_variables %>% filter(MET=="MET") %>% select(cod,MET,agr) %>% unique()

# Filtro HISTORIC DE METFORMINA de PACIENTS NO MET
MET.FACTURATS_PRESCRITS<-FX.FACTURATS_PRESCRITS %>% semi_join(conductor_variables_met) 
MET.FACTURATS_PRESCRITS<-MET.FACTURATS_PRESCRITS %>% semi_join(ids_nomet,by="idp")

# Formatejo dates ----------
MET.FACTURATS_PRESCRITS<-
  MET.FACTURATS_PRESCRITS %>% mutate(datainicial=paste0(as.character(dat),"15") %>% as.Date('%Y%m%d'),
                                 datafinal=datainicial+(env*30))

# Formatejo grup met i hi fusiono id_nomet
MET.FACTURATS_PRESCRITS<-MET.FACTURATS_PRESCRITS %>% mutate(grup="MET") 
# MET.FACTURATS_PRESCRITS_fusio<-MET.FACTURATS_PRESCRITS %>% bind_rows(ids_nomet)
MET.FACTURATS_PRESCRITS<-MET.FACTURATS_PRESCRITS %>% left_join(select(ids_nomet,idp,dtindex,grup_dinici=grup),by="idp")

# Plotejo mostra random -------------------
set.seed(10)
mostra_dt<-mostreig_ids(MET.FACTURATS_PRESCRITS,n_mostra=10)
MAP_ggplot(dades=mostra_dt,datainicial="datainicial",datafinal="datafinal",id="idp",grup_color="grup",add_point = "dtindex")

MAP_ggplot_univariant(dades=MET.FACTURATS_PRESCRITS,datainicial="datainicial",datafinal="datafinal",id="idp",Nmostra = 3,add_point = "dtindex")






