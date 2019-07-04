#      FASE: Preparació      ---------------------
#
#
# 0. Directori de treball / càrrega de funcions    --------------
#
# Lectura de dades     

memory.size(max=160685)
#
#   SOURCE

rm(list=ls())


link_source<-paste0("https://github.com/jrealgatius/Stat_codis/blob/master/funcions_propies.R","?raw=T")
devtools::source_url(link_source)


# 
# ###
# directori.arrel<-c("C:/Users/Jordi/Google Drive", 
#                    "C:/Users/usuari/Google Drive",
#                    "C:/Users/43728088M/Google Drive",
#                    "C:/Users/jreal/Google Drive",
#                    "D:/Google Drive",
#                    "G:/Google Drive",
#                    "E:/Google Drive")
# 
# ##  OJO! NO SOURCE ANTIC
# 
# library(dplyr)
# directori.arrel[file.exists(directori.arrel)] %>% 
#   file.path("Stat_codis/funcions_propies.R") %>% 
#   source()

#  DIRECTORI DE TREBALL              #
#  setwd en directori de treball 

# "CIBERDEM/GEDAPS/METPLUS/SIDIAP" %>% 
#   directori_treball(directori.arrel)

# 0. Inicialització de parametres           -----------------------------

# N test mostra a seleccionar  (Nmostra=Inf)

# Nmostra=Inf  # Seria tota la mostra
Nmostra=Inf

# Conductor cataleg 
fitxer_cataleg<-here::here("dades","cataleg_met.xls")

# Conductor variables
conductor_variables<-here::here("dades","variables_metplus.xls")
# fitxersortida
fitxer_entrada<-here::here("dades/preparades","BD_METPLUS_v2.rds")

# Obrir dades 
dades<-readRDS(fitxer_entrada)  


# 1. Calculs i recodes de variables   ----------------------

library(lubridate)

# dtindex ---> data --------------

dades<-dades %>% mutate(dtindex=as_date(dtindex))

# Recode farmacs basals ---------

# NA --> 0 (Else=1) (No hi ha 0)
dades<-dades %>% mutate_at(vars(starts_with("F.")), 
                           funs(ifelse(is.na(.) | 0,0,1))) 

# Anys desde diabetis ------------

dades<-dades %>% 
  mutate(anys_DM = year(as.period(interval(start = ymd(DG.CI), end = dtindex))))

# Edat ------------
dades<-dades %>% 
  mutate(edat = year(as.period(interval(start = ymd(dnaix), end = dtindex))))

# Any / quarter data index  ------------------
dades<- dades %>% 
  mutate (any_dtindex=year(dtindex),
          Q_dtindex=quarter(dtindex,with_year = T)) 


# 1.1. Criteris d'inclusió  --------------

# 1.1 Metformina

# Inclusió 1 (METformina)  -------

dades<-dades %>% mutate(inclusio_met=ifelse(F.MET | F.METF,1,0))


# Inclusió 2 : Insuficient control glycemic (HbA1c>=7) ----------

dades<-dades %>% mutate(inclusio_HB7=case_when(HBA1C.valor>=7~"1",
                                    HBA1C.valor<7~"0",TRUE~"NA"))

dades<-recodificar(dades,taulavariables = conductor_variables,criteris = "recode")

# Inclusió 3: Edad>18 anys ---------------

dades<-dades %>% mutate (inclusio_edat18=ifelse(edat>=18,1,0))


# Diagnostics basals (Recode)
# NA --> 0 (Else=1) (No hi ha 0)
dades<-dades %>% mutate_at(vars(starts_with("DG.")), 
                           funs(if_else(is.na(.),0,1))) 


# Aplicar criteris d'inclusió  ------------------------

# Faig copia per despres fer flow-chart amb totes les dades 
dadesinicials<-dades

dades<-dades %>% filter(inclusio_met==1 & inclusio_HB7==1 & inclusio_edat18==1)


# Recodificar edat
dades<-recodificar(dades,taulavariables = conductor_variables,criteris = "recode")


# Falta generació de quartils Pes
dades<-dades %>% 
  mutate(PES.CAT.Q4=Hmisc::cut2(PESO.valor,g=4),
         PES.CAT.Q4=ifelse(is.na(PES.CAT.Q4),"NA",PES.CAT.Q4))

# Matching per 3 grups  IDPP4,ISGLT2 ,SU  --------------------------

# Inicialització de paràmetres -----------
caliper<-0.05
set.seed(111)

formula<-formula_compare(x="match",y="grup",taulavariables = conductor_variables)
taula1<-descrTable(formula,data=dades,show.all = T,show.n = T)

taula1

# Selecciono dades per matching 
dadesmatching<-selectorvariables(taula="dadesmatch",taulavariables = conductor_variables,dades)


# Matching a subset 1 (ISGLT2 vs IDPP4)
dades_sub1<-dadesmatching %>% filter(grup=="IDPP4" | grup=="ISGLT2")
dades_sub1<-dades_sub1 %>% mutate(grup_dic=ifelse(grup=="ISGLT2",1,0))

# Aplicar matching A subset     #
formulaPS<-formula_compare(x="match",y="grup_dic",taulavariables = conductor_variables)
exact<-extreure.variables("match_exact",taulavariables = conductor_variables)
m.out<-matchit(formulaPS,method="nearest",data=as.data.frame(dades_sub1),caliper=caliper,ratio=1,exact=exact)

summary(m.out)
#  afegeixo a dades_H la variable PS 
dades_sub1<-data.table(dades_sub1,ps1=m.out$weights) %>% as_tibble

# Fusiono en dadesmatching la variable ps indicadora
dadesmatching<-dadesmatching %>% left_join(
  select(dades_sub1,idp,ps1),by="idp") 

# Ara fer matching 2 --- Selecciono subset + 3 grup (SU)
dadesmatching<-dadesmatching %>% filter(ps1==1 | grup=="SU")

formula<-formula_compare(x="match",y="grup",taulavariables = conductor_variables)

taula2<-descrTable(formula,data=dadesmatching,show.all = T,show.n = T)

taula2

# Matching a subset 1 (ISGLT2+vs SU)
dades_sub1<-dadesmatching %>% select(-ps1) %>% filter(grup=="SU" | grup=="ISGLT2")
dades_sub1<-dades_sub1 %>% mutate(grup_dic=ifelse(grup=="ISGLT2",1,0))

# Aplicar matching A subset     #
formulaPS<-formula_compare(x="match",y="grup_dic",taulavariables = conductor_variables)
exact<-extreure.variables("match_exact",taulavariables = conductor_variables)

m.out<-matchit(formulaPS,method="nearest",data=as.data.frame(dades_sub1),caliper=caliper,ratio=1,exact=exact)

# m.out<-matchit(formulaPS,method="nearest",data=dades_sub1,caliper=caliper,ratio=1)

# Afegeixo a dades_H la variable PS 
dades_sub1<-data.table(dades_sub1,ps2=m.out$weights) %>% as_tibble
#
# Fusiono en dadesmatching la variable ps indicadora

dadesmatching<-dadesmatching %>% left_join(
  select(dades_sub1,idp,ps2),by="idp")

dadesmatching<-dadesmatching %>% mutate(ps=case_when(ps1==1~1,
                                                     ps2==1~1,
                                                     TRUE~0))


temp=dadesmatching %>% filter(ps==1)

formula<-formula_compare(x="match",y="grup",taulavariables = conductor_variables)

taulaPS<-descrTable(formula,data=temp,show.all = T,show.n = T)
taulaPS


# Afegir PS indicadora  -------------------
dades<-dades %>%  left_join(select(dadesmatching,c(idp,ps)),by="idp")

dadesinicials<-dadesinicials %>%  left_join(select(dadesmatching,c(idp,ps)),by="idp") 


# Fi matching 

rm(dades_sub1,dadesmatching,taula2)


# Generar flowcharts des dadesinicials --------------

flow_global<-criteris_exclusio_diagrama(dadesinicials,taulavariables = conductor_variables,criteris = "exclusio",
                                        etiquetes="exc_lab",pob_lab = c("Population","N Final"),grups="grup",ordre = "exc_ordre")
flow_global

# Recode ps a 0 
dades<-dades %>% mutate(ps=ifelse(is.na(ps),0,ps))

flow_global2<-criteris_exclusio_diagrama(dades,taulavariables = conductor_variables,criteris = "excl2",
                                        etiquetes="excl2_lab",pob_lab = c("Population","N Final"),grups="grup",ordre = "exc_ordre")
flow_global2



# Ara seleccionar dades / fer descriptiva basal  -----------------------

dades<-dades %>% filter(ps==1)


# FActoritzar -------------

dades<-factoritzar.NO.YES(dades,columna = "factoritzar.yes.no",taulavariables = conductor_variables)

# Labels  -------------

dades<-etiquetar(dades,taulavariables = conductor_variables,camp_descripcio = "Descripcio")

dades<-etiquetar_valors(dades,variables_factors = conductor_variables,fulla="value_labels",camp_etiqueta = "etiqueta")


# Taules descriptives exploratories  ----------------
taules<-llistadetaules.compare(tablero=c("table1","table2","table3","table4"),y="grup",variables = conductor_variables,dt=dades)

formula<-formula_compare(x="table1",y="grup",taulavariables = conductor_variables)
taula1.post<-descrTable(formula,data=dades,show.p.overall = F)


# Salvar objectes 

output_Rdata<-here::here("resultats","Output_metplus.RData")

save(flow_global,flow_global2,taula1,taulaPS,taula1.post,taules,file=output_Rdata)




