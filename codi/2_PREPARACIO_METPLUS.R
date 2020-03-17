#  FASE: Preparació      ---------------------
#
#
# 0. Directori de treball / càrrega de funcions    --------------



  #
  # Lectura de dades     
  
  # memory.size(max=160685)
  #
  #   SOURCE
  # rm(list=ls())
  # 
  # link_source<-paste0("https://github.com/jrealgatius/Stat_codis/blob/master/funcions_propies.R","?raw=T")
  # devtools::source_url(link_source)
  
  gc()


# 0. Lectura de dades          -----------------------------

  # fitxersortida
  fitxer_entrada<-fitxersortida
  
# Lectura dades
  dades<-readRDS(fitxer_entrada)  
  # dt_historic_fd<-readRDS(here::here("dades/preparades","historic_fd.rds"))
  # dt_historic_fd_sgaps<-readRDS(here::here("dades/preparades","historic_fd_sgaps.rds"))
    

# 1. Calculs i recodes de variables   ----------------------

library(lubridate)

#   dtindex ---> data --------------

dades<-dades %>% mutate(dtindex=as_date(dtindex))


#   Anys desde diabetis ------------
dades<-dades %>% 
    mutate(anys_DM = year(as.period(interval(start = ymd(DG.CI), end = dtindex))))

# Si Missings DATA DM (Aquesta es posterior a inclusió) --> data primera metformina o mateixa data index
dades<-dades %>% 
  mutate(anys_DM=case_when(
    is.na(anys_DM)~year(as.period(interval(start = F_dataMET,end = dtindex))),
    TRUE~anys_DM)) %>% 
  mutate(anys_DM=ifelse(is.na(anys_DM),0,anys_DM)) 

#   Recode farmacs basals ---------
# NA --> 0 (Else=1) (No hi ha 0)
dades<-dades %>% mutate_at(vars(starts_with("F.")), 
                           funs(ifelse(is.na(.) | 0,0,1))) 

#   Edat ------------
dades<-dades %>% 
  mutate(edat = year(as.period(interval(start = ymd(dnaix), end = dtindex))))

#   Any / quarter data index  ------------------
dades<- dades %>% 
  mutate (any_dtindex=year(dtindex),
          Q_dtindex=quarter(dtindex,with_year = T)) 

# 2. Criteris d'inclusió  --------------

# 1.1 Metformina

#   Inclusió 1 (METformina)  -------

dades<-dades %>% mutate(inclusio_met=ifelse(F.MET | F.METF,1,0))


#   Inclusió 2 : Insuficient control glycemic (HbA1c>=7) ----------

dades<-dades %>% mutate(inclusio_HB7=case_when(HBA1C.valor>=7~"1",
                                    HBA1C.valor<7~"0",TRUE~"NA"))


dades<-recodificar(dades,taulavariables = conductor_variables,criteris = "recode")


#   Inclusió 3: Edad>18 anys ---------------

dades<-dades %>% mutate (inclusio_edat18=ifelse(edat>=18,1,0))


#   Inclusió 4: Pes not missing ----------
dades<-dades %>% mutate (inclusio_pes=ifelse(!is.na(PESO.valor),1,0))

# Diagnostics basals (Recode)
# NA --> 0 (Else=1) (No hi ha 0)
dades<-dades %>% mutate_at(vars(starts_with("DG.")), 
                           funs(if_else(is.na(.) | 0,0,1))) 


# Comorbilidad basal (Suma de comorbiditats)  ---------------
llista_comorb<-extreure.variables("ncomorb",taulavariables = conductor_variables)
dades<-comptar_valors(dades,variables=llista_comorb,valor="1") %>% rename(DG.Ncomorb=num_valors)

dades<-dades %>% mutate(DG.Ncomorb.cat3=case_when(
  DG.Ncomorb==0~"None",
  DG.Ncomorb==1 | DG.Ncomorb==2~"1-2",
  DG.Ncomorb>=3~">3"))


# 3. Aplicar criteris d'inclusió  ------------------------

# Faig copia per despres fer flow-chart amb totes les dades 
dadesinicials<-dades
dades<-dades %>% filter(inclusio_met==1 & inclusio_HB7==1 & inclusio_edat18==1 & inclusio_pes==1)


# Recodificar edat
dades<-recodificar(dades,taulavariables = conductor_variables,criteris = "recode",missings = F)
dades<-recodificar(dades,taulavariables = conductor_variables,criteris = "recode_mis",missings = T)

# Falta generació de quartils Pes
dades<-dades %>% 
  mutate(PES.CAT.Q4=Hmisc::cut2(PESO.valor,g=4))

# Generació de quINTILS anys_dm 
dades<-dades %>% 
  mutate(anys_DM.Q5=Hmisc::cut2(anys_DM,g=5))

# 4. Matching per 3 grups  IDPP4,ISGLT2 ,SU  --------------------------

#   4.1. Inicialització de paràmetres -----------
caliper<-0.01


# Selecciono dades per matching 
dadesmatching<-selectorvariables(taula="dadesmatch",taulavariables = conductor_variables,dades)

# Matching a subset 1 (ISGLT2 vs IDPP4)
dades_sub1<-dadesmatching %>% filter(grup=="IDPP4" | grup=="ISGLT2")
dades_sub1<-dades_sub1 %>% mutate(grup_dic=ifelse(grup=="ISGLT2",1,0))

# Aplicar matching A subset     #
set.seed(111)
formulaPS<-formula_compare(x="match",y="grup_dic",taulavariables = conductor_variables)
exact<-extreure.variables("match_exact",taulavariables = conductor_variables)

# m.out<-matchit(formulaPS,method="nearest",data=as.data.frame(dades_sub1),caliper=caliper,ratio=1,exact=exact)
m.out<-matchit(formulaPS,method="nearest",data=as.data.frame(dades_sub1),caliper=caliper,ratio=1)
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

set.seed(111)
# m.out<-matchit(formulaPS,method="nearest",data=as.data.frame(dades_sub1),caliper=caliper,ratio=1,exact=exact)
m.out<-matchit(formulaPS,method="nearest",data=as.data.frame(dades_sub1),caliper=caliper,ratio=1)

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


#   4.2. Afegir PS indicadora  -------------------
dades<-dades %>%  left_join(select(dadesmatching,c(idp,ps)),by="idp")
dadesinicials<-dadesinicials %>%  left_join(select(dadesmatching,c(idp,ps)),by="idp") 

# Verificació de matching 
formula<-formula_compare(x="table1",y="grup",taulavariables = conductor_variables)
descrTable(formula,data=dades %>% filter(ps==1),show.all = T,show.n = T)


# Fi matching 
rm(dades_sub1,dadesmatching,taula2)


# 5. Generar flowcharts desde dadesinicials (Excloc no metformina previa i menors de 18 anys) --------------
dades_temp<-dadesinicials %>% filter(inclusio_met==1 & inclusio_edat18==1)
flow_global<-criteris_exclusio_diagrama(dades_temp,taulavariables = conductor_variables,criteris = "exclusio",
                                    etiquetes="exc_lab",pob_lab = c("Population","N Final"),grups="grup",ordre = "exc_ordre",sequencial = T)

flow_global
rm(dades_temp)

# Recode ps a 0 
dades<-dades %>% mutate(ps=ifelse(is.na(ps),0,ps))

flow_global2<-criteris_exclusio_diagrama(dades_prematch,taulavariables = conductor_variables,criteris = "excl2",
                                        etiquetes="excl2_lab",pob_lab = c("Population","N Final"),grups="grup",ordre = "exc_ordre")
flow_global2


rm(list=c("dt_historic_fd_sgaps","dt_historic_fd"))
gc()


# Generar taules pre  #####
dades<-etiquetar_valors(dades,variables_factors = conductor_variables,fulla="value_labels",camp_etiqueta = "etiqueta")

dades<-etiquetar(dades,taulavariables = conductor_variables)

formula<-formula_compare(x="match",y="grup",taulavariables = conductor_variables)
taula1<-descrTable(formula,data=dades,show.all = T,show.n = T)
taula1

formula<-formula_compare(x="table1",y="grup",taulavariables = conductor_variables)
taula1.2<-descrTable(formula,data=dades,show.all = T,show.n = T)
taula1.2

rm(dt_historic_fd)
rm(dt_historic_fd_sgaps)

# 6. Salvar i carregar *.RData  ---------------------------

save.image(here::here("resultats","temp.Rdata"))

load(here::here("resultats","temp.Rdata"))

# 7. Ara seleccionar dades / fer descriptiva basal  -----------------------

# Copia dades
dades_prematch<-dades

dades<-dades %>% filter(ps==1) %>% as_tibble()
formula<-formula_compare(x="match",y="grup",taulavariables = conductor_variables)
descrTable(formula,data=temp,show.all = T,show.n = T)

# 8. Fer IMPUTACIO I GUARDAR LONG + DATASET INICIAL  ----------------------

# Mice metode
library(mice)
# 1. Selecciono data_set de variables que vull utilitzar per la imputació   -------
variables_a_imputar<-extreure.variables("imp4",taulavariables = conductor_variables)

dades_imputation<-dades %>% select("idp",variables_a_imputar)
dades_no_imputades<-dades %>% select(-variables_a_imputar,"idp")

# Aplico model d'imputació  (Genero les dades imputades)  ----------
num_imputacions<-10 # Normalment >=10
num_iteracions<-5 # Normalment 5
# Generar imputacions
start_time <- Sys.time()
# mice(data=dades_imputation,m=num_imputacions,maxit=5,meth='pmm',seed=123,remove.collinear=T)
tempData <- mice(data=dades_imputation,m=num_imputacions,maxit=num_iteracions,meth='cart',seed=123,remove.collinear=T)
# ,remove.collinear=F)
end_time <- Sys.time()
end_time-start_time

# Generar llista de dades imputades en llista i en data_frame
data_list <- 1:num_imputacions %>% map(~mice::complete(tempData, .x)) 
data_long<-mice::complete(tempData, action = "long", include = F)
data_long_total<-mice::complete(tempData, action = "long", include = T)

# Juntar data_long_total les dades no imputades
dades<-data_long_total %>% left_join(dades_no_imputades,by="idp")
# Convertir objecte MICE
miceData<-as.mids(data_long_total)

# Verificar imputació i missings  ----------------
descrTable(~.,data=data_long,method = 2,Q1=0,Q3=1)
descrTable(~.,data=dades_imputation,method = 2,Q1=0,Q3=1)

# Evaluació d'outcome entre grups: 
library(dplyr)

# Actualitzar imatge 
save.image(here::here("resultats","temp2.Rdata"))
load(here::here("resultats","temp2.Rdata"))

# 9. Calcular outcomes: (Reducció de HbA1c i Reducció de PES)  ---------------------
# HBA1C.valor12m HBA1C.valor324m HBA1C.valor24m
library(dplyr)
dades<-dades %>% mutate (
  HBA1C.dif324m=HBA1C.valor-HBA1C.valor324m,
  HBA1C.dif324m.cat=if_else(HBA1C.dif324m>0.5,1,0),                
  PESO.dif324m=PESO.valor-PESO.valor324m,
  PESO.dif324m.cat=if_else(PESO.dif324m/PESO.valor>0.03,1,0),
  PESHB.dif324m.cat=case_when(is.na(HBA1C.dif324m.cat) | is.na(PESO.dif324m.cat)~NA_real_,
                              HBA1C.dif324m.cat==0 | PESO.dif324m.cat==0~0,
                              HBA1C.dif324m.cat==1 & PESO.dif324m.cat==1~1)) 

# 9.1. Calcular reduccions de PAS COLHDL.valor COLLDL.valor COLTOT.valor TG.valor
dades<-dades %>% mutate (
  PAS.dif324m=PAS.valor-PAS.valor324m,
  COLHDL.dif324m=COLHDL.valor-COLHDL.valor324m,
  COLLDL.dif324m=COLLDL.valor-COLLDL.valor324m,
  COLTOT.dif324m=COLTOT.valor-COLTOT.valor324m,
  TG.valor.dif324m=TG.valor-TG.valor324m) 

# 9.2. Outcomes Adherencia / Suspensions  #####

# Recode missings de dispensació a 0
dades<-dades %>% mutate_at(vars(starts_with("NenvasTX.")), 
                           funs(if_else(is.na(.),0,.))) 


# 9.3. Unifico numero d'envasos i temps de tractament per cada grup

# Nombre d'envasos per grup d'estudi
dades<-dades %>% 
  mutate(NenvasTX.GRUP= case_when(grup=="ISGLT2"~NenvasTX.iSGLT2,
                                  grup=="IDPP4"~NenvasTX.IDPP4,
                                  grup=="SU"~NenvasTX.SU))
# 9.4. Dies de tractament per grup d'estudi
dades<-dades %>% 
  mutate(tempsTX.GRUP= case_when(grup=="ISGLT2"~tempsTX.iSGLT2,
                                 grup=="IDPP4"~tempsTX.IDPP4,
                                 grup=="SU"~tempsTX.SU))

# 9.5. Calculo de Medication possession ratio (PMR)
dades<-dades %>% mutate(MPR.TX=NenvasTX.GRUP*30.4/tempsTX.GRUP) 
                 
# Si el ratio de medicació es superior a 1 --> 1 (hi ha mes dies de tractament que de prescripció)
dades<-dades %>% mutate(MPR.TX=if_else(MPR.TX>1,1,MPR.TX))

# Si el ratio >0.8 --> Adherencia ok
dades<-dades %>% mutate(MPR.TX.cat=case_when(MPR.TX>0.8 ~"Yes",
                                  MPR.TX<=0.8~"No",
                                  is.na(MPR.TX)~"NA"))


# 9.6. Calculo data STOP  del grup que inicia
dades<-dades %>% mutate(STOP.FD.GRUP=case_when(
  grup=="IDPP4" ~STOP.FD.IDPP4,
  grup=="SU" ~STOP.FD.SU,
  grup=="ISGLT2" ~STOP.FD.iSGLT2)) 

# 9.7. Calculo suspensions/dropouts of treatment at 6,12,24 mesos en base a STOP's Dispensados 
dades<-dades %>% mutate(temps.STOP.FD=STOP.FD.GRUP-dtindex)

# Ojo repensar censures i com es tracten en data de sortida(20171231)
# Cal fixar una data de censura (Exemple: 20171231) si la data STOP és superior (-92 dies) a la data STOP es censura 
# Hi persones que se les ha seguit menys de 24 mesos (Cal estudiar seguiment)
# Verificar data màxima d'entrada

# Calculo data de censura (datafi) (Considero data de trasllat com a censura / Si data d exitus)
dades<-dades %>% mutate (datafi_seguiment=case_when(
  sortida<20171231 ~ lubridate::ymd(sortida),        # Sortida previa per (T o D) previa a 20171231 --> datasortida
  sortida>=20171231 ~ lubridate::ymd(20171231),      # Sortida posterior --> 20171231
  TRUE~lubridate::ymd(20171231))) %>% 
  mutate (datafi_seguiment=if_else(datafi_seguiment<dtindex,dtindex,datafi_seguiment)) %>%  # Si datafi<dtindex igualarlo (No negatius)
  mutate(temps_seguiment=datafi_seguiment-dtindex)

# Verificar data de sortida
MAP_ggplot_univariant(dades,datainicial = "dtindex",datafinal = "datafi_seguiment",add_final = "situacio",Nmostra = 10,id="idp")
dades %>% as_tibble() %>% filter (lubridate::ymd(sortida)<=dtindex) %>% select(dtindex,sortida,situacio,HBA1C.valor324m)


# Existeix STOP tractament si aquest es 92 dies previs a la data fi de seguiment
# Generar STOP.FD (0/1) previ fi de seguiment (censura)
dades<-dades %>% as_tibble() %>% 
  mutate(STOP.FD=case_when(
    is.na(STOP.FD.GRUP)~NA_real_,
    STOP.FD.GRUP<datafi_seguiment-92~1,
    STOP.FD.GRUP>=datafi_seguiment-92~0))

MAP_ggplot_univariant(dades,datainicial = "dtindex",datafinal = "STOP.FD.GRUP",add_point = "datafi_seguiment",add_final = "STOP.FD",Nmostra = 10,id="idp")

# STOPS a 6,12,24 mesos durant el seguiment entre grups  -------------------

# Curva survfit STOP tractament 

# Stop previ a 24m (Només d'aquells amb seguiment mínim a 24 mesos)
dades<-dades %>% mutate(
  STOP24m.FD=case_when(
  temps.STOP.FD<730 & temps_seguiment>=730 ~"Si Stop<24meses",
  temps.STOP.FD>=730 & temps_seguiment>=730 ~"+24m",
  TRUE~NA_character_))

descrTable(grup~STOP24m.FD,dades)
# Stop 12m (Només d'aquells amb seguiment mínim 12 mesos)
dades<-dades %>% mutate(
  STOP12m.FD=case_when(
    temps.STOP.FD<365 & temps_seguiment>=365 ~"Si Stop<12meses",
    temps.STOP.FD>=365 & temps_seguiment>=365 ~"+12meses",
    TRUE~NA_character_))

descrTable(grup~STOP12m.FD,dades)

# Stop 6m (Només d'aquells amb seguiment mínim 6 mesos)
dades<-dades %>% mutate(
  STOP6m.FD=case_when(
    temps.STOP.FD<182 & temps_seguiment>=182 ~"Si Stop<6m",
    temps.STOP.FD>=182 & temps_seguiment>=182 ~"+6m",
    TRUE~NA_character_))


# 7.2. Calcular datafi OT On treatment (datafiOT)  ----------------
# Definition of follow-up period and premature discontinuation
# Death, the switch or addition of a new antidiabetic treatment, last billing of study drugs before 24 months after prescription, transfers to non-ICS centers.

# CanviTx (data minima de totes)
dades<-dades %>% mutate(datafiOT= case_when(
  grup=="IDPP4" ~ pmin(STOP.FD.IDPP4,CANVITX.iSGLT2,CANVITX.SU,na.rm=T),
  grup=="ISGLT2" ~ pmin(CANVITX.IDPP4,STOP.FD.iSGLT2,CANVITX.SU,na.rm=T),
  grup=="SU" ~ pmin(CANVITX.IDPP4,CANVITX.iSGLT2,STOP.FD.SU,na.rm=T)))

# Mateixa data index (Aquells que no tenen dispensació)
dades<-dades %>% mutate(datafiOT=ifelse(is.na(datafiOT),dtindex,datafiOT),
                        datafiOT=lubridate::as_date(datafiOT))

# Verificar mitjançant map
MAP_ggplot_univariant(dades,datainicial = "dtindex",datafinal = "STOP.FD.GRUP",add_point = "datafiOT",id="idp",Nmostra = 10)

MAP_ggplot_univariant(dades,datainicial = "dtindex",datafinal = "datafiOT",add_point = "STOP.FD.GRUP",id="idp",Nmostra = 10)

# Actualitzar a maxim 24 mesos (730 dies) o fi de seguiment (exitus/trasllat/31122017)
dades<-dades %>% 
  mutate(datafiOT= case_when(
    datafiOT-dtindex>=730 ~ pmin(dtindex+730,datafi_seguiment),
    datafiOT-dtindex<=730 ~ pmin(datafiOT,datafi_seguiment)))

# Verificar dades 
MAP_ggplot_univariant(dades,datainicial = "dtindex",datafinal = "datafiOT",id="idp",Nmostra = 20,add_point = "datafi_seguiment",add_final = "situacio")
descrTable(grup~STOP24m.FD+STOP12m.FD+STOP6m.FD, data=dades)
dades %>% filter(str_detect(idp,"b7ed96f5dda")) %>% select(idp,dtindex,situacio,sortida,datafiOT,STOP.FD)

# 8. Generar variables grup indicadora (en relació a la resta) ---------
dades<-make_dummies(dades,"grup","grup_")


# 9. Labels/ factoritzar   -------------
# dades<-etiquetar_valors(dades,variables_factors = conductor_variables,fulla="value_labels",camp_etiqueta = "etiqueta")
# 
dades<-factoritzar.NO.YES(dades,columna = "factoritzar.yes.no",taulavariables = conductor_variables)
dades<-factoritzar(dades,variables=extreure.variables("factoritzar",conductor_variables))
#
dades<-etiquetar(dades,taulavariables = conductor_variables,camp_descripcio = "descripcio")


# Actualitzar conductor(fitxers xls --> xlsx)
# ActualitzarConductor(dades,taulavariables = conductor_variables)

# Actualitzar imatge 
save.image(here::here("resultats","temp3.Rdata"))
load(here::here("resultats","temp3.Rdata"))

# Relevel grup
dades<-dades %>% mutate(grup2=stats::relevel(grup,ref="ISGLT2"))


# 11. Resultados 1  -----------------

# 11.1 DESCRIPTIU outcomes dades completes vs dades imputades
formula_temp<-formula_compare("outcomes1",y="grup",taulavariables = "variables_metplus.xls")
temp_comp<-compareGroups::descrTable(formula_temp,data=dades %>% filter(.imp==0),show.p.overall = F,show.n = T,extra.labels =c(""))
# Descriptivo datos imputados:
temp_imp <-compareGroups::descrTable(formula_temp,data=dades %>% filter(.imp>0),show.p.overall = F,show.n = T,extra.labels =c(""))
cbind("Complete case Analysis" = temp_comp, "Imputation cases analysis (m=10)" = temp_imp)


# 11.2 COMPARATIVA d'outcomes principals (Respecte un grup de referencia IDPP4 i respecte la resta)
vector_ajust<-extreure.variables("v.ajust",conductor_variables)

# Mesures d'asociació per cada outcome 
llista_outcomes<-extreure.variables("outcomes1",taulavariables = conductor_variables)

taula_estimacions_GRUP<-llista_outcomes %>% purrr::map_df(
  ~extreure_resum_outcomes_imputation(dades,outcome=.x,grups="grup",v.ajust=vector_ajust)) %>% 
  filter(categoria!="(Intercept)") %>% select(-mean)

taula_estimacions_GRUP_ref_ISGLT2<-llista_outcomes %>% purrr::map_df(
  ~extreure_resum_outcomes_imputation(dades,outcome=.x,grups="grup2",v.ajust=vector_ajust)) %>% 
  filter(categoria!="(Intercept)") %>% select(-mean)


# Respecte global
taula_estimacions_IDPP4<-llista_outcomes %>% purrr::map_df(
  ~extreure_resum_outcomes_imputation(dades,outcome=.x,grups="grup_IDPP4",v.ajust=vector_ajust)) %>% 
  filter(categoria!="(Intercept)") %>% select(-mean)

taula_estimacions_SU<-llista_outcomes %>% purrr::map_df(
  ~extreure_resum_outcomes_imputation(dades,outcome=.x,grups="grup_SU",v.ajust=vector_ajust)) %>% 
  filter(categoria!="(Intercept)") %>% select(-mean)

taula_estimacions_ISGLT2<-llista_outcomes %>% purrr::map_df(
  ~extreure_resum_outcomes_imputation(dades,outcome=.x,grups="grup_ISGLT2",v.ajust=vector_ajust)) %>% 
  filter(categoria!="(Intercept)") %>% select(-mean)


# 12 Comparativa outcomes secundaris PAS (Resultados 2) --------------

# 1. DESCRIPTIVA
formula_temp<-formula_compare("table6",y="grup",taulavariables = "variables_metplus.xls")
temp_comp<-compareGroups::descrTable(formula_temp,data=dades %>% filter(.imp==0),show.p.overall = F,show.n = T,extra.labels =c(""))
# Descriptivo datos imputados:
temp_imp <-compareGroups::descrTable(formula_temp,data=dades %>% filter(.imp>0),show.p.overall = F,show.n = T,extra.labels =c(""))
cbind("Complete case Analysis" = temp_comp, "Imputation cases analysis (m=10)" = temp_imp)

# 2. COMPARATIVA D'OUTCOMES

# Mesures d'asociació per cada outcome 
llista_outcomes<-extreure.variables("table6",taulavariables = conductor_variables)

taula_estimacions2_GRUP<-llista_outcomes %>% purrr::map_df(
  ~extreure_resum_outcomes_imputation(dades,outcome=.x,grups="grup",v.ajust=vector_ajust)) %>% 
  filter(categoria!="(Intercept)") %>% select(-mean)

taula_estimacions2_GRUP_ref_ISGLT2<-llista_outcomes %>% purrr::map_df(
  ~extreure_resum_outcomes_imputation(dades,outcome=.x,grups="grup2",v.ajust=vector_ajust)) %>% 
  filter(categoria!="(Intercept)") %>% select(-mean)


taula_estimacions2_IDPP4<-llista_outcomes %>% purrr::map_df(
  ~extreure_resum_outcomes_imputation(dades,outcome=.x,grups="grup_IDPP4",v.ajust=vector_ajust)) %>% 
  filter(categoria!="(Intercept)") %>% select(-mean)

taula_estimacions2_SU<-llista_outcomes %>% purrr::map_df(
  ~extreure_resum_outcomes_imputation(dades,outcome=.x,grups="grup_SU",v.ajust=vector_ajust)) %>% 
  filter(categoria!="(Intercept)") %>% select(-mean)

taula_estimacions2_ISGLT2<-llista_outcomes %>% purrr::map_df(
  ~extreure_resum_outcomes_imputation(dades,outcome=.x,grups="grup_ISGLT2",v.ajust=vector_ajust)) %>% 
  filter(categoria!="(Intercept)") %>% select(-mean)


# 13 Comparativa outcomes secundaris PAS (Resultados 3)

# 1. DESCRIPTIVA

formula_temp<-formula_compare("table7",y="grup",taulavariables = "variables_metplus.xls")
temp_comp<-compareGroups::descrTable(formula_temp,data=dades %>% filter(.imp==0),show.p.overall = F,show.n = T,extra.labels =c(""))
# Descriptivo datos imputados:
temp_imp <-compareGroups::descrTable(formula_temp,data=dades %>% filter(.imp>0),show.p.overall = F,show.n = T,extra.labels =c(""))
cbind("Complete case Analysis" = temp_comp, "Imputation cases analysis (m=10)" = temp_imp)

# 2. COMPARATIVA 

# Mesures d'asociació per cada outcome 
llista_outcomes<-extreure.variables("table7",taulavariables = conductor_variables)

taula_estimacions3_GRUP<-llista_outcomes %>% purrr::map_df(
  ~extreure_resum_outcomes_imputation(dades,outcome=.x,grups="grup",v.ajust=vector_ajust)) %>% 
  filter(categoria!="(Intercept)") %>% select(-mean)

taula_estimacions3_GRUP_ref_ISGLT2<-llista_outcomes %>% purrr::map_df(
  ~extreure_resum_outcomes_imputation(dades,outcome=.x,grups="grup2",v.ajust=vector_ajust)) %>% 
  filter(categoria!="(Intercept)") %>% select(-mean)

taula_estimacions3_IDPP4<-llista_outcomes %>% purrr::map_df(
  ~extreure_resum_outcomes_imputation(dades,outcome=.x,grups="grup_IDPP4",v.ajust=vector_ajust)) %>% 
  filter(categoria!="(Intercept)") %>% select(-mean)

taula_estimacions3_SU<-llista_outcomes %>% purrr::map_df(
  ~extreure_resum_outcomes_imputation(dades,outcome=.x,grups="grup_SU",v.ajust=vector_ajust)) %>% 
  filter(categoria!="(Intercept)") %>% select(-mean)

taula_estimacions3_ISGLT2<-llista_outcomes %>% purrr::map_df(
  ~extreure_resum_outcomes_imputation(dades,outcome=.x,grups="grup_ISGLT2",v.ajust=vector_ajust)) %>% 
  filter(categoria!="(Intercept)") %>% select(-mean)

# 14 ADHERENCIA  Resultados 4 ------------------------
extreure.variables("table8",taulavariables = conductor_variables)

# Recodifico amb missings (No funciona amb tres categories)
dades<-dades %>% mutate(MPR.TX.cat=recode_factor(MPR.TX.cat,"Yes"="Yes","No"="No","NA" = NA_character_))
# Canvio la categoria de referencia
dades$MPR.TX.cat<-relevel(dades$MPR.TX.cat,"No")

# DESCRIPTIVA
formula<-formula_compare("table8",y="grup",taulavariables = conductor_variables)
descrTable(formula,dades)

formula_temp<-formula_compare("table8",y="grup",taulavariables = "variables_metplus.xls")
temp_comp<-compareGroups::descrTable(formula_temp,data=dades %>% filter(.imp==0),show.p.overall = F,show.n = T,extra.labels =c(""))
# Descriptivo datos imputados:
temp_imp <-compareGroups::descrTable(formula_temp,data=dades %>% filter(.imp>0),show.p.overall = F,show.n = T,extra.labels =c(""))
cbind("Complete case Analysis" = temp_comp, "Imputation cases analysis (m=10)" = temp_imp)

# COMPARATIVA
llista_outcomes<-extreure.variables("table8",conductor_variables)

taula_adhrecia_GRUP<-llista_outcomes %>% purrr::map_df(
    ~extreure_resum_outcomes_imputation(dades,outcome=.x,grups="grup",v.ajust=vector_ajust)) %>% 
  filter(categoria!="(Intercept)") %>% select(-mean)


# 15. SUSPENSIONS/DROPOUTS  (RESULTADOS 5) --------------------
#
# Analisis dades completes

dades_completes<-dades %>% filter(.imp==0)

cfit<-survfit(Surv(temps.STOP.FD,STOP.FD)~grup,data=dades_completes)

# Figura 
survminer::ggsurvplot(cfit, data = dades_completes,
                      main = "Survival curve",
                      title= "Suspensions/dropouts during follow-up before 24 months",
                      size = 0.5,
                      ylim = c(0,1),
                      xlim = c(0,730),
                      break.x.by=180,
                      xlab = "Time in days",
                      risk.table = F,
                      censor.shape="|", censor.size = 1)

taula_percentatge_stops<-broom::tidy(cfit) %>% filter(time==180) %>% 
  bind_rows(broom::tidy(cfit) %>% filter(time==365)) %>% 
  bind_rows(broom::tidy(cfit) %>% filter(time==730)) %>% 
  transmute(strata,time_in_days=time,estimate_stop=1-estimate,CI_linf=1-conf.high,CI_sup=1-conf.low) 

taula_percentatge_stops


dades_completes$temps_fins_stop<-with(dades_completes,Surv(temps.STOP.FD,STOP.FD))

descrTable(temps_fins_stop~grup,data=dades_completes,show.ratio = T)

# 16. Descriptivo de efectos adversos (Resultados 6) --------------


# Generar events com a Surv i recodificar events com a 0 en només en dades completes  -----------------

# Funció generar_Surv Generar columna Surv en dades, event ("20150531"), dtindex, sortida(20171231), 
generar_Surv<-function(dt,event){
  x<-sym(event)
  temp<-dt %>% select(dtindex,!!x,sortida) %>% 
    mutate(
      event=case_when(as.Date(as.character(!!x),"%Y%m%d")>0~1,
                      is.na(!!x)~0),
      data_final=case_when(as.Date(as.character(!!x),"%Y%m%d")>0~as.Date(as.character(!!x),"%Y%m%d"),
                           is.na(!!x)~as.Date(as.character(sortida),"%Y%m%d"))
    ) %>% 
    mutate(temps=(data_final-dtindex) %>% as.numeric())
  # Genero l'objecte Surv
  temp$event_surv<-Surv(temp$temps,temp$event)
  # Selecciono i renombro
  nom_surv=paste0(event,".surv")
  temp<-temp %>% select(event_surv) 
  colnames(temp)=nom_surv
  temp 
  }

# Extrect llista de camps 
llista_events<-extreure.variables("dates_events",conductor_variables)
llista_events<-semi_join(tibble(id=llista_events), tibble(id=names(dades)),by="id") %>% pull(id)
# Genera dades_surv
dades_surv<-map(llista_events,~generar_Surv(dt=dades_completes,.)) %>% 
  as.data.frame()
# Fusiona amb dades  
dades_completes<-dades_completes %>% cbind(dades_surv)


# Recofificar en 0 i 1's
dades_completes<-dades_completes %>% mutate_at(llista_events, 
                                               funs(case_when(
                                                 lubridate::ymd(.)>=dtindex~1,
                                                 is.na(.)~0,
                                                 TRUE~0))) 

# table10

# Factoritzar events / etiquetar 
dades_completes<-factoritzar.NO.YES(dades_completes,"dates_events",conductor_variables)
dades_completes<-etiquetar(dades_completes,conductor_variables)

# Generar taula
formula_temp<-formula_compare("table10",y="grup",taulavariables = conductor_variables)
compareGroups::descrTable(formula_temp,data=dades_completes,show.p.overall = F,hide = T)

# Salvar temporal 

save.image(here::here("resultats","temp4.Rdata"))

load(here::here("resultats","temp4.Rdata"))

flow_global2<-criteris_exclusio_diagrama(dades_prematch,taulavariables = conductor_variables,criteris = "excl2",
                                         etiquetes="excl2_lab",pob_lab = c("Population","N Final"),grups="grup",ordre = "exc_ordre")
flow_global2


# 17 Salvar objectes ----------
# output_Rdata<-"Output_metplus.RData"
save(dades,dades_completes,flow_global,flow_global2,taula1,taula1.2,taulaPS,
     
     taula_estimacions_GRUP_ref_ISGLT2,
     taula_estimacions_GRUP,
     taula_estimacions_IDPP4,
     taula_estimacions_SU,
     taula_estimacions_ISGLT2,
     
     taula_estimacions2_GRUP_ref_ISGLT2,
     taula_estimacions2_GRUP,
     taula_estimacions2_IDPP4,
     taula_estimacions2_SU,
     taula_estimacions2_ISGLT2,
     
     taula_estimacions3_GRUP_ref_ISGLT2,
     taula_estimacions3_GRUP,
     taula_estimacions3_IDPP4,
     taula_estimacions3_SU,
     taula_estimacions3_ISGLT2,
     
     taula_adhrecia_GRUP,
     
     file=here::here("resultats",output_Rdata))




