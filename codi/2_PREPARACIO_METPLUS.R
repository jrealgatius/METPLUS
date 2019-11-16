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
  dt_historic_fd<-readRDS(here::here("dades/preparades","historic_fd.rds"))
  dt_historic_fd_sgaps<-readRDS(here::here("dades/preparades","historic_fd_sgaps.rds"))
    

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
formula<-formula_compare(x="match",y="grup",taulavariables = conductor_variables)
taula1<-descrTable(formula,data=dades,show.all = T,show.n = T)

formula<-formula_compare(x="table1",y="grup",taulavariables = conductor_variables)
taula1.2<-descrTable(formula,data=dades,show.all = T,show.n = T)
taula1.2

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

flow_global2<-criteris_exclusio_diagrama(dades,taulavariables = conductor_variables,criteris = "excl2",
                                        etiquetes="excl2_lab",pob_lab = c("Population","N Final"),grups="grup",ordre = "exc_ordre")
flow_global2



# 6. Ara seleccionar dades / fer descriptiva basal  -----------------------
temp<-dades %>% filter(ps==1) %>% as_tibble()
formula<-formula_compare(x="table1",y="grup",taulavariables = conductor_variables)
descrTable(formula,data=temp,show.all = T,show.n = T)


# 7. Calcular outcomes: (Reducció de HbA1c i Reducció de PES)  ---------------------
# HBA1C.valor12m HBA1C.valor324m HBA1C.valor24m

dades<-dades %>% mutate (
  HBA1C.dif324m=HBA1C.valor-HBA1C.valor324m,
  HBA1C.dif324m.cat=if_else(HBA1C.dif324m>0.5,1,0),                
  PESO.dif324m=PESO.valor-PESO.valor324m,
  PESO.dif324m.cat=if_else(PESO.dif324m/PESO.valor>0.03,1,0),
  PESHB.dif324m.cat=case_when(is.na(HBA1C.dif324m.cat) | is.na(PESO.dif324m.cat)~NA_real_,
                              HBA1C.dif324m.cat==0 | PESO.dif324m.cat==0~0,
                              HBA1C.dif324m.cat==1 & PESO.dif324m.cat==1~1)) 

# Calcular reduccions de PAS COLHDL.valor COLLDL.valor COLTOT.valor TG.valor
dades<-dades %>% mutate (
  PAS.dif324m=PAS.valor-PAS.valor324m,
  COLHDL.dif324m=COLHDL.valor-COLHDL.valor324m,
  COLLDL.dif324m=COLLDL.valor-COLLDL.valor324m,
  COLTOT.dif324m=COLTOT.valor-COLTOT.valor324m,
  TG.valor.dif324m=TG.valor-TG.valor324m) 

# 7.1. Outcomes Adherencia / Suspensions  #####

# Recode missings de dispensació a 0
dades<-dades %>% mutate_at(vars(starts_with("NenvasTX.")), 
                           funs(if_else(is.na(.),0,.))) 



# Calculo de Medication possession ratio (PMR)
dades<-dades %>% 
  mutate(MPR.TX=case_when(grup=="ISGLT2"~(NenvasTX.iSGLT2*30.4)/tempsTX.iSGLT2,
                          grup=="IDPP4"~(NenvasTX.IDPP4*30.4)/tempsTX.IDPP4,
                          grup=="SU"~(NenvasTX.SU*30.4)/tempsTX.SU))

dades<-dades %>% mutate(MPR.TX=if_else(MPR.TX>1,1,MPR.TX))


dades<-dades %>% mutate(MPR.TX.cat=case_when(MPR.TX>0.8 ~"Yes",
                                  MPR.TX<=0.8~"No",
                                  is.na(MPR.TX)~"NA"))


# Calculo suspensions/dropouts of treatment at 6,12,24 mesos en base a STOP's Dispensados 
dades<-dades %>% mutate(temps.STOP.FD=case_when(
  grup=="IDPP4" ~STOP.FD.IDPP4-dtindex,
  grup=="SU" ~STOP.FD.SU-dtindex,
  grup=="ISGLT2" ~STOP.FD.iSGLT2-dtindex ))
  


# Ojo repensar censures i com es tracten sortida(20171231)
# Cal fixar una data de censura (Exemple: 20171231) si la data STOP és superior (-92 dies) a la data STOP es censura 
# Hi persones que se les ha seguit menys de 24 mesos (Cal estudiar seguiment)
# Verificar data màxima d'entrada

# Calculo data de censura (datafi) (No considero data de trasllat com a censura)
dades<-dades %>% mutate (datafi_seguiment=case_when(
  situacio=="A" | situacio=="T" ~ lubridate::ymd(20171231),
  situacio=="D" & sortida>=20171231 ~ lubridate::ymd(20171231),
  situacio=="D" & sortida<20171231 ~ lubridate::ymd(sortida),
  TRUE~lubridate::ymd(20171231))) %>% 
  mutate(temps_seguiment=datafi_seguiment-dtindex)

# Generar STOP.FD previ fi de seguiment (censura)
dades<-dades %>% mutate(
  STOP.FD=case_when(
    is.na(temps.STOP.FD)~NA_real_,
    temps.STOP.FD<temps_seguiment-92 ~1,
    temps.STOP.FD>temps_seguiment-92 ~0)) 

# Stop 24m (Només d'aquells amb seguiment mínim a 24 mesos)
dades<-dades %>% mutate(
  STOP24m.FD=case_when(
  is.na(temps.STOP.FD)~"No dispesación",
  temps.STOP.FD<730 & temps_seguiment>=730 ~"Si Stop<24meses",
  temps.STOP.FD>=730 & temps_seguiment>=730 ~"+24m")) 

# Stop 12m (Només d'aquells amb seguiment mínim 12 mesos)
dades<-dades %>% mutate(
  STOP12m.FD=case_when(
    is.na(temps.STOP.FD)~"No dispesación",
    temps.STOP.FD<365 & temps_seguiment>=365 ~"Si Stop<12meses",
    temps.STOP.FD>=365 & temps_seguiment>=365 ~"+12meses"))

# Stop 6m (Només d'aquells amb seguiment mínim 6 mesos)
dades<-dades %>% mutate(
  STOP6m.FD=case_when(
    is.na(temps.STOP.FD)~"No dispesación",
    temps.STOP.FD<182 & temps_seguiment>=182 ~"Si Stop<6m",
    temps.STOP.FD>=182 & temps_seguiment>=182 ~"+6m"))


# Genero objecte surv Stop tractament
# Si no hi ha dispensació poso =0 dies en temps.STOP.FD 
dades<-dades %>% mutate(
  temps.STOP.FD=if_else(is.na(temps.STOP.FD),dtindex-dtindex,temps.STOP.FD))

dades$STOP.FD_surv<-Surv(dades$temps.STOP.FD,dades$STOP.FD)


# 7.2. Calcular datafi OT On treatment (datafiOT)  ----------------
# Definition of follow-up period and premature discontinuation
# Death, the switch or addition of a new antidiabetic treatment, last billing of study drugs before 24 months after prescription, transfers to non-ICS centers.

# CanviTx (data minima de totes)
dades<-dades %>% mutate(datafiOT= case_when(
  grup=="IDPP4" ~ pmin(STOP.FD.IDPP4,CANVITX.iSGLT2,CANVITX.SU,na.rm=T),
  grup=="ISGLT2" ~ pmin(STOP.FD.iSGLT2,CANVITX.IDPP4,CANVITX.SU,na.rm=T),
  grup=="SU" ~ pmin(STOP.FD.SU,CANVITX.iSGLT2,CANVITX.IDPP4,na.rm=T)))

# Actualitzar a maxim 24 mesos o fi de seguiment (exitus/trasllat/31122017)
dades<-dades %>% mutate(datafiOT= case_when(
  datafiOT-dtindex>=365 ~ dtindex+365,
  datafiOT-dtindex<=365 ~ datafiOT)) %>% mutate (
    case_when(
      (datafiOT-dtindex<=365) & (situacio=="D" | situacio=="T") ~ pmin(datafi_seguiment,datafiOT,na.rm=T),
      TRUE~datafiOT))


descrTable(grup~STOP24m.FD+STOP12m.FD+STOP6m.FD, data=dades)


# 7.3. Calcular events coma Surv: ---------------


# Funció generar_Surv Generar columna Surv a partir de dades, event ("20150531"), dtindex, sortida(20171231), 
generar_Surv<-function(dt,event){
# dt<-dades
# event<-"EV.AMPU"
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

llista_events<-semi_join(tibble(id=llista_events), data_frame(id=names(dades)),by="id") %>% pull(id)

# Genera dades_surv
dades_surv<-map(llista_events,~generar_Surv(dt=dades,.)) %>% 
  as.data.frame()
# Fusiona amb dades  
dades<-dades %>% cbind(dades_surv)


# 8. Generar variables grup indicadora (en relació a la resta) ---------

dades<-make_dummies(dades,"grup","grup_")

# 9. Labels  -------------
dades<-etiquetar(dades,taulavariables = conductor_variables,camp_descripcio = "descripcio")
dades<-etiquetar_valors(dades,variables_factors = conductor_variables,fulla="value_labels",camp_etiqueta = "etiqueta")

# 10. FActoritzar -------------
dades<-factoritzar.NO.YES(dades,columna = "factoritzar.yes.no",taulavariables = conductor_variables)
dades<-factoritzar(dades,variables=extreure.variables("factoritzar",conductor_variables))


# 11 Verificació de seguiment de farmacs  --------------

dt_historic_fd_sgaps<-dt_historic_fd_sgaps %>% semi_join(dades,by="idp")

# Verificació de duració del tractament dispensat mitjançant historic de faramacs
set.seed(111)
dt_historic<-dt_historic_fd_sgaps %>% mostreig_ids(n_mostra=20)
mapeig_farmacs1<-MAP_ggplot(dt_historic,datainicial = "dat",datafinal = "datafi",id="idp",grup_color = "GRUP")

mapeig_farmacs1

dt_historic2<-dt_historic_fd %>% semi_join(dt_historic,by="idp")
mapeig_farmacs2<-MAP_ggplot(dt_historic2,datainicial = "dat",datafinal = "datafi",id="idp",grup_color = "GRUP")

mapeig_farmacs2


# Salvar objectes ----------
# output_Rdata<-"Output_metplus.RData"



save(flow_global,flow_global2,taula1,taula1.2,taulaPS,dades,mapeig_farmacs1,mapeig_farmacs2,file=here::here("resultats",output_Rdata))


#

