
gc()
rm(list=ls())

load("dades/preparades/dadesmapeig.RData")



#   SOURCE
# CArrego funcions -------------------
link_source<-paste0("https://github.com/jrealgatius/Stat_codis/blob/master/funcions_propies.R","?raw=T")
devtools::source_url(link_source)

LLEGIR.CLINIQUES<-function(n=Nmostra) {
  readRDS(directori_de_dades %>% here::here("METPLUS_entregable_variables_analitiques_20181126_190346.rds"))%>% as_tibble() %>% head(n) }
# 
# LLEGIR.FX.FACTURATS<-function(n=Nmostra) {
#   readRDS(directori_de_dades %>% here::here("METPLUS_entregable_farmacs_facturats_20181126_190346.rds"))%>% as_tibble() %>% head(n) }
# 
# LLEGIR.FX.PRESCRITS<-function(n=Nmostra) {
#   readRDS(directori_de_dades %>% here::here("METPLUS_entregable_farmacs_prescrits_20181126_190346.rds"))%>% as_tibble() %>% head(n) }
# 

library(lubridate)

# Convertir facturats --> prescrits a 30 dies per envas 
formatar_facturats<-function(dt=dt_facturats) {
  dt<-dt %>%
    mutate(dat=lubridate::ymd(paste0(as.character(dat),"15")),dbaixa=dat+(30*env),
           dat=data.to.string(dat) %>% as.numeric(),
           dbaixa=data.to.string(dbaixa) %>% as.numeric())
  dt
}

formatar_prescrits<-function(dades=dt_prescrits) {
  agregar_solapaments_gaps(dt=dades,id="idp",datainici =
                             "dat",datafinal = "dbaixa",gap=0)
}


# Lectura i formateig de dades facturació i prescripció
dt_facturats<-dt_facturats %>% semi_join(conductor_farmacs,by="cod") %>% 
  semi_join(select(dades,idp)) %>% 
  formatar_facturats() %>% 
  left_join(conductor_farmacs,by="cod") %>% 
  transmute(idp,agr=GRUP,dat=ymd(dat),dbaixa=ymd(dbaixa))

dt_prescrits<- dt_prescrits %>% semi_join(conductor_farmacs,by="cod") %>% 
  semi_join(select(dades,idp)) %>% 
  left_join(conductor_farmacs,by="cod") %>% 
  transmute(idp,agr=GRUP,dat=ymd(dat),dbaixa=ymd(dbaixa))


# Afegeixo dades de dades com la data index i data fi i data fiOT i grup 
dt_cliniques_historic<-dt_cliniques_historic %>% 
  left_join(select(dades,idp,dtindex,grup,datafi_seguiment,datafiOT,HBA1C.valor,HBA1C.valor,HBA1C.valor324m))

# Seleccionar N individus i Agregador HBA1c
dt_individus<-dt_cliniques_historic %>% 
  filter(agr=="HBA1C") 
  
# Mapejar historics d'un pacient + info extra periode ITT periode OT 


### Funció que retorna un gràfic amb l'evolució de la HB  el periode ITT el periode OT i els farmacs
  
mapeig_metplus<-function(set_seed=1){

  set_seed<-set_seed+rnorm(1,0,10)
    
  dades_id<-dt_individus %>% 
    mostreig_ids(n_mostra = 1,set_seed=set_seed)
  
  porpo<- dades_id %>% 
    ggplot(aes(x =dat,y =idp,color=idp))+
    geom_line(aes(dat, val))+
    geom_point(aes(dat, val),color="black") +
    theme(legend.position="top",legend.background = element_rect(fill="gray80",size=1, linetype="solid", colour ="black"))+
    geom_text(vjust = -0.5, hjust=0, size = 2,aes(x =dat, y =val, label = paste(round(val, 2),""))) + 
    ylim(5.5,11) +
    geom_segment(data=dades_id,aes(x =dtindex, xend=datafi_seguiment, y =10.5, yend = 10.5,colour = "Periode ITT"),size = 3)+
    geom_text(vjust = -0.75, hjust=0, size = 3,aes(x =dtindex, y =10.5,label = paste(round(HBA1C.valor, 2),""))) +
    geom_text(vjust = -0.75, hjust=0, size = 3,aes(x =datafi_seguiment, y =10.5,label = paste(round(HBA1C.valor324m, 2),""))) +
    geom_segment(aes(x =dtindex, xend=datafiOT, y =10.3, yend = 10.3,colour = "Periode OT"),size = 3) +
    geom_text(vjust = -0.50, hjust=0, size = 3,aes(x =dtindex, y =11,label = paste("Grup: ",grup))) 
  
  # Mapejar farmacs del mateix pajaro
  dades_fac <-dt_facturats %>% semi_join(dades_id,by="idp")
  dades_pre<- dt_prescrits %>% semi_join(dades_id,by="idp")
  dades_farmacs<- mutate(dades_fac,tipus="Facturats") %>% bind_rows(mutate(dades_pre,tipus="Prescrits"))
  
  dades_farmacs<-dades_farmacs %>% unite(id_kk,c(agr,tipus),sep="_",remove = F)
  
  # Fusionar facturats i prescrits
  
  porpo + 
    geom_segment(data=dades_farmacs,aes(x =dat, xend=dbaixa, y =10, yend = 10,color=agr, linetype=tipus),size=1)
  
  
  }



mapeig_metplus()






