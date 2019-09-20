
## Generar una funció que elimini solapaments i discontinuitats per individu d'un grup de farmacs


## la data index i finestra serveix per filtrar

dades_test<-tibble::tibble(idp=c(1,1,1,2,2,3,3),data=c(1,10,5,3,15,9,15),datafi=c(3,15,25,4,20,10,20))
dades_test<-tibble::tibble(idp=c(1,1,1,2,2,3,3),data=c(1,10,5,2,15,9,15),datafi=c(3,15,25,14,20,10,20))

dades_test<-tibble::tibble(idp=c(1,1,1,2,2,3,3),
                           data=c("20190101","20190110","20190105","20190102","20190115","20190109","20190115"),
                           datafi=c("20190103","20190115","20190125","20190114","20190120","20190110","20190120"))

dades_test<-dades_test %>% mutate(data2=as.Date(data,format="%Y%m%d"),datafi2=as.Date(datafi,format="%Y%m%d"))


# Historic de farmacs: idp, datinici,datafi, gap
# Elimina solapaments i discontinuitats petites i retorna dades sense discontinuitats ni solapaments amb igual o menys registres
# Retorna dades amb : id, datainici i datafi amb menys registres, havent eliminat solapaments i gaps (discontinuitat petita)

agregar_solapaments_gaps<-function(dt=dades,id="idp",datainici="data",datafinal="datafi",gap=5){
  
  # dt=dades_test
  # gap=10
  # datainici="data2"
  # datafinal="datafi2"
  # id="idp"

  # Conversió a Sym per evaluació  
  datainici_sym<-rlang::sym(datainici)
  datafinal_sym<-rlang::sym(datafinal)
  idp_sym=rlang::sym(id)
    
  # Seleccionar dades necessaries amb noms sense sym
  dt<-dt %>% select(idp=!!idp_sym, data=!!datainici_sym,datafi=!!datafinal_sym)
  
  # MAP_ggplot(dades=dt,datainicial="data",datafinal="datafi",id="idp",grup_color=NA,grup_linea=NA)
  dt<-dt %>% 
    arrange(idp,data,datafi) %>% 
    group_by(idp) %>% mutate(gap1=(data-lag(datafi))) %>% 
    mutate(gap2=case_when(gap1 > gap ~1, TRUE ~0)) %>% 
    group_by(idp) %>% 
    mutate(gap3=(cumsum(gap2)))%>%ungroup()
  
  # Agregate 
  dt2<-dt %>%  mutate (idp2=idp) %>% 
    select(idp,data,datafi,gap3,idp,idp2) %>%
    group_by(idp,gap3)%>%
    summarise(datainici= min(data), datafi= max(datafi),idp2=min(idp2)) %>% 
    ungroup() 
  
  # MAP_ggplot(dades=dt2,datainicial="data",datafinal="datafi",id="idp")
  dt2<-dt2 %>% select("idp","datainici","datafi") 
  
  # Renombro noms dels camps originals
  colnames(dt2)<-c(idp_sym,datainici_sym,datafinal_sym)

  dt2
  
}



dades_test<-tibble::tibble(idp=c(1,1,1,1,2,2,3,3,3,3,3),
                           data=c("20181201","20190101","20190110","20190105","20190102","20190120","20190109","20190115","20190117","20190101","20181101"),
                           datafi=c("20190201","20190103","20190115","20190125","20190114","20190130","20190110","20190120","20190119","20190201","20181201"),
                           grup=c("B","A","A","A","B","B","B","B","B","B","B"))

dades_test<-dades_test %>% mutate(data2=as.Date(data,format="%Y%m%d"),datafi2=as.Date(datafi,format="%Y%m%d"))


MAP_ggplot_univariant(dades_test,datainicial = "data2",datafinal = "datafi2",id="idp",Nmostra = Inf)
dades_agr<-agregar_solapaments_gaps(dt=dades_test,id="idp",datainici = "data2",datafinal="datafi2",gap=30)
MAP_ggplot_univariant(dades_agr,datainicial = "data2",datafinal = "datafi2",id="idp",Nmostra = Inf)





# Filtrar per registres que inicien prescripció fora de la finestra (24 mesos)
# FX.FACTURATS_PRESCRITS_GRUPS<-FX.FACTURATS_PRESCRITS_GRUPS %>% filter(dat<=dtindex+finestra)

FX.FACTURATS_PRESCRITS_GRUPS<-FX.FACTURATS_PRESCRITS_GRUPS %>% 
  filter(idp=="00a1c12dce4f3e85c317c84bfd0f858ea7d3c68a" & GRUP=="IDPP4") 

gap_dies<-35
farmacs_dt_sense_gaps<-agregar_solapaments_gaps(FX.FACTURATS_PRESCRITS_GRUPS,id="idp",datainici = "dat",datafinal="datafi",gap=gap_dies)

MAP_ggplot_univariant(FX.FACTURATS_PRESCRITS_GRUPS %>% filter(GRUP=="IDPP4"),datainicial = "dat",datafinal = "datafi",id="idp",Nmostra = 1)
MAP_ggplot_univariant(farmacs_dt_sense_gaps ,datainicial = "dat",datafinal = "datafi",id="idp",Nmostra = 1)


