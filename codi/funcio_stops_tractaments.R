
## Generar una funció que elimini solapaments i discontinuitats per individu d'un grup de farmacs


## la data index i finestra serveix per filtrar



# Historic de farmacs: idp, datinici,datafi, gap
# Elimina solapaments i discontinuitats petites i retorna dades sense discontinuitats ni solapaments amb igual o menys registres
# Retorna dades amb : id, datainici i datafi amb menys registres, havent eliminat solapaments i gaps (discontinuitat petita)



# Historic de farmacs: idp, datinici,datafi, gap
# Elimina solapaments i discontinuitats petites i retorna dades sense discontinuitats ni solapaments amb igual o menys registres
# Retorna dades amb : id, datainici i datafi amb menys registres, havent eliminat solapaments i gaps (discontinuitat petita)

agregar_solapaments_gaps<-function(dt=dades,id="idp",datainici="data",datafinal="datafi",gap=5){
  
  # dt=FX.FACTURATS_PRESCRITS_GRUPS
  # gap=60
  # datainici="dat"
  # datafinal="datafi"
  # id="idp"
  
  # Conversió a Sym per evaluació  
  datainici_sym<-rlang::sym(datainici)
  datafinal_sym<-rlang::sym(datafinal)
  idp_sym=rlang::sym(id)
  
  # Seleccionar dades necessaries amb noms sense sym
  dt<-dt %>% select(idp=!!idp_sym, data=!!datainici_sym,datafi=!!datafinal_sym)
  
  
  # 1. Eliminar solapaments 
  dt<-dt %>% 
    group_by(idp) %>% 
    arrange(data) %>% 
    mutate(indx = c(0, cumsum(as.numeric(lead(data)) >
                                cummax(as.numeric(datafi)))[-n()])) %>%
    group_by(idp, indx) %>%
    summarise(data = min(data), datafi = max(datafi)) %>%
    select(-indx) 
  
  
  # 2. ELiminar Gaps (discontinuitats)
  
  # MAP_ggplot(dades=dt,datainicial="data",datafinal="datafi",id="idp",grup_color=NA,grup_linea=NA)
  dt<-dt %>% 
    mutate(gap1=(data-lag(datafi))) %>%
    mutate(gap2=case_when(gap1 > gap ~1, TRUE ~0)) %>%
    mutate(gap3=(cumsum(gap2))) %>% 
    ungroup()
  
  # 3. Agregate per idp-gap
  dt2<-dt %>%  mutate (idp2=idp) %>%
    select(idp,data,datafi,gap3,idp,idp2) %>%
    group_by(idp,gap3)%>%
    summarise(datainici= min(data), datafi= max(datafi),idp2=min(idp2)) %>%
    ungroup()
  
  dt2<-dt2 %>% select("idp","datainici","datafi")
  
  # MAP_ggplot(dades= dt2 %>% head(10),datainicial="datainici",datafinal="datafi",id=id)
  
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



