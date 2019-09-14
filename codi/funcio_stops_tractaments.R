
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



dades_test<-tibble::tibble(idp=c(1,1,1,2,2,3,3,3),
                           data=c("20190101","20190110","20190105","20190102","20190115","20190109","20190115","20190117"),
                           datafi=c("20190103","20190115","20190125","20190114","20190120","20190110","20190120","20190119"),
                           grup=c("A","A","A","B","B","B","B","B"))

dades_test<-dades_test %>% mutate(data2=as.Date(data,format="%Y%m%d"),datafi2=as.Date(datafi,format="%Y%m%d"))

MAP_ggplot(dades=dades_test,datainicial = "data2",datafinal = "datafi2",id="idp")

dades_test
dades_noves<-agregar_solapaments_gaps(dt=dades_test,id="idp",datainici="data2",datafi="datafi2",gap=2)
dades_noves

MAP_ggplot(dades=dades_noves,datainicial = "data2",datafinal = "datafi2",id="idp")

MAP_ggplot_univariant(dades=dades_noves,datainicial = "data2",datafinal = "datafi2",id="idp")

MAP_ggplot_univariant(dades=dades_test,datainicial = "data2",datafinal = "datafi2",id="idp")

# Dibuixa mapa temporal univariant per verificar solapaments
MAP_ggplot_univariant<-function(dades=dt,datainicial="data",datafinal="datafi",id="idp_temp") {
  
  # dades=dades_test
  # datainicial="data"
  # datafinal="datafi"
  # id="idp"

  # Conversió a Sym per evaluació  
  datainicial<-rlang::sym(datainicial)
  datafinal<-rlang::sym(datafinal)
  id<-rlang::sym(id)

  # Calculo dies de duració  
  dades<-dades %>%  mutate(dia0=!!datainicial,diaf=!!datafinal,days_duration=diaf-dia0)
  
  # Gráfico el tema
  ggplot2::ggplot(dades,ggplot2::aes(x =dia0,y =!!id))+
    ggplot2::geom_segment(ggplot2::aes(x =dia0, xend=diaf, y =!!id, yend = !!id),arrow =  ggplot2::arrow(length = ggplot2::unit(0.03, "npc"))) +
    ggplot2::geom_point(ggplot2::aes(dia0, !!id)) + 
    ggplot2::geom_text(vjust = -0.5, hjust=0, size = 3, ggplot2::aes(x =dia0, y = !!id,label = paste(round(days_duration, 2), "days")))+
    ggplot2::scale_colour_brewer(palette = "Set1")+
    ggplot2::theme(legend.position="top",legend.background =  ggplot2::element_rect(fill="gray80",size=1, linetype="solid", colour ="black"))
}



MAP_ggplot2<-function(dades=dt,datainicial="data",datafinal="datafi",id="idp_temp",grup_color=NA,grup_linea=NA,lim_inf=-Inf,lim_sup=Inf) {

  # dades=dades_test
  # datainicial="data"
  # datafinal="datafi"
  # id="idp"

  # grup_color="sexe"
  # grup_linea="sc_bcn"
  # lim_inf=-Inf
  # lim_sup=+Inf
  
  if (is.na(grup_linea)) dades<- dades %>% mutate(Overall="Overall")
  if (is.na(grup_linea)) grup_linea<- "Overall"
  
  if (is.na(grup_color)) dades<- dades %>% mutate(Overall2="Overall2")
  if (is.na(grup_color)) grup_color<- "Overall2"
  
  # # Configuro limits finestra
  if (lim_inf==-Inf) porca1<-min(dades %>% pull(datainicial))
  if (lim_sup==+Inf) porca2<-max(dades %>% pull(datafinal))
  # # 
  if (lim_inf!=-Inf) porca1<-lim_inf
  if (lim_sup!=+Inf) porca2<-lim_sup
  
  # Conversió a Sym per evaluació  
  datainicial<-rlang::sym(datainicial)
  datafinal<-rlang::sym(datafinal)
  id<-rlang::sym(id)
  grup_color<-rlang::sym(grup_color)
  grup_linea<-rlang::sym(grup_linea)
  
  # Calculo dies de duració  
  dades<-dades %>%  mutate(dia0=!!datainicial,diaf=!!datafinal,days_duration=diaf-dia0)
  
  # Gráfico el tema
  ggplot2::ggplot(dades,ggplot2::aes(x =dia0,y =!!id, color=!!grup_color,group=!!grup_linea,linetype=!!grup_linea))+
    ggplot2::geom_segment(ggplot2::aes(x =dia0, xend=diaf, y =!!id, yend = !!id),arrow =  ggplot2::arrow(length = ggplot2::unit(0.03, "npc"))) +
    ggplot2::geom_point(ggplot2::aes(dia0, !!id)) + 
    ggplot2::geom_text(vjust = -0.5, hjust=0, size = 3, ggplot2::aes(x =dia0, y = !!id,label = paste(round(days_duration, 2), "days")))+
    ggplot2::scale_colour_brewer(palette = "Set1")+
    ggplot2::xlim(porca1,porca2)+
    ggplot2::theme(legend.position="top",legend.background =  ggplot2::element_rect(fill="gray80",size=1, linetype="solid", colour ="black"))
  
}

