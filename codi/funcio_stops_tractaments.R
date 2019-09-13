
# Historic de farmacs amb idp, agr,dat,datafi,gap 

stops_tractaments<-function(dt=dades,gap=14,finestraX=c(NA,NA)){
  
  # dt=FX.FACTURATS_PRESCRITS_GRUPS
  # K=60
  # Nmostra=Inf
  # finestraX=c(0,365)
  # llavor=123
  
  # Si Nmostra es infinit o mes gran que la mostra agafo el màxim
  Nmostra_maxim<- dt %>% distinct(idp) %>% nrow()
  if (Nmostra==Inf | Nmostra>Nmostra_maxim) Nmostra<- Nmostra_maxim
  
  farmacs_list<-dt %>%distinct(agr)%>%dplyr::pull()
  
  dt<-dt%>% mutate(agr=factor(agr))
  set.seed(llavor) # S'ha d'actualitzar 
  id_sample<-dt %>% distinct(idp) %>%sample_n(size=Nmostra) 
  
  dt<-id_sample %>% left_join(dt,by="idp") 
  
  # Si no existeix tipus 
  if ("tipus"%in%colnames(dt)==F) dt$tipus="Global"
  
  dt<-dt%>%select(idp,agr,data=dat,datafi,FACTPRESC=tipus)   
  
  # Calculo dies de duració  
  dt<-dt %>% 
    mutate(
      data=lubridate::ymd(data),
      datafi=lubridate::ymd(datafi),
      days_duration=lubridate::interval(data,datafi) %>% lubridate::as.duration()/lubridate::ddays())
  
  dt<-dt %>% mutate (idp2=idp, idp=paste0(idp,agr,".",str_sub(FACTPRESC,1,1)))
  
  dt<-dt%>%select(idp,agr,data,datafi,days_duration,idp2,FACTPRESC)  
  # Genera mapa origen (n) 
  
  dt<-dt %>% mutate (idp_temp=paste0(stringr::str_sub(dt$idp,1,6),agr,".",str_sub(FACTPRESC,1,1)))
  
  
  if (is.na(finestraX[1]))  porca1<-lubridate::ymd(min(dt$data))
  if (is.na(finestraX[2]))  porca2<-lubridate::ymd(max(dt$datafi))
  if (!is.na(finestraX[1])) porca1<-lubridate::ymd(finestraX[1])
  if (!is.na(finestraX[2])) porca2<-lubridate::ymd(finestraX[2])
  
  dt<-dt %>% mutate(datafi =case_when(porca2<=datafi ~ porca2,TRUE ~ datafi))
  
  # Recalcular intervals en dies a partir de les finetres!
  dt<-dt%>%mutate(days_duration=lubridate::interval(data,datafi)%>%lubridate::as.duration()/lubridate::ddays())
  
  MAP<-MAP_ggplot(dades=dt,datainicial="data",datafinal="datafi",id="idp_temp",grup_color="agr",grup_linea="FACTPRESC",lim_inf=porca1,lim_sup=porca2)
  
  
  dt<-dt%>%arrange(idp,data,datafi)
  dt<-mutate(dt,data=ymd(data),datafi=ymd(datafi))
  dt<-dt%>%group_by(idp)%>% mutate(gap=(data-lag(datafi)))
  dt<-dt%>%mutate(gap2=case_when(gap>K ~1, TRUE ~0))
  dt<-dt%>%group_by(idp)%>%mutate(gap3=(cumsum(gap2)))%>%ungroup()
  
  # Agregate 
  dt2<-dt %>% 
    select(idp,data,datafi,gap3,agr,idp2, FACTPRESC) %>%
    group_by(idp,agr,gap3)%>%
    summarise(data= min(data), datafi= max(datafi),idp2=min(idp2),FACTPRESC=min(FACTPRESC))%>% 
    ungroup
  # 
  
  # Tornem a Recalcular intervals en dies a partir dels Gaps i Fienstra!. 
  dt2<-dt2%>%mutate(days_duration=interval(data,datafi)%>%as.duration()/ddays())
  
  
  dt2<-dt2 %>% mutate(idp_temp=paste0(stringr::str_sub(dt2$idp,1,6),agr,".",str_sub(FACTPRESC,1,1)))
  
  
  MAP2<-MAP_ggplot(dades=dt2,datainicial="data",datafinal="datafi",id="idp_temp",grup_color="agr",grup_linea="FACTPRESC",lim_inf=porca1,lim_sup=porca2)
  
  
  
  #MAP2
  
  dt2<-dt2 %>% select(idp2,idp,agr,data,datafi,FACTPRESC)
  
  #dt2
  
  list(dades1=dt,dades2=dt2,Mapa_pre=MAP,Mapa_post=MAP2)
  
  
}