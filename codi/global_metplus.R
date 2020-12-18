
## Funcions FOREST_PLOT customitzades pel met_plus ----

forest_MET<-function(dt_temp=taula_estimacions_GRUP,grupfarmac="grupISGLT2",titul="DPP4i vs. SGLT-2i",color=TRUE, llista_out=llista_outcomes){
  
  # dt_temp=taula_estimacions_GRUP
  # grupfarmac="grupSU"
  # titul="DPP4i  vs.  SU"
  # color=TRUE
  # llista_out=llista_outcomes
  
  dt_temp<-dt_temp %>% semi_join(llista_out)
  
  # Preparar taula filtrant per grup i seleccionar camps
  dt_temp<-dt_temp %>% filter(categoria==grupfarmac) %>% 
    transmute(datos,type,outcome,estimate=Std_Coefficient,Linf=CIStd_low,Lsup=CIStd_high)
  
  # Cambiar etiquetes outcomes i tipo d'anàlisis 
  dt_temp<-etiquetar_taula(dt_temp,camp="outcome",taulavariables = conductor_variables)
  dt_temp<-etiquetar_taula(dt_temp,camp="datos",taulavariables = conductor_variables)
  dt_temp<-etiquetar_taula(dt_temp,camp="type",taulavariables = conductor_variables)
  
  # Ploteja
  forest.plot.met(dt_temp,mean="estimate",lower="Linf",upper="Lsup", color=color)+
    ggtitle(titul) +theme(plot.title = element_text(hjust = 0.5)) 
  
}


forest.plot.met<-function(dadesmodel=dt_temp,mean="estimate",lower="Linf",upper="Lsup",label_X="Effect size (CI)",
                         intercept=0,
                         nivell="outcome", factor1="type",factor2="datos", color=TRUE) {
  # dadesmodel=dt_temp
  # mean="estimate"
  # lower="Linf"
  # upper="Lsup"
  # label_X="Effect size (95%CI)"
  # color=TRUE
  # intercept=0
  # nivell="outcome"
  # factor1="type"
  # factor2="datos"

  # Generar data set 
  dadesmodel <- dadesmodel %>% select(valor=!!mean,Linf=!!lower,Lsup=!!upper,nivell=!!nivell, factor1=!!factor1,factor2=!!factor2)
  
  ## Preparar taula (Genero etiqueta)
  taula_betas<-dadesmodel %>% mutate(etiqueta=paste0("     ",factor2," ",factor1),
                                     Method = paste0(factor2," ",factor1))

  ## factoritzar per manetnir l'ordre
  taula_betas$nivell <- factor(taula_betas$nivell, levels=unique(taula_betas$nivell))
  
  # Afegir fila com un punt nivell per outcome i genero label de group
  taula_betas<-taula_betas %>% split(.$nivell) %>% 
    map_dfr(~add_row(.x,.before = 0),.id = "outcome" ) %>% 
    mutate (etiqueta2=if_else(is.na(etiqueta),outcome,"")) %>% 
    mutate (etiqueta=if_else(is.na(etiqueta),outcome,etiqueta))
  
  # AFegir etiqueta 3 mes centrada
  taula_betas<-taula_betas %>% mutate(etiqueta3=lag(etiqueta2),
                                      etiqueta3=if_else(is.na(etiqueta3),"",etiqueta3),
                                      etiqueta4=lag(etiqueta3),
                                      etiqueta4=if_else(is.na(etiqueta4),"",etiqueta4))
 
  # Generar id 
  taula_betas<-taula_betas %>% mutate(id=seq(n())) %>% mutate(id=n()-id+1)

  # REomplir missings en factor1 i factor2
  taula_betas<-taula_betas %>% fill(c(factor1,factor2,Method),.direction="updown")

  # Relevel mateix ordre tal com surt taula   
  ordre_levels<-taula_betas %>% pull(Method) %>% unique()
  taula_betas$Method<-factor(taula_betas$Method, levels = ordre_levels)
  
  fp <- ggplot(data=taula_betas,aes(x=id, y=valor, ymin=Linf, ymax=Lsup)) +
    geom_errorbar(size=0.2, width=0.5) +
    geom_hline(yintercept=intercept, lty=1) +  # add a dotted line at x=1 after flip
    coord_flip(ylim=c(-0.8,0.6)) +  # flip coordinates (puts labels on y axis)
    xlab("Outcome") + ylab(label_X) +
    scale_x_continuous(breaks=taula_betas %>% pull(id),labels=taula_betas %>% pull(etiqueta4))+
    theme_minimal() + theme(axis.text.y = element_text(hjust = 0,vjust=0,size=10)) 
  
  if (color) {fp<-fp + geom_point(aes(color=Method),size=3)} else 
    {fp<-fp + geom_point(aes(shape=Method),size=3)}
  
  # Add banda d'error
  fp <- fp +  annotate("rect", xmin = 0, xmax = Inf, ymin = intercept -0.1, ymax = intercept+ 0.1, fill="grey",alpha=0.3) + 
    geom_rect(aes(xmin = 0, xmax = Inf, ymin = intercept -0.1, ymax = intercept+ 0.1,fill="* Small< |0.10|"),colour=NA,alpha=0.01) +
     scale_fill_manual('Effect size band',values = 'grey', guide = guide_legend(override.aes = list(alpha = 0.6))) 
  
  # limits i marques
  fp + ylim(-0.8,0.6) + scale_y_continuous(breaks = seq(-0.8,0.6,0.2) %>% round(1))

  
}


## Funció per render analisis
arguments_render<-function(analisis_OT=T,mostra_test=T) {
  gc()
  if (mostra_test) mostra<<-"test" else mostra<<-"global"
  if (analisis_OT) {
    tipoanalisis<<-"On Treatment"
    nom<<-"OT"
  } else {
    tipoanalisis<<-"Intention to treat (ITT)"
    nom="ITT"}
  nom_output<<-paste0("OUTPUT_",mostra,"_",nom)
  analisis_OT<<-analisis_OT
  }



