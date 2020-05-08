
## Funcions customitzades pel met_plus ----

forest.plot.met<-function(dadesmodel=dt_estimacions,mean="estimate",lower="Linf",upper="Lsup",label_X="OR (98% CI)",
                         intercept=0,
                         nivell="outcome", factor1="type",factor2="datos", color=TRUE) {
  
  # dadesmodel=dt_temp
  # mean="estimate"
  # lower="Linf"
  # upper="Lsup"
  # label_X="Effect size (95%CI)"
  # color=TRUE

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
    geom_pointrange(size=0.2) + 
    geom_hline(yintercept=intercept, lty=1) +  # add a dotted line at x=1 after flip
    coord_flip() +  # flip coordinates (puts labels on y axis)
    xlab("Outcome") + ylab(label_X) +
    scale_x_continuous(breaks=taula_betas %>% pull(id),labels=taula_betas %>% pull(etiqueta4))

  fp<-fp + theme_minimal() + theme(axis.text.y = element_text(hjust = 0,vjust=0,size=10)) 
  
  if (color) {fp<-fp + geom_point(aes(color=Method),size=3)} else 
    {fp<-fp + geom_point(aes(shape=Method),size=3)}
  
  # Add banda d'error
  fp<-fp + geom_hline(yintercept = c(intercept+0.1,intercept-0.1),linetype=2)
  
  
  fp + ylim(-0.6,0.6)

  
}


