
####################################################  -----------------------------
analisis_OT<-TRUE
mostra_test=TRUE

if (mostra_test) mostra="test" else mostra="global"
if (analisis_OT) tipoanalisis="On Treatment" else tipoanalisis="Intention to treat (ITT)"

## 1. Lectura 

rmarkdown::render(input="./codi/1_LECTURA_METPLUS.Rmd",
                  params = list(analisis_OT= analisis_OT,mostra=mostra))

## 2. Preparació
rmarkdown::render(input="./codi/2_PREPARACIO_METPLUS.Rmd",
                  params =  list(analisis_OT= analisis_OT,mostra=mostra))

## 3. RESULTATS 
rmarkdown::render(input="./codi/3_RESULTATS_METPLUS.Rmd",output_file="OUTPUT_test_OT",
                  params = list(
                    analisis_OT= analisis_OT,
                    mostra=mostra,
                    analisis=tipoanalisis))

#################################################### ----------------------------