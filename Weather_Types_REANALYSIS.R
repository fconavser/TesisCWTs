##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################
#################################### SCRIPT COMO FUNCIÓN:#########################################
##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################

############################# 
# FUNCIÓN "weathertypes", EN LA QUE SÓLO HAY QUE RELLENAR LOS PARÁMETROS BÁSICOS DE ENTRADA.
#############################

# Le he metido una condicion para que si no metes la latitud en intervalos de 5 en 5 pues que falle.
# Ademas en esa condicion está que el punto central tiene que estar entre los 40º y 80º de latitud,
# porque el reanalisis del que absorve los datos sólo tiene disponibles de 30ºN a 90ºN, y así la malla
# puede cubrirlo.
install.packages("curl")
install.packages("data.table")
library(curl)
library(data.table)

weathertypes <- function(latp8,lonp8,start,finish,classification_method){
  ###########################
  ######### WARNING! DONT TOUCH! AUTOMATIC SCRIPT. (RUN ALL, UNTIL THE END)
  ###########################
  # Packages
  # install.packages("curl")
  # install.packages("data.table")
  library(curl)
  library(data.table)
  
  if(is.element(latp8,seq(40,80,by=5)) & is.element(lonp8,seq(-180,180,by=5))){
    interv_lon = 10
    interv_lat = 5
    start_date = as.Date(paste(substr(start,start=1,stop=4),"-",substr(start,start=5,stop=6),"-",substr(start,start=7, stop=8),sep=''), format='%Y-%m-%d')
    finish_date = as.Date(paste(substr(finish,start=1,stop=4),"-",substr(finish,start=5,stop=6),"-",substr(finish,start=7, stop=8),sep=''), format='%Y-%m-%d')
    row_number = difftime(finish_date,start_date)+1
    
    # Create Table
    Table = as.data.frame(array(NA, dim=c(row_number, 4)))
    
    # Write dates
    Table[,1] <- seq(start_date,finish_date,by="days")
    Table[,2] <- as.character(substr(Table[,1],start=1, stop=4))   #year
    Table[,3] <- as.character(substr(Table[,1],start=6, stop=7))   #month
    Table[,4] <- as.character(substr(Table[,1],start=9, stop=10))   #day
    
    # Download SLP points time series (p8 is the reference gridpoint position). NCAR Sea Level Pressure Data (https://climatedataguide.ucar.edu/climate-data/ncar-sea-level-pressure)
    p01 = paste("https://rda.ucar.edu/cgi-bin/dattore/gridts?sd=",start,"&ed=",finish,"&lat=",latp8+(2*interv_lat),"&latdir=N&lon=",lonp8+(0*interv_lon),"&londir=E&format=slp&dsnum=ds010.0&input=dailydata.bin&t=daily",sep='')
    p02 = paste("https://rda.ucar.edu/cgi-bin/dattore/gridts?sd=",start,"&ed=",finish,"&lat=",latp8+(2*interv_lat),"&latdir=N&lon=",lonp8+(1*interv_lon),"&londir=E&format=slp&dsnum=ds010.0&input=dailydata.bin&t=daily",sep='')
    
    p03 = paste("https://rda.ucar.edu/cgi-bin/dattore/gridts?sd=",start,"&ed=",finish,"&lat=",latp8+(1*interv_lat),"&latdir=N&lon=",lonp8-(1*interv_lon),"&londir=E&format=slp&dsnum=ds010.0&input=dailydata.bin&t=daily",sep='')
    p04 = paste("https://rda.ucar.edu/cgi-bin/dattore/gridts?sd=",start,"&ed=",finish,"&lat=",latp8+(1*interv_lat),"&latdir=N&lon=",lonp8+(0*interv_lon),"&londir=E&format=slp&dsnum=ds010.0&input=dailydata.bin&t=daily",sep='')
    p05 = paste("https://rda.ucar.edu/cgi-bin/dattore/gridts?sd=",start,"&ed=",finish,"&lat=",latp8+(1*interv_lat),"&latdir=N&lon=",lonp8+(1*interv_lon),"&londir=E&format=slp&dsnum=ds010.0&input=dailydata.bin&t=daily",sep='')
    p06 = paste("https://rda.ucar.edu/cgi-bin/dattore/gridts?sd=",start,"&ed=",finish,"&lat=",latp8+(1*interv_lat),"&latdir=N&lon=",lonp8+(2*interv_lon),"&londir=E&format=slp&dsnum=ds010.0&input=dailydata.bin&t=daily",sep='')
    
    p07 = paste("https://rda.ucar.edu/cgi-bin/dattore/gridts?sd=",start,"&ed=",finish,"&lat=",latp8+(0*interv_lat),"&latdir=N&lon=",lonp8-(1*interv_lon),"&londir=E&format=slp&dsnum=ds010.0&input=dailydata.bin&t=daily",sep='')
    p08 = paste("https://rda.ucar.edu/cgi-bin/dattore/gridts?sd=",start,"&ed=",finish,"&lat=",latp8+(0*interv_lat),"&latdir=N&lon=",lonp8+(0*interv_lon),"&londir=E&format=slp&dsnum=ds010.0&input=dailydata.bin&t=daily",sep='')
    p09 = paste("https://rda.ucar.edu/cgi-bin/dattore/gridts?sd=",start,"&ed=",finish,"&lat=",latp8+(0*interv_lat),"&latdir=N&lon=",lonp8+(1*interv_lon),"&londir=E&format=slp&dsnum=ds010.0&input=dailydata.bin&t=daily",sep='')
    p10 = paste("https://rda.ucar.edu/cgi-bin/dattore/gridts?sd=",start,"&ed=",finish,"&lat=",latp8+(0*interv_lat),"&latdir=N&lon=",lonp8+(2*interv_lon),"&londir=E&format=slp&dsnum=ds010.0&input=dailydata.bin&t=daily",sep='')
    
    p11 = paste("https://rda.ucar.edu/cgi-bin/dattore/gridts?sd=",start,"&ed=",finish,"&lat=",latp8-(1*interv_lat),"&latdir=N&lon=",lonp8-(1*interv_lon),"&londir=E&format=slp&dsnum=ds010.0&input=dailydata.bin&t=daily",sep='')
    p12 = paste("https://rda.ucar.edu/cgi-bin/dattore/gridts?sd=",start,"&ed=",finish,"&lat=",latp8-(1*interv_lat),"&latdir=N&lon=",lonp8+(0*interv_lon),"&londir=E&format=slp&dsnum=ds010.0&input=dailydata.bin&t=daily",sep='')
    p13 = paste("https://rda.ucar.edu/cgi-bin/dattore/gridts?sd=",start,"&ed=",finish,"&lat=",latp8-(1*interv_lat),"&latdir=N&lon=",lonp8+(1*interv_lon),"&londir=E&format=slp&dsnum=ds010.0&input=dailydata.bin&t=daily",sep='')
    p14 = paste("https://rda.ucar.edu/cgi-bin/dattore/gridts?sd=",start,"&ed=",finish,"&lat=",latp8-(1*interv_lat),"&latdir=N&lon=",lonp8+(2*interv_lon),"&londir=E&format=slp&dsnum=ds010.0&input=dailydata.bin&t=daily",sep='')
    
    p15 = paste("https://rda.ucar.edu/cgi-bin/dattore/gridts?sd=",start,"&ed=",finish,"&lat=",latp8-(2*interv_lat),"&latdir=N&lon=",lonp8+(0*interv_lon),"&londir=E&format=slp&dsnum=ds010.0&input=dailydata.bin&t=daily",sep='')
    p16 = paste("https://rda.ucar.edu/cgi-bin/dattore/gridts?sd=",start,"&ed=",finish,"&lat=",latp8-(2*interv_lat),"&latdir=N&lon=",lonp8+(1*interv_lon),"&londir=E&format=slp&dsnum=ds010.0&input=dailydata.bin&t=daily",sep='')
    
    # Download SLPdata series
    dir.create("./SLPdata/")
    download.file(url = p01, destfile = "./SLPdata/p01.txt", method = "wininet", quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))
    download.file(url = p02, destfile = "./SLPdata/p02.txt", method = "wininet", quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))
    download.file(url = p03, destfile = "./SLPdata/p03.txt", method = "wininet", quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))
    download.file(url = p04, destfile = "./SLPdata/p04.txt", method = "wininet", quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))
    download.file(url = p05, destfile = "./SLPdata/p05.txt", method = "wininet", quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))
    download.file(url = p06, destfile = "./SLPdata/p06.txt", method = "wininet", quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))
    download.file(url = p07, destfile = "./SLPdata/p07.txt", method = "wininet", quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))
    download.file(url = p08, destfile = "./SLPdata/p08.txt", method = "wininet", quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))
    download.file(url = p09, destfile = "./SLPdata/p09.txt", method = "wininet", quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))
    download.file(url = p10, destfile = "./SLPdata/p10.txt", method = "wininet", quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))
    download.file(url = p11, destfile = "./SLPdata/p11.txt", method = "wininet", quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))
    download.file(url = p12, destfile = "./SLPdata/p12.txt", method = "wininet", quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))
    download.file(url = p13, destfile = "./SLPdata/p13.txt", method = "wininet", quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))
    download.file(url = p14, destfile = "./SLPdata/p14.txt", method = "wininet", quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))
    download.file(url = p15, destfile = "./SLPdata/p15.txt", method = "wininet", quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))
    download.file(url = p16, destfile = "./SLPdata/p16.txt", method = "wininet", quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))
    
    # Load SLPdata series
    pointlist = list.files('./SLPdata/', pattern='.txt$')
	pointlist <- pointlist[1:16]
    contador = 5
    
    for(i in length(pointlist):1){
      print(paste("Point ", i," of ", length(pointlist)))
      archivo_txt = read.table(paste("./SLPdata/",pointlist[i],sep=''),header = TRUE, sep="\t",skip=4, row.names=NULL)
      archivo_txt2 = data.frame(do.call('rbind', strsplit(as.character(archivo_txt$DATE.......TIME................VALUE),'    ',fixed=TRUE)))
      archivo_txt3 = archivo_txt2[,1:2]
      archivo_txt3$X3 = as.numeric(as.character(archivo_txt3$X2))
      archivo_txt4=archivo_txt3[,c(1,3)]
      rm(archivo_txt,archivo_txt2,archivo_txt3)
      archivo_txt = archivo_txt4
      rm(archivo_txt4)
      archivo_txt[,"DATE"] <- substr(archivo_txt[,1],start = 1,stop = 10)
      archivo_txt=archivo_txt[,c(3,2)]
      archivo_txt <- archivo_txt[!(is.na(archivo_txt$X3)),]
      
      archivo_txt_agg = aggregate(archivo_txt[, 2], list(archivo_txt$DATE), mean)
      
      names(archivo_txt_agg)[1] <- "FECHA"
      names(archivo_txt_agg)[2] <- paste("P_",i,sep='')
      names(Table)[1] <- "FECHA"
      names(Table)[2] <- "YEAR"
      names(Table)[3] <- "MONTH"
      names(Table)[4] <- "DAY"
      
      Table$FECHA <- as.character(Table$FECHA)
      
      dt1 <- data.table(Table, key = "FECHA")
      dt2 <- data.table(archivo_txt_agg, key = "FECHA")
      union <- dt2[dt1]
      Table[,contador] <- as.data.frame(union)[,2]
      
      contador = contador + 1
    }
    
    # Names of variables
    for(i in 5:20){
      names(Table)[i]<-paste("P_",rev(seq(1:16))[i-4],sep='')
    }
    
    # Westerly flow (W)
    Table[,"W"] <- (0.5*(Table[,"P_12"]+Table[,"P_13"]))-(0.5*(Table[,"P_4"]+Table[,"P_5"]))
    
    for(i in 1:dim(Table)[1]){
      if(is.na(Table[i,"W"])){
        Table[i,"W"] <- Table[i,"W"]
      } else{
        if(Table[i,"W"]==0){
          Table[i,"W"] <- 0.001
        }
      }
    }
    
    # Southerly flow (S)
    Table[,"S"] <- (1/(cos(latp8*pi/180)))*((0.25*(Table[,"P_5"]+(2*Table[,"P_9"])+Table[,"P_13"]))-(0.25*(Table[,"P_4"]+(2*Table[,"P_8"])+Table[,"P_12"])))
    
    for(i in 1:dim(Table)[1]){
      if(is.na(Table[i,"S"])){
        Table[i,"S"] <- Table[i,"S"]
      } else{
        if(Table[i,"S"]==0){
          Table[i,"S"] <- 0.001
        }
      }
    }
    
    
    # Resultant flow (F)
    Table[,"F"] <- (Table[,"S"]^2 + Table[,"W"]^2)^0.5
    
    # Westerly shear velocity (ZW)
    Table[,"ZW"] <- (sin((latp8)*pi/180)/sin((latp8-interv_lat)*pi/180)) * ((0.5*(Table[,"P_15"]+Table[,"P_16"])) - (0.5*(Table[,"P_8"]+Table[,"P_9"]))) - (sin((latp8)*pi/180)/sin((latp8+interv_lat)*pi/180)) * ((0.5*(Table[,"P_8"]+Table[,"P_9"])) - (0.5*(Table[,"P_1"]+Table[,"P_2"])))
    
    # Southerly shear velocity (ZS)
    Table[,"ZS"] <- (1/(2*(cos(latp8*pi/180))^2)) * ((0.25*(Table[,"P_6"]+2*(Table[,"P_10"])+Table[,"P_14"])) - (0.25*(Table[,"P_5"]+2*(Table[,"P_9"])+Table[,"P_13"])) - (0.25*(Table[,"P_4"]+2*(Table[,"P_8"])+Table[,"P_12"])) + (0.25*(Table[,"P_3"]+2*(Table[,"P_7"])+Table[,"P_11"])))
    
    # Total shear velocity(Z)
    Table[,"Z"] <- Table[,"ZW"]+Table[,"ZS"]
    
    # Direction of flow (D)
    Table[,"D"] <- atan(Table[,"W"]/Table[,"S"])*180/pi  # Aunque suene raro que aquí sea 180*pi....dejarlo así, porque deben ser rollos de usar atan, en vez de tan y tal.
    
    print("The variables have been calculated")
    
    # Conditions for Angle(º) 4 "CONDITIONS ANGLE"
    # C.0 Si no hay dato, pon en angle <- NA
    # C.1 =SI(Y(W>0;S<0);360+D;"")
    # C.2 ==SI(Y(W>0;S>0);180+D;"")
    # C.3 =SI(Y(W<0;S>0);180+D;"")
    # C.4 =SI(Y(W<0;S<0);D;"")
    
    for(i in 1:dim(Table)[1]){
      valor_W <- Table[i,"W"]
      valor_S <- Table[i,"S"]
      if(is.na(Table[i,"W"]) | is.na(Table[i,"S"])){  ## C.0
        Table[i,"Angle"] <- NA
      } else{
        if(valor_W>0 & valor_S<0){
          Table[i,"Angle"] <- 360+Table[i,"D"]  ## C.1
        } else{
          if(valor_W>0 & valor_S>0){
            Table[i,"Angle"] <- 180+Table[i,"D"]    ## C.2
          } else{
            if(valor_W<0 & valor_S>0){
              Table[i,"Angle"] <- 180+Table[i,"D"]    ## C.3
            } else{
              if(valor_W<0 & valor_S<0){
                Table[i,"Angle"] <- Table[i,"D"]    ## C.4
              }
            }
          }
        }
      }
    }
    
    # Conditions for Direction of Advection (Rule 1)
    for(i in 1:dim(Table)[1]){
      if(is.na(Table[i,"Angle"])){
        Table[i,"Adv"] <- NA
      } else{
        if(Table[i,"Angle"]>337.5 | Table[i,"Angle"]<= 22.5){
          Table[i,"Adv"] <- "N"
        } else{
          if(Table[i,"Angle"]>22.5 & Table[i,"Angle"]<= 67.5){
            Table[i,"Adv"] <- "NE"
          } else{
            if(Table[i,"Angle"]>67.5 & Table[i,"Angle"]<=112.5){
              Table[i,"Adv"] <- "E"
            } else{
              if(Table[i,"Angle"]>112.5 & Table[i,"Angle"]<= 157.5){
                Table[i,"Adv"] <- "SE"
              } else{
                if(Table[i,"Angle"]>157.5 & Table[i,"Angle"]<= 202.5){
                  Table[i,"Adv"] <- "S"
                } else{
                  if(Table[i,"Angle"]>202.5 & Table[i,"Angle"]<= 247.5){
                    Table[i,"Adv"] <- "SW"
                  } else{
                    if(Table[i,"Angle"]>247.5 & Table[i,"Angle"]<=292.5){
                      Table[i,"Adv"] <- "W"
                    } else{
                      if(Table[i,"Angle"]>292.5 & Table[i,"Angle"]<=337.5){
                        Table[i,"Adv"] <- "NW"
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    
    # Rule 2
    # If |Z| is less than F, then the flow is issentially straight and corresponds to a Lamb pure directional type.
    for(i in 1:dim(Table)[1]){
      if(is.na(Table[i,"Z"]) | is.na(Table[i,"F"])){
        Table[i,"Rule2"] <- NA
      } else{
        if(abs(Table[i,"Z"])<Table[i,"F"]){
          Table[i,"Rule2"] <- "PureAdvective"
        }
      }
    }
    
    # Rule 3
    # If |Z| is greater than 2F, then the pattern is strongly cyclonic (Z>0) or anticyclonic (Z<0). This corresponds to Lamb's pure
    # cyclonic and anticyclonic type.
    for(i in 1:dim(Table)[1]){
      if(is.na(Table[i,"Z"]) | is.na(Table[i,"F"])){
        Table[i,"Rule3"] <- NA
      } else{
        if(abs(Table[i,"Z"])>(2*Table[i,"F"])){
          if(Table[i,"Z"]>0){
            Table[i,"Rule3"] <- "C"
          } else{
            Table[i,"Rule3"] <- "A"
          }
        }
      }
    }
    
    # Rule 4
    # If |Z| lies between F and 2F, the the flow is partly(anti) cyclonic and this corresponds to one of Lamb's synoptic/direction
    # hybrid types, e.g. AE (anticyclonic-east)
    for(i in 1:dim(Table)[1]){
      if(is.na(Table[i,"Z"]) | is.na(Table[i,"F"])){
        Table[i,"Rule4"] <- NA
      } else{
        if(   (Table[i,"F"]<abs(Table[i,"Z"])) & (abs(Table[i,"Z"]) < (2*Table[i,"F"])) & Table[i,"Z"]>0 ){
          Table[i,"Rule4"] <- "C"
        } else{
          if(   (Table[i,"F"]<abs(Table[i,"Z"])) & (abs(Table[i,"Z"]) < (2*Table[i,"F"])) & Table[i,"Z"]<0){
            Table[i,"Rule4"] <- "A"
          }
        }
      }
    }
    
    
    # Rule 5
    # If F is less than 6 and |Z| es less than 6, there is a light indeterminate flow corresponding to Lamb's unclassified type U.
    # The choice of 6 is dependent on the grid spacing and would need tuning if used with a finer grid resolution.
    # This rule is dominant front previous rules
    for(i in 1:dim(Table)[1]){
      if(is.na(Table[i,"Z"]) | is.na(Table[i,"F"])){
        Table[i,"Rule5"] <- NA
      } else{
        if(Table[i,"F"]<6 & abs(Table[i,"Z"])<6){
          Table[i,"Rule5"] <- "U"
        } else{
          Table[i,"Rule5"] <- NA
        }
      }
    }
    
    print("The rules have been calculated")
    
    ## ESE 6 MARTIN VIDE LO CAMBIA POR 4.8 F y 4.2 para Z (según LOS TIPOS SINÓPTICOS DE JENKINSON 
    ## & COLLISON Y LA INTENSIDAD DE LA ISLA DE CALOR BARCELONESA
    ## Javier MARTÍN-VIDE, Mª Carmen MORENO GARCÍA, Víctor M. ARTOLA y Mª José CORDOBILLA). no lo he aplicado porque no queda claro
    
    
    ############################## CÁLCULO DE LOS TIPOS DE TIEMPO
    
    # Hay 4 métodos para calcular los tipos de tiempo, en función de las clases que se introduzcan.:
    #   
    # El método 1 es el desarollado originalmente por Jekinson and Collison, que trabaja con 2 tipos de tiempo básicos (A,C), 8 tipos
    # direccionales (N,NE,E,SE,S,SW,W,NW), 16 tipos de tiempo híbridos (AN,ANE,AE,ASE,AS,ASW,AW,ANW, CN,CNE,CE,CSE,CS,CSW,CW,CNW) y
    # 1 tipo de tiempo Indeterminado (U), para aquellos días donde no se consiga <- "WT_JC"
    # 
    # El método 2 es el desarrollado por Trigo. Su clasificación es idéntica a la desarrollada por J&C, salvo con la diferencia de que
    # no incorpora el tipo de tiempo Indeterminado (U) <- "WT_Tri"
    # 
    # El método 3 es el desarrollado por Navarro. Su clasificación es idéntica a la desarrollada por J&C, salvo con la diferencia de que
    # incorpora un nuevo tipo de tiempo, el Anticiclónico Severo (A+) <- "WT_Nav"
    # 
    # El método 4 es un mix entre el desarrollado por Trigo, y por Navarro. Este no incorpora el tipo de tiempo Indeterminado (U), pero
    # sí incorpora el Anticiclónico Severo (A+) <- "WT_TriNav"
    
    
    # M.1 ORIGINALS (Jekinson and Collison, 1977) <- "WT_JC"
    for(i in 1:dim(Table)[1]){
      if(!is.na(Table[i,"Rule5"])){
        Table[i,"WT_JC"] <- "U"
      } else{
        if(!is.na(Table[i,"Rule3"])){
          Table[i,"WT_JC"] <- Table[i,"Rule3"]
        } else{
          if(!is.na(Table[i,"Rule4"])){
            Table[i,"WT_JC"] <- paste(Table[i,"Rule4"],Table[i,"Adv"],sep='')
          } else{
            if(!is.na(Table[i,"Rule2"])){
              Table[i,"WT_JC"] <- Table[i,"Adv"]
            }
          }
        }
      }
    }
    
    
    # M. 2 ORIGINALS WITHOUT UNCLASSIFIED (Trigo) -> "WT_Trigo"
    
    for(i in 1:dim(Table)[1]){
      if(!is.na(Table[i,"Rule3"])){
        Table[i,"WT_Tri"] <- Table[i,"Rule3"]
      } else{
        if(!is.na(Table[i,"Rule4"])){
          Table[i,"WT_Tri"] <- paste(Table[i,"Rule4"],Table[i,"Adv"],sep='')
        } else{
          if(!is.na(Table[i,"Rule2"])){
            Table[i,"WT_Tri"] <- Table[i,"Adv"]
          }
        }
      }
    }
    
    
    # M. 3 ORIGINALS WITH SEVERE ANTICYCLONIC (Navarro, 2018) -> "WT_Navarro"
    for(i in 1:dim(Table)[1]){
      if(!is.na(Table[i,"Rule5"])){
        Table[i,"WT_Nav"] <- "U"
      } else{
        if(!is.na(Table[i,"Rule3"])){
          Table[i,"WT_Nav"] <- Table[i,"Rule3"]
        } else{
          if(!is.na(Table[i,"Rule4"])){
            Table[i,"WT_Nav"] <- paste(Table[i,"Rule4"],Table[i,"Adv"],sep='')
          } else{
            if(!is.na(Table[i,"Rule2"])){
              Table[i,"WT_Nav"] <- Table[i,"Adv"]
            }
          }
        }
      }
    }
    for(i in 1:dim(Table)[1]){
      if(is.na(Table[i,"WT_Nav"])){ # si es NA, pues se deja como NA
        Table[i,"WT_Nav"] <- NA
      } else{
        if(Table[i,"WT_Nav"]=="A" & Table[i,"P_8"]>1030){ # si es Anticiclonico y ademas tiene más de 1030, pues pon A+
          Table[i,"WT_Nav"] <- "A+"
        } else{
          Table[i,"WT_Nav"] <- Table[i,"WT_Nav"] # si no, pues pon la clasificacion original tal cual
        }
      }
    }
    
    
    # M. 4 ORIGINALS WITHOUT UNCLASSIFIED (Trigo) AND WITH SEVERE ANTICYCLONIC (Navarro)-> "WT_TriNav"
    for(i in 1:dim(Table)[1]){
      if(!is.na(Table[i,"Rule3"])){
        Table[i,"WT_TriNav"] <- Table[i,"Rule3"]
      } else{
        if(!is.na(Table[i,"Rule4"])){
          Table[i,"WT_TriNav"] <- paste(Table[i,"Rule4"],Table[i,"Adv"],sep='')
        } else{
          if(!is.na(Table[i,"Rule2"])){
            Table[i,"WT_TriNav"] <- Table[i,"Adv"]
          }
        }
      }
    }
    for(i in 1:dim(Table)[1]){
      if(is.na(Table[i,"WT_TriNav"])){ # si es NA, pues se deja como NA
        Table[i,"WT_TriNav"] <- NA
      } else{
        if(Table[i,"WT_TriNav"]=="A" & Table[i,"P_8"]>1030){ # si es Anticiclonico y ademas tiene más de 1030, pues pon A+
          Table[i,"WT_TriNav"] <- "A+"
        } else{
          Table[i,"WT_TriNav"] <- Table[i,"WT_TriNav"] # si no, pues pon la clasificacion original tal cual
        }
      }
    }
    
    print("The original weather types have been calculated")
    ############################## TRATAMIENTO DEL TIPO DE TIEMPO INDETERMINADO (U)
    
    
    # Hay 2 métodos para reclasificar tipos de tiempo Indeterminado (U)
    #   
    # El método 1 es el desarollado originalmente por Rasilla et al., que reclasifica los días Indeterminados (U) como Anticiclónicos (A)
    # o como Ciclónicos, en función de la presión atmosférica del punto 8 del grid. Si esta es inferior a 1010 (C), o mayor a 1020 (A). P8
    # entre 1010 y 1020 se mantendrán como Indeterminados (U). Este solo puede aplicarse con "WT_JC" como dato de entrada.
    #
    # El método 2 es una adaptación del método 1 de Rasilla, realizada por Navarro. En este caso, el proceso es idéntico, pero se incorpora
    # el tipo de tiempo Anticiclónico Severo (A+) para aquellos casos en los que el día sea indeterminado (U) y el punto8 tenga una presión
    # atmosférica mayor que 1030 mb. Este solo puede aplicarse con "WT_Nav" como dato de entrada
    
    # M. 1 RASILLA (U, con P8<1010 = C; U, con P8>1020 = A. Aun quedarán días con U (los comprendidos entre 1010 y 1020))
    # Solo tiene sentido desde dato Original, porque Trigo no tiene U, y Navarro debería incluir tambien a los A+
    
    for(i in 1:dim(Table)[1]){
      if(!is.na(Table[i,"Rule5"])){
        Table[i,"WT_JC_Ras"] <- "U"
      } else{
        if(!is.na(Table[i,"Rule3"])){
          Table[i,"WT_JC_Ras"] <- Table[i,"Rule3"]
        } else{
          if(!is.na(Table[i,"Rule4"])){
            Table[i,"WT_JC_Ras"] <- paste(Table[i,"Rule4"],Table[i,"Adv"],sep='')
          } else{
            if(!is.na(Table[i,"Rule2"])){
              Table[i,"WT_JC_Ras"] <- Table[i,"Adv"]
            }
          }
        }
      }
    }
    for(i in 1:dim(Table)[1]){
      if(!is.na(Table[i,"WT_JC_Ras"])){
        if(Table[i,"WT_JC_Ras"]=="U" & Table[i,"P_8"]<1010){
          Table[i,"WT_JC_Ras"] <- "C"
        } else{
          if(Table[i,"WT_JC_Ras"]=="U" & Table[i,"P_8"]>1020){
            Table[i,"WT_JC_Ras"] <- "A"
          } 
        }
      }
    }
    
    
    # M. 2 RASILLA ADAPTADO (U, con P8<1010 = C; U, con P8>1020 = A; U, con P8>1030 = A+. Aun quedarán días con U
    # (los comprendidos entre 1010 y 1020)). Solo tiene sentido desde dato Navarro, porque es el que tiene esta clase A+
    
    for(i in 1:dim(Table)[1]){
      if(!is.na(Table[i,"Rule5"])){
        Table[i,"WT_Nav_Nav"] <- "U"
      } else{
        if(!is.na(Table[i,"Rule3"])){
          Table[i,"WT_Nav_Nav"] <- Table[i,"Rule3"]
        } else{
          if(!is.na(Table[i,"Rule4"])){
            Table[i,"WT_Nav_Nav"] <- paste(Table[i,"Rule4"],Table[i,"Adv"],sep='')
          } else{
            if(!is.na(Table[i,"Rule2"])){
              Table[i,"WT_Nav_Nav"] <- Table[i,"Adv"]
            }
          }
        }
      }
    }
    for(i in 1:dim(Table)[1]){
      if(is.na(Table[i,"WT_Nav_Nav"])){ # si es NA, pues se deja como NA
        Table[i,"WT_Nav_Nav"] <- NA
      } else{
        if(Table[i,"WT_Nav_Nav"]=="A" & Table[i,"P_8"]>1030){ # si es Anticiclonico y ademas tiene más de 1030, pues pon A+
          Table[i,"WT_Nav_Nav"] <- "A+"
        } else{
          Table[i,"WT_Nav_Nav"] <- Table[i,"WT_Nav_Nav"] # si no, pues pon la clasificacion original tal cual
        }
      }
    }
    for(i in 1:dim(Table)[1]){
      if(!is.na(Table[i,"WT_Nav_Nav"])){
        if(Table[i,"WT_Nav_Nav"]=="U" & Table[i,"P_8"]<1010){
          Table[i,"WT_Nav_Nav"] <- "C"
        } else{
          if(Table[i,"WT_Nav_Nav"]=="U" & Table[i,"P_8"]>1020){
            Table[i,"WT_Nav_Nav"] <- "A"
          } else{
            if(Table[i,"WT_Nav_Nav"]=="U" & Table[i,"P_8"]>1030){
              Table[i,"WT_Nav_Nav"] <- "A+"
            }
          }
        }
      }
    }
    
    print("The Unclassified weather type has been analyzed")
    
    ############################## SINTETIZACIÓN DE TIPOS DE TIEMPO
    
    # METODOS DE SINTETIZARLOS EN MENOS TIPOS. SOLO VOY A PONER EL QUE HAGO YO (DIRECCIONALES A PUROS). Y otras personas que
    # QUIERAN USAR TIPOS DE TIEMPO, QUE SE PREOCUPEN ELLOS DE VER QUÉ MÉTODO USA (porque el de 0.5 a cada uno es complicado
    # para hacer una serie). El summ viene de "Summarize"
    # Método Navarro: Los hibridos pasan todos a direccionales. Desde JC. Desde Trigo. Desde Navarro. Desde TriNav. WT_JC_Ras. WT_Nav_Nav
    
    # Desde WT_JC <- "WT_JC_Summ"
    for(i in 1:dim(Table)[1]){
      if(!is.na(Table[i,"WT_JC"])){
        if(nchar(Table[i,"WT_JC"])==3){
          Table[i,"WT_JC_Summ"] <- substr(Table[i,"WT_JC"],start=2, stop=3)
        } else{
          if(nchar(Table[i,"WT_JC"])==2 & substr(Table[i,"WT_JC"],start=2, stop=2)=="+"){
            Table[i,"WT_JC_Summ"] <- Table[i,"WT_JC"]
          } else{
            if(nchar(Table[i,"WT_JC"])==2 & (substr(Table[i,"WT_JC"],start=1,stop=1)=="A" | substr(Table[i,"WT_JC"],start=1,stop=1)=="C")){
              Table[i,"WT_JC_Summ"] <- substr(Table[i,"WT_JC"],start=2, stop=2)
            } else{
              Table[i,"WT_JC_Summ"] <- Table[i,"WT_JC"]
            }
          }
        }
      }
    }
    
    
    # Desde WT_Tri <- "WT_Tri_Summ"
    for(i in 1:dim(Table)[1]){
      if(!is.na(Table[i,"WT_Tri"])){
        if(nchar(Table[i,"WT_Tri"])==3){
          Table[i,"WT_Tri_Summ"] <- substr(Table[i,"WT_Tri"],start=2, stop=3)
        } else{
          if(nchar(Table[i,"WT_Tri"])==2 & substr(Table[i,"WT_Tri"],start=2, stop=2)=="+"){
            Table[i,"WT_Tri_Summ"] <- Table[i,"WT_Tri"]
          } else{
            if(nchar(Table[i,"WT_Tri"])==2 & (substr(Table[i,"WT_Tri"],start=1,stop=1)=="A" | substr(Table[i,"WT_Tri"],start=1,stop=1)=="C")){
              Table[i,"WT_Tri_Summ"] <- substr(Table[i,"WT_Tri"],start=2, stop=2)
            } else{
              Table[i,"WT_Tri_Summ"] <- Table[i,"WT_Tri"]
            }
          }
        }
      }
    }
    
    
    # Desde WT_Nav <- "WT_Nav_Summ"
    for(i in 1:dim(Table)[1]){
      if(!is.na(Table[i,"WT_Nav"])){
        if(nchar(Table[i,"WT_Nav"])==3){
          Table[i,"WT_Nav_Summ"] <- substr(Table[i,"WT_Nav"],start=2, stop=3)
        } else{
          if(nchar(Table[i,"WT_Nav"])==2 & substr(Table[i,"WT_Nav"],start=2, stop=2)=="+"){
            Table[i,"WT_Nav_Summ"] <- Table[i,"WT_Nav"]
          } else{
            if(nchar(Table[i,"WT_Nav"])==2 & (substr(Table[i,"WT_Nav"],start=1,stop=1)=="A" | substr(Table[i,"WT_Nav"],start=1,stop=1)=="C")){
              Table[i,"WT_Nav_Summ"] <- substr(Table[i,"WT_Nav"],start=2, stop=2)
            } else{
              Table[i,"WT_Nav_Summ"] <- Table[i,"WT_Nav"]
            }
          }
        }
      }
    }
    
    
    # Desde WT_TriNav <- "WT_TriNav_Summ"
    for(i in 1:dim(Table)[1]){
      if(!is.na(Table[i,"WT_TriNav"])){
        if(nchar(Table[i,"WT_TriNav"])==3){
          Table[i,"WT_TriNav_Summ"] <- substr(Table[i,"WT_TriNav"],start=2, stop=3)
        } else{
          if(nchar(Table[i,"WT_TriNav"])==2 & substr(Table[i,"WT_TriNav"],start=2, stop=2)=="+"){
            Table[i,"WT_TriNav_Summ"] <- Table[i,"WT_TriNav"]
          } else{
            if(nchar(Table[i,"WT_TriNav"])==2 & (substr(Table[i,"WT_TriNav"],start=1,stop=1)=="A" | substr(Table[i,"WT_TriNav"],start=1,stop=1)=="C")){
              Table[i,"WT_TriNav_Summ"] <- substr(Table[i,"WT_TriNav"],start=2, stop=2)
            } else{
              Table[i,"WT_TriNav_Summ"] <- Table[i,"WT_TriNav"]
            }
          }
        }
      }
    }
    
    # Desde WT_JC_Ras <- "WT_JC_Ras_Summ"
    for(i in 1:dim(Table)[1]){
      if(!is.na(Table[i,"WT_JC_Ras"])){
        if(nchar(Table[i,"WT_JC_Ras"])==3){
          Table[i,"WT_JC_Ras_Summ"] <- substr(Table[i,"WT_JC_Ras"],start=2, stop=3)
        } else{
          if(nchar(Table[i,"WT_JC_Ras"])==2 & substr(Table[i,"WT_JC_Ras"],start=2, stop=2)=="+"){
            Table[i,"WT_JC_Ras_Summ"] <- Table[i,"WT_JC_Ras"]
          } else{
            if(nchar(Table[i,"WT_JC_Ras"])==2 & (substr(Table[i,"WT_JC_Ras"],start=1,stop=1)=="A" | substr(Table[i,"WT_JC_Ras"],start=1,stop=1)=="C")){
              Table[i,"WT_JC_Ras_Summ"] <- substr(Table[i,"WT_JC_Ras"],start=2, stop=2)
            } else{
              Table[i,"WT_JC_Ras_Summ"] <- Table[i,"WT_JC_Ras"]
            }
          }
        }
      }
    }
    
    # Desde WT_Nav_Nav "WT_Nav_Nav_Summ"
    for(i in 1:dim(Table)[1]){
      if(!is.na(Table[i,"WT_Nav_Nav"])){
        if(nchar(Table[i,"WT_Nav_Nav"])==3){
          Table[i,"WT_Nav_Nav_Summ"] <- substr(Table[i,"WT_Nav_Nav"],start=2, stop=3)
        } else{
          if(nchar(Table[i,"WT_Nav_Nav"])==2 & substr(Table[i,"WT_Nav_Nav"],start=2, stop=2)=="+"){
            Table[i,"WT_Nav_Nav_Summ"] <- Table[i,"WT_Nav_Nav"]
          } else{
            if(nchar(Table[i,"WT_Nav_Nav"])==2 & (substr(Table[i,"WT_Nav_Nav"],start=1,stop=1)=="A" | substr(Table[i,"WT_Nav_Nav"],start=1,stop=1)=="C")){
              Table[i,"WT_Nav_Nav_Summ"] <- substr(Table[i,"WT_Nav_Nav"],start=2, stop=2)
            } else{
              Table[i,"WT_Nav_Nav_Summ"] <- Table[i,"WT_Nav_Nav"]
            }
          }
        }
      }
    }
    
    print("Weather types have been syntetized")
    
    ################
    # Generate weather types daily series
    ################
    
    Result = Table[,c("FECHA","YEAR","MONTH","DAY",classification_method)]
    write.csv(Result,paste('./SLPdata/WeatherTypes','_P8_',latp8,'_',lonp8,'.csv',sep=''), row.names = F)
    print("Weather types time serie generated")
    print("WARNING: The time serie could contain NA days")
    
    rm(archivo_txt,archivo_txt_agg,dt1,dt2,union,contador,finish,finish_date,i,interv_lat,interv_lon,latp8,lonp8,p01,p02,p03,p04,p05,p06,p07,p08,p09,p10,p11,p12,p13,p14,p15,p16,pointlist,row_number,start,start_date,valor_S,valor_W)
    print("Finish.Results have been generated in ./SLPdata/WeatherTypes.csv")
  } else{
    print("Set correctly latitude-longitude parameters")
  }
}

setwd("E:/Temporales/")

# Elección del centroide geográfico (de 5 en 5). Creo que es lo más claro, ignorando la posición del p8, que ya se automatiza
# desde esto:
latcen = 40 # Latitud central (sin echar cuenta de puntos de grid)
loncen = -5 # Longitud central (sin echar cuenta de puntos de grid, tendrá 5º más que longp8 y 5º menos que long p9)
weathertypes(latp8 = latcen,lonp8 = loncen-5,start = 19790101,finish = 20191031,classification_method = c("WT_JC","WT_JC_Ras_Summ","WT_Nav_Nav_Summ"))

# Results will be generated in "./SLPdata/WeatherTypes.csv"

###
## FUNCTION PARAMETERS HELP:
###

## Step 1/5: Set work directory
# setwd("c:/")

## Step 1/4: Set p.8 (in degrees, en múltiplos de 5. Southern Hemisphere and Western Hemisphere: negative degrees). P8-P9 are the grid centroid points
#   Please, check the distribution of the whole grid over your study area. The grid must completely cover the study area. 
#   The example is prepared to 40ºN 10ºW as p8. Standard coordinate grid distribution
#   https://freva.met.fu-berlin.de/static/preview/doc/cwt/cwt0y.png (Jones et al., 1993. For British Islands).
#   It must be applied at mid latitudes (between 30-70º, more or less)

# latp8 = 40
# lonp8 = -10
# 
# ## Step 2/3: Set start and end dates (yyyymmdd format)
# start = 19000101  #mantengo este formato para que la fase de descargas pueda usar este objeto llamado "start"
# finish = 20171031
# 
# 
# ## Step 3/3: Set Weather Types method classification/s: (i.e. Prepared by WT_JC, WT_JC_Summ, WT_Nav, WT_Nav_Nav_Summ)
# classification_method = c("WT_JC","WT_JC_Ras_Summ","WT_Nav_Nav_Summ")

#NON-SUMMARIZED METHODS:
"WT_JC"      # Weather Types developed by Jekinson and Collison (1977), based on original classification of Lamb (1972)
"WT_Tri"     # Weather Types developed by Trigo and DaCamara (2000), based on Jekinson and Collison, removing Unclassified (U) type.
"WT_Nav"     # Weather Types devoloped by Navarro et al. (2018), based on Jekinson and Collison, adding Severe Anticyclonic (A+) type.
"WT_TriNav"  # Weather Types based on Jekinson and Collison, removing "U" type (Trigo and DaCamara, 2000), and adding "A+" (Navarro et al. 2018)
"WT_JC_Ras"  # Weather Types developed by Rasilla et al. (2002), based on Jekinson and Collison, reclassifying some "U" as "C" or "A" types. 
"WT_Nav_Nav" # Weather Types developed by Navarro et al. (2018), based on Jekinson and Collison, reclassifying some "U" as "C", "A" or "A+" types.

#SUMMARIZED METHODS (to simplify the original weather types, hybrid types have been reclassified as their directional flow)
"WT_JC_Summ"      # Weather Types developed by Jekinson and Collison (1977), based on original classification of Lamb (1972)
"WT_Tri_Summ"     # Weather Types developed by Trigo and DaCamara (2000), based on Jekinson and Collison, removing Unclassified (U) type.
"WT_Nav_Summ"     # Weather Types devoloped by Navarro et al. (2018), based on Jekinson and Collison, adding Severe Anticyclonic (A+) type.
"WT_TriNav_Summ"  # Weather Types based on Jekinson and Collison, removing "U" type (Trigo and DaCamara, 2000), and adding "A+" (Navarro et al. 2018)
"WT_JC_Ras_Summ"  # Weather Types developed by Rasilla et al. (2002), based on Jekinson and Collison, reclassifying some "U" as "C" or "A" types. 
"WT_Nav_Nav_Summ" # Weather Types developed by Navarro et al. (2018), based on Jekinson and Collison, reclassifying some "U" as "C", "A" or "A+" types.

## Days with NA: No pressure data were available.




















##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################
######################## SCRIPT DESARROLLADO ORIGINALMENTE (ES IDÉNTICO):#########################
##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################


#   Se aplica en latitudes medias (entre 30 y 70º, aproximadamente)
#   El multiplicador "2 por.." en el centro de algunas fórmulas es para corregir que la latitud la ponemos de 5 en 5 y la longitud de 10 en 10
#   al elegir los puntos del grid
#   En el artículo https://link.springer.com/article/10.1007/s00704-015-1711-8 se describe en su apendice 1 de donde vienen los valores constantes
#   dentro de la fórmula (1.74; 1.07; 0.95; 1.52, que están medidos para una latitud de 55ºN, pero que si lo queremos para 40ºN, pues son diferentes
#   porque Coriolis afecta de distinta forma). Este artículo es muy bueno por que es un análsis de la clasificacion de J&C y la explica muy bien.

# Standard coordinate grid distribution
# https://freva.met.fu-berlin.de/static/preview/doc/cwt/cwt0y.png (Jones et al., 1993. For British Islands)

## Step 1/5: Set work directory
setwd("c:/")

## Step 2/5: Set p.8 (in degrees. Southern Hemisphere and Western Hemisphere: negative degrees). P8-P9 are the grid centroid points
#           Please, check the distribution of the whole grid over your study area. The grid must completely cover the study area. 
#           The example is prepared to 40ºN 10ºW as p8.
latp8 = 40
lonp8 = -10

## Step 3/5: Set start and end dates (yyyymmdd format)
start = 19000101  #mantengo este formato para que la fase de descargas pueda usar este objeto llamado "start"
finish = 20171031

## Step 4/5: Set coordinate interval (in degrees, DAFAULT is 10º longitude / 5º latitude)
interv_lon = 10
interv_lat = 5

## Step 5/5: Set Weather Types method classification/s: (i.e. Prepared by WT_JC, WT_JC_Summ, WT_Nav, WT_Nav_Nav_Summ)
classification_method = c("WT_JC","WT_JC_Summ","WT_Nav","WT_Nav_Nav_Summ")

#NON-SUMMARIZED METHODS:
"WT_JC"      # Weather Types developed by Jekinson and Collison (1977), based on original classification of Lamb (1972)
"WT_Tri"     # Weather Types developed by Trigo and DaCamara (2000), based on Jekinson and Collison, removing Unclassified (U) type.
"WT_Nav"     # Weather Types devoloped by Navarro et al. (2018), based on Jekinson and Collison, adding Severe Anticyclonic (A+) type.
"WT_TriNav"  # Weather Types based on Jekinson and Collison, removing "U" type (Trigo and DaCamara, 2000), and adding "A+" (Navarro et al. 2018)
"WT_JC_Ras"  # Weather Types developed by Rasilla et al. (2002), based on Jekinson and Collison, reclassifying some "U" as "C" or "A" types. 
"WT_Nav_Nav" # Weather Types developed by Navarro et al. (2018), based on Jekinson and Collison, reclassifying some "U" as "C", "A" or "A+" types.

#SUMMARIZED METHODS (to simplify the original weather types, hybrid types have been reclassified as their directional flow)
"WT_JC_Summ"      # Weather Types developed by Jekinson and Collison (1977), based on original classification of Lamb (1972)
"WT_Tri_Summ"     # Weather Types developed by Trigo and DaCamara (2000), based on Jekinson and Collison, removing Unclassified (U) type.
"WT_Nav_Summ"     # Weather Types devoloped by Navarro et al. (2018), based on Jekinson and Collison, adding Severe Anticyclonic (A+) type.
"WT_TriNav_Summ"  # Weather Types based on Jekinson and Collison, removing "U" type (Trigo and DaCamara, 2000), and adding "A+" (Navarro et al. 2018)
"WT_JC_Ras_Summ"  # Weather Types developed by Rasilla et al. (2002), based on Jekinson and Collison, reclassifying some "U" as "C" or "A" types. 
"WT_Nav_Nav_Summ" # Weather Types developed by Navarro et al. (2018), based on Jekinson and Collison, reclassifying some "U" as "C", "A" or "A+" types.

## Days with NA: No pressure data were available.




###########################
######### WARNING! DONT TOUCH! AUTOMATIC SCRIPT. (RUN ALL, UNTIL THE END)
###########################
# Packages
# install.packages("curl")
# install.packages("data.table")
library(curl)
library(data.table)

start_date = as.Date(paste(substr(start,start=1,stop=4),"-",substr(start,start=5,stop=6),"-",substr(start,start=7, stop=8),sep=''), format='%Y-%m-%d')
finish_date = as.Date(paste(substr(finish,start=1,stop=4),"-",substr(finish,start=5,stop=6),"-",substr(finish,start=7, stop=8),sep=''), format='%Y-%m-%d')
row_number = difftime(finish_date,start_date)+1

# Create Table
Table = as.data.frame(array(NA, dim=c(row_number, 4)))

# Write dates
Table[,1] <- seq(start_date,finish_date,by="days")
Table[,2] <- as.character(substr(Table[,1],start=1, stop=4))   #year
Table[,3] <- as.character(substr(Table[,1],start=6, stop=7))   #month
Table[,4] <- as.character(substr(Table[,1],start=9, stop=10))   #day

# Download SLP points time series (p8 is the reference gridpoint position). NCAR Sea Level Pressure Data (https://climatedataguide.ucar.edu/climate-data/ncar-sea-level-pressure)
p01 = paste("https://rda.ucar.edu/cgi-bin/dattore/gridts?sd=",start,"&ed=",finish,"&lat=",latp8+(2*interv_lat),"&latdir=N&lon=",lonp8+(0*interv_lon),"&londir=E&format=slp&dsnum=ds010.0&input=dailydata.bin&t=daily",sep='')
p02 = paste("https://rda.ucar.edu/cgi-bin/dattore/gridts?sd=",start,"&ed=",finish,"&lat=",latp8+(2*interv_lat),"&latdir=N&lon=",lonp8+(1*interv_lon),"&londir=E&format=slp&dsnum=ds010.0&input=dailydata.bin&t=daily",sep='')

p03 = paste("https://rda.ucar.edu/cgi-bin/dattore/gridts?sd=",start,"&ed=",finish,"&lat=",latp8+(1*interv_lat),"&latdir=N&lon=",lonp8-(1*interv_lon),"&londir=E&format=slp&dsnum=ds010.0&input=dailydata.bin&t=daily",sep='')
p04 = paste("https://rda.ucar.edu/cgi-bin/dattore/gridts?sd=",start,"&ed=",finish,"&lat=",latp8+(1*interv_lat),"&latdir=N&lon=",lonp8+(0*interv_lon),"&londir=E&format=slp&dsnum=ds010.0&input=dailydata.bin&t=daily",sep='')
p05 = paste("https://rda.ucar.edu/cgi-bin/dattore/gridts?sd=",start,"&ed=",finish,"&lat=",latp8+(1*interv_lat),"&latdir=N&lon=",lonp8+(1*interv_lon),"&londir=E&format=slp&dsnum=ds010.0&input=dailydata.bin&t=daily",sep='')
p06 = paste("https://rda.ucar.edu/cgi-bin/dattore/gridts?sd=",start,"&ed=",finish,"&lat=",latp8+(1*interv_lat),"&latdir=N&lon=",lonp8+(2*interv_lon),"&londir=E&format=slp&dsnum=ds010.0&input=dailydata.bin&t=daily",sep='')

p07 = paste("https://rda.ucar.edu/cgi-bin/dattore/gridts?sd=",start,"&ed=",finish,"&lat=",latp8+(0*interv_lat),"&latdir=N&lon=",lonp8-(1*interv_lon),"&londir=E&format=slp&dsnum=ds010.0&input=dailydata.bin&t=daily",sep='')
p08 = paste("https://rda.ucar.edu/cgi-bin/dattore/gridts?sd=",start,"&ed=",finish,"&lat=",latp8+(0*interv_lat),"&latdir=N&lon=",lonp8+(0*interv_lon),"&londir=E&format=slp&dsnum=ds010.0&input=dailydata.bin&t=daily",sep='')
p09 = paste("https://rda.ucar.edu/cgi-bin/dattore/gridts?sd=",start,"&ed=",finish,"&lat=",latp8+(0*interv_lat),"&latdir=N&lon=",lonp8+(1*interv_lon),"&londir=E&format=slp&dsnum=ds010.0&input=dailydata.bin&t=daily",sep='')
p10 = paste("https://rda.ucar.edu/cgi-bin/dattore/gridts?sd=",start,"&ed=",finish,"&lat=",latp8+(0*interv_lat),"&latdir=N&lon=",lonp8+(2*interv_lon),"&londir=E&format=slp&dsnum=ds010.0&input=dailydata.bin&t=daily",sep='')

p11 = paste("https://rda.ucar.edu/cgi-bin/dattore/gridts?sd=",start,"&ed=",finish,"&lat=",latp8-(1*interv_lat),"&latdir=N&lon=",lonp8-(1*interv_lon),"&londir=E&format=slp&dsnum=ds010.0&input=dailydata.bin&t=daily",sep='')
p12 = paste("https://rda.ucar.edu/cgi-bin/dattore/gridts?sd=",start,"&ed=",finish,"&lat=",latp8-(1*interv_lat),"&latdir=N&lon=",lonp8+(0*interv_lon),"&londir=E&format=slp&dsnum=ds010.0&input=dailydata.bin&t=daily",sep='')
p13 = paste("https://rda.ucar.edu/cgi-bin/dattore/gridts?sd=",start,"&ed=",finish,"&lat=",latp8-(1*interv_lat),"&latdir=N&lon=",lonp8+(1*interv_lon),"&londir=E&format=slp&dsnum=ds010.0&input=dailydata.bin&t=daily",sep='')
p14 = paste("https://rda.ucar.edu/cgi-bin/dattore/gridts?sd=",start,"&ed=",finish,"&lat=",latp8-(1*interv_lat),"&latdir=N&lon=",lonp8+(2*interv_lon),"&londir=E&format=slp&dsnum=ds010.0&input=dailydata.bin&t=daily",sep='')

p15 = paste("https://rda.ucar.edu/cgi-bin/dattore/gridts?sd=",start,"&ed=",finish,"&lat=",latp8-(2*interv_lat),"&latdir=N&lon=",lonp8+(0*interv_lon),"&londir=E&format=slp&dsnum=ds010.0&input=dailydata.bin&t=daily",sep='')
p16 = paste("https://rda.ucar.edu/cgi-bin/dattore/gridts?sd=",start,"&ed=",finish,"&lat=",latp8-(2*interv_lat),"&latdir=N&lon=",lonp8+(1*interv_lon),"&londir=E&format=slp&dsnum=ds010.0&input=dailydata.bin&t=daily",sep='')

# Download SLPdata series
dir.create("./SLPdata")
download.file(url = p01, destfile = "./SLPdata/p01.txt", method = "wininet", quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))
download.file(url = p02, destfile = "./SLPdata/p02.txt", method = "wininet", quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))
download.file(url = p03, destfile = "./SLPdata/p03.txt", method = "wininet", quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))
download.file(url = p04, destfile = "./SLPdata/p04.txt", method = "wininet", quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))
download.file(url = p05, destfile = "./SLPdata/p05.txt", method = "wininet", quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))
download.file(url = p06, destfile = "./SLPdata/p06.txt", method = "wininet", quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))
download.file(url = p07, destfile = "./SLPdata/p07.txt", method = "wininet", quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))
download.file(url = p08, destfile = "./SLPdata/p08.txt", method = "wininet", quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))
download.file(url = p09, destfile = "./SLPdata/p09.txt", method = "wininet", quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))
download.file(url = p10, destfile = "./SLPdata/p10.txt", method = "wininet", quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))
download.file(url = p11, destfile = "./SLPdata/p11.txt", method = "wininet", quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))
download.file(url = p12, destfile = "./SLPdata/p12.txt", method = "wininet", quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))
download.file(url = p13, destfile = "./SLPdata/p13.txt", method = "wininet", quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))
download.file(url = p14, destfile = "./SLPdata/p14.txt", method = "wininet", quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))
download.file(url = p15, destfile = "./SLPdata/p15.txt", method = "wininet", quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))
download.file(url = p16, destfile = "./SLPdata/p16.txt", method = "wininet", quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))

# Load SLPdata series
pointlist = list.files('./SLPdata/', pattern='.txt$')
contador = 5

for(i in length(pointlist):1){
  print(paste("Point ", i," of ", length(pointlist)))
  archivo_txt = read.table(paste("./SLPdata/",pointlist[i],sep=''),header = TRUE, sep="\t",skip=4, row.names=NULL)
  archivo_txt2 = data.frame(do.call('rbind', strsplit(as.character(archivo_txt$DATE.......TIME................VALUE),'    ',fixed=TRUE)))
  archivo_txt3 = archivo_txt2[,1:2]
  archivo_txt3$X3 = as.numeric(as.character(archivo_txt3$X2))
  archivo_txt4=archivo_txt3[,c(1,3)]
  rm(archivo_txt,archivo_txt2,archivo_txt3)
  archivo_txt = archivo_txt4
  rm(archivo_txt4)
  archivo_txt[,"DATE"] <- substr(archivo_txt[,1],start = 1,stop = 10)
  archivo_txt=archivo_txt[,c(3,2)]
  archivo_txt <- archivo_txt[!(is.na(archivo_txt$X3)),]
  
  archivo_txt_agg = aggregate(archivo_txt[, 2], list(archivo_txt$DATE), mean)
  
  names(archivo_txt_agg)[1] <- "FECHA"
  names(archivo_txt_agg)[2] <- paste("P_",i,sep='')
  names(Table)[1] <- "FECHA"
  names(Table)[2] <- "YEAR"
  names(Table)[3] <- "MONTH"
  names(Table)[4] <- "DAY"
  
  Table$FECHA <- as.character(Table$FECHA)
  
  dt1 <- data.table(Table, key = "FECHA")
  dt2 <- data.table(archivo_txt_agg, key = "FECHA")
  union <- dt2[dt1]
  Table[,contador] <- as.data.frame(union)[,2]
  
  contador = contador + 1
}

# Names of variables
for(i in 5:20){
  names(Table)[i]<-paste("P_",rev(seq(1:16))[i-4],sep='')
}

# Westerly flow (W)
Table[,"W"] <- (0.5*(Table[,"P_12"]+Table[,"P_13"]))-(0.5*(Table[,"P_4"]+Table[,"P_5"]))

# Southerly flow (S)
Table[,"S"] <- (1/(cos(latp8*pi/180)))*((0.25*(Table[,"P_5"]+(2*Table[,"P_9"])+Table[,"P_13"]))-(0.25*(Table[,"P_4"]+(2*Table[,"P_8"])+Table[,"P_12"])))

# Resultant flow (F)
Table[,"F"] <- (Table[,"S"]^2 + Table[,"W"]^2)^0.5

# Westerly shear velocity (ZW)
Table[,"ZW"] <- (sin((latp8)*pi/180)/sin((latp8-interv_lat)*pi/180)) * ((0.5*(Table[,"P_15"]+Table[,"P_16"])) - (0.5*(Table[,"P_8"]+Table[,"P_9"]))) - (sin((latp8)*pi/180)/sin((latp8+interv_lat)*pi/180)) * ((0.5*(Table[,"P_8"]+Table[,"P_9"])) - (0.5*(Table[,"P_1"]+Table[,"P_2"])))

# Southerly shear velocity (ZS)
Table[,"ZS"] <- (1/(2*(cos(latp8*pi/180))^2)) * ((0.25*(Table[,"P_6"]+2*(Table[,"P_10"])+Table[,"P_14"])) - (0.25*(Table[,"P_5"]+2*(Table[,"P_9"])+Table[,"P_13"])) - (0.25*(Table[,"P_4"]+2*(Table[,"P_8"])+Table[,"P_12"])) + (0.25*(Table[,"P_3"]+2*(Table[,"P_7"])+Table[,"P_11"])))

# Total shear velocity(Z)
Table[,"Z"] <- Table[,"ZW"]+Table[,"ZS"]

# Direction of flow (D)
Table[,"D"] <- atan(Table[,"W"]/Table[,"S"])*180/pi  # Aunque suene raro que aquí sea 180*pi....dejarlo así, porque deben ser rollos de usar atan, en vez de tan y tal.

# Conditions for Angle(º) 4 "CONDITIONS ANGLE"
# C.0 Si no hay dato, pon en angle <- NA
# C.1 =SI(Y(W>0;S<0);360+D;"")
# C.2 ==SI(Y(W>0;S>0);180+D;"")
# C.3 =SI(Y(W<0;S>0);180+D;"")
# C.4 =SI(Y(W<0;S<0);D;"")

for(i in 1:dim(Table)[1]){
  valor_W <- Table[i,"W"]
  valor_S <- Table[i,"S"]
  if(is.na(Table[i,"W"]) | is.na(Table[i,"S"])){  ## C.0
    Table[i,"Angle"] <- NA
  } else{
    if(valor_W>0 & valor_S<0){
      Table[i,"Angle"] <- 360+Table[i,"D"]  ## C.1
    } else{
      if(valor_W>0 & valor_S>0){
        Table[i,"Angle"] <- 180+Table[i,"D"]    ## C.2
      } else{
        if(valor_W<0 & valor_S>0){
          Table[i,"Angle"] <- 180+Table[i,"D"]    ## C.3
        } else{
          if(valor_W<0 & valor_S<0){
            Table[i,"Angle"] <- Table[i,"D"]    ## C.4
          }
        }
      }
    }
  }
}

# Conditions for Direction of Advection (Rule 1)
for(i in 1:dim(Table)[1]){
  if(is.na(Table[i,"Angle"])){
    Table[i,"Adv"] <- NA
  } else{
    if(Table[i,"Angle"]>337.5 | Table[i,"Angle"]<= 22.5){
      Table[i,"Adv"] <- "N"
    } else{
      if(Table[i,"Angle"]>22.5 & Table[i,"Angle"]<= 67.5){
        Table[i,"Adv"] <- "NE"
      } else{
        if(Table[i,"Angle"]>67.5 & Table[i,"Angle"]<=112.5){
          Table[i,"Adv"] <- "E"
        } else{
          if(Table[i,"Angle"]>112.5 & Table[i,"Angle"]<= 157.5){
            Table[i,"Adv"] <- "SE"
          } else{
            if(Table[i,"Angle"]>157.5 & Table[i,"Angle"]<= 202.5){
              Table[i,"Adv"] <- "S"
            } else{
              if(Table[i,"Angle"]>202.5 & Table[i,"Angle"]<= 247.5){
                Table[i,"Adv"] <- "SW"
              } else{
                if(Table[i,"Angle"]>247.5 & Table[i,"Angle"]<=292.5){
                  Table[i,"Adv"] <- "W"
                } else{
                  if(Table[i,"Angle"]>292.5 & Table[i,"Angle"]<=337.5){
                    Table[i,"Adv"] <- "NW"
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

# Rule 2
# If |Z| is less than F, then the flow is issentially straight and corresponds to a Lamb pure directional type.
for(i in 1:dim(Table)[1]){
  if(is.na(Table[i,"Z"]) | is.na(Table[i,"F"])){
    Table[i,"Rule2"] <- NA
  } else{
    if(abs(Table[i,"Z"])<Table[i,"F"]){
      Table[i,"Rule2"] <- "PureAdvective"
    }
  }
}

# Rule 3
# If |Z| is greater than 2F, then the pattern is strongly cyclonic (Z>0) or anticyclonic (Z<0). This corresponds to Lamb's pure
# cyclonic and anticyclonic type.
for(i in 1:dim(Table)[1]){
  if(is.na(Table[i,"Z"]) | is.na(Table[i,"F"])){
    Table[i,"Rule3"] <- NA
  } else{
    if(abs(Table[i,"Z"])>(2*Table[i,"F"])){
      if(Table[i,"Z"]>0){
        Table[i,"Rule3"] <- "C"
      } else{
      Table[i,"Rule3"] <- "A"
      }
    }
  }
}

# Rule 4
# If |Z| lies between F and 2F, the the flow is partly(anti) cyclonic and this corresponds to one of Lamb's synoptic/direction
# hybrid types, e.g. AE (anticyclonic-east)
for(i in 1:dim(Table)[1]){
  if(is.na(Table[i,"Z"]) | is.na(Table[i,"F"])){
    Table[i,"Rule4"] <- NA
  } else{
    if(   (Table[i,"F"]<abs(Table[i,"Z"])) & (abs(Table[i,"Z"]) < (2*Table[i,"F"])) & Table[i,"Z"]>0 ){
      Table[i,"Rule4"] <- "C"
    } else{
      if(   (Table[i,"F"]<abs(Table[i,"Z"])) & (abs(Table[i,"Z"]) < (2*Table[i,"F"])) & Table[i,"Z"]<0){
        Table[i,"Rule4"] <- "A"
      }
    }
  }
}


# Rule 5
# If F is less than 6 and |Z| es less than 6, there is a light indeterminate flow corresponding to Lamb's unclassified type U.
# The choice of 6 is dependent on the grid spacing and would need tuning if used with a finer grid resolution.
# This rule is dominant front previous rules
for(i in 1:dim(Table)[1]){
  if(is.na(Table[i,"Z"]) | is.na(Table[i,"F"])){
    Table[i,"Rule5"] <- NA
  } else{
    if(Table[i,"F"]<6 & abs(Table[i,"Z"])<6){
      Table[i,"Rule5"] <- "U"
    }
  }
}

## ESE 6 MARTIN VIDE LO CAMBIA POR 4.8 F y 4.2 para Z (según LOS TIPOS SINÓPTICOS DE JENKINSON 
## & COLLISON Y LA INTENSIDAD DE LA ISLA DE CALOR BARCELONESA
## Javier MARTÍN-VIDE, Mª Carmen MORENO GARCÍA, Víctor M. ARTOLA y Mª José CORDOBILLA). no lo he aplicado porque no queda claro


############################## CÁLCULO DE LOS TIPOS DE TIEMPO

# Hay 4 métodos para calcular los tipos de tiempo, en función de las clases que se introduzcan.:
#   
# El método 1 es el desarollado originalmente por Jekinson and Collison, que trabaja con 2 tipos de tiempo básicos (A,C), 8 tipos
# direccionales (N,NE,E,SE,S,SW,W,NW), 16 tipos de tiempo híbridos (AN,ANE,AE,ASE,AS,ASW,AW,ANW, CN,CNE,CE,CSE,CS,CSW,CW,CNW) y
# 1 tipo de tiempo Indeterminado (U), para aquellos días donde no se consiga <- "WT_JC"
# 
# El método 2 es el desarrollado por Trigo. Su clasificación es idéntica a la desarrollada por J&C, salvo con la diferencia de que
# no incorpora el tipo de tiempo Indeterminado (U) <- "WT_Tri"
# 
# El método 3 es el desarrollado por Navarro. Su clasificación es idéntica a la desarrollada por J&C, salvo con la diferencia de que
# incorpora un nuevo tipo de tiempo, el Anticiclónico Severo (A+) <- "WT_Nav"
# 
# El método 4 es un mix entre el desarrollado por Trigo, y por Navarro. Este no incorpora el tipo de tiempo Indeterminado (U), pero
# sí incorpora el Anticiclónico Severo (A+) <- "WT_TriNav"


# M.1 ORIGINALS (Jekinson and Collison, 1977) <- "WT_JC"
for(i in 1:dim(Table)[1]){
  if(!is.na(Table[i,"Rule5"])){
    Table[i,"WT_JC"] <- "U"
  } else{
    if(!is.na(Table[i,"Rule3"])){
      Table[i,"WT_JC"] <- Table[i,"Rule3"]
    } else{
      if(!is.na(Table[i,"Rule4"])){
        Table[i,"WT_JC"] <- paste(Table[i,"Rule4"],Table[i,"Adv"],sep='')
      } else{
        if(!is.na(Table[i,"Rule2"])){
          Table[i,"WT_JC"] <- Table[i,"Adv"]
        }
      }
    }
  }
}
  
  
# M. 2 ORIGINALS WITHOUT UNCLASSIFIED (Trigo) -> "WT_Trigo"

for(i in 1:dim(Table)[1]){
  if(!is.na(Table[i,"Rule3"])){
    Table[i,"WT_Tri"] <- Table[i,"Rule3"]
  } else{
    if(!is.na(Table[i,"Rule4"])){
      Table[i,"WT_Tri"] <- paste(Table[i,"Rule4"],Table[i,"Adv"],sep='')
    } else{
      if(!is.na(Table[i,"Rule2"])){
        Table[i,"WT_Tri"] <- Table[i,"Adv"]
      }
    }
  }
}


# M. 3 ORIGINALS WITH SEVERE ANTICYCLONIC (Navarro, 2018) -> "WT_Navarro"
for(i in 1:dim(Table)[1]){
  if(!is.na(Table[i,"Rule5"])){
    Table[i,"WT_Nav"] <- "U"
  } else{
    if(!is.na(Table[i,"Rule3"])){
      Table[i,"WT_Nav"] <- Table[i,"Rule3"]
    } else{
      if(!is.na(Table[i,"Rule4"])){
        Table[i,"WT_Nav"] <- paste(Table[i,"Rule4"],Table[i,"Adv"],sep='')
      } else{
        if(!is.na(Table[i,"Rule2"])){
          Table[i,"WT_Nav"] <- Table[i,"Adv"]
        }
      }
    }
  }
}
for(i in 1:dim(Table)[1]){
  if(is.na(Table[i,"WT_Nav"])){ # si es NA, pues se deja como NA
    Table[i,"WT_Nav"] <- NA
  } else{
    if(Table[i,"WT_Nav"]=="A" & Table[i,"P_8"]>1030){ # si es Anticiclonico y ademas tiene más de 1030, pues pon A+
      Table[i,"WT_Nav"] <- "A+"
    } else{
      Table[i,"WT_Nav"] <- Table[i,"WT_Nav"] # si no, pues pon la clasificacion original tal cual
    }
  }
}


# M. 4 ORIGINALS WITHOUT UNCLASSIFIED (Trigo) AND WITH SEVERE ANTICYCLONIC (Navarro)-> "WT_TriNav"
for(i in 1:dim(Table)[1]){
  if(!is.na(Table[i,"Rule3"])){
    Table[i,"WT_TriNav"] <- Table[i,"Rule3"]
  } else{
    if(!is.na(Table[i,"Rule4"])){
      Table[i,"WT_TriNav"] <- paste(Table[i,"Rule4"],Table[i,"Adv"],sep='')
    } else{
      if(!is.na(Table[i,"Rule2"])){
        Table[i,"WT_TriNav"] <- Table[i,"Adv"]
      }
    }
  }
}
for(i in 1:dim(Table)[1]){
  if(is.na(Table[i,"WT_TriNav"])){ # si es NA, pues se deja como NA
    Table[i,"WT_TriNav"] <- NA
  } else{
    if(Table[i,"WT_TriNav"]=="A" & Table[i,"P_8"]>1030){ # si es Anticiclonico y ademas tiene más de 1030, pues pon A+
      Table[i,"WT_TriNav"] <- "A+"
    } else{
      Table[i,"WT_TriNav"] <- Table[i,"WT_TriNav"] # si no, pues pon la clasificacion original tal cual
    }
  }
}


############################## TRATAMIENTO DEL TIPO DE TIEMPO INDETERMINADO (U)


# Hay 2 métodos para reclasificar tipos de tiempo Indeterminado (U)
#   
# El método 1 es el desarollado originalmente por Rasilla et al., que reclasifica los días Indeterminados (U) como Anticiclónicos (A)
# o como Ciclónicos, en función de la presión atmosférica del punto 8 del grid. Si esta es inferior a 1010 (C), o mayor a 1020 (A). P8
# entre 1010 y 1020 se mantendrán como Indeterminados (U). Este solo puede aplicarse con "WT_JC" como dato de entrada.
#
# El método 2 es una adaptación del método 1 de Rasilla, realizada por Navarro. En este caso, el proceso es idéntico, pero se incorpora
# el tipo de tiempo Anticiclónico Severo (A+) para aquellos casos en los que el día sea indeterminado (U) y el punto8 tenga una presión
# atmosférica mayor que 1030 mb. Este solo puede aplicarse con "WT_Nav" como dato de entrada

# M. 1 RASILLA (U, con P8<1010 = C; U, con P8>1020 = A. Aun quedarán días con U (los comprendidos entre 1010 y 1020))
# Solo tiene sentido desde dato Original, porque Trigo no tiene U, y Navarro debería incluir tambien a los A+

for(i in 1:dim(Table)[1]){
  if(!is.na(Table[i,"Rule5"])){
    Table[i,"WT_JC_Ras"] <- "U"
  } else{
    if(!is.na(Table[i,"Rule3"])){
      Table[i,"WT_JC_Ras"] <- Table[i,"Rule3"]
    } else{
      if(!is.na(Table[i,"Rule4"])){
        Table[i,"WT_JC_Ras"] <- paste(Table[i,"Rule4"],Table[i,"Adv"],sep='')
      } else{
        if(!is.na(Table[i,"Rule2"])){
          Table[i,"WT_JC_Ras"] <- Table[i,"Adv"]
        }
      }
    }
  }
}
for(i in 1:dim(Table)[1]){
  if(!is.na(Table[i,"WT_JC_Ras"])){
    if(Table[i,"WT_JC_Ras"]=="U" & Table[i,"P_8"]<1010){
      Table[i,"WT_JC_Ras"] <- "C"
    } else{
      if(Table[i,"WT_JC_Ras"]=="U" & Table[i,"P_8"]>1020){
        Table[i,"WT_JC_Ras"] <- "A"
      } 
    }
  }
}


# M. 2 RASILLA ADAPTADO (U, con P8<1010 = C; U, con P8>1020 = A; U, con P8>1030 = A+. Aun quedarán días con U
# (los comprendidos entre 1010 y 1020)). Solo tiene sentido desde dato Navarro, porque es el que tiene esta clase A+

for(i in 1:dim(Table)[1]){
  if(!is.na(Table[i,"Rule5"])){
    Table[i,"WT_Nav_Nav"] <- "U"
  } else{
    if(!is.na(Table[i,"Rule3"])){
      Table[i,"WT_Nav_Nav"] <- Table[i,"Rule3"]
    } else{
      if(!is.na(Table[i,"Rule4"])){
        Table[i,"WT_Nav_Nav"] <- paste(Table[i,"Rule4"],Table[i,"Adv"],sep='')
      } else{
        if(!is.na(Table[i,"Rule2"])){
          Table[i,"WT_Nav_Nav"] <- Table[i,"Adv"]
        }
      }
    }
  }
}
for(i in 1:dim(Table)[1]){
  if(is.na(Table[i,"WT_Nav_Nav"])){ # si es NA, pues se deja como NA
    Table[i,"WT_Nav_Nav"] <- NA
  } else{
    if(Table[i,"WT_Nav_Nav"]=="A" & Table[i,"P_8"]>1030){ # si es Anticiclonico y ademas tiene más de 1030, pues pon A+
      Table[i,"WT_Nav_Nav"] <- "A+"
    } else{
      Table[i,"WT_Nav_Nav"] <- Table[i,"WT_Nav_Nav"] # si no, pues pon la clasificacion original tal cual
    }
  }
}
for(i in 1:dim(Table)[1]){
  if(!is.na(Table[i,"WT_Nav_Nav"])){
    if(Table[i,"WT_Nav_Nav"]=="U" & Table[i,"P_8"]<1010){
      Table[i,"WT_Nav_Nav"] <- "C"
    } else{
      if(Table[i,"WT_Nav_Nav"]=="U" & Table[i,"P_8"]>1020){
        Table[i,"WT_Nav_Nav"] <- "A"
      } else{
        if(Table[i,"WT_Nav_Nav"]=="U" & Table[i,"P_8"]>1030){
          Table[i,"WT_Nav_Nav"] <- "A+"
        }
      }
    }
  }
}

############################## SINTETIZACIÓN DE TIPOS DE TIEMPO

# METODOS DE SINTETIZARLOS EN MENOS TIPOS. SOLO VOY A PONER EL QUE HAGO YO (DIRECCIONALES A PUROS). Y otras personas que
# QUIERAN USAR TIPOS DE TIEMPO, QUE SE PREOCUPEN ELLOS DE VER QUÉ MÉTODO USA (porque el de 0.5 a cada uno es complicado
# para hacer una serie). El summ viene de "Summarize"
# Método Navarro: Los hibridos pasan todos a direccionales. Desde JC. Desde Trigo. Desde Navarro. Desde TriNav. WT_JC_Ras. WT_Nav_Nav

# Desde WT_JC <- "WT_JC_Summ"
for(i in 1:dim(Table)[1]){
  if(!is.na(Table[i,"WT_JC"])){
    if(nchar(Table[i,"WT_JC"])==3){
      Table[i,"WT_JC_Summ"] <- substr(Table[i,"WT_JC"],start=2, stop=3)
    } else{
      if(nchar(Table[i,"WT_JC"])==2 & substr(Table[i,"WT_JC"],start=2, stop=2)=="+"){
        Table[i,"WT_JC_Summ"] <- Table[i,"WT_JC"]
      } else{
        if(nchar(Table[i,"WT_JC"])==2 & (substr(Table[i,"WT_JC"],start=1,stop=1)=="A" | substr(Table[i,"WT_JC"],start=1,stop=1)=="C")){
          Table[i,"WT_JC_Summ"] <- substr(Table[i,"WT_JC"],start=2, stop=2)
        } else{
          Table[i,"WT_JC_Summ"] <- Table[i,"WT_JC"]
        }
     }
    }
  }
}


# Desde WT_Tri <- "WT_Tri_Summ"
for(i in 1:dim(Table)[1]){
  if(!is.na(Table[i,"WT_Tri"])){
    if(nchar(Table[i,"WT_Tri"])==3){
      Table[i,"WT_Tri_Summ"] <- substr(Table[i,"WT_Tri"],start=2, stop=3)
    } else{
      if(nchar(Table[i,"WT_Tri"])==2 & substr(Table[i,"WT_Tri"],start=2, stop=2)=="+"){
        Table[i,"WT_Tri_Summ"] <- Table[i,"WT_Tri"]
      } else{
        if(nchar(Table[i,"WT_Tri"])==2 & (substr(Table[i,"WT_Tri"],start=1,stop=1)=="A" | substr(Table[i,"WT_Tri"],start=1,stop=1)=="C")){
          Table[i,"WT_Tri_Summ"] <- substr(Table[i,"WT_Tri"],start=2, stop=2)
        } else{
          Table[i,"WT_Tri_Summ"] <- Table[i,"WT_Tri"]
        }
      }
    }
  }
}


# Desde WT_Nav <- "WT_Nav_Summ"
for(i in 1:dim(Table)[1]){
  if(!is.na(Table[i,"WT_Nav"])){
    if(nchar(Table[i,"WT_Nav"])==3){
      Table[i,"WT_Nav_Summ"] <- substr(Table[i,"WT_Nav"],start=2, stop=3)
    } else{
      if(nchar(Table[i,"WT_Nav"])==2 & substr(Table[i,"WT_Nav"],start=2, stop=2)=="+"){
        Table[i,"WT_Nav_Summ"] <- Table[i,"WT_Nav"]
      } else{
        if(nchar(Table[i,"WT_Nav"])==2 & (substr(Table[i,"WT_Nav"],start=1,stop=1)=="A" | substr(Table[i,"WT_Nav"],start=1,stop=1)=="C")){
          Table[i,"WT_Nav_Summ"] <- substr(Table[i,"WT_Nav"],start=2, stop=2)
        } else{
          Table[i,"WT_Nav_Summ"] <- Table[i,"WT_Nav"]
        }
      }
    }
  }
}


# Desde WT_TriNav <- "WT_TriNav_Summ"
for(i in 1:dim(Table)[1]){
  if(!is.na(Table[i,"WT_TriNav"])){
    if(nchar(Table[i,"WT_TriNav"])==3){
      Table[i,"WT_TriNav_Summ"] <- substr(Table[i,"WT_TriNav"],start=2, stop=3)
    } else{
      if(nchar(Table[i,"WT_TriNav"])==2 & substr(Table[i,"WT_TriNav"],start=2, stop=2)=="+"){
        Table[i,"WT_TriNav_Summ"] <- Table[i,"WT_TriNav"]
      } else{
        if(nchar(Table[i,"WT_TriNav"])==2 & (substr(Table[i,"WT_TriNav"],start=1,stop=1)=="A" | substr(Table[i,"WT_TriNav"],start=1,stop=1)=="C")){
          Table[i,"WT_TriNav_Summ"] <- substr(Table[i,"WT_TriNav"],start=2, stop=2)
        } else{
          Table[i,"WT_TriNav_Summ"] <- Table[i,"WT_TriNav"]
        }
      }
    }
  }
}

# Desde WT_JC_Ras <- "WT_JC_Ras_Summ"
for(i in 1:dim(Table)[1]){
  if(!is.na(Table[i,"WT_JC_Ras"])){
    if(nchar(Table[i,"WT_JC_Ras"])==3){
      Table[i,"WT_JC_Ras_Summ"] <- substr(Table[i,"WT_JC_Ras"],start=2, stop=3)
    } else{
      if(nchar(Table[i,"WT_JC_Ras"])==2 & substr(Table[i,"WT_JC_Ras"],start=2, stop=2)=="+"){
        Table[i,"WT_JC_Ras_Summ"] <- Table[i,"WT_JC_Ras"]
      } else{
        if(nchar(Table[i,"WT_JC_Ras"])==2 & (substr(Table[i,"WT_JC_Ras"],start=1,stop=1)=="A" | substr(Table[i,"WT_JC_Ras"],start=1,stop=1)=="C")){
          Table[i,"WT_JC_Ras_Summ"] <- substr(Table[i,"WT_JC_Ras"],start=2, stop=2)
        } else{
          Table[i,"WT_JC_Ras_Summ"] <- Table[i,"WT_JC_Ras"]
        }
      }
    }
  }
}

# Desde WT_Nav_Nav "WT_Nav_Nav_Summ"
for(i in 1:dim(Table)[1]){
  if(!is.na(Table[i,"WT_Nav_Nav"])){
    if(nchar(Table[i,"WT_Nav_Nav"])==3){
      Table[i,"WT_Nav_Nav_Summ"] <- substr(Table[i,"WT_Nav_Nav"],start=2, stop=3)
    } else{
      if(nchar(Table[i,"WT_Nav_Nav"])==2 & substr(Table[i,"WT_Nav_Nav"],start=2, stop=2)=="+"){
        Table[i,"WT_Nav_Nav_Summ"] <- Table[i,"WT_Nav_Nav"]
      } else{
        if(nchar(Table[i,"WT_Nav_Nav"])==2 & (substr(Table[i,"WT_Nav_Nav"],start=1,stop=1)=="A" | substr(Table[i,"WT_Nav_Nav"],start=1,stop=1)=="C")){
          Table[i,"WT_Nav_Nav_Summ"] <- substr(Table[i,"WT_Nav_Nav"],start=2, stop=2)
        } else{
          Table[i,"WT_Nav_Nav_Summ"] <- Table[i,"WT_Nav_Nav"]
        }
      }
    }
  }
}

################
# Generate weather types daily series
################

Result = Table[,c("FECHA","YEAR","MONTH","DAY",classification_method)]
write.csv(Result,paste('./SLPdata/WeatherTypes','.csv',sep=''))

rm(archivo_txt,archivo_txt_agg,dt1,dt2,union,contador,finish,finish_date,i,interv_lat,interv_lon,latp8,lonp8,p01,p02,p03,p04,p05,p06,p07,p08,p09,p10,p11,p12,p13,p14,p15,p16,pointlist,row_number,start,start_date,valor_S,valor_W)































############################# 
# FUNCIÓN "weathertypes" DEJANDO FIJO VALORES 55N DE CORIOLIS
#############################

# Le he metido una condicion para que si no metes la latitud en intervalos de 5 en 5 pues que falle.

weathertypes <- function(latp8,lonp8,start,finish,classification_method){
  ###########################
  ######### WARNING! DONT TOUCH! AUTOMATIC SCRIPT. (RUN ALL, UNTIL THE END)
  ###########################
  # Packages
  # install.packages("curl")
  # install.packages("data.table")
  library(curl)
  library(data.table)
  
  if(is.element(latp8,seq(40,80,by=5)) & is.element(lonp8,seq(-180,180,by=5))){
    interv_lon = 10
    interv_lat = 5
    start_date = as.Date(paste(substr(start,start=1,stop=4),"-",substr(start,start=5,stop=6),"-",substr(start,start=7, stop=8),sep=''), format='%Y-%m-%d')
    finish_date = as.Date(paste(substr(finish,start=1,stop=4),"-",substr(finish,start=5,stop=6),"-",substr(finish,start=7, stop=8),sep=''), format='%Y-%m-%d')
    row_number = difftime(finish_date,start_date)+1
    
    # Create Table
    Table = as.data.frame(array(NA, dim=c(row_number, 4)))
    
    # Write dates
    Table[,1] <- seq(start_date,finish_date,by="days")
    Table[,2] <- as.character(substr(Table[,1],start=1, stop=4))   #year
    Table[,3] <- as.character(substr(Table[,1],start=6, stop=7))   #month
    Table[,4] <- as.character(substr(Table[,1],start=9, stop=10))   #day
    
    # Download SLP points time series (p8 is the reference gridpoint position). NCAR Sea Level Pressure Data (https://climatedataguide.ucar.edu/climate-data/ncar-sea-level-pressure)
    p01 = paste("https://rda.ucar.edu/cgi-bin/dattore/gridts?sd=",start,"&ed=",finish,"&lat=",latp8+(2*interv_lat),"&latdir=N&lon=",lonp8+(0*interv_lon),"&londir=E&format=slp&dsnum=ds010.0&input=dailydata.bin&t=daily",sep='')
    p02 = paste("https://rda.ucar.edu/cgi-bin/dattore/gridts?sd=",start,"&ed=",finish,"&lat=",latp8+(2*interv_lat),"&latdir=N&lon=",lonp8+(1*interv_lon),"&londir=E&format=slp&dsnum=ds010.0&input=dailydata.bin&t=daily",sep='')
    
    p03 = paste("https://rda.ucar.edu/cgi-bin/dattore/gridts?sd=",start,"&ed=",finish,"&lat=",latp8+(1*interv_lat),"&latdir=N&lon=",lonp8-(1*interv_lon),"&londir=E&format=slp&dsnum=ds010.0&input=dailydata.bin&t=daily",sep='')
    p04 = paste("https://rda.ucar.edu/cgi-bin/dattore/gridts?sd=",start,"&ed=",finish,"&lat=",latp8+(1*interv_lat),"&latdir=N&lon=",lonp8+(0*interv_lon),"&londir=E&format=slp&dsnum=ds010.0&input=dailydata.bin&t=daily",sep='')
    p05 = paste("https://rda.ucar.edu/cgi-bin/dattore/gridts?sd=",start,"&ed=",finish,"&lat=",latp8+(1*interv_lat),"&latdir=N&lon=",lonp8+(1*interv_lon),"&londir=E&format=slp&dsnum=ds010.0&input=dailydata.bin&t=daily",sep='')
    p06 = paste("https://rda.ucar.edu/cgi-bin/dattore/gridts?sd=",start,"&ed=",finish,"&lat=",latp8+(1*interv_lat),"&latdir=N&lon=",lonp8+(2*interv_lon),"&londir=E&format=slp&dsnum=ds010.0&input=dailydata.bin&t=daily",sep='')
    
    p07 = paste("https://rda.ucar.edu/cgi-bin/dattore/gridts?sd=",start,"&ed=",finish,"&lat=",latp8+(0*interv_lat),"&latdir=N&lon=",lonp8-(1*interv_lon),"&londir=E&format=slp&dsnum=ds010.0&input=dailydata.bin&t=daily",sep='')
    p08 = paste("https://rda.ucar.edu/cgi-bin/dattore/gridts?sd=",start,"&ed=",finish,"&lat=",latp8+(0*interv_lat),"&latdir=N&lon=",lonp8+(0*interv_lon),"&londir=E&format=slp&dsnum=ds010.0&input=dailydata.bin&t=daily",sep='')
    p09 = paste("https://rda.ucar.edu/cgi-bin/dattore/gridts?sd=",start,"&ed=",finish,"&lat=",latp8+(0*interv_lat),"&latdir=N&lon=",lonp8+(1*interv_lon),"&londir=E&format=slp&dsnum=ds010.0&input=dailydata.bin&t=daily",sep='')
    p10 = paste("https://rda.ucar.edu/cgi-bin/dattore/gridts?sd=",start,"&ed=",finish,"&lat=",latp8+(0*interv_lat),"&latdir=N&lon=",lonp8+(2*interv_lon),"&londir=E&format=slp&dsnum=ds010.0&input=dailydata.bin&t=daily",sep='')
    
    p11 = paste("https://rda.ucar.edu/cgi-bin/dattore/gridts?sd=",start,"&ed=",finish,"&lat=",latp8-(1*interv_lat),"&latdir=N&lon=",lonp8-(1*interv_lon),"&londir=E&format=slp&dsnum=ds010.0&input=dailydata.bin&t=daily",sep='')
    p12 = paste("https://rda.ucar.edu/cgi-bin/dattore/gridts?sd=",start,"&ed=",finish,"&lat=",latp8-(1*interv_lat),"&latdir=N&lon=",lonp8+(0*interv_lon),"&londir=E&format=slp&dsnum=ds010.0&input=dailydata.bin&t=daily",sep='')
    p13 = paste("https://rda.ucar.edu/cgi-bin/dattore/gridts?sd=",start,"&ed=",finish,"&lat=",latp8-(1*interv_lat),"&latdir=N&lon=",lonp8+(1*interv_lon),"&londir=E&format=slp&dsnum=ds010.0&input=dailydata.bin&t=daily",sep='')
    p14 = paste("https://rda.ucar.edu/cgi-bin/dattore/gridts?sd=",start,"&ed=",finish,"&lat=",latp8-(1*interv_lat),"&latdir=N&lon=",lonp8+(2*interv_lon),"&londir=E&format=slp&dsnum=ds010.0&input=dailydata.bin&t=daily",sep='')
    
    p15 = paste("https://rda.ucar.edu/cgi-bin/dattore/gridts?sd=",start,"&ed=",finish,"&lat=",latp8-(2*interv_lat),"&latdir=N&lon=",lonp8+(0*interv_lon),"&londir=E&format=slp&dsnum=ds010.0&input=dailydata.bin&t=daily",sep='')
    p16 = paste("https://rda.ucar.edu/cgi-bin/dattore/gridts?sd=",start,"&ed=",finish,"&lat=",latp8-(2*interv_lat),"&latdir=N&lon=",lonp8+(1*interv_lon),"&londir=E&format=slp&dsnum=ds010.0&input=dailydata.bin&t=daily",sep='')
    
    # Download SLPdata series
    dir.create("./SLPdata/")
    download.file(url = p01, destfile = "./SLPdata/p01.txt", method = "wininet", quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))
    download.file(url = p02, destfile = "./SLPdata/p02.txt", method = "wininet", quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))
    download.file(url = p03, destfile = "./SLPdata/p03.txt", method = "wininet", quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))
    download.file(url = p04, destfile = "./SLPdata/p04.txt", method = "wininet", quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))
    download.file(url = p05, destfile = "./SLPdata/p05.txt", method = "wininet", quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))
    download.file(url = p06, destfile = "./SLPdata/p06.txt", method = "wininet", quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))
    download.file(url = p07, destfile = "./SLPdata/p07.txt", method = "wininet", quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))
    download.file(url = p08, destfile = "./SLPdata/p08.txt", method = "wininet", quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))
    download.file(url = p09, destfile = "./SLPdata/p09.txt", method = "wininet", quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))
    download.file(url = p10, destfile = "./SLPdata/p10.txt", method = "wininet", quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))
    download.file(url = p11, destfile = "./SLPdata/p11.txt", method = "wininet", quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))
    download.file(url = p12, destfile = "./SLPdata/p12.txt", method = "wininet", quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))
    download.file(url = p13, destfile = "./SLPdata/p13.txt", method = "wininet", quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))
    download.file(url = p14, destfile = "./SLPdata/p14.txt", method = "wininet", quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))
    download.file(url = p15, destfile = "./SLPdata/p15.txt", method = "wininet", quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))
    download.file(url = p16, destfile = "./SLPdata/p16.txt", method = "wininet", quiet = FALSE, mode = "w", cacheOK = TRUE, extra = getOption("download.file.extra"))
    
    # Load SLPdata series
    pointlist = list.files('./SLPdata/', pattern='.txt$')
    contador = 5
    
    for(i in length(pointlist):1){
      print(paste("Point ", i," of ", length(pointlist)))
      archivo_txt = read.table(paste("./SLPdata/",pointlist[i],sep=''),header = TRUE, sep="\t",skip=4, row.names=NULL)
      archivo_txt2 = data.frame(do.call('rbind', strsplit(as.character(archivo_txt$DATE.......TIME................VALUE),'    ',fixed=TRUE)))
      archivo_txt3 = archivo_txt2[,1:2]
      archivo_txt3$X3 = as.numeric(as.character(archivo_txt3$X2))
      archivo_txt4=archivo_txt3[,c(1,3)]
      rm(archivo_txt,archivo_txt2,archivo_txt3)
      archivo_txt = archivo_txt4
      rm(archivo_txt4)
      archivo_txt[,"DATE"] <- substr(archivo_txt[,1],start = 1,stop = 10)
      archivo_txt=archivo_txt[,c(3,2)]
      archivo_txt <- archivo_txt[!(is.na(archivo_txt$X3)),]
      
      archivo_txt_agg = aggregate(archivo_txt[, 2], list(archivo_txt$DATE), mean)
      
      names(archivo_txt_agg)[1] <- "FECHA"
      names(archivo_txt_agg)[2] <- paste("P_",i,sep='')
      names(Table)[1] <- "FECHA"
      names(Table)[2] <- "YEAR"
      names(Table)[3] <- "MONTH"
      names(Table)[4] <- "DAY"
      
      Table$FECHA <- as.character(Table$FECHA)
      
      dt1 <- data.table(Table, key = "FECHA")
      dt2 <- data.table(archivo_txt_agg, key = "FECHA")
      union <- dt2[dt1]
      Table[,contador] <- as.data.frame(union)[,2]
      
      contador = contador + 1
    }
    
    # Names of variables
    for(i in 5:20){
      names(Table)[i]<-paste("P_",rev(seq(1:16))[i-4],sep='')
    }
    
    # Westerly flow (W)
    Table[,"W"] <- (0.5*(Table[,"P_12"]+Table[,"P_13"]))-(0.5*(Table[,"P_4"]+Table[,"P_5"]))
    
    # Southerly flow (S)
    Table[,"S"] <- (1/(cos(55*pi/180)))*((0.25*(Table[,"P_5"]+(2*Table[,"P_9"])+Table[,"P_13"]))-(0.25*(Table[,"P_4"]+(2*Table[,"P_8"])+Table[,"P_12"])))
    
    # Resultant flow (F)
    Table[,"F"] <- (Table[,"S"]^2 + Table[,"W"]^2)^0.5
    
    # Westerly shear velocity (ZW)
    Table[,"ZW"] <- (sin((55)*pi/180)/sin((55-5)*pi/180)) * ((0.5*(Table[,"P_15"]+Table[,"P_16"])) - (0.5*(Table[,"P_8"]+Table[,"P_9"]))) - (sin((55)*pi/180)/sin((55+5)*pi/180)) * ((0.5*(Table[,"P_8"]+Table[,"P_9"])) - (0.5*(Table[,"P_1"]+Table[,"P_2"])))
    
    # Southerly shear velocity (ZS)
    Table[,"ZS"] <- (1/(2*(cos(55*pi/180))^2)) * ((0.25*(Table[,"P_6"]+2*(Table[,"P_10"])+Table[,"P_14"])) - (0.25*(Table[,"P_5"]+2*(Table[,"P_9"])+Table[,"P_13"])) - (0.25*(Table[,"P_4"]+2*(Table[,"P_8"])+Table[,"P_12"])) + (0.25*(Table[,"P_3"]+2*(Table[,"P_7"])+Table[,"P_11"])))
    
    # Total shear velocity(Z)
    Table[,"Z"] <- Table[,"ZW"]+Table[,"ZS"]
    
    # Direction of flow (D)
    Table[,"D"] <- atan(Table[,"W"]/Table[,"S"])*180/pi  # Aunque suene raro que aquí sea 180*pi....dejarlo así, porque deben ser rollos de usar atan, en vez de tan y tal.
    
    # Conditions for Angle(º) 4 "CONDITIONS ANGLE"
    # C.0 Si no hay dato, pon en angle <- NA
    # C.1 =SI(Y(W>0;S<0);360+D;"")
    # C.2 ==SI(Y(W>0;S>0);180+D;"")
    # C.3 =SI(Y(W<0;S>0);180+D;"")
    # C.4 =SI(Y(W<0;S<0);D;"")
    
    for(i in 1:dim(Table)[1]){
      valor_W <- Table[i,"W"]
      valor_S <- Table[i,"S"]
      if(is.na(Table[i,"W"]) | is.na(Table[i,"S"])){  ## C.0
        Table[i,"Angle"] <- NA
      } else{
        if(valor_W>0 & valor_S<0){
          Table[i,"Angle"] <- 360+Table[i,"D"]  ## C.1
        } else{
          if(valor_W>0 & valor_S>0){
            Table[i,"Angle"] <- 180+Table[i,"D"]    ## C.2
          } else{
            if(valor_W<0 & valor_S>0){
              Table[i,"Angle"] <- 180+Table[i,"D"]    ## C.3
            } else{
              if(valor_W<0 & valor_S<0){
                Table[i,"Angle"] <- Table[i,"D"]    ## C.4
              }
            }
          }
        }
      }
    }
    
    # Conditions for Direction of Advection (Rule 1)
    for(i in 1:dim(Table)[1]){
      if(is.na(Table[i,"Angle"])){
        Table[i,"Adv"] <- NA
      } else{
        if(Table[i,"Angle"]>337.5 | Table[i,"Angle"]<= 22.5){
          Table[i,"Adv"] <- "N"
        } else{
          if(Table[i,"Angle"]>22.5 & Table[i,"Angle"]<= 67.5){
            Table[i,"Adv"] <- "NE"
          } else{
            if(Table[i,"Angle"]>67.5 & Table[i,"Angle"]<=112.5){
              Table[i,"Adv"] <- "E"
            } else{
              if(Table[i,"Angle"]>112.5 & Table[i,"Angle"]<= 157.5){
                Table[i,"Adv"] <- "SE"
              } else{
                if(Table[i,"Angle"]>157.5 & Table[i,"Angle"]<= 202.5){
                  Table[i,"Adv"] <- "S"
                } else{
                  if(Table[i,"Angle"]>202.5 & Table[i,"Angle"]<= 247.5){
                    Table[i,"Adv"] <- "SW"
                  } else{
                    if(Table[i,"Angle"]>247.5 & Table[i,"Angle"]<=292.5){
                      Table[i,"Adv"] <- "W"
                    } else{
                      if(Table[i,"Angle"]>292.5 & Table[i,"Angle"]<=337.5){
                        Table[i,"Adv"] <- "NW"
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    
    # Rule 2
    # If |Z| is less than F, then the flow is issentially straight and corresponds to a Lamb pure directional type.
    for(i in 1:dim(Table)[1]){
      if(is.na(Table[i,"Z"]) | is.na(Table[i,"F"])){
        Table[i,"Rule2"] <- NA
      } else{
        if(abs(Table[i,"Z"])<Table[i,"F"]){
          Table[i,"Rule2"] <- "PureAdvective"
        }
      }
    }
    
    # Rule 3
    # If |Z| is greater than 2F, then the pattern is strongly cyclonic (Z>0) or anticyclonic (Z<0). This corresponds to Lamb's pure
    # cyclonic and anticyclonic type.
    for(i in 1:dim(Table)[1]){
      if(is.na(Table[i,"Z"]) | is.na(Table[i,"F"])){
        Table[i,"Rule3"] <- NA
      } else{
        if(abs(Table[i,"Z"])>(2*Table[i,"F"])){
          if(Table[i,"Z"]>0){
            Table[i,"Rule3"] <- "C"
          } else{
            Table[i,"Rule3"] <- "A"
          }
        }
      }
    }
    
    # Rule 4
    # If |Z| lies between F and 2F, the the flow is partly(anti) cyclonic and this corresponds to one of Lamb's synoptic/direction
    # hybrid types, e.g. AE (anticyclonic-east)
    for(i in 1:dim(Table)[1]){
      if(is.na(Table[i,"Z"]) | is.na(Table[i,"F"])){
        Table[i,"Rule4"] <- NA
      } else{
        if(   (Table[i,"F"]<abs(Table[i,"Z"])) & (abs(Table[i,"Z"]) < (2*Table[i,"F"])) & Table[i,"Z"]>0 ){
          Table[i,"Rule4"] <- "C"
        } else{
          if(   (Table[i,"F"]<abs(Table[i,"Z"])) & (abs(Table[i,"Z"]) < (2*Table[i,"F"])) & Table[i,"Z"]<0){
            Table[i,"Rule4"] <- "A"
          }
        }
      }
    }
    
    
    # Rule 5
    # If F is less than 6 and |Z| es less than 6, there is a light indeterminate flow corresponding to Lamb's unclassified type U.
    # The choice of 6 is dependent on the grid spacing and would need tuning if used with a finer grid resolution.
    # This rule is dominant front previous rules
    for(i in 1:dim(Table)[1]){
      if(is.na(Table[i,"Z"]) | is.na(Table[i,"F"])){
        Table[i,"Rule5"] <- NA
      } else{
        if(Table[i,"F"]<6 & abs(Table[i,"Z"])<6){
          Table[i,"Rule5"] <- "U"
        }
      }
    }
    
    ## ESE 6 MARTIN VIDE LO CAMBIA POR 4.8 F y 4.2 para Z (según LOS TIPOS SINÓPTICOS DE JENKINSON 
    ## & COLLISON Y LA INTENSIDAD DE LA ISLA DE CALOR BARCELONESA
    ## Javier MARTÍN-VIDE, Mª Carmen MORENO GARCÍA, Víctor M. ARTOLA y Mª José CORDOBILLA). no lo he aplicado porque no queda claro
    
    
    ############################## CÁLCULO DE LOS TIPOS DE TIEMPO
    
    # Hay 4 métodos para calcular los tipos de tiempo, en función de las clases que se introduzcan.:
    #   
    # El método 1 es el desarollado originalmente por Jekinson and Collison, que trabaja con 2 tipos de tiempo básicos (A,C), 8 tipos
    # direccionales (N,NE,E,SE,S,SW,W,NW), 16 tipos de tiempo híbridos (AN,ANE,AE,ASE,AS,ASW,AW,ANW, CN,CNE,CE,CSE,CS,CSW,CW,CNW) y
    # 1 tipo de tiempo Indeterminado (U), para aquellos días donde no se consiga <- "WT_JC"
    # 
    # El método 2 es el desarrollado por Trigo. Su clasificación es idéntica a la desarrollada por J&C, salvo con la diferencia de que
    # no incorpora el tipo de tiempo Indeterminado (U) <- "WT_Tri"
    # 
    # El método 3 es el desarrollado por Navarro. Su clasificación es idéntica a la desarrollada por J&C, salvo con la diferencia de que
    # incorpora un nuevo tipo de tiempo, el Anticiclónico Severo (A+) <- "WT_Nav"
    # 
    # El método 4 es un mix entre el desarrollado por Trigo, y por Navarro. Este no incorpora el tipo de tiempo Indeterminado (U), pero
    # sí incorpora el Anticiclónico Severo (A+) <- "WT_TriNav"
    
    
    # M.1 ORIGINALS (Jekinson and Collison, 1977) <- "WT_JC"
    for(i in 1:dim(Table)[1]){
      if(!is.na(Table[i,"Rule5"])){
        Table[i,"WT_JC"] <- "U"
      } else{
        if(!is.na(Table[i,"Rule3"])){
          Table[i,"WT_JC"] <- Table[i,"Rule3"]
        } else{
          if(!is.na(Table[i,"Rule4"])){
            Table[i,"WT_JC"] <- paste(Table[i,"Rule4"],Table[i,"Adv"],sep='')
          } else{
            if(!is.na(Table[i,"Rule2"])){
              Table[i,"WT_JC"] <- Table[i,"Adv"]
            }
          }
        }
      }
    }
    
    
    # M. 2 ORIGINALS WITHOUT UNCLASSIFIED (Trigo) -> "WT_Trigo"
    
    for(i in 1:dim(Table)[1]){
      if(!is.na(Table[i,"Rule3"])){
        Table[i,"WT_Tri"] <- Table[i,"Rule3"]
      } else{
        if(!is.na(Table[i,"Rule4"])){
          Table[i,"WT_Tri"] <- paste(Table[i,"Rule4"],Table[i,"Adv"],sep='')
        } else{
          if(!is.na(Table[i,"Rule2"])){
            Table[i,"WT_Tri"] <- Table[i,"Adv"]
          }
        }
      }
    }
    
    
    # M. 3 ORIGINALS WITH SEVERE ANTICYCLONIC (Navarro, 2018) -> "WT_Navarro"
    for(i in 1:dim(Table)[1]){
      if(!is.na(Table[i,"Rule5"])){
        Table[i,"WT_Nav"] <- "U"
      } else{
        if(!is.na(Table[i,"Rule3"])){
          Table[i,"WT_Nav"] <- Table[i,"Rule3"]
        } else{
          if(!is.na(Table[i,"Rule4"])){
            Table[i,"WT_Nav"] <- paste(Table[i,"Rule4"],Table[i,"Adv"],sep='')
          } else{
            if(!is.na(Table[i,"Rule2"])){
              Table[i,"WT_Nav"] <- Table[i,"Adv"]
            }
          }
        }
      }
    }
    for(i in 1:dim(Table)[1]){
      if(is.na(Table[i,"WT_Nav"])){ # si es NA, pues se deja como NA
        Table[i,"WT_Nav"] <- NA
      } else{
        if(Table[i,"WT_Nav"]=="A" & Table[i,"P_8"]>1030){ # si es Anticiclonico y ademas tiene más de 1030, pues pon A+
          Table[i,"WT_Nav"] <- "A+"
        } else{
          Table[i,"WT_Nav"] <- Table[i,"WT_Nav"] # si no, pues pon la clasificacion original tal cual
        }
      }
    }
    
    
    # M. 4 ORIGINALS WITHOUT UNCLASSIFIED (Trigo) AND WITH SEVERE ANTICYCLONIC (Navarro)-> "WT_TriNav"
    for(i in 1:dim(Table)[1]){
      if(!is.na(Table[i,"Rule3"])){
        Table[i,"WT_TriNav"] <- Table[i,"Rule3"]
      } else{
        if(!is.na(Table[i,"Rule4"])){
          Table[i,"WT_TriNav"] <- paste(Table[i,"Rule4"],Table[i,"Adv"],sep='')
        } else{
          if(!is.na(Table[i,"Rule2"])){
            Table[i,"WT_TriNav"] <- Table[i,"Adv"]
          }
        }
      }
    }
    for(i in 1:dim(Table)[1]){
      if(is.na(Table[i,"WT_TriNav"])){ # si es NA, pues se deja como NA
        Table[i,"WT_TriNav"] <- NA
      } else{
        if(Table[i,"WT_TriNav"]=="A" & Table[i,"P_8"]>1030){ # si es Anticiclonico y ademas tiene más de 1030, pues pon A+
          Table[i,"WT_TriNav"] <- "A+"
        } else{
          Table[i,"WT_TriNav"] <- Table[i,"WT_TriNav"] # si no, pues pon la clasificacion original tal cual
        }
      }
    }
    
    
    ############################## TRATAMIENTO DEL TIPO DE TIEMPO INDETERMINADO (U)
    
    
    # Hay 2 métodos para reclasificar tipos de tiempo Indeterminado (U)
    #   
    # El método 1 es el desarollado originalmente por Rasilla et al., que reclasifica los días Indeterminados (U) como Anticiclónicos (A)
    # o como Ciclónicos, en función de la presión atmosférica del punto 8 del grid. Si esta es inferior a 1010 (C), o mayor a 1020 (A). P8
    # entre 1010 y 1020 se mantendrán como Indeterminados (U). Este solo puede aplicarse con "WT_JC" como dato de entrada.
    #
    # El método 2 es una adaptación del método 1 de Rasilla, realizada por Navarro. En este caso, el proceso es idéntico, pero se incorpora
    # el tipo de tiempo Anticiclónico Severo (A+) para aquellos casos en los que el día sea indeterminado (U) y el punto8 tenga una presión
    # atmosférica mayor que 1030 mb. Este solo puede aplicarse con "WT_Nav" como dato de entrada
    
    # M. 1 RASILLA (U, con P8<1010 = C; U, con P8>1020 = A. Aun quedarán días con U (los comprendidos entre 1010 y 1020))
    # Solo tiene sentido desde dato Original, porque Trigo no tiene U, y Navarro debería incluir tambien a los A+
    
    for(i in 1:dim(Table)[1]){
      if(!is.na(Table[i,"Rule5"])){
        Table[i,"WT_JC_Ras"] <- "U"
      } else{
        if(!is.na(Table[i,"Rule3"])){
          Table[i,"WT_JC_Ras"] <- Table[i,"Rule3"]
        } else{
          if(!is.na(Table[i,"Rule4"])){
            Table[i,"WT_JC_Ras"] <- paste(Table[i,"Rule4"],Table[i,"Adv"],sep='')
          } else{
            if(!is.na(Table[i,"Rule2"])){
              Table[i,"WT_JC_Ras"] <- Table[i,"Adv"]
            }
          }
        }
      }
    }
    for(i in 1:dim(Table)[1]){
      if(!is.na(Table[i,"WT_JC_Ras"])){
        if(Table[i,"WT_JC_Ras"]=="U" & Table[i,"P_8"]<1010){
          Table[i,"WT_JC_Ras"] <- "C"
        } else{
          if(Table[i,"WT_JC_Ras"]=="U" & Table[i,"P_8"]>1020){
            Table[i,"WT_JC_Ras"] <- "A"
          } 
        }
      }
    }
    
    
    # M. 2 RASILLA ADAPTADO (U, con P8<1010 = C; U, con P8>1020 = A; U, con P8>1030 = A+. Aun quedarán días con U
    # (los comprendidos entre 1010 y 1020)). Solo tiene sentido desde dato Navarro, porque es el que tiene esta clase A+
    
    for(i in 1:dim(Table)[1]){
      if(!is.na(Table[i,"Rule5"])){
        Table[i,"WT_Nav_Nav"] <- "U"
      } else{
        if(!is.na(Table[i,"Rule3"])){
          Table[i,"WT_Nav_Nav"] <- Table[i,"Rule3"]
        } else{
          if(!is.na(Table[i,"Rule4"])){
            Table[i,"WT_Nav_Nav"] <- paste(Table[i,"Rule4"],Table[i,"Adv"],sep='')
          } else{
            if(!is.na(Table[i,"Rule2"])){
              Table[i,"WT_Nav_Nav"] <- Table[i,"Adv"]
            }
          }
        }
      }
    }
    for(i in 1:dim(Table)[1]){
      if(is.na(Table[i,"WT_Nav_Nav"])){ # si es NA, pues se deja como NA
        Table[i,"WT_Nav_Nav"] <- NA
      } else{
        if(Table[i,"WT_Nav_Nav"]=="A" & Table[i,"P_8"]>1030){ # si es Anticiclonico y ademas tiene más de 1030, pues pon A+
          Table[i,"WT_Nav_Nav"] <- "A+"
        } else{
          Table[i,"WT_Nav_Nav"] <- Table[i,"WT_Nav_Nav"] # si no, pues pon la clasificacion original tal cual
        }
      }
    }
    for(i in 1:dim(Table)[1]){
      if(!is.na(Table[i,"WT_Nav_Nav"])){
        if(Table[i,"WT_Nav_Nav"]=="U" & Table[i,"P_8"]<1010){
          Table[i,"WT_Nav_Nav"] <- "C"
        } else{
          if(Table[i,"WT_Nav_Nav"]=="U" & Table[i,"P_8"]>1020){
            Table[i,"WT_Nav_Nav"] <- "A"
          } else{
            if(Table[i,"WT_Nav_Nav"]=="U" & Table[i,"P_8"]>1030){
              Table[i,"WT_Nav_Nav"] <- "A+"
            }
          }
        }
      }
    }
    
    ############################## SINTETIZACIÓN DE TIPOS DE TIEMPO
    
    # METODOS DE SINTETIZARLOS EN MENOS TIPOS. SOLO VOY A PONER EL QUE HAGO YO (DIRECCIONALES A PUROS). Y otras personas que
    # QUIERAN USAR TIPOS DE TIEMPO, QUE SE PREOCUPEN ELLOS DE VER QUÉ MÉTODO USA (porque el de 0.5 a cada uno es complicado
    # para hacer una serie). El summ viene de "Summarize"
    # Método Navarro: Los hibridos pasan todos a direccionales. Desde JC. Desde Trigo. Desde Navarro. Desde TriNav. WT_JC_Ras. WT_Nav_Nav
    
    # Desde WT_JC <- "WT_JC_Summ"
    for(i in 1:dim(Table)[1]){
      if(!is.na(Table[i,"WT_JC"])){
        if(nchar(Table[i,"WT_JC"])==3){
          Table[i,"WT_JC_Summ"] <- substr(Table[i,"WT_JC"],start=2, stop=3)
        } else{
          if(nchar(Table[i,"WT_JC"])==2 & substr(Table[i,"WT_JC"],start=2, stop=2)=="+"){
            Table[i,"WT_JC_Summ"] <- Table[i,"WT_JC"]
          } else{
            if(nchar(Table[i,"WT_JC"])==2 & (substr(Table[i,"WT_JC"],start=1,stop=1)=="A" | substr(Table[i,"WT_JC"],start=1,stop=1)=="C")){
              Table[i,"WT_JC_Summ"] <- substr(Table[i,"WT_JC"],start=2, stop=2)
            } else{
              Table[i,"WT_JC_Summ"] <- Table[i,"WT_JC"]
            }
          }
        }
      }
    }
    
    
    # Desde WT_Tri <- "WT_Tri_Summ"
    for(i in 1:dim(Table)[1]){
      if(!is.na(Table[i,"WT_Tri"])){
        if(nchar(Table[i,"WT_Tri"])==3){
          Table[i,"WT_Tri_Summ"] <- substr(Table[i,"WT_Tri"],start=2, stop=3)
        } else{
          if(nchar(Table[i,"WT_Tri"])==2 & substr(Table[i,"WT_Tri"],start=2, stop=2)=="+"){
            Table[i,"WT_Tri_Summ"] <- Table[i,"WT_Tri"]
          } else{
            if(nchar(Table[i,"WT_Tri"])==2 & (substr(Table[i,"WT_Tri"],start=1,stop=1)=="A" | substr(Table[i,"WT_Tri"],start=1,stop=1)=="C")){
              Table[i,"WT_Tri_Summ"] <- substr(Table[i,"WT_Tri"],start=2, stop=2)
            } else{
              Table[i,"WT_Tri_Summ"] <- Table[i,"WT_Tri"]
            }
          }
        }
      }
    }
    
    
    # Desde WT_Nav <- "WT_Nav_Summ"
    for(i in 1:dim(Table)[1]){
      if(!is.na(Table[i,"WT_Nav"])){
        if(nchar(Table[i,"WT_Nav"])==3){
          Table[i,"WT_Nav_Summ"] <- substr(Table[i,"WT_Nav"],start=2, stop=3)
        } else{
          if(nchar(Table[i,"WT_Nav"])==2 & substr(Table[i,"WT_Nav"],start=2, stop=2)=="+"){
            Table[i,"WT_Nav_Summ"] <- Table[i,"WT_Nav"]
          } else{
            if(nchar(Table[i,"WT_Nav"])==2 & (substr(Table[i,"WT_Nav"],start=1,stop=1)=="A" | substr(Table[i,"WT_Nav"],start=1,stop=1)=="C")){
              Table[i,"WT_Nav_Summ"] <- substr(Table[i,"WT_Nav"],start=2, stop=2)
            } else{
              Table[i,"WT_Nav_Summ"] <- Table[i,"WT_Nav"]
            }
          }
        }
      }
    }
    
    
    # Desde WT_TriNav <- "WT_TriNav_Summ"
    for(i in 1:dim(Table)[1]){
      if(!is.na(Table[i,"WT_TriNav"])){
        if(nchar(Table[i,"WT_TriNav"])==3){
          Table[i,"WT_TriNav_Summ"] <- substr(Table[i,"WT_TriNav"],start=2, stop=3)
        } else{
          if(nchar(Table[i,"WT_TriNav"])==2 & substr(Table[i,"WT_TriNav"],start=2, stop=2)=="+"){
            Table[i,"WT_TriNav_Summ"] <- Table[i,"WT_TriNav"]
          } else{
            if(nchar(Table[i,"WT_TriNav"])==2 & (substr(Table[i,"WT_TriNav"],start=1,stop=1)=="A" | substr(Table[i,"WT_TriNav"],start=1,stop=1)=="C")){
              Table[i,"WT_TriNav_Summ"] <- substr(Table[i,"WT_TriNav"],start=2, stop=2)
            } else{
              Table[i,"WT_TriNav_Summ"] <- Table[i,"WT_TriNav"]
            }
          }
        }
      }
    }
    
    # Desde WT_JC_Ras <- "WT_JC_Ras_Summ"
    for(i in 1:dim(Table)[1]){
      if(!is.na(Table[i,"WT_JC_Ras"])){
        if(nchar(Table[i,"WT_JC_Ras"])==3){
          Table[i,"WT_JC_Ras_Summ"] <- substr(Table[i,"WT_JC_Ras"],start=2, stop=3)
        } else{
          if(nchar(Table[i,"WT_JC_Ras"])==2 & substr(Table[i,"WT_JC_Ras"],start=2, stop=2)=="+"){
            Table[i,"WT_JC_Ras_Summ"] <- Table[i,"WT_JC_Ras"]
          } else{
            if(nchar(Table[i,"WT_JC_Ras"])==2 & (substr(Table[i,"WT_JC_Ras"],start=1,stop=1)=="A" | substr(Table[i,"WT_JC_Ras"],start=1,stop=1)=="C")){
              Table[i,"WT_JC_Ras_Summ"] <- substr(Table[i,"WT_JC_Ras"],start=2, stop=2)
            } else{
              Table[i,"WT_JC_Ras_Summ"] <- Table[i,"WT_JC_Ras"]
            }
          }
        }
      }
    }
    
    # Desde WT_Nav_Nav "WT_Nav_Nav_Summ"
    for(i in 1:dim(Table)[1]){
      if(!is.na(Table[i,"WT_Nav_Nav"])){
        if(nchar(Table[i,"WT_Nav_Nav"])==3){
          Table[i,"WT_Nav_Nav_Summ"] <- substr(Table[i,"WT_Nav_Nav"],start=2, stop=3)
        } else{
          if(nchar(Table[i,"WT_Nav_Nav"])==2 & substr(Table[i,"WT_Nav_Nav"],start=2, stop=2)=="+"){
            Table[i,"WT_Nav_Nav_Summ"] <- Table[i,"WT_Nav_Nav"]
          } else{
            if(nchar(Table[i,"WT_Nav_Nav"])==2 & (substr(Table[i,"WT_Nav_Nav"],start=1,stop=1)=="A" | substr(Table[i,"WT_Nav_Nav"],start=1,stop=1)=="C")){
              Table[i,"WT_Nav_Nav_Summ"] <- substr(Table[i,"WT_Nav_Nav"],start=2, stop=2)
            } else{
              Table[i,"WT_Nav_Nav_Summ"] <- Table[i,"WT_Nav_Nav"]
            }
          }
        }
      }
    }
    
    ################
    # Generate weather types daily series
    ################
    
    Result = Table[,c("FECHA","YEAR","MONTH","DAY",classification_method)]
    write.csv(Result,paste('./SLPdata/WeatherTypes','.csv',sep=''))
    
    rm(archivo_txt,archivo_txt_agg,dt1,dt2,union,contador,finish,finish_date,i,interv_lat,interv_lon,latp8,lonp8,p01,p02,p03,p04,p05,p06,p07,p08,p09,p10,p11,p12,p13,p14,p15,p16,pointlist,row_number,start,start_date,valor_S,valor_W)
    print("Finish")
  } else{
    print("Set correctly latitude-longitude parameters")
  }
}

setwd("c:/")
weathertypes(latp8 = 40,lonp8 = -10,start = 20000101,finish = 20001231,classification_method = c("WT_JC")
)

# Results will be generated in "./SLPdata/WeatherTypes.csv"

###
## FUNCTION PARAMETERS HELP:
###

## Step 1/5: Set work directory
# setwd("c:/")

## Step 1/4: Set p.8 (in degrees, en múltiplos de 5. Southern Hemisphere and Western Hemisphere: negative degrees). P8-P9 are the grid centroid points
#   Please, check the distribution of the whole grid over your study area. The grid must completely cover the study area. 
#   The example is prepared to 40ºN 10ºW as p8. Standard coordinate grid distribution
#   https://freva.met.fu-berlin.de/static/preview/doc/cwt/cwt0y.png (Jones et al., 1993. For British Islands).
#   It must be applied at mid latitudes (between 30-70º, more or less)

# latp8 = 40
# lonp8 = -10
# 
# ## Step 2/3: Set start and end dates (yyyymmdd format)
# start = 19000101  #mantengo este formato para que la fase de descargas pueda usar este objeto llamado "start"
# finish = 20171031
# 
# 
# ## Step 3/3: Set Weather Types method classification/s: (i.e. Prepared by WT_JC, WT_JC_Summ, WT_Nav, WT_Nav_Nav_Summ)
# classification_method = c("WT_JC","WT_JC_Summ","WT_Nav","WT_Nav_Nav_Summ")

#NON-SUMMARIZED METHODS:
"WT_JC"      # Weather Types developed by Jekinson and Collison (1977), based on original classification of Lamb (1972)
"WT_Tri"     # Weather Types developed by Trigo and DaCamara (2000), based on Jekinson and Collison, removing Unclassified (U) type.
"WT_Nav"     # Weather Types devoloped by Navarro et al. (2018), based on Jekinson and Collison, adding Severe Anticyclonic (A+) type.
"WT_TriNav"  # Weather Types based on Jekinson and Collison, removing "U" type (Trigo and DaCamara, 2000), and adding "A+" (Navarro et al. 2018)
"WT_JC_Ras"  # Weather Types developed by Rasilla et al. (2002), based on Jekinson and Collison, reclassifying some "U" as "C" or "A" types. 
"WT_Nav_Nav" # Weather Types developed by Navarro et al. (2018), based on Jekinson and Collison, reclassifying some "U" as "C", "A" or "A+" types.

#SUMMARIZED METHODS (to simplify the original weather types, hybrid types have been reclassified as their directional flow)
"WT_JC_Summ"      # Weather Types developed by Jekinson and Collison (1977), based on original classification of Lamb (1972)
"WT_Tri_Summ"     # Weather Types developed by Trigo and DaCamara (2000), based on Jekinson and Collison, removing Unclassified (U) type.
"WT_Nav_Summ"     # Weather Types devoloped by Navarro et al. (2018), based on Jekinson and Collison, adding Severe Anticyclonic (A+) type.
"WT_TriNav_Summ"  # Weather Types based on Jekinson and Collison, removing "U" type (Trigo and DaCamara, 2000), and adding "A+" (Navarro et al. 2018)
"WT_JC_Ras_Summ"  # Weather Types developed by Rasilla et al. (2002), based on Jekinson and Collison, reclassifying some "U" as "C" or "A" types. 
"WT_Nav_Nav_Summ" # Weather Types developed by Navarro et al. (2018), based on Jekinson and Collison, reclassifying some "U" as "C", "A" or "A+" types.

## Days with NA: No pressure data were available.


















