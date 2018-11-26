#################
#Trabajo Practico Final
#Ola de calor en Diciembre 2013 Buenos Aires

#Se cuenta con datos diarios en puntos de retícula de la temperatura y geopotencial del mes
#de diciembre 2013.

#a) Abrir los archivos y extraer las variables de temperatura y geopotencial en los
#niveles de 1000, 850 y 500 hPa, para todos los tiempos y guardarlas en formato
#binario.

#b) Graficar el campo medio mensual de temperatura y altura geopotencial en un mismo
#grafico para los tres niveles seleccionados.

#c) Graficar 4 mapas del campo de temperatura y altura geopotencial , a las 18 UTC, en
#el nivel de 1000 hPa, para el 10,15,20 y 25 de diciembre 2013.

#d) Buscar el punto de reticula más cercano a Buenos Aires y Mendoza y calcular la
#temperatura máxima y mínima para cada día y graficar la marcha de temperatura
#máxima y mínima para todo el mes diciembre 2013 para ambas ciudades.

#Limpiar el espacio de trabajo
rm(list = ls())

###Abrir archivos NCDF

require(ncdf4)
require(ggplot2)
require(maps)
require(ggpubr)
require(metR)
require(reshape)

#Analizo los datos que tengo 
hgt_nc<-nc_open("HGT_dec2013.nc")
temp_nc<-nc_open("Temp_dec2013.nc")

#extraigo las variables 
hgt<-ncvar_get(hgt_nc,"hgt")
temp<-ncvar_get(temp_nc,"air")

#Convierto los valores faltantes en NA (aunque no hay ninguno)
hgt[hgt==32766]<-NA
temp[temp==32766]<-NA


#En una lista guardo las dimensiones de mis arrays, siendo que ambos tienen las mismas 
nombres_dimensiones<-names(hgt_nc$dim)
datos_dimensiones<-0

for(i in 1:4) {
  datos_dimensiones[i]<-list(ncvar_get(hgt_nc,nombres_dimensiones[i]))
}

names(datos_dimensiones)<-nombres_dimensiones


#####################################
# Item a

#Quiero los datos para los niveles 1000, 850 y 500 hpa de hgt y temp
niveles<-c(which(datos_dimensiones$level==1000),
           which(datos_dimensiones$level==850),
           which(datos_dimensiones$level==500))

hgtRen<-hgt[,,niveles,]
tempRen<-temp[,,niveles,]

#Creo un array vacio con las dimensiones de ambos arrays
array<-array(0,c(dim(hgt),2))

#Lleno el array con los datos 
array[,,,,1]=hgtRen
array[,,,,2]=tempRen

#Lo convierto en un vector para guardar en formato binario
datos<-as.vector(array)

#Creo el espacio
zz<-file("Varibles","wb")

#Guardo los datos
writeBin(datos,zz)


#######################################################
#Item b

#Creo un vector con las fechas separadas cada seis horas utilizando el formato
#POSIX
t1<-datos_dimensiones$time[1]*3600
tiempos<-as.POSIXct((datos_dimensiones$time*3600)-t1,tz="GMT",origin="2013-12-01 00:00:00")
datos_dimensiones$time<-tiempos
  
#Graficar el campo medio mensual para los niveles 850,1000 y 500

#Seteo los parametros de mapa y gradiente 
mapa<-map_data("world2") 
my_fill<-scale_fill_gradient(name=expression(" C°"),high  = "red", low = "royalblue")


#Utilizo un ciclo para generar tres graficos de los tres niveles
for (i in c(1000,850,500)) {
  
  #Busco la posicion del vector "level" donde sea igual al 1000, 850 y 500
  posicion_nivel<-which(datos_dimensiones$level==i)
  
  #Me quedo con los niveles que quiero
  temp_level<-temp[,,posicion_nivel,]
  hgt_level<-hgt[,,posicion_nivel,]
  
  #Calculo la media mensual, es decir la media sobre la dimension temporal
  media_temp<-as.vector(apply(temp_level,c(1,2),mean))
  media_hgt<-as.vector(apply(hgt_level,c(1,2),mean))
  
  #Acomdo los datos en un data frame para ggplot (un data frame para cada uno)
  df_temp<-data.frame(x=rep(datos_dimensiones$lon,length(datos_dimensiones$lat)),
                  y=rep(datos_dimensiones$lat,each=length(datos_dimensiones$lon)),
                  z=matrix(media_temp,nrow=(length(datos_dimensiones$lat)*length(datos_dimensiones$lon)),ncol=1))
 
  df_hgt<-data.frame(x=rep(datos_dimensiones$lon,length(datos_dimensiones$lat)),
                     y=rep(datos_dimensiones$lat,each=length(datos_dimensiones$lon)),
                     z=matrix(media_hgt,nrow=(length(datos_dimensiones$lat)*length(datos_dimensiones$lon)),ncol=1))
  
  #Graficar (x= lon Y=lat)
  ggplot(data=df_temp,aes(x=x,y=y)) +
    geom_tile(aes(fill=z-273)) +
    my_fill +
    geom_text_contour(aes(z=df_hgt$z),skip=1,stroke = 0.2)+
    stat_contour(data=df_hgt,aes(x=x,y=y,z=z),color="black",size=0.5,show.legend = TRUE) +
    geom_polygon(data=mapa,aes(x=long ,y=lat, group=group),fill=NA,color="black",size=0.2) +
    coord_fixed(1) +
    xlim(270,330) + ylim(-70,10) +
    ggtitle(paste("Campo de temperatura y altura geopotencial",i,"hPa")) +
    theme_classic() +
    xlab("Longitud") + ylab("Latitud")
  
  #Guardo los graficos
  ggsave(paste("Campo",i,".pdf",sep=""))
  
  
}



###############################
# item c

lista<-list()

#extraigo los dias que quiero a las 18 UTC
for(j in 1:4) {
  
  dias<-c(10,15,20,25)

  #Busco la posiciones de los dias 10,15,20,25 de diciembre de 2013
  DiasPOSIX<-as.POSIXct(paste("2013-12-",dias[j]," 18:00:00",sep=""),"GMT")
  posicion_dias<-which(datos_dimensiones$time==DiasPOSIX)

  #Evaluo en la matrices en la dimension temporal y en el nivel 1000
  temp2<-as.vector(temp[,,1,posicion_dias])
  hgt2<-as.vector(hgt[,,1,posicion_dias])
  
  #Creo los data frames necesarios
  data_temp<-data.frame(x=rep(datos_dimensiones$lon,length(datos_dimensiones$lat)),
                      y=rep(datos_dimensiones$lat,each=length(datos_dimensiones$lon)),
                      z=matrix(temp2,nrow=(length(datos_dimensiones$lat)*length(datos_dimensiones$lon)),ncol=1))
  
  data_hgt<-data.frame(x=rep(datos_dimensiones$lon,length(datos_dimensiones$lat)),
                      y=rep(datos_dimensiones$lat,each=length(datos_dimensiones$lon)),
                      z=matrix(hgt2,nrow=(length(datos_dimensiones$lat)*length(datos_dimensiones$lon)),ncol=1))
  
  #Grafico 
campo<-ggplot(data=data_temp,aes(x=x,y=y)) +
    geom_tile(aes(fill=z-273)) +
    my_fill +
    geom_text_contour(aes(z=data_hgt$z),skip=3,stroke = 0.2)+
    stat_contour(data=data_hgt,aes(x=x,y=y,z=z),color="black",size=0.5,show.legend = TRUE) +
    geom_polygon(data=mapa,aes(x=long ,y=lat, group=group),fill=NA,color="black",size=0.2) +
    coord_fixed(1) +
    theme_minimal()+
 
    xlim(270,330) + ylim(-70,10) +
    ggtitle(paste(dias[j],"de Diciembre")) +
    xlab("Long") + ylab("Lat")
  
  #Guardo los graficos en una lista
  lista[[j]]<-campo

  
 
  }

#Coloco los cuatro plots en un mismo grafico
p<-ggarrange(plotlist = lista,common.legend = TRUE,legend = "right")
annotate_figure(p,top = text_grob("Campos de temperatura y altura geopotencial",face = "bold"))

ggsave("Campos_b.pdf")


#ACLARACION
#Al guardar los campos en la lista se sobreescriben los numeros asociados a las 
#isolineas de altura geopotencial. Esto puede deberse al procesamiento
#interno de la funcion "geom_text_contour", el cual excede mis conociemientos.


######################################################################
# Item d

#Busco las posiciones de latitud y longitud sean lo mas cercanas a mis valores
BsAs_coor<-c(-34.5997,301.6181) #latitud negativa por ser HS
Mend_coor<-c(-32.8833,291.1667)
Puntos<-list(BsAs_coor,Mend_coor)
names(Puntos)<-c("Buenos Aires","Mendoza")

#Creo matrices de latitud y longitud
lat<-rep(datos_dimensiones$lat,each=length(datos_dimensiones$lon))
matriz_lat<-matrix(lat,byrow = T,ncol = length(datos_dimensiones$lon))

lon<-rep(datos_dimensiones$lon,length(datos_dimensiones$lat))
matriz_lon<-matrix(lon,ncol=length(datos_dimensiones$lon),nrow = length(datos_dimensiones$lat),byrow = T)

# Para cada punto calculo los valores de reticula mas cercanos y grafico
for(k in 1:length(Puntos)) { #este ciclo esta hecho para que se puedan agragar mas puntos
  
  #Resto en ambas matrices los valores correspondientes 
  a<-(matriz_lat-Puntos[[k]][1])^2
  b<-(matriz_lon-Puntos[[k]][2])^2  
  
  #Calculo cuales son las latitudes y coordenadas mas cercanas a las reales
  hipotenusa<-sqrt(a+b)
  coordenadas<-arrayInd(which.min(hipotenusa),.dim = dim(matriz_lat))
  

  #Al restringir a partir de una latitud, una longitud y el nivel 1000,solo tengo un vector
  #de 124 datos temporales
  
  #Me quedo con el punto de grilla
  Locacion<-temp[coordenadas[2],coordenadas[1],1,] #Longitud es la primer dimension 
  
  #Lo convierto a matriz para realizar los calculos diarios
  Locacion<-matrix(Locacion,ncol = 4,nrow = 31)
  
  max<-apply(Locacion,1,max)-273
  min<-apply(Locacion,1,min)-273
  
  #Ordenos los datos en un data frame
  data_locacion<-data.frame(Dias=1:31,Maximo=max,Minimo=min)
  data_locacion<-melt(data_locacion,id.vars = "Dias")
  
  
  #Graficos 
  ggplot(data = data_locacion,aes(x=Dias,y=value,col=variable)) +
    geom_line() +
    geom_point() +
    theme_bw()+
    scale_color_manual(values = c("red","blue"))+
    ggtitle(paste("Marcha de temperatura minima y maxima en",names(Puntos)[k]))+
    ylab("Temperatura (C°)") + labs(color="")
  
  
  ggsave(paste("Marcha",names(Puntos)[k],".pdf",sep = ""))
  
  
  
}


