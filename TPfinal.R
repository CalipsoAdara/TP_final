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

#d) Buscar el punto de grilla más cercano a Buenos Aires y Mendoza y calcular la
#temperatura máxima y mínima para cada día y graficar la marcha de temperatura
#máxima y mínima para todo el mes diciembre 2013 para ambas ciudades.


####abrir archivos NCDF

require(ncdf4)
require(ggplot2)
require(maps)
require(directlabels)
require(ggpubr)

#Analizo los datos que tengo 
hgt_nc<-nc_open("HGT_dec2013.nc")
temp_nc<-nc_open("Temp_dec2013.nc")

#extraigo las variables 
hgt<-ncvar_get(hgt_nc,"hgt")
temp<-ncvar_get(temp_nc,"air")

####sentencias utiles para mi
names(temp_nc$var)
dim(hgt)
#####



#En una lista guardo las dimensiones de mis arrays, siendo que ambos tienen las mismas 
nombres_dimensiones<-names(hgt_nc$dim)
datos_dimensiones<-0

for(i in 1:4) {
  datos_dimensiones[i]<-list(ncvar_get(hgt_nc,nombres_dimensiones[i]))
  
}

names(datos_dimensiones)<-nombres_dimensiones
datos_dimensiones


#Quiero los datos para los niveles 1000, 850 y 500 hpa de hgt y temp
g<-hgt[,,c(1,3,6),]
t<-temp[,,c(1,3,6),]

array<-array(0,c(21,33,3,124,2))
h<-as.vector(array)

array[,,,,1]=g
array[,,,,2]=t

#espacio
zz<-file("Varibles","wb")

writeBin(h,zz)
readBin("Varibles",what = )

help("writeBin")

#leerlo y meterlo en un nuevo array y restarle el viejo. tomar el maximo y minimo si es cero
#######################################################
#item b

#tiempos bien
t1<-datos_dimensiones$time[1]*3600
tiempos<-as.POSIXct((datos_dimensiones$time*3600)-t1,tz="GMT",origin="2013-12-01 00:00:00")
tiempos
  
#Graficar el campo medio mensual para los niveles 850,1000 y 500


#Seteo los parametros de mapa y gradiente 
mapa<-map_data("world2") #xlim=c(270,320), ylim=c(-70,10))
my_fill<-scale_fill_gradient(name=expression(" C°"),high  = "red", low = "royalblue")

for (i in c(1,3,6)) {
  
  #Me quedo con los niveles que quiero
  temp_level<-temp[,,i,]
  hgt_level<-hgt[,,i,]
  
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
    stat_contour(data=df_hgt,aes(x=x,y=y,z=z),color="black",size=0.5,show.legend = TRUE) +
    geom_polygon(data=mapa,aes(x=long ,y=lat, group=group),fill=NA,color="black",size=0.2) +
    coord_fixed(1) +
    theme_transparent()+
    xlim(270,330) + ylim(-70,10) +
    ggtitle(paste("Campo de temperatura",i))
  
  #Guardo los graficos
  ggsave(paste("campo",i,".pdf"))
  
  
}

seq(df2$z[1],df2$z[693],100)

direct.label(a,list("bottom.pieces"))

help("direct.label")

temp_level

temp_level_1000<-temp[,,1,]
temp_level_850<-temp[,,3,]
temp_level_500<-temp[,,6,]

hgt_level_1000<-hgt[,,1,]
hgt_level_850<-hgt[,,3,]
hgt_level_500<-hgt[,,6,]

#Calcular la media mensual, es decir sobre el tiempo (una par 1000 otra para 850, 500)
media_temp_1000<-as.vector(apply(temp_level_1000,c(1,2),mean))
media_temp_850<-as.vector(apply(temp_level_850,c(1,2),mean))
media_temp_500<-as.vector(apply(temp_level_500,c(1,2),mean))

media_hgt_1000<-as.vector(apply(hgt_level_1000,c(1,2),mean))
media_hgt_850<-as.vector(apply(hgt_level_850,c(1,2),mean))
media_hgt_500<-as.vector(apply(hgt_level_500,c(1,2),mean))

#Acomodar datos en data frame 
df1<-data.frame(x=rep(datos_dimensiones$lon,length(datos_dimensiones$lat)),
                y=rep(datos_dimensiones$lat,each=length(datos_dimensiones$lon)),
                z=matrix(media_temp_1000,nrow=(length(datos_dimensiones$lat)*length(datos_dimensiones$lon)),ncol=1))

df2<-data.frame(x=rep(datos_dimensiones$lon,length(datos_dimensiones$lat)),
                y=rep(datos_dimensiones$lat,each=length(datos_dimensiones$lon)),
                z=matrix(media_hgt_1000,nrow=(length(datos_dimensiones$lat)*length(datos_dimensiones$lon)),ncol=1))

  


#Graficar (x= lon Y=lat)
ggplot(data=df1,aes(x=x,y=y)) +
 
  geom_tile(aes(fill=z-273)) +
  my_fill +
  stat_contour(data=df2,aes(x=x,y=y,z=z),color="black",size=0.5,show.legend = TRUE) +
  geom_polygon(data=mapa,aes(x=long ,y=lat, group=group),fill=NA,color="black",size=0.2) +
  coord_fixed(1) +
  theme_transparent()+
  labs(title = "Campo de temperatura")+
  xlim(270,330) + ylim(-70,10)

seq(df2$z[1],df2$z[693],100)

direct.label(a,list("bottom.pieces"))

  help("direct.label")
ggsave("campo.pdf")

map1<-map_data("world")
map1$long
#buscar tonalidades
colors()[grep("blue",colors())]

my_fill<- scale_fill_gradientn(name=expression("C"),colours=rev(brewer.pal(11,"RdYlBu")),breaks=seq(270,320,2.5),limits=c(270,320))
my_fill<-scale_fill_gradient(high  = "red", low = "royalblue")

###############################
# item c

#acomodo tiempos
datos_dimensiones$time<-tiempos
datos_dimensiones$time

#extraigo los dias que quiero a las 18 UTC
which(datos_dimensiones$time==as.POSIXct("2013-12-10 18:00:00","GMT"))

for(j in c(10,15,20,25)){

  #Busco la posiciones de los dias 10,15,20,25 de diciembre de 2013
  DiasPOSIX<-as.POSIXct(paste("2013-12-",j," 18:00:00",sep=""),"GMT")
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
  
  #Grafico el campo de temperatura 
  ggplot(data=data_temp,aes(x=x,y=y)) +
    geom_tile(aes(fill=z-273)) +
    my_fill +
    stat_contour(data=data_hgt,aes(x=x,y=y,z=z),color="black",size=0.5,show.legend = TRUE) +
    geom_polygon(data=mapa,aes(x=long ,y=lat, group=group),fill=NA,color="black",size=0.2) +
    coord_fixed(1) +
    theme_transparent()+
    xlim(270,330) + ylim(-70,10) +
    ggtitle("Campo de temperatura")
  
  }
 help("ggtitle")
class(datos_dimensiones$time[40])
p<-as.vector(hgt[,,1,40])
p1<-as.vector(hgt[,,1,60])
class(p)

data_10<-data.frame(x=rep(datos_dimensiones$lon,length(datos_dimensiones$lat)),
                y=rep(datos_dimensiones$lat,each=length(datos_dimensiones$lon)),
                z=matrix(p,nrow=(length(datos_dimensiones$lat)*length(datos_dimensiones$lon)),ncol=1))

data_15<-data.frame(x=rep(datos_dimensiones$lon,length(datos_dimensiones$lat)),
                    y=rep(datos_dimensiones$lat,each=length(datos_dimensiones$lon)),
                    z=matrix(p1,nrow=(length(datos_dimensiones$lat)*length(datos_dimensiones$lon)),ncol=1))


campo10<-ggplot(data=data_10,aes(x=x,y=y)) +
  geom_tile(aes(fill=z))
campo15<-ggplot(data=data_15,aes(x=x,y=y))+
  geom_tile(aes(fill=z))

ggarrange(campo10,campo15,campo10,campo15)
