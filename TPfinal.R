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
require(directlabels)
require(ggpubr)
require(metR)
require(reshape)

#Analizo los datos que tengo 
hgt_nc<-nc_open("HGT_dec2013.nc")
temp_nc<-nc_open("Temp_dec2013.nc")

#extraigo las variables 
hgt<-ncvar_get(hgt_nc,"hgt")
temp<-ncvar_get(temp_nc,"air")

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

g<-hgt[,,niveles,]
t<-temp[,,niveles,]

#Creo un array vacio con las dimensiones de ambos arrays
array<-array(0,c(21,33,3,124,2))

#Lleno el array con los datos 
array[,,,,1]=g
array[,,,,2]=t

#Lo convierto en un vector para guardar en formato binario
h<-as.vector(array)

#Creo el espacio
zz<-file("Varibles","wb")

#Guardo los datos
writeBin(h,zz)


bin<-readBin("Varibles",what ="numeric",n=515592 )

array2<-array(bin,c(21,33,3,124,2))

bin2<-array-array2
max(bin2)
min(bin2)
which.max(bin2)
which(bin2!=0)
help("writeBin")

#leerlo y meterlo en un nuevo array y restarle el viejo. tomar el maximo y minimo si es cero



#######################################################
#Item b

#Creo un vector con las fechas separadas cada seis horas utilizando el formato
#POSIX
t1<-datos_dimensiones$time[1]*3600
tiempos<-as.POSIXct((datos_dimensiones$time*3600)-t1,tz="GMT",origin="2013-12-01 00:00:00")
datos_dimensiones$time<-tiempos
  
#Graficar el campo medio mensual para los niveles 850,1000 y 500

#Seteo los parametros de mapa y gradiente 
mapa<-map_data("world2") #xlim=c(270,320), ylim=c(-70,10))
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
    geom_text_contour(aes(z=df_hgt$z),skip=1)+
    stat_contour(data=df_hgt,aes(x=x,y=y,z=z),color="black",size=0.5,show.legend = TRUE) +
    geom_polygon(data=mapa,aes(x=long ,y=lat, group=group),fill=NA,color="black",size=0.2) +
    coord_fixed(1) +
    xlim(270,330) + ylim(-70,10) +
    ggtitle(paste("Campo de temperatura y altura geopotencial",i,"hPa")) +
    theme_classic() +
    xlab("Longitud") + ylab("Latitud")
  
  #Guardo los graficos
  ggsave(paste("Campo",i,".pdf"))
  
  
}







ggplot(data=df_temp,aes(x=x,y=y)) +
  geom_tile(aes(fill=z-273)) +
  my_fill +
  stat_contour(data=df_hgt,aes(x=x,y=y,z=z),color="black",size=0.5,show.legend = TRUE) +
  geom_text_contour(aes(z=df_hgt$z),skip = 1,stroke = 0.1)+
  geom_polygon(data=mapa,aes(x=long ,y=lat, group=group),fill=NA,color="black",size=0.2) +
  coord_fixed(1) +
  
  xlim(270,330) + ylim(-70,10) +
  ggtitle(paste("Campo de temperatura",i))





seq(df2$z[1],df2$z[693],100)

direct.label(p,list("bottom.pieces"))

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

  







  help("direct.label")
ggsave("campo.pdf")

map1<-map_data("world")
map1$long
#buscar tonalidades
colors()[grep("orange",colors())]

my_fill<- scale_fill_gradientn(name=expression("C"),colours=rev(brewer.pal(11,"RdYlBu")),breaks=seq(270,320,2.5),limits=c(270,320))
my_fill<-scale_fill_gradient(high  = "red", low = "royalblue")

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
  
  #Grafico el campo de temperatura 
  campo<-ggplot(data=data_temp,aes(x=x,y=y)) +
    geom_tile(aes(fill=z-273)) +
    my_fill +
    geom_text_contour(aes(z=data_hgt$z),skip=3)+
    stat_contour(data=data_hgt,aes(x=x,y=y,z=z),color="black",size=0.5,show.legend = TRUE) +
    geom_polygon(data=mapa,aes(x=long ,y=lat, group=group),fill=NA,color="black",size=0.2) +
    coord_fixed(1) +
    theme_minimal()+
    theme(axis.text.x = element_blank(),axis.text.y = element_blank(),
          axis.title.x = element_blank(),axis.title.y = element_blank()) +
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



ggplot(data=data_temp,aes(x=x,y=y)) +
  geom_tile(aes(fill=z-273)) +
  my_fill +
  stat_contour(data=data_hgt,aes(x=x,y=y,z=z),color="black",size=0.5,show.legend = TRUE) +
  geom_text_contour(aes(z=data_hgt$z),skip=3,rotate = TRUE)+
  geom_polygon(data=mapa,aes(x=long ,y=lat, group=group),fill=NA,color="black",size=0.2) +
  coord_fixed(1) +
   theme_minimal()+
  theme(axis.text.x = element_blank(),axis.text.y = element_blank(),
        axis.title.x = element_blank(),axis.title.y = element_blank()) +
  xlim(270,330) + ylim(-70,10) +
  ggtitle(paste(dias[j],"de Diciembre")) +
  xlab("Longitud") + ylab("Latitud")


######################################################################
# Item d

# Mendoza 32°53′00″S 68°50′00″O 
# Buenos Aires 34°35′59″S 58°22′55″O

#convetidos 
#32.88333333 S 68.83333333 o
#34.59972222 s 58.38194444 o

#Convertidos bien
#32.88333333 S  291.1667
#34.59972222 s  301.6181


#Busco las posiciones de longitud y latitud sean lo mas cercanas a mis valores
datos_dimensiones$lat

#Creo matrices 

lat<-rep(datos_dimensiones$lat,each=length(datos_dimensiones$lon))
matriz_lat<-matrix(lat,byrow = T,ncol = length(datos_dimensiones$lon))

lon<-rep(datos_dimensiones$lon,length(datos_dimensiones$lat))
matriz_lon<-matrix(lon,ncol=length(datos_dimensiones$lon),nrow = length(datos_dimensiones$lat),byrow = T)

#le resto a ambas matrices los valores correspondientes de el punto BsAs
a<-(matriz_lat-34.599)^2
b<-(matriz_lon-301.6181)^2

#Calculo cuales son las latitudes y coordenadas mas cercanas a las reales
hipotenusa<-sqrt(a+b)
coordenadas<-arrayInd(which.min(hipotenusa),.dim = c(33,21))
coordenadas

hipotenusa[coordenadas]



#me quedo con una lat, una long y el nivel 1000 por lo tanto solo tengo un vector
#de 124 datos temporales

#supongo que son dos graficos: uno de bs as y otro de mendoza

#Me quedo con el punto de grilla
BsAs<-temp[1,4,1,]
BsAs<-matrix(BsAs,ncol = 4,nrow = 31)

max_bsas<-apply(BsAs,1,max)-273
max_bsas
min_bsas<-apply(BsAs,1,min)-273

#convertirlo a matrix 4 columnas y 31 filas y sacas maximos y min 
#Calculo temperatura minima y maxima diaria



data_bsas<-data.frame(Dias=1:31,Maximo=max_bsas,Minimo=min_bsas)
data_bsas<-melt(data_bsas,id.vars = "Dias")


#graficos de prueba
ggplot(data = data_bsas,aes(x=Dias,y=value,col=variable)) +
  geom_line() +
  geom_point() +
  theme_bw()+
  scale_color_manual(values = c("red","blue"))+
  ggtitle("Marcha diaria de Temperatura minima y maxima en Buenos Aires")+
  ylab("Temperatura (C°)") + labs(color="")




