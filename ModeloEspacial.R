#Preprocesar
library(spgwr)
library(spatialreg)
library(sf)
library(tmap)
library(spdep)
library(MASS)
library(car)
library(lmtest)
library(ggpubr)
library(GGally)
library(caret)
library(xtable)
setwd('C:/Users/Asus/Downloads/Programas') 
espac <- st_read(
  "content/Espacio_Publico")
idx = espacio_publico_recortado$X
for(i in 1:length(idx)){
  idx[i] = idx[i] +1
}
espac = espac[idx,]
espac = sf::st_make_valid(espac)
tm_shape(espac) +
  tm_fill() +
  tm_borders()
datos = data_total_s
process <- preProcess(as.data.frame(datos), method=c("range"))
datos <- predict(process, as.data.frame(datos))
#datos = as.data.frame(scale(datos))
new_df = espac$geometry
new_df <- cbind(datos,new_df)
new_df <- na.omit(new_df)
new_df$centroid <- st_centroid(new_df$geometry)
seab<-dnearneigh(new_df$centroid,0,1)
seaw<-nb2listw(seab, style="W", zero.policy = TRUE)
#moran.plot(new_df$hom, listw=seaw )
moran.mc(new_df$iv_hom, seaw, nsim=999)
fit_2 = lm(iv_hom~ denslum+denscam+densarb+distDeporte+distEduc+distCultural+numDeporte+numEduc+numCultural+distFuerzaPub+distJus+numJus+distBares+distEstacion+numBares+numComercio+numEstaciones+AreaBuff+densPob+comuna_10+mod_estrato+mod_educacion+hom_jov+usos_..Uso.Dotacional..+usos_..Áreas.de.baja.mixtura..+usos_..Áreas.y.corredores.de.alta.mixtura..+usos_..Áreas.y.corredores.de.media.mixtura..,data = new_df)
summary(fit_2)
vif(fit_2)
print(xtable(summary(fit_2),type='latex'))
#AIC(fit_2)
#bptest(fit_2) #Existe
#vif(fit_2)
#plot(resid(fit_2))
#ggdensity(fit_2$residuals, 
#          main = "Density plot of tooth length",
#          xlab = "Tooth length")
#shapiro.test(fit_2$residuals)
lm.LMtests(fit_2, seaw, test = c("LMerr","LMlag","RLMerr","RLMlag","SARMA"))
fit.lag<-lagsarlm(hurt ~ denslum+denscam+densarb+distDeporte+distEduc+distCultural+numDeporte+numEduc+numCultural+distFuerzaPub+distJus+numJus+distBares+distEstacion+numBares+numComercio+numEstaciones+AreaBuff+densPob+comuna_10+mod_estrato+mod_educacion+hom_jov+usos_..Uso.Dotacional..+usos_..Áreas.de.baja.mixtura..+usos_..Áreas.y.corredores.de.alta.mixtura..+usos_..Áreas.y.corredores.de.media.mixtura..,data = new_df, listw = seaw) 
summary(fit.lag)
print(xtable(summary(fit.lag),type='latex'))
#fit.err<-errorsarlm(hurtos ~numComercio+numBares+denslum+densarb+denscam+numJus+numEduc+numCultural+numDeporte+mod_estrato+mod_educacion+hom_jov+distEduc+distDeporte+distBares+distEstacion+numEstaciones+AreaBuff+distCultural+distFuerzaPub+distJus+comuna_10+usos_..Uso.Dotacional..+usos_..Áreas.de.baja.mixtura..+usos_..Áreas.y.corredores.de.alta.mixtura..+usos_..Áreas.y.corredores.de.media.mixtura.. ,data = new_df, listw = seaw) 
#summary(fit.err)
coor = st_coordinates(new_df$centroid)
bandw = gwr.sel(hurt ~ denslum+denscam+densarb+distDeporte+distEduc+distCultural+numDeporte+numEduc+numCultural+distFuerzaPub+distJus+numJus+distBares+distEstacion+numBares+numComercio+numEstaciones+AreaBuff+densPob+comuna_10+mod_estrato+mod_educacion+hom_jov+usos_..Uso.Dotacional..+usos_..Áreas.de.baja.mixtura..+usos_..Áreas.y.corredores.de.alta.mixtura..+usos_..Áreas.y.corredores.de.media.mixtura.., data=new_df, coords=coor, adapt = T)
gwr.model = gwr(hurt ~ denslum+denscam+densarb+distDeporte+distEduc+distCultural+numDeporte+numEduc+numCultural+distFuerzaPub+distJus+numJus+distBares+distEstacion+numBares+numComercio+numEstaciones+AreaBuff+densPob+comuna_10+mod_estrato+mod_educacion+hom_jov+usos_..Uso.Dotacional..+usos_..Áreas.de.baja.mixtura..+usos_..Áreas.y.corredores.de.alta.mixtura..+usos_..Áreas.y.corredores.de.media.mixtura.., data=new_df, coords=coor, adapt = bandw, hatmatrix=TRUE, se.fit=TRUE) 
gwr.model
results<-as.data.frame(gwr.model$SDF)
gwr.point1<-ggplot(new_df, aes(x=coor[,1],y=coor[,2]))+geom_point(aes(colour=results$mod_educacion))+scale_colour_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, space = "rgb", na.value = "grey50", guide = "colourbar", guide_legend(title="Coefs"))
gwr.point1+geom_path(data=boroughoutline,aes(long, lat, group=id), colour="grey")+coord_equal()
gwr.point1
gwr.point2<-ggplot(new_df, aes(x=coor[,1],y=coor[,2]))+geom_point(aes(colour=results$denscam))+scale_colour_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, space = "rgb", na.value = "grey50", guide = "colourbar", guide_legend(title="Coefs"))
gwr.point2+geom_path(data=boroughoutline,aes(long, lat, group=id), colour="grey")+coord_equal()
gwr.point2
gwr.point3<-ggplot(new_df, aes(x=coor[,1],y=coor[,2]))+geom_point(aes(colour=results$usos_..Áreas.y.corredores.de.alta.mixtura..))+scale_colour_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, space = "rgb", na.value = "grey50", guide = "colourbar", guide_legend(title="Coefs"))
gwr.point3+geom_path(data=boroughoutline,aes(long, lat, group=id), colour="grey")+coord_equal()
gwr.point3

new_df <- st_as_sf(new_df)
tm_shape(new_df, unit = "mi") +
  tm_polygons(col = "iv_hurt", style = 'quantile', palette = c("orange","red", "black"), 
              border.alpha = 0.1, title = "") +
  tm_scale_bar(breaks = c(0, 2, 4), text.size = 1, position = c("right", "bottom")) +
  tm_layout(main.title = "IV robberies, Medellín 2018-2021 ",  main.title.size = 0.95, frame = FALSE, legend.outside = TRUE, 
            attr.outside = TRUE)
gwr.model$SDF
