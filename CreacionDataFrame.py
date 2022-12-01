from re import I
import geopandas as gp
import pandas as pd
from shapely import wkt
import matplotlib.pyplot as plt
import numpy as np
from scipy import stats
from collections import defaultdict
import geopy.distance
from statistics import mode
import skgstat as skg

def get_espacio_publico(path):
  #Leemos espacios públicos y quitamos los 60% más pequeños
  espacpub = gp.read_file(path)
  espacpub['geometry']=espacpub['geometry'].to_crs(epsg=4326)
  ub = np.percentile(espacpub['SHAPEAREA'],95)
  lb = np.percentile(espacpub['SHAPEAREA'],60)
  espacpub = espacpub[(lb<=espacpub.loc[:,'SHAPEAREA']) & (espacpub.loc[:,'SHAPEAREA']<ub)]
  limite_catastral = gp.read_file('content/Límite_Catastral_de__Comunas_y_Corregimientos.zip')
  limite_catastral = limite_catastral.iloc[0:16,:]
  perimetro = limite_catastral.loc[0,'geometry']
  for i in limite_catastral.loc[:,'geometry']:
      perimetro=perimetro.union(i)
  espacpub = espacpub[espacpub.loc[:,'geometry'].within(perimetro)]
  return espacpub

def get_buffer(buffer, espacpub): 
  '''
  Parámetro: Longitud de buffer (en metros)
  Retorna: Buffer de los espacios públicos
  '''
  espacBuff = espacpub.to_crs("EPSG:3115")
  espacBuff["geometry"] = espacBuff.buffer(distance=buffer) #Distancia
  espacBuff['geometry']=espacBuff['geometry'].to_crs(epsg=4326)
  return espacBuff

#Homicidios
def get_homicidios(path, buffer, espacpub):
  '''
  Parámetro: Path, longitud de buffer para el conteo (en metros)
  Retorna: Conteos de homicidios en cada parque
  '''
  homi = pd.read_csv(path, sep=';')
  homi["geometry"] = homi["geometry"].apply(wkt.loads)
  dfhomicidios = gp.GeoDataFrame(homi, geometry='geometry')
  dfhomicidios.crs = 'epsg:4326'
  dfhomicidios['geometry'] = dfhomicidios['geometry'].to_crs(epsg=4326)
  espacBuff = get_buffer(buffer, espacpub)
  #Contar
  joined_df = gp.sjoin(dfhomicidios, espacBuff, how='right', predicate='within',)
  hom = joined_df.groupby(['OBJECTID'], as_index=False,).count()['fecha_hecho'].tolist()
  return hom

#Hurtos
def get_hurtos(path, buffer, espacpub):
  '''
  Parámetro: Path, longitud de buffer para el conteo (en metros)
  Retorna: Conteos de hurtos en cada parque
  '''
  #Recorté los datos de los hurtos para que fueran también de 2018-2022
  hurtos = pd.read_csv(path, sep = ';', encoding = 'ISO 8859-1')
  hurtos["geometry"] = hurtos["geometry"].apply(wkt.loads)
  dfhurtos = gp.GeoDataFrame(hurtos, geometry='geometry')
  dfhurtos.crs = 'epsg:4326'
  dfhurtos['geometry'] = dfhurtos['geometry'].to_crs(epsg=4326)
  espacBuff = get_buffer(buffer,espacpub)
  #Contar por año
  joined_df = gp.sjoin(dfhurtos, espacBuff, how='right', predicate='within',)
  hurt = joined_df.groupby(['OBJECTID'], as_index=False,).count()['cantidad'].tolist()
  return hurt

def get_luminarias(path,buffer, espacpub):
  '''
  Parámetro: Path, longitud de buffer para el conteo (en metros)
  Retorna: Diccionario con conteos de luminarias por cada parque
  '''
  #Iluminación
  luminarias = gp.read_file(path)
  luminarias['geometry']=luminarias['geometry'].to_crs(epsg=4326)
  luminarias = luminarias.drop(columns = ['PROYECTO','UBICACION','USO_TRASFO', 'MUNICIPIO', 'NOMBRE_PRO'])
  buffer = get_buffer(10, espacpub)
  #Conteo
  joined_df = gp.sjoin(luminarias, buffer, how='right', predicate='within',)
  counts = joined_df.groupby(['OBJECTID_right'], as_index=False,)['OBJECTID_left'].count()
  return counts['OBJECTID_left'].tolist()

def get_camaras(path, buffer, espacpub):
  '''
  Parámetro: Path, longitud de buffer para el conteo (en metros)
  Retorna: Conteos de cámaras  en cada parque
  '''
  #Cámaras seguridad
  camaras = gp.read_file(path)
  camaras['geometry']=camaras['geometry'].to_crs(epsg=4326)
  espacBuff = get_buffer(buffer,espacpub)
  #Conteo
  joined_df = gp.sjoin(camaras, espacBuff, how='right', predicate='within',)
  cam = joined_df.groupby(['OBJECTID'], as_index=False,).count()['Año'].tolist()
  return cam

def get_proporciones(espacpub): #Sólo lo uso para poder categorizar los parques en el paper
  categorias = espacpub.loc[:,"CATEGORIA"].unique()
  prop_i = []
  n, m = espacpub.shape
  for cat in categorias:
    cont = 0
    for i in espacpub.loc[:,'CATEGORIA']:
      if cat == i:
        cont += 1
    prop_i.append(str((cont/n)*100)+' %')
  proporciones = dict(zip(categorias,prop_i))
  return proporciones

def conteos_equip(equip, buffer, espacpub):
  '''
  Parámetro: df del equipamiento deseado, longitud de buffer para el conteo (en metros)
  Retorna: Lista de conteos del equipamiento en cada parque+buffer.
  '''
  poligonoBuff = get_buffer(buffer, espacpub)
  poligonoBuff = poligonoBuff['geometry']
  equip = equip['geometry']
  conts = []
  for p in poligonoBuff:
    cont = 0
    for i in equip:
      if i.within(p) or i.crosses(p):
        cont += 1
    conts.append(cont)
  return conts

def get_distancia(equip, espacpub):
  '''
  Parámetro: df del equipamiento deseado
  Retorna: Lista con la distancia del equipamiento más cercano para cada parque
  '''
  poligono = espacpub['geometry']
  equip = equip['geometry']
  minDist = []
  for p in poligono:
    dists = []
    for e in equip:
      dists.append(geopy.distance.geodesic((p.centroid.x,p.centroid.y),(e.centroid.x,e.centroid.y)).m)
    minDist.append(min(dists))
  return minDist

def get_equip(path):
  '''
  Parámetro: path de equipamientos
  Retorna: df de cada tipo de equipamiento
  '''
  #Equipamientos (Estaciones de policía, bomberos, inst. educativas)
  equip = gp.read_file(path)
  equip['geometry']=equip['geometry'].to_crs(epsg=4326)
  #Equipamientos de Educación
  educ = equip.loc[equip['COMPONENTE'] == 'Equipamiento de Educación']
  #Equipamientos de Recreación y Deporte
  deporte = equip.loc[equip['COMPONENTE'] == 'Equipamiento de Recreación y Deporte']
  #Equipamientos de Justicia Cercana al Ciudadanos
  jus = equip.loc[equip['COMPONENTE'] == 'Equipamientos de Justicia Cercana al Ciudadano']
  #Equipamienyos culturales
  cultural = equip.loc[equip['COMPONENTE'] == 'Equipamientos Culturales']
  #Equipamientos para la Fuerza Pública
  fuerzapub = equip.loc[equip['COMPONENTE'] == 'Equipamientos para la Fuerza Pública']
  return educ, deporte, jus, cultural, fuerzapub

def get_dens_pob(path, buffer, espacpub):
  '''
  Parámetros: Path de censo, longitud del buffer (metros) y espacio público
  Retorna: Promedio de densidad poblacional de cada parque
  '''
  #Censo
  densidad_manzanas = []
  censo = gp.read_file(path)
  censo['geometry']=censo['geometry'].to_crs(epsg=4326)
  espacBuff = get_buffer(buffer, espacpub)
  espacBuff = espacBuff['geometry']
  geom = censo['geometry']
  densidades = censo['HA_TOT_PER']
  i = []
  for p in espacBuff:
    arrInfo = []
    for i,j in zip(geom, densidades):
      if i.within(p) or i.crosses(p):
        arrInfo.append(j)
    densidad_manzanas.append(arrInfo) #Lista de listas con densidades de manzana por parque 
  #Promedio de densidad poblacional
  densPob = []
  for lista in densidad_manzanas:
    suma = 0
    length = 0
    for j in range(len(lista)):
      if not np.isnan(lista[j]):
        suma += lista[j]
        length += 1
    densPob.append(suma/length) if length != 0 else densPob.append(np.nan) 
  return densPob

def get_usos_suelo(path, buffer, espacpub):
  '''
  Parámetros: Path de usos de suelo, longitud del buffer (metros) y espacio público
  Retorna: Moda de uso de suelo de cada parque
  '''
  #Usos del suelo
  usos = gp.read_file(path)
  usos['geometry']=usos['geometry'].to_crs(epsg=4326)
  espacBuff = get_buffer(buffer, espacpub)
  espacBuff = espacBuff['geometry']
  geom = usos['geometry']
  categ = usos['AREAGRALUS']
  usos_parque = []
  for p in espacBuff:
    arrInfo = []
    for i,j in zip(geom, categ):
      if i.within(p) or i.crosses(p):
        arrInfo.append(j)
    usos_parque.append(arrInfo)
  #Moda del uso de suelo en un buffer de 100 metros
  usos_parque = np.array(usos_parque)
  moda_usos = [stats.mode(x)[0] for x in usos_parque]
  return moda_usos

def get_arboles(path,buffer,espacpub):
  '''
  Parámetro: Path, longitud de buffer para el conteo (en metros)
  Retorna: Conteos de árboles en cada parque
  '''
  #Árboles
  arb = gp.read_file(path)
  arb['geometry']=arb['geometry'].to_crs(epsg=4326)
  espacBuff = get_buffer(buffer,espacpub)
  #Conteo
  joined_df = gp.sjoin(arb, espacBuff, how='right', predicate='within',)
  arboles = joined_df.groupby(['OBJECTID_right'], as_index=False,).count()['RULEID'].tolist()
  return arboles

def get_bares(path, buffer, espacpub):
  '''
  Parámetros: Path de bares
  Retorna: GeoDataFrame de bares
  '''
  bares = gp.read_file(path)
  bares['geometry']=bares['geometry'].to_crs(epsg=4326)
  espacBuff = get_buffer(buffer,espacpub)
  #Conteo
  joined_df = gp.sjoin(bares, espacBuff, how='right', predicate='within',)
  numBares = joined_df.groupby(['OBJECTID'], as_index=False,).count()['NOMBRE_DEL'].tolist()
  return bares, numBares

def get_comercio(path, buffer, espacpub):
  '''
  Parámetros: Path de establecimientos comerciales
  Retorna: GeoDataFrame de establecimietos comerciales
  '''
  comercio = gp.read_file(path)
  comercio['geometry']=comercio['geometry'].to_crs(epsg=4326)
  espacBuff = get_buffer(buffer,espacpub)
  #Conteo
  joined_df = gp.sjoin(comercio, espacBuff, how='right', predicate='within',)
  numComercio = joined_df.groupby(['OBJECTID_right'], as_index=False,).count()['ID_CONTRAT'].tolist()
  return comercio, numComercio

def get_estaciones(path, buffer, espacpub):
  '''
  Parámetros: Path de estaciones de metro
  Retorna: GeoDataFrame estaciones de metro
  '''
  estaciones = gp.read_file(path)
  estaciones['geometry'] = estaciones['geometry'].to_crs(epsg=4326)
  espacBuff = get_buffer(buffer,espacpub)
  #Conteo
  joined_df = gp.sjoin(estaciones, espacBuff, how='right', predicate='within',)
  numEstaciones = joined_df.groupby(['OBJECTID'], as_index=False,).count()['nombre'].tolist()
  return estaciones, numEstaciones

def intensityvalue(path,buffer,espacpub):
  '''
  Parámetros: Path de hurtos/homidicios, longitud del buffer en métros y espacio público
  Retorna: Intensity value de hurtos/homicidios
  '''
  data = pd.read_csv(path, sep = ';', encoding = 'ISO 8859-1')
  data["geometry"] = data["geometry"].apply(wkt.loads)
  data = gp.GeoDataFrame(data, geometry='geometry')
  data.crs = 'epsg:4326'
  data['geometry'] = data['geometry'].to_crs(epsg=4326)
  data = data['geometry']
  espacBuff = get_buffer(buffer, espacpub)
  espacBuff = espacBuff['geometry']
  espacpub = espacpub['geometry']
  weighted_count = [0]*len(espacpub)
  for ocurr in data:
    for i, (park,buff) in enumerate(zip(espacpub,espacBuff)):
      if ocurr.within(park):
        weighted_count[i] += 1
        break
      elif ocurr.within(buff):
        centro = buff.centroid
        dist = geopy.distance.geodesic((ocurr.x, ocurr.y),(centro.x,centro.y)).m
        weighted_count[i] += 1/dist
        break
  return weighted_count
    
def get_socioeconomicas(path, buffer, espacpub):
    '''
    Parámetro: Path de censo, longitud del buffer en métros
    Retorna: Moda del estrato y nivel educativo, y media de población de mujeres
    '''
    censo = gp.read_file(path)
    censo['geometry']=censo['geometry'].to_crs(epsg=4326)
    espacBuff = get_buffer(buffer, espacpub)
    joined_df = gp.sjoin(censo, espacBuff, how='right', predicate='intersects',)
    mod_edu = joined_df.groupby(['OBJECTID'], as_index=False,).agg(pd.Series.mode)['edu_mo'].tolist()
    mod_estrato = joined_df.groupby(['OBJECTID'], as_index=False,).agg(pd.Series.mode)['estrato_mo'].tolist()
    hom_jov = joined_df.groupby(['OBJECTID'], as_index=False,).mean()['pob_jov_h'].tolist()
    muj_jov = joined_df.groupby(['OBJECTID'], as_index=False,).mean()['pob_jov_m'].tolist()
    return mod_estrato, mod_edu, hom_jov, muj_jov

def main():
  '''
  Conteos de  hurtos y homicidios
  '''
  espacpub = get_espacio_publico('/workspaces/Programas/content/Espacio_Publico_Existente.zip')
  hom = get_homicidios('content/Homicidios_2018-2022.csv',10, espacpub)
  hurt = get_hurtos('content/HurtosRecortado.csv',10, espacpub)
  '''
  Intensity values de hurtos y homicidios
  '''
  iv_hom = intensityvalue('content/Homicidios_2018-2022.csv',10, espacpub)
  iv_hurt = intensityvalue('content/HurtosRecortado.csv',10, espacpub)
  print('llegué')
  '''
  Conteos de covariables en buffer de 100 metros
  '''
  arboles = get_arboles('content/Arbol.zip',10,espacpub)
  lum = get_luminarias('content/Luminarias.zip', 10, espacpub)
  cam = get_camaras('content/Cam_sep2021.zip', 10, espacpub)
  educ, deporte, jus, cultural, fuerzapub = get_equip('content/Equipamientos.zip')
  numEduc = conteos_equip(educ, 100, espacpub)
  numDeporte = conteos_equip(deporte, 100, espacpub)
  numJus = conteos_equip(jus, 100, espacpub)
  numCultural = conteos_equip(cultural, 100, espacpub)
  numFuerzaPub = conteos_equip(fuerzapub, 100, espacpub)
  bares, numBares = get_bares('content/Bares.zip',100,espacpub)
  comercio, numComercio = get_comercio('content/Establecimiento_comercial.zip',100,espacpub)
  estaciones, numEstaciones = get_estaciones('content/Estaciones_Sistema_Metro.zip',100,espacpub)
  print('llegué')
  '''
  Distancias de covariables
  ''' 
  distEduc = get_distancia(educ, espacpub)
  distDeporte = get_distancia(deporte, espacpub)
  distFuerzaPub = get_distancia(fuerzapub, espacpub)
  distJus = get_distancia(jus, espacpub)
  distCultural = get_distancia(cultural, espacpub)
  distBares = get_distancia(bares,espacpub)
  distEstacion = get_distancia(estaciones,espacpub)
  print('llegué')
  '''
  Variables socioeconomicas
  '''
  mod_estrato, mod_educacion, hom_jov, muj_jov = get_socioeconomicas('content/Censo.zip',200, espacpub)
  comuna = espacpub['COD_COMUNA']
  comuna = comuna.tolist()
  usos = get_usos_suelo(r'content/Usos_suelo.zip', 200, espacpub)
  densPob = get_dens_pob('content/Censo.zip', 200, espacpub)
  '''
  Area
  '''
  area = espacpub['SHAPEAREA']
  '''
  Creación DF
  '''
  columnss = ['distBares',  'distEstacion','numBares', 'numComercio', 'numEstaciones','arboles','usos', 'area', 'densPob', 'distEduc', 'distDeporte', 'distCultural', 'distFuerzaPub', 'distJus', 'numEduc', 'numDeporte', 'numCultural', 'numFuerzaPub', 'numJus','lum', 'cam', 'comuna', 'mod_estrato','mod_educacion','hom_jov','muj_jov', 'hom', 'hurt', 'iv_hom', 'iv_hurt'] 
  agreg = pd.DataFrame([distBares, distEstacion, numBares, numComercio, numEstaciones, arboles,usos, area, densPob, distEduc, distDeporte, distCultural, distFuerzaPub, distJus, numEduc, numDeporte,  numCultural, numFuerzaPub, numJus,lum, cam, comuna, mod_estrato, mod_educacion,hom_jov, muj_jov, hom, hurt, iv_hom, iv_hurt])
  agreg_ = agreg.T
  agreg_.columns = columnss
  agreg_ = pd.get_dummies(agreg_, columns = ['comuna'])
  agreg_.to_csv('content/data_total.csv', sep = ';')

if __name__ == '__main__':
  main()