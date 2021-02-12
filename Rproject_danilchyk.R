#### Muj projekt predstavuje malou hru, kde uzivatel muze 
# zvolit ze dvou moznosti: 1. - interaktivni mapa Evropy,
# zobrazujici mesta a zalidneni, 2. - hra, kde uzivatel 
# hraje s pocitacem a voli mesta, ktera se zacinaji pismenem,
# na ktere predchozi mesto konci. 
# V obou pripadech byla pouzita vstupni data, datova sada
# world.cities je soucasti balicku maps, a pouzita prostorova data lze 
# snadno nahradit jinymi.
# Doufam, ze si to uzijete :)


# Nahrani potrebnych pro zpracovani dat balicku
library(leaflet)
library(sp)
library(maps)
library(geojsonio)
library(rlist)
library(Hmisc)
library(gdata)


### Priprava dat pro prvni funkci (mapa)

# Nahrani potrebnych prostorovych dat
getwd()
setwd("C:/Users/Данильчик/Documents/UK/Rko/Rproject_danilchyk")
hranice <- geojson_read("./hranice84.geojson",what="sp")
mesta <- geojson_read("./mesta84.geojson",what="sp")

# Priprava dat pro legendu a popupy
bins <- c(0, 50000, 100000, 150000, 300000, 500000, Inf)
pal <- colorBin("YlOrRd", domain = hranice$area, bins = bins)
labels <- sprintf(
  "<strong>%s</strong><br/>%g km<sup>2</sup>",
  hranice$NAME_LATN, hranice$area
) %>% lapply(htmltools::HTML)
mesta$POP <- ifelse(mesta$POP==0,"no data", round(mesta$POP/1000,digits=1))


### Samotna funkce vytvoreni interaktivni mapy.
# Vim, ze jsme to vubec neprobirali, ale studuji kartografii, 
# tak jsem byla moc zvedava jak se da vizualizovat data v Rku. A chtela 
# jsem zaroven pouzit nastroj Shiny, ale zatim jsem k tomu moc nedostala.

m1 <- leaflet(hranice) %>%
  # Nastaveni polohy zobrazovani mapy
  setView(lng = 14.337, lat = 50.075, zoom = 4) %>%
  # Pridani nekolika mapovych podkladu
  addTiles(group="OSM") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Terrain Map") %>%
  # Nastaveni parametru pridani vrstvy "hranice". Krome barev,
  # tam se taky urcuji nastaveni zoomin/zoomout a informace, ktera se zobrazi pri kliknuti
  addPolygons(
    group = "Staty",
    fillColor = ~pal(area),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
  # Pridani legendy na zaklade vyse definovanych promennych
  addLegend(pal = pal, values = ~area, opacity = 0.7, title = "Plocha zeme",
            position = "bottomright") %>%
  # Pridani nove bodove vrstvy a definovani popupu pri kliknuti
  addMarkers(data = mesta, 
             group = "Mesta",
             popup = paste(
               #"<b>", mesta$CITY_NAME, "</b><br/>",
               "town: ", mesta$CITY_NAME, "<br/>",
               "province: ", mesta$ADMIN_NAME,"<br/>",
               "population (ths. ppl): ", mesta$POP),
             clusterOptions = markerClusterOptions()
             ) %>%
  # Pridani ovladaciho panelu, ktery umoznuje vypnuti vrstev a prepinani 
  # mezi mapovymi podklady
  addLayersControl(
    baseGroups = c("OSM", "Terrain Map"),
    overlayGroups = c("Staty", "Mesta"),
    options = layersControlOptions(collapsed = FALSE))


### Hra "Mesta". Pouziva vestaveny dataset "world.cities".
# Na zacatku byly preddefinovany 3 funkce, ktere pak byly
# pridany do konecne varianty.
data(world.cities)

# Definuje a kontroluje vstupni data, ktere uvadi uzivatel
f1 <- function(){
  mesto <- capitalize(readline(prompt = "Please choose the town: "))
  while(!(mesto %in% world.cities$name)){
    print(paste(mesto, " is not on the list. Choose another one"))
    mesto <- capitalize(readline(prompt = "Try again, please: "))
  }
  return(mesto)
} 

# Pridava a uklada vybrane mesto do seznamu
f2 <- function(mesto,seznam){
  seznam <- list.append(seznam,mesto)
  return(seznam)
}  

# Urcuje podminku, aby pocitac vybiral mesto na zaklade posledniho pismena 
# predesleho slova, zvoleneho uzivatelem
f3 <- function(seznam){
  last <- substring(seznam[length(seznam)], nchar(seznam[length(seznam)]))
  demand <- world.cities$name[which(startsWith(world.cities$name,last,ignore.case = TRUE))]
  demand <- sample(demand,1)
  seznam <- list.append(seznam,demand)
  print(seznam[length(seznam)])
}

### Definovani samotne funkce hry o mestech. 
m2 <- function(){
  game.list <- vector(mode="character",length=0L)
  mesto <- f1()
  res <- f3(f2(mesto,game.list))
  gameover <- FALSE
  while(!gameover){
    continue <- readline(prompt = "Do you want continue playing? Y or N: ")
    continue <- toupper(continue)
    if(continue=="Y"){
      mesto <- f1()
      comp <- substring(res[length(res)], nchar(res[length(res)]))
      while(comp != tolower(substring(mesto,1,1))){
        print(paste("The word must start with ", toupper(comp)))
        mesto <- f1()
      }
      print("well done!")
      res <- f3(f2(mesto,res))
    }
    if(continue=="N"){
      print("That was a nice play. See you later")
      gameover <- TRUE
    }
  } 
}


### Konecna funkce, ktera spojuje vsechny predchozi a definuje hru

welcome <- function(){
  print("Welcome to the atlas guidepost. There are 2 options: 1-mapa, 2-mesta. If you don't want to play, type 'quit'")
  volba <- readline(prompt = "Make your choice: ")
  volba <- tolower(volba)
  ans <- c("mapa", "mesta", "quit")
  while(!(volba %in% ans)){
    print("Please follow our rules")
    volba <- readline(prompt = "Enter a valid response: ")
  }
  if(volba=="mapa"){
    return(m1)
  }
  if(volba=="mesta"){
    return(m2())
  }
  if(volba == "quit"){
    print("Thanks for your time. Bye!")
  }
}

welcome()



