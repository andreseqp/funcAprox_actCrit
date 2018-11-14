# ----------- Aesthetic paramters ------------------#

# Define Colours -------------------------------------------------------

colours <- c(rgb(red = .0, green = 0, blue = 0.8, alpha = 0.5),
             rgb(red = .8, green = 0.8, blue = 0, alpha = 0.5))

colboxes<- c('#d7191c','#fdae61','#2b83ba')

coloptions<- c("#d53e4f", "#fc8d59","#fee08b","#e6f598", "#99d594","#3288bd",
               '#d7191c','#fdae61')

colMany<-c("#e41a1c","#377eb8", "#4daf4a", "#984ea3", "#ff7f00", 
           "#a65628", "#f781bf", "#999999","black")

colVisitors<-c(rgb(255,255,212,255,maxColorValue=255),
               rgb(254,227,145,255,maxColorValue=255),
                   rgb(254,196,79,255,maxColorValue=255),
                   rgb(254,153,41,255,maxColorValue=255),
                   rgb(217,95,14,255,maxColorValue=255),
                   rgb(153,52,4,255,maxColorValue=255))[6:1]

colResidents<-c(rgb(255,255,204,255,maxColorValue = 255),
                rgb(217,240,163,255,maxColorValue = 255),
                rgb(173,221,142,255,maxColorValue = 255),
                rgb(120,198,121,255,maxColorValue = 255),
                rgb(49,163,84,255,maxColorValue= 255),
                rgb(0,104,55,255,maxColorValue = 255))[6:1]

paletteCont <- colorRampPalette(c('#d73027','#fc8d59','#fee090',
                                   '#e0f3f8','#91bfdb','#4575b4')[6:1],
                                 alpha=TRUE)
  
palette('default')
