#load('POL1415av.Rdata')
pol.map <- get_map(location=c(lon=21.73, lat=49.58), zoom=11, maptype="hybrid")
Earth$val.cut <- cut(Earth$val, breaks=c(0.1, 0.16, 0.25, 0.4, 0.64, 1,
                                         1.6, 2.5, 4, 6.4, 10, 16, 25, 40)) 
colors <- colorRampPalette(c("#111111", "#232323", "#464646", "#002695","#0146FB", 
                             "#468F19", "#7AF231", "#BFB925","#FFF836", 
                             "#F08920", "#BC340E", "#EA4517", "#FFFFFF"))(length(levels(Earth$val.cut)))
ggmap(pol.map) + geom_raster(alpha = 0.8, data=Earth, aes(x=lon, y=lat, fill=val.cut), interpolate = TRUE) + scale_fill_manual(values=setNames(colors, levels(Earth$val.cut))) + coord_cartesian()
