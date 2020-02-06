# devtools::install_github("ramnathv/rCharts")
require(rCharts)

## gestisce emissioni
Emis <- read.table("/home/giovanni/R/projects/sankey-plot/dat/emissioni-2010.csv",sep=",",header = T)
## proporzione Primario, Secondario, Naturale, calcolata come media
## delle 3 zone: Agglomerato, Pianura Est e Pianura Ovest
fPSN <- c(0.23,0.61,0.16)
## invece includendo l'Appennino:
## fPSN <- c(0.20,0.62,0.18)
emis.pri.tot <- sum(Emis$PM10)
emis.sec.tot <- emis.pri.tot/fPSN[1]*fPSN[2]
emis.nat.tot <- emis.pri.tot/fPSN[1]*fPSN[3]
## http://www3.epa.gov/ttnchie1/conference/ei12/part/johansson.pdf
AF.given <- c(0.64,0.88,0.54,0.02)  # Aerosol formation factors (AF) da letteratura per NH3,NO2,SO2,VOC
names(AF.given) <- colnames(Emis)[-1:-2]
Emis.sec.estim <- t(t(Emis[,-1:-2]) * AF.given) # secondarie calcolate usando AF da letteratura
emis.sec.estim <- rowSums(Emis.sec.estim) 
emis.sec.tot.estim <- sum(emis.sec.estim)
emis.sec <- emis.sec.estim / emis.sec.tot.estim * emis.sec.tot # riscalate sul nostro totale
AF.local <- signif(AF.given / emis.sec.tot.estim * emis.sec.tot, 2)
Emis.sec <- t(t(Emis[,-1:-2]) * AF.local) # secondarie calcolate usando AF locali

## costruisce i link
Pri <- data.frame(source=Emis$source, target="aerosol primario", value=Emis$PM10)
Sec <- data.frame(source=Emis$source, target="aerosol secondario", value=emis.sec)
Tot <- data.frame(source=c("aerosol primario","aerosol secondario","aerosol naturale"),
                  target=rep("PM10",3),
                  value=c(emis.pri.tot,emis.sec.tot,emis.nat.tot))
links <- rbind(Pri,Sec,Tot)
links$value <- round(links$value)

#now we finally have the data in the form we need
sankeyPlot <- rCharts$new()
#sankeyPlot$setLib('~/R/projects/sankey-plot/rCharts_d3_sankey-gh-pages/')
sankeyPlot$setLib('libraries/widgets/d3_sankey')
#sankeyPlot$setTemplate(script = "~/R/projects/sankey-plot/rCharts_d3_sankey-gh-pages/layouts/chart.html")
sankeyPlot$setTemplate(script = "libraries/widgets/d3_sankey/layouts/chart.html")

sankeyPlot$set(
  data = links,
  nodeWidth = 15,
  nodePadding = 30,
  layout = 32,
  width = 600,
  height = 400,
  units = "Mg[PM10eq]/y",
  title = "Sankey Diagram"
)

#sankeyPlot$setLib('http://timelyportfolio.github.io/rCharts_d3_sankey')

sankeyPlot$setTemplate(
  afterScript = "
  <script>
  // to be specific in case you have more than one chart
  d3.selectAll('#{{ chartId }} svg path.link')
  .style('stroke', function(d){
  //here we will use the source color
  //if you want target then sub target for source
  //or if you want something other than gray
  //supply a constant
  //or use a categorical scale or gradient
  return d.source.color;
  })
  //note no changes were made to opacity
  //to do uncomment below but will affect mouseover
  //so will need to define mouseover and mouseout
  //happy to show how to do this also
  // .style('stroke-opacity', .7) 
  </script>
  ")

sankeyPlot$setTemplate(
  afterScript = "
  <script>
  d3.selectAll('#{{ chartId }} svg .node rect')
  .style('stroke', 'none')
  .style('fill', function(d){
  if (d.name == 'aerosol naturale') { 
  return('Green'); 
  } else if (d.name == 'aerosol secondario') { 
  return('Brown');
  } else if (d.name == 'aerosol primario')  {
  return('Navy');
  } else {
  return('DarkGrey');
  }
  })
  d3.selectAll('#{{ chartId }} svg path.link')
  .style('stroke', function(d){
  if (d.source.name == 'aerosol naturale' | d.target.name == 'aerosol naturale') { 
  return('Green'); 
  } else if (d.source.name == 'aerosol secondario' | d.target.name == 'aerosol secondario') { 
  return('Brown');
  } else {
  return('Navy');
  }
  })
  </script>
  ") 


sankeyPlot$save('~/R/projects/sankey-plot/out/sorgentiPM10.html',cdn=TRUE)
