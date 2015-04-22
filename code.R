
library(rCharts)
library(rjson)

# load data from JSON and convert JSON data to data frame
dat <- rjson::fromJSON(file="heatmap-data.json")
dat <- lapply(dat, function(x) {
  x[sapply(x, is.null)] <- NA
  unlist(x)
})
dat <- matrix(dat$data, ncol=3, byrow=TRUE)
colnames(dat) <- c("x","y","value")

# create Highcharts object
p <- Highcharts$new()

# use type='heatmap' for heat maps
p$chart(zoomType = "x", type = 'heatmap')
p$credits(text = "Created with rCharts and Highcharts", href = "http://rcharts.io")
p$title(text='Sales per employee per weekday')

p$series(name = 'Sales per employee',
         data = toJSONArray2(dat, json=FALSE),
         color = "#cccccc",
         dataLabels = list(
           enabled = TRUE,
           color = 'black',
           style = list(
             textShadow = 'none',
             HcTextStroke = NULL
           )
         ))

p$xAxis(categories = c('Alexander', 'Marie', 'Maximilian', 'Sophia', 'Lukas', 
                       'Maria', 'Leon', 'Anna', 'Tim', 'Laura'))

p$yAxis(categories = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'),
        title=list(text = ""))

# colorAxis is required for heat maps        
p$addParams(colorAxis = 
              list(min = 0,
                   minColor='#FFFFFF',
                   maxColor='#7cb5ec'
              )
)

p$legend(align='right',
         layout='vertical',
         margin=0,
         verticalAlign='top',
         y=25,
         symbolHeight=320)

# custom tooltip
p$tooltip(formatter = "#! function() { return '<b>' + this.series.xAxis.categories[this.point.x] + '</b> sold <br><b>' +
          this.point.value + '</b> items on <br><b>' + this.series.yAxis.categories[this.point.y] + '</b>'; } !#")

# need to link additional JavaScript files here that do not ship with rCharts
p$addAssets(js = 
              c("https://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js",
                "https://code.highcharts.com/highcharts.js",
                "https://code.highcharts.com/highcharts-more.js",
                "https://code.highcharts.com/modules/exporting.js",
                "https://code.highcharts.com/modules/heatmap.js"
              )
)

# save to standalone HTML page
p$save(destfile = 'heatmap.html')

# open chart in browser
print(p)