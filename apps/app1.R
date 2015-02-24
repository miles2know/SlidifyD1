
library(shiny)
library(ggvis)


dat <- read.table("C:/Users/jrudokas/Desktop/Notes - Data Experiments/Stat Learning/SlidifyPresentations/Learning/Deck1/assets/files/refData.csv",header=TRUE,sep=",")
guideLine <- read.table("C:/Users/jrudokas/Desktop/Notes - Data Experiments/Stat Learning/SlidifyPresentations/Learning/Deck1/assets/files/guide.csv",header=TRUE,sep=",")
goalLine <- read.table("C:/Users/jrudokas/Desktop/Notes - Data Experiments/Stat Learning/SlidifyPresentations/Learning/Deck1/assets/files/goal.csv",header=TRUE,sep=",")




data_values1 <- function(x) {
 
     if(is.null(x)) return(NULL)
     if(is.null(x$ID)) return(NULL)
 
     all_data <- isolate(x)
     data <- all_data[all_data$ID == x$ID,]
 
     paste0("<b>", data$Scenario, "</b><br>", data$Year, "<br>", 
     format(data$ghgT,digits=0), collapse = "<br />")

} 

plot1g <- reactive({ dat %>% ggvis(~Year,~ghgT,stroke = ~Scenario) %>% layer_lines(strokeWidth := 2.5,strokeWidth.hover := 5) %>% 
     layer_points(size := 25, size.hover := 100,fillOpacity := 0.1, fillOpacity.hover := 0.25,key := ~ID) %>%
     add_tooltip(data_values1, "hover") %>% scale_numeric("y",domain = c(10,225)) %>%
     add_axis("y",format = "d",title = "Million Metric Tons",title_offset = 50,properties = axis_props(labels = list(fontSize = 15))) %>% 
     add_axis("x",format = "d",title = "",properties = axis_props(labels = list(angle = -45,fontSize = 15,align = "right"))) %>% 
     layer_paths(x = ~x, y = ~y, stroke := "black",strokeWidth := 2.5, opacity := .5, data = guideLine) %>%
     layer_paths(x = ~year, y = ~goal, stroke := "red",strokeWidth := 3.5,strokeDash := 5, data = goalLine) })

plot1g %>% bind_shiny("plot1")
 
