# INTRO -------------------------------------------------------------------
# Test simple preliminary models [needs cleaning]
#   Simon Kassel
#   Created: 23 Mar 17

# load helper functions
source("https://raw.githubusercontent.com/simonkassel/IRAIL/master/scripts/IRAIL_helper_functions_032317.R")

# load packages
packages(c("caret", "dplyr", "ggplot2", "C50", "spatstat", "rgdal", "plyr"))

# global options
options(stringsAsFactors = FALSE)
options(scipen = "999")

# data
dat2 <- read.csv("https://raw.githubusercontent.com/simonkassel/IRAIL/master/data/trip_data_cleaned.csv")
stations <- read.csv("https://raw.githubusercontent.com/simonkassel/IRAIL/master/data/stations.csv")


# LINE FIXED EFFECTS ------------------------------------------------------

tl <- read.csv("~/Dropbox/IRAIL/data/Trips_with_lines.txt", stringsAsFactors = TRUE)

dat2$line <- tl$name
dat3 <- filter(dat2, complete.cases(dat2$line))

lines <- unique(dat3$line) %>% as.list()

line_fe <- ldply(lines, function(i) {
  dd <- dat3
  dd <- within(dd, line <- relevel(line, ref = as.character(i)))
  glm1 <- glm(occ_binary ~ line, dd, family = "binomial")
  return(coef(summary(glm1))[1,] %>% unname())
})

dat3$line <- relevel(dat3$line, ref = "0")
glm1 <- glm(occ_binary ~ line, dat3, family = "binomial")


line_fe$line <- lines %>% unlist()
colnames(line_fe) <- c("line.Estimate", "line.Std.Error", "line.zvalue", "line.Pvalue", "name") 
line_fe$line.quint <- ntile(line_fe$line.Pvalue, 5) %>% as.factor()


line_sp <- readOGR(".", "Rail_lines")
line_sp@data <- full_join(line_sp@data, line_fe)
line_sp@data$id <- line_sp@data$name
lsp_tidy <- broom::tidy(line_sp) %>% left_join(line_sp@data)

breaks <- quantile(line_fe$P.value, c(0, .2, .4, .6, .8, 1)) %>% unname() %>% round(digits = 3)

labels <- c()
for (i in c(1:(length(breaks) - 1))){
  a_label <- paste0(breaks[i], " - ", breaks[i + 1])
  labels <- c(labels, a_label)
}

ggplot() + 
  geom_path(data = filter(lsp_tidy, is.na(quint)), 
            aes(x = long, y = lat, group = group), color = "grey10",
            size = 1, lineend = "round", linejoin = "mitre", na.rm = TRUE) + 
  geom_path(data = filter(lsp_tidy, complete.cases(quint)), 
            aes(x = long, y = lat, group = group, color = quint), 
            size = 1.5, lineend = "round", linejoin = "mitre", na.rm = TRUE) + 
  coord_map() + theme_void() + ggtitle("Rail Line Fixed Effect P-Values") +
  theme(plot.background = element_rect(fill = "black"), 
        text = element_text(color = "white"),
        plot.margin = unit(c(.25, .25, .25, .25), "in")) +
  scale_color_brewer("P-Value", palette = "YlGnBu", na.value = "grey10", labels = labels)

# REGION FIXED EFFECTS ----------------------------------------------------

# TO province
to.provinces <- unique(dat4$to.prov) %>% as.list()

line_to.prov <- ldply(to.provinces, function(i) {
  dd <- within(dat4, to.prov <- relevel(to.prov, ref = as.character(i)))
  glm1 <- glm(occ_binary ~ to.prov, dd, family = "binomial")
  return(coef(summary(glm1))[1,] %>% unname())
})

line_to.prov$prov <- to.provinces %>% unlist()
colnames(line_to.prov) <- c("to.Estimate", "to.Std.Error", "to.z.value", "to.P.value", "to.prov") 
