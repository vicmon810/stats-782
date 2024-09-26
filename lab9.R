covid <- read.csv(file.choose())

str(covid)

plot(covid$date, covid$confirmed)
max(covid$date)
min(covid$date)
max(covid$confirmed)
min(covid$confirmed)
covid$dateN <- as.numeric(paste0(substr(covid$date,6,7), substr(covid$date,9,10)))
max(covid$dateN)
min(covid$dateN)


# Assuming 'covid$date' is in date format

covid$date <- as.Date(covid$date, format="%Y-%m-%d") +1

confirm <- function(){


  plot.new()
    
  # Set the plotting window based on the original date range and the range of confirmed cases
  plot.window(xlim = c(min(covid$date) -5, max(covid$date)+5), ylim = range(covid$confirmed, na.rm = TRUE))
  
  # Add axes with proper date labels
  axis.Date(1,pos = 0 , at=seq(min(covid$date)-5, max(covid$date)+10, by="week"), format="%d-%b")  # X-axis (dates)
  y_lable <- seq(0,100,10)
  axis(2, pos = min(covid$date), at=seq(min(covid$confirmed, na.rm = TRUE), max(covid$confirmed, na.rm = TRUE), length.out=11), y_lable)  # Y-axis
  
  # Bar plot settings
  bar_width <- 0.5
  
  # Create the bar chart with dates as the x-axis
  rect(xleft = as.numeric(covid$date)+0.2 - bar_width, 
       ybottom = 0.1, 
       xright = as.numeric(covid$date) + bar_width, 
       ytop = covid$confirmed, 
       col = "orange", border = NA)
}

# Call the function to create the plot
confirm()

seq(min(covid$date),max(covid$date), by="week")

covid$date <- as.Date(covid$date, format="%Y-%m-%d") 

pro <- function(){
  plot.new()
  plot.window(xlim = range(covid$date), ylim = range(covid$probable))
  axis.Date(1,pos = 0 , at=seq(min(covid$date), max(covid$date), by="week"), format="%d-%b")  # X-axis (dates)
  ylab <- seq(0,100,10)
  axis(2, pos= min(covid$date), at=seq(min(covid$probable), max(covid$probable), length.out=11), labels = ylab)
  barwidth <- 0.3
  rect(
    xleft = as.numeric(covid$date) - barwidth,
    ybottom = 0.01,
    xright = as.numeric(covid$date) + barwidth,
    ytop = covid$probable,
    col = "orange", border = NA
  )
}

pro()







d.AD <- data.frame(treatment = gl(3, 3), outcome = gl(3, 1, 9),
                   counts = c(18,17,15,20,10,20,25,13,12))

glm.D93 <- glm(counts ~ outcome + treatment, poisson, data = d.AD)
class(glm.D93)
is.list(glm.D93)
names(glm.D93)


is.glm <- function(obj, ...){
  any(class(obj) == "glm")
}

is.glm(glm.D93)

is.lm <- function(obj, ...){
  any(class(obj) == "lm")
}

is.lm(glm.D93)


betas <- function(obj){
  if(any(class(obj) == "glm") ||any(class(obj) == "lm"))
  obj$coefficients
  else stop("invaild")
}


betas <- function(obj){
  if(any(class(obj) == "glm") || any(class(obj) == "lm")) obj$coefficients
  else stop("invalid")
}


betas(glm.D93)





