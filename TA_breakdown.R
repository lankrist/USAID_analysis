#Technical assistance breakdown
library(stats) #aggregate
library(shiny) #interactive graph
library(ggplot2)
library(plotly) #interactive graph
library(plyr) #ddply
setwd("/Documents/USAID_Internship2017")

budget= read.csv(file = "dataset/financials/field_budget_TA_20170804.csv", header= T)
names(budget)

expense = read.csv(file = "dataset/financials/field_expense_TA_20170804.csv", header = T)
names(expense)

#convert variable types
#To Numerics
fact_to_num = function(fact_val){
  return(as.numeric(gsub(",","",as.character(fact_val))))
}
budget$AWP.Budgeted.Amount =fact_to_num(budget$AWP.Budgeted.Amount)
expense$Actual.Expense.This.Period = fact_to_num(expense$Actual.Expense.This.Period)

#To dates
expense$Financial.Report.Period=as.Date(as.character(expense$Financial.Report.Period) , format='%Y/%m/%d')
#format for date conversion %b is not working

#look at arbitraty country
attach(expense)
aggregate(Actual.Expense.This.Period~Country, FUN=sum)

country_ = "Mozambique"
year_ = "2016"
nat= expense[Country==country_,]

#BLANK THEME
blank_theme = theme_minimal() + 
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 14, face = "bold")
  )

#expense file data manipulation
attach(nat)
colN = colnames(nat)[c(2, 6, 7)]
dummy = expand.grid(unique(Financial.Report.Period), levels(Financial.Report.Technical.Sub.Category),0)
colnames(dummy) = colN; dummy = dummy[!is.na(dummy$Financial.Report.Period),]
nat2 = nat[,colN]
com = rbind(dummy, nat2)
com=aggregate(com$Actual.Expense.This.Period~com$Financial.Report.Period+com$Financial.Report.Technical.Sub.Category, FUN=sum)
colnames(com)= colN
com=com[!is.na(com$Financial.Report.Period),]
#table(com$Financial.Report.Technical.Sub.Category)


#budget file data editing
maxb <- budget[budget$Country == country_,]
maxt=aggregate(maxb$AWP.Budgeted.Amount~maxb$Financial.Report.Period.Fiscal.Year, FUN=sum); colnames(maxt) = c("fiscalY", "budget")

#function converts fiscal year to dates which is first day of FY month
fisc_to_date = function(FY){ #FY input is character
  foo = as.Date(paste(FY, c(1:9), 1), format = "%Y %m %d")
  bar = as.Date(paste((as.numeric(FY)-1),  c(10:12), 1), format = "%Y %m %d")
  return(c(bar, foo))
}

#NEEDS to be fixed
# maxf=data.frame(fiscal_year=fisc_to_date(maxt$fiscalY), budget = maxt$budget, cutoff = maxt$budget)
# class(maxf$fiscal_year)

dev.new()
attach(com)
#stacked chart
base = ggplot(com, aes(x= Financial.Report.Period, y=Actual.Expense.This.Period, 
                fill= Financial.Report.Technical.Sub.Category))
base + geom_area()+
  labs(x = "Year-Month", y = "Expense", 
                       title = paste("TA Expense for", country_))+
  scale_x_date(date_breaks = "2 month")

#cumulative graph for specific year
#add cumulative sum
comy=com[com$Financial.Report.Period %in% fisc_to_date(year_),]
comt=ddply(comy, .(Financial.Report.Technical.Sub.Category), transform, cy = cumsum(Actual.Expense.This.Period))

baset = ggplot(comt, aes(x= Financial.Report.Period, y=cy, 
                         fill=Financial.Report.Technical.Sub.Category))

cumulate =baset + geom_area()+
  labs(x = "Year-Month", y = "Expense", 
       title = paste("Cumulative TA Expense for", country_))+
  scale_x_date(date_breaks = "2 month")
cumulate+ geom_hline(yintercept = maxt[maxt$fiscalY == year_,"budget"], lty = 6)+
  annotate("text", x=min(comt$Financial.Report.Period), 
           y= 0.97*(maxt[maxt$fiscalY == year_,"budget"]),label = paste(year_, "Budget"))
#add horizontal axis for annual budget

ui <- fluidPage(
  plotlyOutput("distPlot")
)

server <- function(input, output) {
  output$distPlot <- renderPlotly({
    cumulate  
  })
}

shinyApp(ui = ui, server = server)
