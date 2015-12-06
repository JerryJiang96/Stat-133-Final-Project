#Is there a trend between Natural Disaster and GDP? 
#First, I'll need to read in the Data sets
Econ <- read.csv('../data/Econ.csv')
Natural_Disasters <- read.csv('../data/NaturalDisasters.csv')

# Next, I'll need to manipulate the Data sets so I can merge the. The thing
# about Natural Disasters, is that I only need to know the total amount of
# damage done in a year--the type of disaster isn't necessary at the moment.
# We'll use dplyr for this

#identify/create variables
Year <- 1995:2015
Damages <- c()

for (i in 1995:2015) {
  Dmg_per_year <- sum(filter(Natural_Disasters, year == i)[,'cost'])
  Damages <- c(Damages,Dmg_per_year)
}

#create the data frame for total damage caused per year
Natural_Disaster_Damages <- data.frame(
  'Years' = Year,
  'Damages' = Damages
)
Econ[,'GDP']<- Econ[,'GDP'] *1000000

#merge data frames
Econ_vs_ND_Damages <- merge(Econ,Natural_Disaster_Damages,by = "Years")

#Create the Graph
#Note that GDP is measured in Billions, 
#and the Damage is measured in dollar value.

#Trend of GDP per Year
ggplot(Econ_vs_ND_Damages, aes(Years,GDP)) + 
  geom_point(color = '#ff0000', size = 3) + 
  ggtitle("GDP vs Years")

ggsave("../images/GDPvsYears.png")
ggsave("../images/GDPvsYears.pdf")

#Trend of Damages per Year
ggplot(Econ_vs_ND_Damages, aes(Years,Damages)) + 
  geom_point(color = '#ff0000', size = 3) + 
  ggtitle("Damages vs Years")

ggsave("../images/DamagesvsYears.png")
ggsave("../images/DamagesvsYears.pdf")

#There appears to be no correlation between GDP and Damages per year. 
#Even though GDP is rising each year, 
#Damages per Year has no correlation each year.

#Which Natural Disaster causes the most damage/influence in GDP
# Well, in the previous analysis, there is no clear distinction on the
# relationship between GDP and Natural Damages year by year. 
#Perhaps though, the natural disasters that occurs the most in one year, 
#may be cause the most impact on GDP that year

# x-axis = year
# y- axis = total damage that year 
# variable = type of natural disaster
ggplot(Natural_Disasters,aes(year,cost)) + 
  geom_bar(aes(fill = type),stat = 'identity') + 
  ggtitle("Which Natural Disaster caused the most Damage per Year")

ggsave("../images/Natural_Disaster_and_Most_Yearly_Dmg.png")
ggsave("../images/Natural_Disaster_and_Most_Yearly_Dmg.pdf")

#wow, now we can see obvious that for 2005, Forest Fire caused a lot of damage.
#In 2004, ash fall was the largest, and
#in 2012, Storm caused the next major source of damage.
#It should be noted that despite forest fires causing the most damage in 2005,
#the natural disaster type "storm" has the most "most damage in a year".
#Therefore, storm yearly may create more of an impact on GDP
#(if natural disasters actually affect GDP per year). 
