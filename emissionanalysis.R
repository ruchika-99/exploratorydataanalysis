#load the required packages into R
library(dplyr)
library(ggplot2)
library(scales)
library(data.table)

#read the required files
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#examining the NEI dataset
head(NEI)
str(NEI)

#examining the SCC dataset
head(SCC)
str(SCC)

#Answer 1
annual <- NEI %>% group_by(year) %>% 
  filter(year == 1999|2002|2005|2008) %>% 
  summarize(Annual.Total = sum(Emissions));
pts <- pretty(annual$Annual.Total/1000000);
yrs <- c(1999,2002,2005,2008)
plot(annual$year, annual$Annual.Total/1000000, type = "l", lwd = 2, axes = FALSE,
     xlab = "Year", 
     ylab = expression("Total Tons of PM"[2.5]*" Emissions"), 
     main = expression("Total Tons of PM"[2.5]*" Emissions in the United States"));
axis(1, at = yrs, labels = paste(yrs));
axis(2, at = pts, labels = paste(pts, "M", sep = ""));
box()

#Answer 2
baltimore <- NEI %>% 
  filter(fips == "24510") %>% 
  group_by(year) %>% 
  summarize(Annual.Total = sum(Emissions));
baltimore.pts <- pretty(baltimore$Annual.Total/1000);
plot(baltimore$year, baltimore$Annual.Total/1000, type = "l", lwd = 2, axes = FALSE,
     xlab = "Year", 
     ylab = expression("Total Tons of PM"[2.5]*" Emissions"), 
     main = expression("Total Tons of PM"[2.5]*" Emissions in Baltimore"));
axis(1, at = c(1999,2002,2005,2008))

axis(2, at = baltimore.pts, labels = paste(baltimore.pts, "K", sep = ""));
box();

#Answer 3
nei.baltimore <- NEI %>% filter(fips == "24510") %>% group_by(type, year) %>% summarize(Annual.Total = sum(Emissions));
nei.baltimore$type <- factor(nei.baltimore$type, levels = c("ON-ROAD", "NON-ROAD", "POINT", "NONPOINT")) # Re-order factor levels so they plot in the order we wish
ggplot(nei.baltimore, aes(x = factor(year), y = Annual.Total, fill = type)) + 
  geom_bar(stat = "identity") + 
  facet_grid(. ~ type) + 
  xlab("Year") + 
  ylab(expression("Total Tons of PM"[2.5]*" Emissions")) + 
  ggtitle(expression("Total Tons of PM"[2.5]*" Emissions in Baltimore by Source Type")) +
  theme(axis.text.x=element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_y_continuous(labels = comma) +
  guides(fill = FALSE)

#Answer 4
scc.coal <- SCC[grep("Fuel Comb.*Coal", SCC$EI.Sector),  ];
scc.coal.list <- unique(scc.coal$SCC);
nei.coal <- subset(NEI, SCC %in% scc.coal.list);
nei.coal <- nei.coal %>% group_by(type, year) %>% summarize(Annual.Total = sum(Emissions))
nei.coal.total <- nei.coal %>% group_by(year) %>% summarize(Annual.Total = sum(Annual.Total)) %>% mutate(type = "TOTAL");
nei.coal <- nei.coal %>% select(Annual.Total, type, year);
nei.coal <- bind_rows(nei.coal, nei.coal.total);
nei.coal$type <- factor(nei.coal$type, levels = c("TOTAL", "ON-ROAD", "NON-ROAD", "POINT", "NONPOINT")); # Re-order factor levels to they plot in the order we wish
ggplot(nei.coal, aes(x = factor(year), y = Annual.Total, fill = type)) +
  geom_bar(stat = "identity") +
  facet_grid(. ~ type) +
  xlab("Year") +
  ylab(expression("Total Tons of PM"[2.5]*" Emissions")) + 
  ggtitle(expression(atop("Total Tons of PM"[2.5]*" Emissions in the United States", paste("from Coal Combustion-Related Sources")))) +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  scale_y_continuous(labels = comma) +
  scale_fill_brewer(palette = "Dark2") +
  guides(fill = FALSE)

#Answer 5
scc.vehicles <- SCC[grep("Mobile.*Vehicles", SCC$EI.Sector),  ]; # Pattern match mobile vehicles in SCC description
scc.vehicles.list <- unique(scc.vehicles$SCC); # Create motor vehicle lookup list by SCC
nei.vehicles <- subset(NEI, SCC %in% scc.vehicles.list); # Filter for motor vehicle sources
nei.vehicles <- nei.vehicles %>% filter(fips == "24510") # Filter for Baltimore
nei.vehicles <- merge(x = nei.vehicles, y = scc.vehicles[, c("SCC", "SCC.Level.Two", "SCC.Level.Three")], by = "SCC") # Join in descriptive data on SCC codes
nei.vehicles <- nei.vehicles %>% group_by(year, SCC.Level.Two, SCC.Level.Three) %>% summarize(Annual.Total = sum(Emissions))
nei.vehicles.total <- nei.vehicles %>% group_by(year) %>% summarize(Annual.Total = sum(Annual.Total)) %>% mutate(SCC.Level.Two = "Total")
nei.vehicles <- bind_rows(nei.vehicles, nei.vehicles.total);
nei.vehicles$SCC.Level.Two <- factor(nei.vehicles$SCC.Level.Two, levels = c("Total", "Highway Vehicles - Diesel", "Highway Vehicles - Gasoline"));
ggplot(nei.vehicles, aes(x = factor(year), y = Annual.Total, fill = SCC.Level.Two)) +
  geom_bar(stat = "identity") +
  facet_grid(. ~ SCC.Level.Two) +
  xlab("Year") +
  ylab(expression("Total Tons of PM"[2.5]*" Emissions")) + 
  ggtitle(expression(atop("Total Tons of PM"[2.5]*" Emissions in Baltimore City", paste("from Motor Vehicle Sources")))) +
  theme(plot.title = element_text(hjust = 0.5)) + # Center the plot title
  theme(plot.margin = unit(c(1,1,1,1), "cm")) + # Adjust plot margins
  scale_fill_brewer(palette = "Set1") +
  guides(fill = FALSE)

#Answer 6
scc.vehicles <- SCC[grep("Mobile.*Vehicles", SCC$EI.Sector),  ]; # Pattern match mobile vehicles in SCC description
scc.vehicles.list <- unique(scc.vehicles$SCC); # Create motor vehicle lookup list by SCC
nei.vehicles <- subset(NEI, SCC %in% scc.vehicles.list); # Filter for motor vehicle sources
nei.vehicles <- nei.vehicles %>% filter(fips == "24510"| fips == "06037"); # Filter for Baltimore City or Los Angeles County
nei.vehicles$fips[nei.vehicles$fips == "24510"] <- "Baltimore";
nei.vehicles$fips[nei.vehicles$fips == "06037"] <- "Los Angeles";
nei.vehicles <- merge(x = nei.vehicles, y = scc.vehicles[, c("SCC", "SCC.Level.Two")], by = "SCC"); # Join in descriptive data on SCC codes
nei.vehicles <- nei.vehicles %>% group_by(fips, year, SCC.Level.Two) %>% summarize(Annual.Total = sum(Emissions));
nei.vehicles.total <- nei.vehicles %>% group_by(fips, year) %>% summarize(Annual.Total = sum(Annual.Total)) %>% mutate(SCC.Level.Two = "Total");
nei.vehicles <- bind_rows(nei.vehicles, nei.vehicles.total);
nei.vehicles$SCC.Level.Two <- factor(nei.vehicles$SCC.Level.Two, levels = c("Total", "Highway Vehicles - Diesel", "Highway Vehicles - Gasoline"));
ggplot(nei.vehicles, aes(x = factor(year), y = Annual.Total, fill = SCC.Level.Two)) +
  geom_bar(stat = "identity") +
  facet_grid(fips ~ SCC.Level.Two, scales = "free") + # Setup facets and allow scales to adjust to data in each location
  xlab("Year") +
  ylab(expression("Total Tons of PM"[2.5]*" Emissions")) + 
  ggtitle(expression(atop("Total Tons of PM"[2.5]*" Emissions from Motor Vehicle Sources", paste("in Baltimore City, MD and Los Angeles County, CA")))) +
  theme(plot.title = element_text(hjust = 0.5)) + # Center the plot title
  theme(plot.margin = unit(c(1,1,1,1), "cm")) + # Adjust plot margins
  scale_fill_brewer(palette = "Set1") +
  guides(fill = FALSE)