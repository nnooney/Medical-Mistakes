## This file loads the STATA files into individual data frames
## On Thing3, this takes about 40 minutes and consumes 50% of total RAM

# Need the foriegn package
library(foreign)

# Read the data into variables
LA1995 = read.dta("1995_LA_base.dta")
N1995 = read.dta("1995_North_base.dta")
S1995 = read.dta("1995_South_base.dta")
LA1996 = read.dta("1996_LA_base.dta")
N1996 = read.dta("1996_North_base.dta")
S1996 = read.dta("1996_South_base.dta")
LA1997 = read.dta("1997_LA_base.dta")
N1997 = read.dta("1997_North_base.dta")
S1997 = read.dta("1997_South_base.dta")
LA1998 = read.dta("1998_LA_base.dta")
N1998 = read.dta("1998_North_base.dta")
S1998 = read.dta("1998_South_base.dta")
LA1999 = read.dta("1999_LA_base.dta")
N1999 = read.dta("1999_North_base.dta")
S1999 = read.dta("1999_South_base.dta")
LA2000 = read.dta("2000_LA_base.dta")
N2000 = read.dta("2000_North_base.dta")
S2000 = read.dta("2000_South_base.dta")
LA2001 = read.dta("2001_LA_base.dta")
N2001 = read.dta("2001_North_base.dta")
S2001 = read.dta("2001_South_base.dta")
LA2002 = read.dta("2002_LA_base.dta")
N2002 = read.dta("2002_North_base.dta")
S2002 = read.dta("2002_South_base.dta")
LA2003 = read.dta("2003_LA_base.dta")
N2003 = read.dta("2003_North_base.dta")
S2003 = read.dta("2003_South_base.dta")
LA2004 = read.dta("2004_LA_base.dta")
N2004 = read.dta("2004_North_base.dta")
S2004 = read.dta("2004_South_base.dta")
LA2005 = read.dta("2005_LA_base.dta")
N2005 = read.dta("2005_North_base.dta")
S2005 = read.dta("2005_South_base.dta")
LA2006 = read.dta("2006_LA_base.dta")
N2006 = read.dta("2006_North_base.dta")
S2006 = read.dta("2006_South_base.dta")
LA2007 = read.dta("2007_LA_base.dta")
N2007 = read.dta("2007_North_base.dta")
S2007 = read.dta("2007_South_base.dta")
LA2008 = read.dta("2008_LA_base.dta")
N2008 = read.dta("2008_North_base.dta")
S2008 = read.dta("2008_South_base.dta")
LA2009 = read.dta("2009_LA_base.dta")
N2009 = read.dta("2009_North_base.dta")
S2009 = read.dta("2009_South_base.dta")
LA2010 = read.dta("2010_LA_base.dta")
N2010 = read.dta("2010_North_base.dta")
S2010 = read.dta("2010_South_base.dta")
LA2011 = read.dta("2011_LA_base.dta")
N2011 = read.dta("2011_North_base.dta")
S2011 = read.dta("2011_South_base.dta")
LA2012 = read.dta("2012_LA_base.dta")
N2012a = read.dta("2012_North_a_base.dta")
N2012b = read.dta("2012_North_b_base.dta")
S2012 = read.dta("2012_South_base.dta")

## End loading the data

# An array of the filenames for looping over the data
filenames = c("LA1995", "N1995", "S1995", "LA1996", "N1996", "S1996", "LA1997", "N1997", "S1997", "LA1998", "N1998", "S1998", "LA1999", "N1999", "S1999", "LA2000", "N2000", "S2000", "LA2001", "N2001", "S2001", "LA2002", "N2002", "S2002", "LA2003", "N2003", "S2003", "LA2004", "N2004", "S2004", "LA2005", "N2005", "S2005", "LA2006", "N2006", "S2006", "LA2007", "N2007", "S2007", "LA2008", "N2008", "S2008", "LA2009", "N2009", "S2009", "LA2010", "N2010", "S2010", "LA2011", "N2011", "S2011", "LA2012",  "N2012a", "N2012b", "S2012")

## Count the number of observations
observations = 0;
for(name in filenames) {
  observations = observations + nrow(get(name))
}
# 68898196

## Get all the column names for the data, and then get the unique columnnames
columnnames = vector()
for(name in filenames) {
  columnnames = c(columnnames, colnames(get(name)))
}
uniquecols = unique(columnnames)

column.df = data.frame()
for(name in filenames) {
  small.df = data.frame(var=colnames(get(name)),year=name)
  column.df = rbind(column.df,small.df)
}
colfrequencies = as.data.frame.matrix(table(column.df$year, column.df$var))

## This table shows that we need to combine some variables
