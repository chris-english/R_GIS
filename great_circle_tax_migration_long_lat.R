## Great Circles Tax Migration Long Lat
## to match logic with Nathan Yau, FlowingData
## http://flowingdata.com/2011/05/11/how-to-map-connections-with-great-circles/
 library(maps)
 library(geosphere)

## POSTGIS 2.0 Notes on preparing the centroids for use in R.  
## map data from http://www.census.gov/geo/www/cob/co2000.html
## co99_d00 is a multipolygon and needs to be dumped to polygon
## and centroids taken on those polygons to avoid massive headaches.
## My two step approach, first dump to polygon:
##
## create table us_cnty_poly as
## select 
## co99_d00.state,
## co99_d00.county,
## co99_d00.fips,
## (ST_Dump(co99_d00.geom)).geom as geom
## from co99_d00
## group by state,
## co99_d00.county,
## co99_d00.fips,
## co99_d00.geom
## order by co99_d00.state,
## co99_d00.county;
##
## create index idx_us_cnty_poly_gist on us_cnty_poly using gist(geom);
## select populate_geometry_columns('us_cnty_poly'::regclass);
## (can use pgsql2shp and use these to plot county outlines if it doesn't
##  make the map look too busy aesthetically)
##
## (next centroids from the polygons)
## create table us_cnty_centroid
## as select state, county, fips,
## ST_Centroid(geom) as centroid,
## ST_AsText(ST_Centroid(geom)) as point_wkt,
## ST_AsEWKB(ST_Centroid(geom)) as cent_b,
## ST_y(ST_Centroid(geom)) as lat,
## ST_x(ST_Centroid(geom)) as long
## from us_cnty_poly; 
##
## create index idx_us_cnty_centroid_gist on us_cnty_centroid using gist(centroid);
## select populate_geometry_columns('us_cnty_centroid'::regclass);
## gcIntermediate can consume different types of geometries and I want to play around
## hence the many different geometries in the centroid table
##
## Ship it off to use with R using pgsql2shp from command line
## -- remember to refresh your tables in pgAdmin prior to shipping or
## -- pgsql2shp will just ship a .dbf which won't help maptools
##  you'll know you didn't refresh if you see this:
## Done (postgis major version: 2).
## Output shape: Unknown <- this tells you didn't refresh
## Dumping: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX [3118 rows].
## 
## rather than this
## Initializing...
## Done (postgis major version: 2).
## Output shape: Point <- ah, points, lovely points
## Dumping: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX [3118 rows].
##
## ship it off
##
##C:\Documents and Settings\Owner>pgsql2shp -f c:/gisdata/us_cnty_centroid -h localhost -p 5434 -u postgres geocoder us_cn
## ty_centroid
## Initializing...
## Done (postgis major version: 2).
## Output shape: Point
## Dumping: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX [3489 rows].
##
## and ship off us_cnty_poly as well
## and now we can play with R


## read in Counties - note colClass that preserves leading 0's in fips
## this is from POSTGIS of - had to fix several 02(state) longitude that were
## missing '-' ie. -178.xxxxxxxxx was 178.xxxxxxxx that snarled antipodal
## comes up further down - fixed in Notepad** on visual inspection
## have to come up with a way to change programmatically.
##
require(maptools) 

county_centroid <- readShapeSpatial("c:/gisdata/us_cnty_centroid.shp" )

names(county_centroid)  

names(county_centroid) <- c("state", "county", "fips", "point_wkt", "cent_b","lat", "long") ##lower case
##see what we've got so far
str(county_centroid) 

## address those factors

## county_centroid$state <- as.character(county_centroid$state)
## county_centroid$county <- as.character(county_centroid$county)
## county_centroid$fips <- as.character(county_centroid$fips)
## county_centroid$point_wkt <- as.character(county_centroid$point_wkt)
## county_centroid$centroid_b <-as.character(county_centroid$centroid_b)
## county_centroid$cent_ewkt <- as.character(county_centroid$cent_ewkt)
## put proj$string
proj4string(county_centroid) <- CRS(as.character("+proj=longlat +ellps=GRS80")) 
proj = proj4string(county_centroid) ## just storing this for later

## we're matching on fips so check number of characters
min(nchar(as.character(county_centroid$fips)))  ## [1]  5

## read the inflow data 2009_2010
## I took out the file header with Notepad++ and examined the file
## for obvious errors like embedded "
## source - http://www.irs.gov/pub/irs-soi/countyinflow0910.csv
## except for " scrubbing as it comes from IRS

inflow0910 = read.csv("c:/gisdata/census/migration/countyinflow0910.csv",
sep=",",
stringsAsFactors=FALSE,
as.is=TRUE,
colClass=c(rep("character",6),rep("numeric",3)),
col.names=c("st_code_dest","co_code_dest","st_code_orig","co_code_orig",
"st_abbv","co_name","return_num","exmpt_num","aggr_agi")
)

## since we're going to match this data with census maps by fips
## codes - check the IRS fips are giving valid fips code, five characters -
## leading two characters - state code, last three characters - county

require(stringr)

## actually want nchar() for these tests
max(nchar(inflow0910$st_code_dest))## this comes up 2

min(nchar(inflow0910$st_code_dest))## this comes up 1

min(nchar(inflow0910$co_code_orig))## this comes up 1

min(nchar(inflow0910$co_code_dest))## this comes up 1



## so the IRS files strip out leading 0's so we put them back in -
## from names(inflow0910)
## [1] "st_code_dest" "co_code_dest" "st_code_orig" "co_code_orig"
## we have four codes that need left padding with 0's

inflow0910$st_code_dest <- str_pad(c(inflow0910$st_code_dest),
 2, side="left", pad="0")

min(nchar(inflow0910$st_code_dest)) #2

## now fix the other three

inflow0910$st_code_orig <- str_pad(c(inflow0910$st_code_orig),
2, side="left", pad="0")

inflow0910$co_code_dest <- str_pad(c(inflow0910$co_code_dest),
3, side="left", pad="0")

inflow0910$co_code_orig <- str_pad(c(inflow0910$co_code_orig),
3, side="left", pad="0")

nrow(inflow0910)


## we've still got something like 30,000 summary records
## that won't match against county centroids and cause
## mismatches in gcIntermediate so we dispatch them with some sqldf
## this will leave a county matches itself but we may find that useful
## this could be achieved with merge but I don't know how yet
require(sqldf)

## take out summary records
inflow0910 <- sqldf("SELECT st_code_dest, co_code_dest, st_code_orig, co_code_orig,
	st_abbv, co_name, return_num, exmpt_num, aggr_agi FROM inflow0910 WHERE
	st_code_dest NOT LIKE '00' and st_code_orig NOT LIKE '00' and st_code_dest NOT LIKE '01'
	and st_code_orig NOT LIKE '01'and st_code_orig NOT LIKE '96' and st_code_orig NOT LIKE '97'
	and st_code_orig NOT LIKE '98' and st_code_dest NOT LIKE '96' and st_code_dest NOT LIKE '97'
	and st_code_dest NOT LIKE '98' and co_code_dest NOT LIKE '000' and st_abbv NOT LIKE 'DS' 
	and st_abbv NOT LIKE 'SS' and st_abbv NOT LIKE 'FR' and co_code_orig NOT LIKE '000'")

## compute fips_in_orig fips_in_dest
inflow0910$fips_in_dest <- paste(inflow0910$st_code_dest, inflow0910$co_code_dest, sep="")

inflow0910$fips_in_orig <- paste(inflow0910$st_code_orig, inflow0910$co_code_orig, sep="")

head(inflow0910_sql) ## take a look at it

tail(inflow0910_sql) ## look again

#

# read the outflow data 2009_10
## again took out header and cleaned out " around text in Notepad++
## which wasn't present in the inflow file
## source - http://www.irs.gov/pub/irs-soi/countyoutflow0910.csv
outflow0910 = read.csv("c:/gisdata/census/migration/countyoutflow0910.csv",
sep=",",
stringsAsFactors=FALSE,
as.is=TRUE,
colClass=c(rep("character",6),rep("numeric",3)),
col.names=c("st_code_orig", "co_code_orig", "st_code_dest", "co_code_dest",
"st_abbv", "co_name", "return_num", "exmpt_num", "aggr_agi")
)

## fix these up with padding as above

outflow0910$st_code_dest <- str_pad(c(outflow0910$st_code_dest),2, side="left", pad="0")
outflow0910$st_code_orig <- str_pad(c(outflow0910$st_code_orig),2, side="left", pad="0")
outflow0910$co_code_dest <- str_pad(c(outflow0910$co_code_dest),3, side="left", pad="0")
outflow0910$co_code_orig <- str_pad(c(outflow0910$co_code_orig),3, side="left", pad="0")

## take out summary records

outflow0910 <-  sqldf("SELECT st_code_dest, co_code_dest, st_code_orig, co_code_orig,
	st_abbv, co_name, return_num, exmpt_num, aggr_agi FROM outflow0910 WHERE
	st_code_dest NOT LIKE '00' and st_code_orig NOT LIKE '00' and st_code_dest NOT LIKE '01'
	and st_code_orig NOT LIKE '01'and st_code_orig NOT LIKE '96' and st_code_orig NOT LIKE '97'
	and st_code_orig NOT LIKE '98' and st_code_dest NOT LIKE '96' and st_code_dest NOT LIKE '97'
	and st_code_dest NOT LIKE '98' and co_code_dest NOT LIKE '000' and st_abbv NOT LIKE 'DS' 
	and st_abbv NOT LIKE 'SS' and st_abbv NOT LIKE 'FR' and co_code_orig NOT LIKE '000'")



head(outflow0910_sql)

tail(outflow0910_sql)

## compute outflow origin FIPS code
outflow0910$fips_out_orig <- paste(outflow0910$st_code_orig, outflow0910$co_code_orig, sep="")

## compute outflow destination FIPS
outflow0910$fips_out_dest <- paste(outflow0910$st_code_dest, outflow0910$co_code_dest, sep="")

## library(maptools)
## map data from http://www.census.gov/geo/www/cob/co2000.html
## state.map <- readShapeSpatial("c:/gisdata/st99_d00.shp")
## county.map <- readShapeSpatial("c:/gisdata/census/co99_d00.shp")

# Color
	pal <- colorRampPalette(c("#f2f2f2", "green"))
	colors <- pal(100)
	pal2 <- colorRampPalette(c("pink", "red"))
	colors2 <- pal2(100)
xlim <- c(-171.738281, -56.601563)
ylim <- c(12.039321, 71.856229)

## get rid of cases that aren't running all the way through
movein_0910 <- unique(inflow0910$st_abbv)
movein_0910 <- movein_0910[!grepl("TX$|IL$|MN$|CO$|NM$|AZ$|CA$|WA$|PR$|AK$|NV$|UT$", movein_0910)]
moveout_0910 <- unique(outflow_0910$st_abbv)
moveout_0910 <- moveout_0910[!grepl("TX$|IL$|MN$|CO$|NM$|AZ$|CA$|WA$|PR$|AK$|NV$|UT$", moveout_0910)]



for (i in 1:length(movein_0910)) {

map("world", col="#f2f2f2", fill=TRUE, bg="white", lwd=0.05, xlim=xlim, ylim=ylim)
m_insub <- inflow0910[inflow0910$st_abbv == "NY",] ##options  "NY",] or movein_0910[1],]
	
m_insub <- m_insub[order(m_insub$return_num),]
max_return_num <- max(m_insub$return_num)
	for (j in 1:(length)(m_insub$fips_in_orig)) {
	message(j)
		##for(k in 1:length) (inter)) {
			##message(k)
	movein_orig <- county_centroid[county_centroid$fips == (m_insub[j,]$fips_in_orig),]
	movein_dest <- county_centroid[county_centroid$fips == (m_insub[j,]$fips_in_dest),]

	inter <- gcIntermediate(c(movein_orig[1,]$long, movein_orig[1,]$lat), c(movein_dest[1,]$long,
			 movein_dest[1,]$lat),sp=TRUE, n=100, addStartEnd=TRUE)
	lines_in <<- inter
	colindex <- round((m_insub[j,]$return_num / (max_return_num/10000)) * length(colors))#per Paul Butler Facebook visualize
	lines(inter, col=colors[colindex], lwd=0.6)
	
			}
	
	##}

dev.off()	
	}

(i in 1:length(moveout_0910)) {

m_outsub <- outflow0910[outflow0910$st_abbv == 	moveout_0910[i],]	##was "NY",]
m_outsub <- m_outsub[order(m_outsub$return_num),]
max_return_num <- max(m_outsub$return_num)
	for (j in 1:length(m_outsub$fips_out_orig)) {
	message(j)
	moveout_orig <- county_centroid[county_centroid$fips == (m_outsub[j,]$fips_out_orig),]
	moveout_dest <- county_centroid[county_centroid$fips == (m_outsub[j,]$fips_out_dest),]

	inter <- gcIntermediate(c(moveout_orig[1,]$long, moveout_orig[1,]$lat), c(moveout_dest[1,]$long,
			 moveout_dest[1,]$lat),	sp=TRUE, n=100, addStartEnd=TRUE)
			lines_out <<- inter
			colindex2 <- round( (m_outsub[j,]$return_num /( max_return_num/10000)) * length(colors2))
			##for k in 1:length(inter){
				##lines_out <<- inter[k,]$
			lines(inter, col=colors2[colindex2], lwd=0.6)

		}
	dev.off()
	}
	
require(raster)
require(gdistance)



