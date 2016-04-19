http://waterservices.usgs.gov/nwis/site/?format=rdb&countyCd=51059,51061&startDT=2000-04-12&endDT=2005-04-13&outputDataTypeCd=dv&parameterCd=00060&siteType=ST

library(rvest)

streamflow <- function(fips, date_min, date_max){

}

url <- paste0("http://waterservices.usgs.gov/nwis/site/?format=rdb&countyCd=",
              "51059,51061&startDT=2000-04-12&endDT=2005-04-13&outputDataTypeCd=dv&parameterCd=00060&siteType=ST"
)

test1 <- read.table("http://waterservices.usgs.gov/nwis/site/?format=rdb&countyCd=51059,51061&startDT=2000-04-12&endDT=2005-04-13&outputDataTypeCd=dv&parameterCd=00060&siteType=ST",
                   sep = "\t", comment.char = "#", header = TRUE)
test1 <- test1[-1, ]

#
#
# US Geological Survey
# retrieved: 2016-04-13 23:03:31 -04:00	(vaas01)
#
# The Site File stores location and general information about groundwater,
# surface water, and meteorological sites
# for sites in USA.
#
# File-format description:  http://help.waterdata.usgs.gov/faq/about-tab-delimited-output
# Automated-retrieval info: http://waterservices.usgs.gov/rest/Site-Service.html
#
# Contact:   gs-w_support_nwisweb@usgs.gov
#
# The following selected fields are included in this output:
#
#  agency_cd       -- Agency
#  site_no         -- Site identification number
#  station_nm      -- Site name
#  site_tp_cd      -- Site type
#  dec_lat_va      -- Decimal latitude
#  dec_long_va     -- Decimal longitude
#  coord_acy_cd    -- Latitude-longitude accuracy
#  dec_coord_datum_cd -- Decimal Latitude-longitude datum
#  alt_va          -- Altitude of Gage/land surface
#  alt_acy_va      -- Altitude accuracy
#  alt_datum_cd    -- Altitude datum
#  huc_cd          -- Hydrologic unit code
#  data_type_cd    -- Data type
#  parm_cd         -- Parameter code
#  stat_cd         -- Statistical code
#  dd_nu           -- Internal database key
#  loc_web_ds      -- Additional measurement description
#  medium_grp_cd   -- Medium group code
#  parm_grp_cd     -- Parameter group code
#  srs_id          -- SRS ID
#  access_cd       -- Access code
#  begin_date      -- Begin date
#  end_date        -- End date
#  count_nu        -- Record count
#
