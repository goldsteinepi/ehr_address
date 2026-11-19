##################
# EHR patient address analysis
# Citation: Goldstein ND, Poulsen MN, Kirchner HL, Chabris CF, Goren A. On the use of patient address in electronic health record research: a methodological note. Manuscript in preparation.
# 3/18/26 -- Neal Goldstein
##################


### FUNCTIONS ###

#agreement between multiple addresses
count_discrepancies_addr = function(x) {
  x = na.omit(tolower(as.character(x)))
  length(unique(x))
}

#agreement between ZIP codes (5 digit)
count_discrepancies_zip5 = function(x) {
  x = na.omit(substr(as.character(x), 1, 5))
  length(unique(x))
}

#agreement between ZIP codes (3 digit)
count_discrepancies_zip3 = function(x) {
  x = na.omit(substr(as.character(x), 1, 3))
  length(unique(x))
}

#significant address changes
count_sig_addr = function(x) {
  x = na.omit(tolower(as.character(x)))
  sum(x=="y")
}

#escape special characters for regex searching
escape_regex <- function(x) {
  gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)
}

library(psych) #describe
library(campfin) #normal_address


### READ DATA ###

#Epic/Clarity discharge data
discharges = read.csv("1117GPEKK1205250504260735-Post Surgery Outreach-v1.08.csv", as.is=T, na.strings="")
str(discharges)

#recast
discharges$ADMSN_DT = as.POSIXct(discharges$ADMSN_DT, format="%Y-%m-%d", tz="UTC")
discharges$DISCH_DT = as.POSIXct(discharges$DISCH_DT, format="%Y-%m-%d", tz="UTC")

#retain 2023-2025
discharges = discharges[discharges$DISCH_DT>=as.POSIXct("2023-01-01"), ]
discharges = discharges[discharges$DISCH_DT<=as.POSIXct("2025-12-31"), ]

length(unique(discharges$PAT_ID))

#address history data
address_hx = read.csv("1117GPEKK1205250318260802-Post Surgery Outreach Patient Address History-v1.01.csv", as.is=T, na.strings="")
str(address_hx)

#Geisinger facility locations
geisinger_loc = c("414 East Drinker Street", "480 Central Rd.", "1 E. Norwegian St.", "1000 East Mountain Boulevard", "4752 SR 655", "100 North Academy Avenue", "27 Cjems Lane", "531 Mt Pleasant Drive", "300 Hollywood Boulevard", "126 Market Way", "3 West Olive Street", "4203 Hospital Dr.", "1000 E Mountain Dr", "529 Terry Reiley Way", "426 Airport Road", "1201 Oak Street", "425 East First St.", "1000 East Mountain Boulevard", "809 Hunter Highway", "821 Oak Street", "400 Highland Avenue", "100 North Academy Avenue", "675 Baltimore Drive", "809 Hunter Highway",
                  "119 Nevada Drive", "1151 North 4th Street", "300 Hollywood Boulevard", "114 Lt Michael Cleary Drive", "68 Spring Street", "549 Fair Street", "592 South Main Street", "255 Route 220 Highway", "400 Highland Avenue", "100 North Academy Avenue", "25 Church Street", "549 Fair Street", "620 Baltimore Drive", "64 Rehab Lane", "3 West Olive Street", "560 Pierce Street", "3 West Olive Street", "106 Derry Heights Boulevard", "263 Henry Street", "132 Abigail Lane", "106 Derry Heights Blvd", "1020 Thompson Street", "155 South Arch Street", "1000 East Mountain Blvd", 
                  "16 Woodbine Lane", "100 N Academy Ave", "132 Abigail Lane", "255 Route 220 Highway", "439 East First St.", "2 Lewis Street", "525 Pine Street", "100 North Academy Ave.", "8 Dundaff St.", "27 Sandy Lane", "950 East Mt Blvd", "132 Abigail Lane", "400 Highland Avenue", "16 Woodbine Lane", "175 South Wilkes Barre Boulevard", "68 Spring Street", "1800 Mulberry Street", "2266 Wilkes Barre Township Market Place", "15 Wesner Lane", "675 Baltimore Drive", "190 Welles St.", "10 Gateway Drive", "106 South Market Street", "35 Justin Drive", "1020 Thompson St.", 
                  "435 Scranton-Carbondale Highway", "1155 East Mountain Boulevard", "529 Terry Reiley Way", "1020 Thompson Street", "529 High Street", "529 Terry Reiley Way", "10 Gateway Drive", "480 Pierce Street", "21 Geisinger Lane", "1155 East Mountain Boulevard", "950 East Mountain Boulevard", "531 Mt Pleasant Drive", "21 Geisinger Lane", "31 South Main Street", "128 East Main Street", "240 Mall Boulevard", "300 Lackawanna Avenue", "4200 Hospital Road", "4830 Londonderry Road", "240 Mall Boulevard", "631 Northern Boulevard", "32 - 36 Central Ave.", "701 West Oak Street",
                  "3 West Olive Street", "4469 Red Rock Road", "1717 West Market Street", "155 South Arch St.", "1155 East Mountain Blvd.", "106 S. Market St.", "1 Guthrie Square", "128 E. Main St.", "436 W. Valley Ave.", "131 JPM Rd.", "100 North Academy Avenue", "480 Central Road", "126 Market Way", "255 Route 220 Highway", "114 Lt Michael Cleary Drive", "620 Baltimore Drive", "16 Woodbine Lane", "410 Glen Avenue", "126 Market Way", "1575 Old Trail Rd.", "818 South Main Street ", "5 Lakeview Drive", "240 Mall Boulevard", "166 Hanover St.", "211 3rd Street",
                  "1000 E Mountain Dr", "3 West Olive Street", "9 Stearns Lane ", "950 East Mountain Boulevard", "7 S. Ohio Avenue", "100 North Academy Avenue", "1 Guthrie Square", "30 Baldwin Boulevard", "20 Diana Lane", "3 West Olive Street", "425 Market Street", "210 Medical Center Drive", "200 Scenery Drive", "42 North Main Street", "35 South Mountain Boulevard", "25 Church Street", "4200 Hospital Road", "201 Roosevelt Avenue", "3228 Cold Springs Road", "400 Highland Avenue", "25 Church Street", "9333 State Route 61", "293 Patriot Lane", "255 Route 220 Highway",
                  "1800 Mulberry Street", "1000 East Mountain Boulevard", "1020 Thompson Street", "10 Gateway Drive", "1204 Harry P O'Neill Highway", "1500 Main Street", "4200 Hospital Road", "225 East 4th Street", "10 Gateway Drive", "44 Woodbine Lane", "126 Market Way", "2200 West Front Street", "310 Electric Avenue", "1000 E Mountain Dr", "300 Hollywood Boulevard", "5 Morgan Highway", "126 Market Way", "114 Lt. Michael Cleary Dr.", "1100 Spruce St.", "240 Mall Boulevard", "560 Pierce St", "226 Buckaroo Lane", "3 West Olive Street", "25 Church Street", "60 Glenmaura National Blvd",
                  "529 Terry Reiley Way", "250 Reitz Blvd", "6850 Lows Rd", "226 Buckaroo Lane", "206 Maria Hall Drive", "1800 Mulberry St.", "75 Medical Park Dr.", "3 West Olive Street", "529 Terry Reiley Way", "157 Roosevelt Avenue", "1717 West Market St.", "300 Lackawanna Avenue", "1201 Oak Street", "1201 Oak Street", "120 Hamm Dr.", "116 Interstate Pkwy", "10 Choate Circle", "2407 Reichart Road", "21 Geisinger Lane", "5 Atrium Court", "132 Abigal Lane", "16 Woodbine Lane", "1525 North Church Street", "100 North Academy Avenue", "16 Woodbine Lane", "549 Fair St.", "120 Hamm Dr.",
                  "428 Windmere Dr. ", "435 Scranton Carbondale Highway", "16 Woodbine Lane", "204 Curtin St.", "2200 West Front St.", "16 Woodbine Lane", "10 Dundaff Street", "27 Cjems Lane", "425 East 1st Street", "701 West Oak", "12 Lilly Lake road", "703 Martin F. Gibbons Blvd", "8 Church Street", "3 Hospital Drive", "255 Route 220 Highway", "436 Market Street", "1201 Oak Street", "1175 East Mountain Boulevard", "426 Airport Road", "437 Market St.", "423 Scranton Carbondale Highway", "4200 Hospital Road", "255 Route 220 Highway", "102 Desmond St.", "240 Mall Boulevard",
                  "660 Baltimore Drive", "250 Reitz Boulevard", "400 Highland Avenue", "310 Electric Ave.", "3 West Olive Street", "499 Wyoming Avenue", "41 South Main Street", "4469 Red Rock Road", "1201 Oak Street", "31 South Main St.", "132 Abigail Lane", "1020 Thompson St.", "100 North Academy Avenue", "5 Lakeview Drive", "155 South Arch St.", "27 CJEMS Lane", "1000 E Mountain Dr", "210 Medical Center Dr.", "809 Hunter Highway", "675 Baltimore Dr.", "68 Spring Street", "529 Terry Reiley Way", "10 Dundaff Street", "42 N. Main St.", "35 S. Mountain Blvd.",
                  "1175 East Mountain Boulevard", "300 Hollywood Blvd.", "3228 Cold Springs Rd.", "1151 North 4th Street", "100 North Academy Ave.", "190 Welles St.", "200 Scenery Dr.", "201 Roosevelt Ave.", "55 Medical Park Drive", "57 N. Main St.", "400 Highland Ave.", "21 Geisinger Lane", "3 West Olive Street", "950 East Mountain Boulevard", "16 Woodbine Lane", "300 Lackawanna Avenue", "217 South Logan Boulevard", "4203 Hospital Dr.", "620 University Ave.", "27 Sandy Lane", "1740 E Broad St.", "27 Sandy Lane", "100 North Academy Ave.", "4200 Hospital Rd.",
                  "119 Nevada Drive", "5 Morgan Highway", "560 Pierce Street", "224 N. Logan Blvd.", "175 Northumberland Street", "2631 US-11", "300 Hollywood Boulevard", "35 S. Mountain Blvd.", "560 McElhattan Dr.", "5 Lakeview Commons", "10990 Rt. 61", "1201 Oak St.", "128 East Main Street", "68 Spring Street", "166 Hanover Street", "3 West Olive St.", "631 Northern Boulevard", "1155 E. Mountain Blvd.", "1824 E. 3rd St.", "410 Glenn Ave.", "25 Church Street", "1630 N. Atherton St.", "42 North Main St.", "157 Roosevelt Avenue", "809 Hunter Highway", "240 Mall Boulevard",
                  "114 Lt Michael Cleary Drive", "226 Buckaroo Lane", "300 Lackawanna Avenue", "35 Justin Drive", "32 Justin Drive")


### ADDRESS CLEANING ###

#note that "*" and "update(d) address" appear a lot in these data; often an address is found in the second field
flagged_record = grep("*", discharges$PRIM_ADDR_LINE_1, fixed=T)
discharges$PRIM_ADDR_LINE_1[flagged_record] = ifelse(!is.na(discharges$PRIM_ADDR_LINE_2[flagged_record]), discharges$PRIM_ADDR_LINE_2[flagged_record], discharges$PRIM_ADDR_LINE_1[flagged_record])

flagged_record = grep("update address", discharges$PRIM_ADDR_LINE_1, ignore.case=T)
discharges$PRIM_ADDR_LINE_1[flagged_record] = ifelse(!is.na(discharges$PRIM_ADDR_LINE_2[flagged_record]), discharges$PRIM_ADDR_LINE_2[flagged_record], discharges$PRIM_ADDR_LINE_1[flagged_record])

flagged_record = grep("updated address", discharges$PRIM_ADDR_LINE_1, ignore.case=T)
discharges$PRIM_ADDR_LINE_1[flagged_record] = ifelse(!is.na(discharges$PRIM_ADDR_LINE_2[flagged_record]), discharges$PRIM_ADDR_LINE_2[flagged_record], discharges$PRIM_ADDR_LINE_1[flagged_record])

flagged_record = grep("*", address_hx$ADDR_HX_LINE1, fixed=T)
address_hx$ADDR_HX_LINE1[flagged_record] = ifelse(!is.na(address_hx$ADDR_HX_LINE2[flagged_record]), address_hx$ADDR_HX_LINE2[flagged_record], address_hx$ADDR_HX_LINE1[flagged_record])

flagged_record = grep("update address", address_hx$ADDR_HX_LINE1, ignore.case=T)
address_hx$ADDR_HX_LINE1[flagged_record] = ifelse(!is.na(address_hx$ADDR_HX_LINE2[flagged_record]), address_hx$ADDR_HX_LINE2[flagged_record], address_hx$ADDR_HX_LINE1[flagged_record])

flagged_record = grep("updated address", address_hx$ADDR_HX_LINE1, ignore.case=T)
address_hx$ADDR_HX_LINE1[flagged_record] = ifelse(!is.na(address_hx$ADDR_HX_LINE2[flagged_record]), address_hx$ADDR_HX_LINE2[flagged_record], address_hx$ADDR_HX_LINE1[flagged_record])
rm(flagged_record)

#clean up remaining address fields by removing asterisk flagged comments
discharges$PRIM_ADDR_LINE_1 = gsub("\\*{1,3}[A-Za-z0-9 /-]+\\*{1,3}", "", discharges$PRIM_ADDR_LINE_1)
discharges$PRIM_ADDR_LINE_1 = gsub("\\*+", "", discharges$PRIM_ADDR_LINE_1)
discharges$PRIM_ADDR_LINE_1 = trimws(discharges$PRIM_ADDR_LINE_1)
discharges$PRIM_ADDR_LINE_1 = ifelse(discharges$PRIM_ADDR_LINE_1=="", NA, discharges$PRIM_ADDR_LINE_1)
address_hx$ADDR_HX_LINE1 = gsub("\\*{1,3}[A-Za-z0-9 /-]+\\*{1,3}", "", address_hx$ADDR_HX_LINE1)
address_hx$ADDR_HX_LINE1 = gsub("\\*+", "", address_hx$ADDR_HX_LINE1)
address_hx$ADDR_HX_LINE1 = trimws(address_hx$ADDR_HX_LINE1)
address_hx$ADDR_HX_LINE1 = ifelse(address_hx$ADDR_HX_LINE1=="", NA, address_hx$ADDR_HX_LINE1)

#apply address normalizer
geisinger_loc = normal_address(geisinger_loc, abbs=usps_street, abb_end=F)
discharges$PRIM_ADDR_LINE_1 = normal_address(discharges$PRIM_ADDR_LINE_1, abbs=usps_street, abb_end=F)
discharges$PRIM_ADDR_LINE_2 = normal_address(discharges$PRIM_ADDR_LINE_2, abbs=usps_street, abb_end=F)
discharges$TEMP_ADDR_LINE_1 = normal_address(discharges$TEMP_ADDR_LINE_1, abbs=usps_street, abb_end=F)
discharges$Rx_STREET_ADDR = normal_address(discharges$Rx_STREET_ADDR, abbs=usps_street, abb_end=F)
discharges$Hsp_Bill_Addr1 = normal_address(discharges$Hsp_Bill_Addr1, abbs=usps_street, abb_end=F)
address_hx$ADDR_HX_LINE1 = normal_address(address_hx$ADDR_HX_LINE1, abbs=usps_street, abb_end=F)

#cast date types for address history
address_hx$EFF_START_DATE = as.POSIXct(address_hx$EFF_START_DATE, format="%Y-%m-%d %H:%M:%OS", tz="UTC")
address_hx$EFF_END_DATE = as.POSIXct(address_hx$EFF_END_DATE, format="%Y-%m-%d %H:%M:%OS", tz="UTC")

#no end date indicates present day (date of data retrieval)
address_hx$EFF_END_DATE = as.POSIXct(ifelse(is.na(address_hx$EFF_END_DATE), as.POSIXct("2026-03-18 00:00:00.000", format="%Y-%m-%d %H:%M:%OS", tz="UTC"), address_hx$EFF_END_DATE), tz="UTC")

#save data
save.image("ehr_address.RData")


### TIME-INVARIANT ANALYSES ###

load("ehr_address.RData")

#missing address (primary address)
sum(is.na(discharges$PRIM_ADDR_LINE_1))
sum(is.na(discharges$PRIM_ADDR_LINE_1)) / nrow(discharges)

#PO box address (primary address)
discharges$address_discrepancies_po_box = 0
discharges$address_discrepancies_po_box[grep("po box", discharges$PRIM_ADDR_LINE_1, ignore.case=T)] = 1
discharges$address_discrepancies_po_box[grep("po box", discharges$PRIM_ADDR_LINE_2, ignore.case=T)] = 1
table(discharges$address_discrepancies_po_box)
round(prop.table(table(discharges$address_discrepancies_po_box)),2)

#homeless (primary address)
discharges$address_discrepancies_homeless = 0
discharges$address_discrepancies_homeless[grep("homeless", discharges$PRIM_ADDR_LINE_1, ignore.case=T)] = 1
discharges$address_discrepancies_homeless[grep("homeless", discharges$PRIM_ADDR_LINE_2, ignore.case=T)] = 1
table(discharges$address_discrepancies_homeless)
round(prop.table(table(discharges$address_discrepancies_homeless)),2)

#Geisinger address
geisinger_search = gregexpr(paste(escape_regex(geisinger_loc),collapse="|"), discharges$PRIM_ADDR_LINE_1, ignore.case=T)
geisinger_matches = as.integer(lapply(geisinger_search, attr, "match.length"))
sum(geisinger_matches!=-1, na.rm=T)
sum(geisinger_matches!=-1, na.rm=T) / nrow(discharges)

#multiple addresses
discharges$address_num = (!is.na(discharges$PRIM_ADDR_LINE_1)) + as.integer(!is.na(discharges$TEMP_ADDR_LINE_1)) + as.integer(!is.na(discharges$Rx_STREET_ADDR)) + as.integer(!is.na(discharges$Hsp_Bill_Addr1))
table(discharges$address_num)
sum(table(discharges$address_num)[3:5])
round(prop.table(table(discharges$address_num[discharges$address_num>=2])),2)
hist(discharges$address_num)

#multiple addresses: permutations with primary
table((!is.na(discharges$PRIM_ADDR_LINE_1)) + as.integer(!is.na(discharges$TEMP_ADDR_LINE_1)))
table((!is.na(discharges$PRIM_ADDR_LINE_1)) + as.integer(!is.na(discharges$Rx_STREET_ADDR)))
table((!is.na(discharges$PRIM_ADDR_LINE_1)) + as.integer(!is.na(discharges$Hsp_Bill_Addr1)))

#different addresses
discharges$address_discrepancies = apply(discharges[, c("PRIM_ADDR_LINE_1", "TEMP_ADDR_LINE_1", "Rx_STREET_ADDR", "Hsp_Bill_Addr1")], 1, count_discrepancies_addr)
table(discharges$address_discrepancies)
sum(table(discharges$address_discrepancies)[3:5])
sum(table(discharges$address_discrepancies)[3:5])/sum(table(discharges$address_num)[3:5])
round(prop.table(table(discharges$address_discrepancies[discharges$address_discrepancies>=2])),2)
hist(discharges$address_discrepancies)

#different address: which address fields
discharges$address_fields = paste("Primary", ifelse(is.na(discharges$TEMP_ADDR_LINE_1), "", ",Temporary"), ifelse(is.na(discharges$Rx_STREET_ADDR), "", ",Prescription"), ifelse(is.na(discharges$Hsp_Bill_Addr1), "", ",Billing"))
table(discharges$address_fields[discharges$address_discrepancies>=2])
round(prop.table(table(discharges$address_fields[discharges$address_discrepancies>=2])),2)

#different ZIP code (5 digit)
discharges$address_discrepancies_zip5 = apply(discharges[, c("PRIM_ZIP", "TEMP_ZIP", "Rx_Zip", "Hsp_Bill_Zip")], 1, count_discrepancies_zip5)
table(discharges$address_discrepancies_zip5)
sum(table(discharges$address_discrepancies_zip5)[3:5])
sum(table(discharges$address_discrepancies_zip5)[3:5])/sum(table(discharges$address_num)[3:5])
round(prop.table(table(discharges$address_discrepancies_zip5[discharges$address_discrepancies_zip5>=2])),2)
hist(discharges$address_discrepancies_zip5)

#different ZIP code (3 digit)
discharges$address_discrepancies_zip3 = apply(discharges[, c("PRIM_ZIP", "TEMP_ZIP", "Rx_Zip", "Hsp_Bill_Zip")], 1, count_discrepancies_zip3)
table(discharges$address_discrepancies_zip3)
sum(table(discharges$address_discrepancies_zip3)[3:5])
sum(table(discharges$address_discrepancies_zip3)[3:5])/sum(table(discharges$address_num)[3:5])
round(prop.table(table(discharges$address_discrepancies_zip3[discharges$address_discrepancies_zip3>=2])),2)
hist(discharges$address_discrepancies_zip3)


### TIME-VARIANT ANALYSES (10-yr address history) ###

#retain only address history that matches to the discharges (23 patients are in discharges without address history)
address_hx = address_hx[address_hx$PAT_ID %in% unique(discharges$PAT_ID), ]
#sum(unique(discharges$PAT_ID) %in% address_hx_wide$PAT_ID) / length(unique(discharges$PAT_ID)) #100%

#length of time (days)
address_hx$EFF_DAYS = as.integer(difftime(address_hx$EFF_END_DATE, address_hx$EFF_START_DATE, unit="days"))
hist(address_hx$EFF_DAYS, breaks="fd")
describe(address_hx$EFF_DAYS); quantile(address_hx$EFF_DAYS, probs=c(0.25,0.75))

#create a wide-format data frame to store results (sorting by PAT_ID)
address_hx_wide = data.frame("PAT_ID"=unique(address_hx$PAT_ID)[order(unique(address_hx$PAT_ID))], stringsAsFactors=F)

#years of follow-up from earliest date to present day (date of data retrieval)
address_hx_oldest = aggregate(EFF_START_DATE ~ PAT_ID, data=address_hx, FUN=min, na.action=na.pass)
address_hx_oldest$followup = as.numeric(difftime(as.POSIXct("2026-03-18 00:00:00.000", format="%Y-%m-%d %H:%M:%OS", tz="UTC"), address_hx_oldest$EFF_START_DATE, tz="UTC", units="days") / 365.25)
address_hx_oldest$EFF_START_DATE = NULL
address_hx_wide = merge(address_hx_wide, address_hx_oldest, by.x="PAT_ID", by.y="PAT_ID", all.x=T, all.y=F, sort=F)
rm(address_hx_oldest)
describe(address_hx_wide$followup); quantile(address_hx_wide$followup, probs=c(0.25,0.75))
sum(address_hx_wide$followup)

#number of address records, total
address_hx_wide$address_total = aggregate(LINE ~ PAT_ID, data=address_hx, FUN=max, na.action=na.pass)[, 2]
address_hx_wide$address_total_py = address_hx_wide$address_total/address_hx_wide$followup
describe(address_hx_wide$address_total); quantile(address_hx_wide$address_total, probs=c(0.25,0.75))
min(address_hx_wide$address_total)
max(address_hx_wide$address_total)
table(address_hx_wide$address_total>1); round(prop.table(table(address_hx_wide$address_total>1)), 2)
table(address_hx_wide$address_total>5); round(prop.table(table(address_hx_wide$address_total>5)), 2)
table(address_hx_wide$address_total>10); round(prop.table(table(address_hx_wide$address_total>10)), 2)
describe(address_hx_wide$address_total_py); quantile(address_hx_wide$address_total_py, probs=c(0.25,0.75))

#number of ZIP codes (5 digit), unique
address_hx_wide$zip5_unique = aggregate(ZIP_HX ~ PAT_ID, data=address_hx, FUN=count_discrepancies_zip5, na.action=na.pass)[, 2]
table(address_hx_wide$zip5_unique[address_hx_wide$address_total>=2]>1)
round(prop.table(table(address_hx_wide$zip5_unique[address_hx_wide$address_total>=2]>1)), 2)
table(address_hx_wide$zip5_unique[address_hx_wide$address_total>=2 & address_hx_wide$zip5_unique>1])
round(prop.table(table(address_hx_wide$zip5_unique[address_hx_wide$address_total>=2 & address_hx_wide$zip5_unique>1])), 2)

#number of ZIP codes (3 digit), unique
address_hx_wide$zip3_unique = aggregate(ZIP_HX ~ PAT_ID, data=address_hx, FUN=count_discrepancies_zip3, na.action=na.pass)[, 2]
table(address_hx_wide$zip3_unique[address_hx_wide$address_total>=2]>1)
round(prop.table(table(address_hx_wide$zip3_unique[address_hx_wide$address_total>=2]>1)), 2)
table(address_hx_wide$zip3_unique[address_hx_wide$address_total>=2 & address_hx_wide$zip3_unique>1])
round(prop.table(table(address_hx_wide$zip3_unique[address_hx_wide$address_total>=2 & address_hx_wide$zip3_unique>1])), 2)

#number of "significant" address changes
address_hx_wide$sig_change = aggregate(SIGNIFICANT_CHANGE_YN ~ PAT_ID, data=address_hx, FUN=count_sig_addr, na.action=na.pass)[, 2]
table(address_hx_wide$sig_change[address_hx_wide$address_total>=2]>1)
round(prop.table(table(address_hx_wide$sig_change[address_hx_wide$address_total>=2]>1)), 2)

