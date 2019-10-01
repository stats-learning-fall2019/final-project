cleanse_data <- function(data, drop_columns = FALSE) {
  # This should not be needed again. It was used to prune the text fields from the
  # data set initially, but the data was updated it github after running this command.
  if (drop_columns) {
    data = subset(
      data,
      select = -c(
        addnotes,
        alternative,
        alternative_txt,
        approxdate,
        attacktype1_txt,
        attacktype2_txt,
        attacktype3_txt,
        claimmode2_txt,
        claimmode3_txt,
        claimmode_txt,
        corp1,
        country_txt,
        dbsource,
        hostkidoutcome_txt,
        location,
        motive,
        natlty1_txt,
        natlty2_txt,
        propcomment,
        propextent_txt,
        region_txt,
        scite1,
        scite2,
        scite3,
        summary,
        target1,
        targsubtype1_txt,
        targsubtype2_txt,
        targsubtype3_txt,
        targtype1_txt,
        targtype2_txt,
        targtype3_txt,
        weapdetail,
        weapsubtype1_txt,
        weapsubtype2_txt,
        weapsubtype3_txt,
        weapsubtype4_txt,
        weaptype1_txt,
        weaptype2_txt,
        weaptype3_txt,
        weaptype4_txt
      )
    )
  }
  
  # Do Something
  return(data)
}
