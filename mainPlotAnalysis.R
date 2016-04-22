#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

main = function() {
  
  devtools::load_all()

  dir = "plots/"
  if(!dir.exists(dir)) {
    catf(" - Creating dir: plots/")
    dir.create(dir)
  }

  # just for tests
  has.OpenML.data = FALSE

  if(has.OpenML.data) {
    # data from OpenML
    data = getRunResultsByTag(tag = "randomBot")
  } else {
    # data from file (study-7)
    filename = "data/full_data_results.RData"
    load(filename)
  }
  
  # Generate results
  cat(" # Results Analysis: \n")
  doTheAnalysis(data = data, k = 5)
  
  cat(" All done !\n")

}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

main()

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------