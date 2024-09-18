# init geojson object
geojson_data <- reactiveVal()

# Set async strategy
future::plan(future::multisession)

# notify
cat(MODEULE, "Asynchronous -- start reading geojson data...")

# async read data
future::future(
    # read the data
    geojsonio::geojson_read("./mydata.geojson", what = "sp"))

    # assign output to reactive
    # (pipe a %...!% = a %>% catch(b)
    geojson_data() %...!%
      # if error
      (function(e) {
          # feed reactival until NULL
          geojson_data(NULL)

          # generate warning
          warning(e)})
