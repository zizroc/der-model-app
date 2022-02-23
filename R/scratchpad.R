data_input <- reactive({
  req(input$food_feed_path_csv)
  fread(input$food_feed_path_csv$datapath)
})

loadData <- function() {
  # Read all the files into a list
  filesInfo <- drop_dir("https://ucsb.box.com/s/ejb8tbhnzij8g0o7iw5ix7ie7dnopbx0")
  filePaths <- filesInfo$path_display
  data <- lapply(filePaths, drop_read_csv, stringsAsFactors = FALSE)
  # Concatenate all data together into one data.frame
  data <- do.call(rbind, data)
  data
}
