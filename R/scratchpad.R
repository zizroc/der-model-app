data_input <- reactive({
  req(input$food_feed_path_csv)
  fread(input$food_feed_path_csv$datapath)
})

drop_auth(rdstoken = "dropbox_token.rds")

drop_auth(
  new_user = FALSE,
  key = "mmhfsybffdom42w",
  secret = "l8zeqqqgm1ne5z0",
  cache = TRUE
)

load_data_dropbox <- function() {
  # Read all the files into a list
  filesInfo <- drop_dir("https://ucsb.box.com/s/5or22uc2xf97vop19c9fnfok5blq9fy0")
  filePaths <- filesInfo$path_display
  data_df <- lapply(filePaths, drop_read_csv, stringsAsFactors = FALSE)
  # Concatenate all data together into one data.frame
  data_df <- do.call(rbind, data_df)
  data_df
}
