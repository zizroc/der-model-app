# graphics elements for plotting
colorz <-
  c(
    "animal products" = "#F17EB8",
    "fish & seafood" = "#489ED3",
    "plant products" = "#EEAF35"
  )
xlabz <- c("CNS-CFP" = "CFP",
           "ELA-PHD" = "ELA",
           "FDA-SDP" = "USDA")
labz <-
  c("Animal Products", "Fish and Seafood", "Plant Products")

mitigate_arrow <-
  ggplot(data = data.frame(x = c(0, 1), y = c(0, 1)),
         aes(x = x, y = y)) +
  annotate(
    "segment",
    x = 0,
    xend = 0,
    y = 0,
    yend = 1,
    arrow = arrow()
  ) +
  annotate(
    "text",
    x = -0.02,
    y = 0.5,
    label = "Socio-economic challenges for mitigation",
    size = 4,
    angle = 90
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank()
  ) +
  coord_cartesian(xlim = c(-0.04, 0.04), y = c(0, 1))

adapt_arrow <- ggplot(data = data.frame(x = c(0, 1), y = c(0, 1)),
                      aes(x = x, y = y)) +
  annotate(
    "segment",
    x = 0,
    xend = 1,
    y = 0,
    yend = 0,
    arrow = arrow()
  ) +
  annotate(
    "text",
    x = 0.5,
    y = 0.025,
    label = "Socio-economic challenges for adaptation",
    size = 4
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank()
  ) +
  coord_cartesian(xlim = c(0, 1), y = c(-0.04, 0.04))