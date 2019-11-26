
library("htmltools")
library("bsplus")


carousel <- 
  bs_carousel(id = "the_images", use_indicators = TRUE) %>%
  bs_append(
    content = bs_carousel_image(src = "img/cmd.png"),
    caption = bs_carousel_caption("","Cassava Mosaic Disease")
  ) %>%
  bs_append(
    content = bs_carousel_image(src = "img/cmd.png"),
    caption = bs_carousel_caption("","")
  ) %>%
  bs_append(
    content = bs_carousel_image(src = "img/cmd.png"),
    caption = bs_carousel_caption("","")
  ) %>%
  bs_append(
    content = bs_carousel_image(src = "img/cmd.png"),
    caption = bs_carousel_caption("","")
  ) 