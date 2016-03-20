# [ ] Create recipes.had.co.nz S3 bucket 
# [ ] Generate redirect xml (see below)
# [ ] Test redirect xml by going to full S3 url
# [ ] Switch recipes.had.co.nz Route 53 to point to S3

# http://docs.aws.amazon.com/AmazonS3/latest/dev/HowDoIWebsiteConfiguration.html#configure-bucket-as-website-routing-rule-syntax

library(dplyr)

recipes <- src_sqlite("data-raw/recipes.sqlite3") %>% 
  tbl("recipes") %>% 
  collect() %>% 
  left_join(categories, by = "category_id")

recipes

src_c <- paste0("category/", categories$category_id)
dst_c <- categories$path

src_r <- paste0(recipes$id, "/", slug(recipes$name))
dst_r <- paste0(recipes$path, "/", slug(recipes$name), ".html")


src <- c(src_c, src_r)
dst <- c(dst_c, dst_r)
