# library(dplyr)
# library(sf)
# 
# # important to do with planar coords!
# # try without st_transform to see effects!
# 
# # test data
# nc = read_sf(system.file("gpkg/nc.gpkg", package="sf")) %>% 
#   st_transform(32119)
# 
# plot(st_geometry(nc))
# 
# cntrd = st_centroid(nc)
# plot(st_geometry(cntrd))
# 
# # 'from' features
# p_in <- cntrd %>% 
#   filter(CNTY_ID <= 1900) %>% 
#   select(CNTY_ID, NAME) %>% 
#   rename(id_in = CNTY_ID,
#          name_in = NAME) 
# 
# # 'to' features
# p_out <- cntrd %>% 
#   filter(CNTY_ID > 1900) %>% 
#   select(CNTY_ID, NAME, BIR74) %>% 
#   rename(id_out = CNTY_ID,
#          name_out = NAME,
#          BIR74_out = BIR74) 
# 
# # results, with coordinates of 'to' features
# res <- bind_cols(p_out[st_nearest_feature(p_in, p_out), ], 
#                  st_drop_geometry(p_in)) %>% 
#   relocate(id_in, name_in)
# 
# # background
# plot(st_geometry(nc))
# plot(st_geometry(p_in), col = "red", add = TRUE)
# plot(st_geometry(p_out), col = "purple", add = TRUE)
# 
# # furthest point
# plot(st_geometry(p_in)[4, ], col = "red", pch = 16, add = TRUE)
# plot(st_geometry(res)[4, ], col = "purple", pch = 16, add = TRUE)
# line <- st_nearest_points(p_in[4, ], res[4, ])
# plot(st_geometry(line), col = "blue", add = TRUE)
# 
# # closest point
# plot(st_geometry(p_in)[22, ], col = "red", pch = 16, add = TRUE)
# plot(st_geometry(res)[22, ], col = "purple", pch = 16, add = TRUE)
# line <- st_nearest_points(p_in[22, ], res[22, ])
# plot(st_geometry(line), col = "blue", add = TRUE)
# 
# # adding distance
# res$distance <- NA
# 
# for (i in seq_len(nrow(p_in))){
#   
#   # index of feature with min dist
#   which <- p_out[which.min(st_distance(p_in[i, ], p_out)), ]
#   
#   # extract distance
#   res$distance[i] <- st_distance(p_in[i, ], p_out[which, ])[1, 1]
#   
# }
# 
# res %>% 
#   st_drop_geometry() %>% 
#   tibble::rowid_to_column("ID") %>% 
#   arrange(distance) %>% 
#   select(-BIR74) %>% 
#   slice(1)
# 
# res %>% 
#   st_drop_geometry() %>% 
#   tibble::rowid_to_column("ID") %>% 
#   arrange(desc(distance)) %>% 
#   select(-BIR74) %>% 
#   slice(1)
