

# for the map
ggplot() +
  # geom_tile(aes(x = lon, y = lat, fill = mbs)) +
  geom_tile(aes(x = LON, y = LAT, fill = bathy)) +
  scale_fill_brewer(type="qual", palette = "Dark2") +
  coord_quickmap() +  # Prevents stretching when resizing
  theme_bw() +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(legend.title = element_blank()) +
  coastd
