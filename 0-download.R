dir.create("raw_data")
dir.create("data")

download.file("https://coronadatascraper.com/timeseries.csv", "raw_data/timeseries.csv", mode = "wb")
## timeseries is also on the machine at /Users/adamhughes/coronadatascraper/dist/

# download.file("https://coronadatascraper.com/timeseries.json", "raw_data/timeseries.json", mode = "wb")

# download.file("https://coronadatascraper.com/timeseries-tidy.csv", "raw_data/timeseries-tidy.csv", mode = "wb")

download.file("https://coronadatascraper.com/data.csv", "raw_data/data.csv", mode = "wb")

