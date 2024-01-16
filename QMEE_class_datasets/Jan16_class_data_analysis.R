# Jan 16 class dataset analysis

getwd()
setwd("/Users/noahsmith/QMEE/QMEE_class_datasets")
download.file(url="https://ndownloader.figshare.com/files/2292169",
              destfile = "/Users/noahsmith/QMEE/QMEE_class_datasets/Jan16_class_dataset.csv")

# need to update R -- # see chapter 5 of happygitwithr

Jan16data = read_csv("Jan16_class_data.csv")
View(Jan16data)
