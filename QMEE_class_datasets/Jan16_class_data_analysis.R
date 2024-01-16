# Jan 16 class dataset analysis

getwd()
setwd("/Users/noahsmith/QMEE/QMEE_class_datasets")
download.file(url="https://ndownloader.figshare.com/files/2292169",
              destfile = "/Users/noahsmith/QMEE/QMEE_class_datasets/Jan16_class_dataset.csv")

# need to update R -- # see chapter 5 of happygitwithr
# should I push the 3 MB csv to my github?

Jan16data = read_csv("Jan16_class_data.csv")
View(Jan16data)

## select (grabs columns from the dataframe you want)

select(Jan16data, plot_id, species_id, weight)

## you can also leave out the columns by using a minus sign.

select(Jan16data, -record_id, -species_id)

## filter (allows you to include/exclude observations/rows based on their name)

filter(Jan16data, year == 1995)

## pipe, there are two kinds, and both look like right arrows. 
## %>% |> 
## pipe means --> run a funciton, but substitute the thing on the left side of the pipe
## with the first argument on the right side of the pipe.
## take Jan16data, and pass it to the select function, and select those columns

Jan16data %>% select(record_id)

## and you can also use the pipe to set up chains of commands that filter based on various things. 
## however, we are including the pipe at the end of the line because it looks 
## more tidy. It also tells r that you want your next line to be included in the current command/sorting

Jan16data_filtered <- Jan16data %>%
  select(year, record_id) %>%
  filter(year==1995)

## mutate 
## we don't use "==" here 
## <- is for assignment, and "==" is to test whether things are two equal
## and we use "=" here (single equals), because idk. 

Jan16data %>% mutate(weight_kg = weight/1000)

## this creates a new weight column named "weight_kg"

## if you want to outright replace the weight column, you can use this code

# Jan16data %>% mutate(weight = weight/1000)

