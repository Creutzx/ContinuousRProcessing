###   ####   ###   ##   #####
#  #  #  #  #     #  #    #
#  #  #  #  ###   ####    #
#  #  #  #     #  #  #    #
###   ####  ###   #  #    #  EDITS

# Data quality level for logged dissolved oxygen satuation data is assigned from the DQL for DO data

####
#
# ##
#  #
### RADE THE DO DATA FIRST!!

################################################################
################################################################

#clean out exisiting environment
#helps to avoid overwriting
rm(list = ls())

#Choose file to work on
dofile <- file.choose()
load(dofile)

fname <- file.choose()


# Rename
dodata <- tmp_data

# Load the DOsat data
load(fname)

# Assign DQL's 
tmp_data$rDQL <- dodata$rDQL

# Assign Comments
tmp_data$cmnt <- dodata$cmnt

# If needed use standard edits scripts to adjust DO sat if necessary before saving


#When you have made all the edits run this line to save it back to the shiny app data folder
rm(dodata)
save(tmp_data, file = fname)


####
#  #
#  #
####THER OPTIONS BELOW

#Assign DO rDQL to DOsat rDQL when DOsat data is truncated shorter than DO
# dataframes are not the same length
length(dodata$r) == length(tmp_data$r)
tmp_data$DATETIME[1] == dodata$DATETIME[1]
tmp_data$DATETIME[length(tmp_data$DATETIME)] ==  dodata$DATETIME[length(tmp_data$DATETIME)]

# Assign DQL's 
tmp_data$rDQL[1:length(tmp_data$rDQL)] <- dodata$rDQL[1:length(tmp_data$rDQL)]

# Assign Comments
tmp_data$cmnt[1:length(tmp_data$rDQL)] <- dodata$cmnt[1:length(tmp_data$rDQL)]



