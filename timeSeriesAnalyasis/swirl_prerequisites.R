# 1. Install RGtk2. Select "Install GTK+" when prompted.
install.packages("RGtk2")
library("RGtk2")

# 2. Install rattle
install.packages("https://togaware.com/access/rattle_5.0.14.tar.gz", repos=NULL, type="source")
library("rattle")

# 3. Install the following packages
install.packages(c("rpart","rpart.plot","caret","RColorBrewer","swirl"))
library(swirl)  # The other libraries would be loaded automatically once you run the swirl lesson.

# Install the swirl lesson
install_course()  # select the file-"Decision_Trees.swc"

# Run the swirl lesson
swirl("Decision Trees")

# Go to the terminal at this point.
# Select "Let's get going!" when prompted
# Select the course "Decision Trees"
# Select the dataset "Bank Marketing Decision Tree"
# Enjoy the lesson!