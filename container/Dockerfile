# Base R Shiny image
FROM rocker/shiny:4.3.2

# Make a directory in the container
RUN mkdir /home/shiny-app

# Install R dependencies
# shiny is not installed since it comes from line 2 above (rocker)
RUN R -e "install.packages(c('tidyverse', 'shinydashboard', 'shinydashboardPlus', 'DT', 'rvest'))"

# Copy the Shiny app code
COPY ./scripts/app.R /home/shiny-app/app.R

# Expose the application port; should be the same port
# hard-coded in app.R using options(shiny.port = 8180) 
EXPOSE 8181 

# Run the R Shiny app everytime you launch the container
# alternative is using ENTRYPOINT [ "executable" ]
CMD Rscript /home/shiny-app/app.R


# To run the app inside the container after building,
# just run <docker run -p 8181:8181 shiny_rpkgs>