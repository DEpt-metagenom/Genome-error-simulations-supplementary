FROM debian:12.1

COPY overlooked_phenomenon_shinyapp_docker.R /opt/overlooked_phenomenon_shinyapp.R
COPY load_vars.RData /opt/

RUN    TZ=Europe/Budapest
RUN    ln -snf "/usr/share/zoneinfo/$TZ" /etc/localtime && echo $TZ > /etc/timezone

RUN    apt update

RUN    apt install -y r-base r-cran-shiny r-cran-shinydashboard r-cran-httr r-cran-tidyverse r-cran-ggplot2 r-cran-plotly r-cran-ggally

RUN    Rscript -e "install.packages(c(\"shinydashboardPlus\", \"DT\", \"hablar\", \"magrittr\", \"betareg\"))"
#RUN    Rscript -e "install.packages('DT')"

EXPOSE    6186/tcp

CMD    echo "You can access the ShinyApp by opening the following link in your browser: http://127.0.0.1:6186/" & R --quiet --no-save < /opt/overlooked_phenomenon_shinyapp.R 1>/dev/null 2>/dev/null
