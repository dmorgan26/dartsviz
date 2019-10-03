FROM rocker/verse:3.6.1

# add shiny server
RUN export ADD=shiny && bash /etc/cont-init.d/add

################
#install linux deps
################

RUN apt-get update -y && \
	apt-get install -y \ 
		curl

############
#install devtools
###########

RUN R -e "install.packages('devtools')"
RUN R -e "devtools::install_github('dmorgan26/dartsviz', build_vignettes = TRUE)"

################
# configure shiny server
################


COPY ./shiny-server.conf /etc/shiny-server/shiny-server.conf
COPY ./plot-app /home/daniel/Documents/dartsviz
