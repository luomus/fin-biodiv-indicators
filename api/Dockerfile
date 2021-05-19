FROM rstudio/plumber:latest

RUN  echo "ttf-mscorefonts-installer msttcorefonts/accepted-mscorefonts-eula select true" | debconf-set-selections \
  && apt-get update -qq \
  && apt-get install -y ttf-mscorefonts-installer libpq-dev \
  && fc-cache -f

ARG PGUSER
ARG PGPASSWORD
ARG FINBIF_ACCESS_TOKEN
ARG FINBIF_EMAIL
ARG FINBIF_WAREHOUSE_QUERY="warehouse/query/"

RUN  echo "PGHOST='postgres'" >> ${R_HOME}/etc/Renviron.site \
  && echo "PGUSER='${PGUSER}'" >> ${R_HOME}/etc/Renviron.site \
  && echo "PGPASSWORD='${PGPASSWORD}'" >> ${R_HOME}/etc/Renviron.site \
  && echo "FINBIF_ACCESS_TOKEN='${FINBIF_ACCESS_TOKEN}'" >> ${R_HOME}/etc/Renviron.site \
  && echo "FINBIF_EMAIL='${FINBIF_EMAIL}'" >> ${R_HOME}/etc/Renviron.site \
  && echo "FINBIF_WAREHOUSE_QUERY='${FINBIF_WAREHOUSE_QUERY}'" >> ${R_HOME}/etc/Renviron.site

COPY pkg indicators

RUN  install2.r readr \
  && R -e "remotes::install_github('wkmor1/plumber')" \
  && R -e "remotes::install_github('luomus/finbif@dev')" \
  && R -e "remotes::install_local('indicators')"

RUN mkdir -p /home/user/tmpsvgs

ENV HOME /home/user
ENV OPENBLAS_NUM_THREADS 1

COPY entrypoint.sh /home/user/entrypoint.sh
COPY init.R /home/user/init.R
COPY api.R /home/user/api.R

RUN  chgrp -R 0 /home/user \
  && chmod -R g=u /home/user /etc/passwd

WORKDIR /home/user

ENTRYPOINT ["./entrypoint.sh"]
