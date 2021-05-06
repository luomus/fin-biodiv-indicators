FROM bitnami/minideb:unstable

RUN  echo 'APT::Install-Recommends "false";' > /etc/apt/apt.conf.d/90local-no-recommends

RUN  apt-get update \
  && apt-get install -y \
       libfontconfig1-dev \
       libpoppler-cpp-dev \
       libpq-dev \
       libssl-dev \
       locales \
       r-base-dev \
       python3-apt

RUN  echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen \
  && locale-gen en_US.utf8 \
  && /usr/sbin/update-locale LANG=en_US.UTF-8

ENV LC_ALL en_US.UTF-8
ENV LANG en_US.UTF-8

ARG PGUSER
ARG PGPASSWORD
ARG FINBIF_ACCESS_TOKEN
ARG FINBIF_EMAIL

RUN  echo "PGHOST='postgres'" >> /usr/lib/R/etc/Renviron.site \
  && echo "PGUSER='${PGUSER}'" >> /usr/lib/R/etc/Renviron.site \
  && echo "PGPASSWORD='${PGPASSWORD}'" >> /usr/lib/R/etc/Renviron.site \
  && echo "FINBIF_ACCESS_TOKEN='${FINBIF_ACCESS_TOKEN}'" >> /usr/lib/R/etc/Renviron.site \
  && echo "FINBIF_EMAIL='${FINBIF_EMAIL}'" >> /usr/lib/R/etc/Renviron.site

RUN  R -e "install.packages('bspm')" \
  && R -e "bspm::enable(); install.packages(c('dbplyr', 'curl', 'ggplot2', 'remotes', 'readr', 'rlang', 'RPostgres', 'rtrim', 'sodium', 'tidyr'))"

RUN  echo "deb http://deb.debian.org/debian unstable main contrib" > /etc/apt/sources.list \
  && echo "ttf-mscorefonts-installer msttcorefonts/accepted-mscorefonts-eula select true" | debconf-set-selections \
  && install_packages ttf-mscorefonts-installer

RUN  R -e "install.packages(c('fastmap', 'future', 'httr', 'lutz', 'plumber', 'promises', 'svglite'))"

RUN mkdir -p /home/user/tmpsvgs

ENV HOME /home/user

COPY entrypoint.sh /home/user/entrypoint.sh
COPY init.R /home/user/init.R
COPY api.R /home/user/api.R
COPY pkg /home/user/indicators

RUN  chgrp -R 0 /home/user \
  && chmod -R g=u /home/user /etc/passwd

WORKDIR /home/user

RUN  R -e "remotes::install_github('luomus/finbif@dev', dependencies = FALSE, upgrade = 'never')"
RUN  R -e "remotes::install_local('indicators', dependencies = FALSE, upgrade = 'never')"

EXPOSE 8000

ENTRYPOINT ["./entrypoint.sh"]
