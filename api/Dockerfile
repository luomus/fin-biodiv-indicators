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

RUN  echo "PGHOST='postgres'" >> /usr/lib/R/etc/Renviron.site \
  && echo "PGUSER='${PGUSER}'" >> /usr/lib/R/etc/Renviron.site \
  && echo "PGPASSWORD='${PGPASSWORD}'" >> /usr/lib/R/etc/Renviron.site \
  && echo "FINBIF_ACCESS_TOKEN='${FINBIF_ACCESS_TOKEN}'" >> /usr/lib/R/etc/Renviron.site

RUN  R -e "install.packages('bspm')" \
  && echo "bspm::enable()" >> /etc/R/Rprofile.site \
  && R -e "install.packages(c('dbplyr', 'finbif', 'plumber', 'remotes', 'rlang', 'RPostgres'))"

RUN  echo "deb http://deb.debian.org/debian unstable main contrib" > /etc/apt/sources.list \
  && echo "ttf-mscorefonts-installer msttcorefonts/accepted-mscorefonts-eula select true" | debconf-set-selections \
  && install_packages ttf-mscorefonts-installer \
  && R -e "bspm::disable(); install.packages('svglite')"

COPY init.R init.R

RUN mkdir tmpsvgs

EXPOSE 8000
ENTRYPOINT ["Rscript", "init.R"]
