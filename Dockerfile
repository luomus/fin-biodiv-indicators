FROM rstudio/plumber:v1.2.0

RUN  echo "ttf-mscorefonts-installer msttcorefonts/accepted-mscorefonts-eula select true" | debconf-set-selections \
  && apt-get update -qq \
  && apt-get install -y ttf-mscorefonts-installer libpq-dev nano \
  && fc-cache -f

HEALTHCHECK --interval=1m --timeout=10s \
  CMD curl -sfI -o /dev/null 0.0.0.0:8000/healthz || exit 1

RUN  install2.r -e \
       arm \
       config \
       covr \
       dbplyr \
       DT \
       ggplot2 \
       lme4 \
       logger \
       pool \
       rapidoc \
       readr \
       RPostgres \
       rtrim \
       svglite \
       tictoc \
       tinytest \
       tidyr

RUN  R -e "remotes::install_github('luomus/finbif@ddf7fe1b')" \
  && R -e "remotes::install_github('MarcoEnea/speedglm')" \
  && R -e "remotes::install_github('RetoSchmucki/rbms')"

COPY entrypoint.sh /home/user/entrypoint.sh
COPY init.R /home/user/init.R
COPY update_indices.R /home/user/update_indices.R
COPY api.R /home/user/api.R
COPY config/favicon.ico /home/user/favicon.ico
COPY config/robots.txt /home/user/robots.txt
COPY pkg /home/user/fbi
COPY config/config.yml /home/user/config.yml

ENV HOME /home/user
ENV OPENBLAS_NUM_THREADS 1

WORKDIR /home/user

RUN  R -e "remotes::install_local('fbi', NULL, FALSE, 'never')" \
  && mkdir -p \
       /home/user/coverage \
       /home/user/var \
       /home/user/tmp \
  && chgrp -R 0 \
       /home/user \
       /usr/local/lib/R/site-library/rapidoc/dist \
  && chmod -R g=u \
       /home/user \
       /usr/local/lib/R/site-library/rapidoc/dist \
       /etc/passwd

USER 1000

EXPOSE 8000

ENTRYPOINT ["./entrypoint.sh"]

CMD ["Rscript", "--vanilla", "init.R"]
