FROM rstudio/plumber:latest

RUN  echo "ttf-mscorefonts-installer msttcorefonts/accepted-mscorefonts-eula select true" | debconf-set-selections \
  && apt-get update -qq \
  && apt-get install -y ttf-mscorefonts-installer libpq-dev \
  && fc-cache -f

HEALTHCHECK --interval=1m --timeout=10s \
  CMD curl -sfI -o /dev/null 0.0.0.0:8000/healthz || exit 1

RUN  install2.r \
       rapidoc \
       readr

RUN  R -e "remotes::install_github('luomus/finbif@43bc598e')"

COPY entrypoint.sh /home/user/entrypoint.sh
COPY init.R /home/user/init.R
COPY api.R /home/user/api.R
COPY pkg /home/user/indicators

ENV HOME /home/user
ENV OPENBLAS_NUM_THREADS 1

WORKDIR /home/user

RUN  R -e "remotes::install_local('indicators')" \
  && mkdir -p \
       /home/user/tmp \
       /home/user/logs \
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
