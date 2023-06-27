FROM ghcr.io/luomus/base-r-image@sha256:7b02c5e1679ea46fa44e1d8ad8a56551fff2f90779e509676a378670e8e85517

COPY renv.lock /home/user/renv.lock
COPY update_indices.R /home/user/update_indices.R
COPY api.R /home/user/api.R
COPY api.md /home/user/api.md
COPY config/favicon.ico /home/user/favicon.ico
COPY config/config.yml /home/user/config.yml
COPY pkg /home/user/pkg

RUN R -e "renv::restore()" \
  && sed -i 's/RapiDoc/fin-biodiv-indicators/g' \
    `R --slave -e "cat(.libPaths()[[1]])"`/rapidoc/dist/index.html \
  && mkdir -p /home/user/coverage /home/user/var /home/user/tmp \
  && chgrp -R 0 /home/user \
  && chmod -R g=u /home/user /etc/passwd
