FROM ghcr.io/luomus/base-r-image@sha256:5cbe930c53a4e71a00ef7770f1667f30f176908b75779696f5e3bc0ac7f44ded

COPY renv.lock /home/user/renv.lock
COPY update_indices.R /home/user/update_indices.R
COPY api.R /home/user/api.R
COPY api.md /home/user/api.md
COPY config/favicon.ico /home/user/favicon.ico
COPY config/config.yml /home/user/config.yml
COPY pkg /home/user/pkg

RUN R -e "renv::restore()" \
  && sed -i 's/RapiDoc/Finnish Biodiversity Indicators/g' \
    `R --slave -e "cat(.libPaths()[[1]])"`/rapidoc/dist/index.html \
  && mkdir -p /home/user/coverage /home/user/var /home/user/tmp \
  && chgrp -R 0 /home/user \
  && chmod -R g=u /home/user /etc/passwd
