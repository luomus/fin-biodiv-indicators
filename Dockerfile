FROM ghcr.io/luomus/base-r-image@sha256:b61f78d380e35c41b4161a55b56b4ba2c6ba9baeb5837df9504d141e1a8cdce7

COPY renv.lock /home/user/renv.lock
COPY update_indices.R /home/user/update_indices.R
COPY api.R /home/user/api.R
COPY api.md /home/user/api.md
COPY config/favicon.ico /home/user/favicon.ico
COPY config/config.yml /home/user/config.yml
COPY pkg /home/user/pkg

RUN R -e "renv::restore()" \
  && R -e 'remotes::install_local(dependencies = FALSE, upgrade = FALSE)' \
  && permissions.sh

