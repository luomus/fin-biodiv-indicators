FROM ghcr.io/luomus/base-r-image@sha256:047e13660472b4e82a2de18b3aca9900edd0ac67ddcf729e71a6c53a29c6c09b

COPY renv.lock /home/user/renv.lock
COPY update_indices.R /home/user/update_indices.R
COPY api.R /home/user/api.R
COPY api.md /home/user/api.md
COPY config/favicon.ico /home/user/favicon.ico
COPY config/config.yml /home/user/config.yml
COPY DESCRIPTION /home/user/DESCRIPTION
COPY inst /home/user/inst
COPY man /home/user/man
COPY NAMESPACE /home/user/NAMESPACE
COPY R /home/user/R
COPY tests /home/user/tests

RUN  R -e "renv::restore()" \
  && R -e 'remotes::install_local(dependencies = FALSE, upgrade = FALSE)' \
  && permissions.sh
