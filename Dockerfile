FROM ghcr.io/luomus/base-r-image@sha256:db3ad26dc6edfdaed2611f87b948cb3ee9fd41cf9fab8dc701508f5c70906020

COPY renv.lock /home/user/renv.lock
COPY update_indices.R /home/user/update_indices.R
COPY api.R /home/user/api.R
COPY api.md /home/user/api.md
COPY favicon.ico /home/user/favicon.ico
COPY config.yml /home/user/config.yml
COPY DESCRIPTION /home/user/DESCRIPTION
COPY inst /home/user/inst
COPY man /home/user/man
COPY NAMESPACE /home/user/NAMESPACE
COPY R /home/user/R
COPY tests /home/user/tests
COPY docs /home/user/docs

RUN  R -e "renv::restore()" \
  && R -e 'remotes::install_local(dependencies = FALSE, upgrade = FALSE)' \
  && permissions.sh
