# docker manifest inspect ghcr.io/luomus/base-r-image:main -v | jq '.Descriptor.digest'
FROM ghcr.io/luomus/base-r-image@sha256:581cbb119820a9de9f416a216520d928489becb1ac474303d5476353f458140b

COPY renv.lock /home/user/renv.lock

RUN R -s -e "renv::restore()"

COPY update_indices.R /home/user/update_indices.R
COPY run_update.R /home/user/run_update.R
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
COPY .Rbuildignore /home/user/.Rbuildignore

RUN R CMD INSTALL .
RUN permissions.sh
