FROM tercen/runtime-flowsuite:3.15-1

COPY . /operator
WORKDIR /operator

ENV TERCEN_SERVICE_URI https://tercen.com

RUN R -e "renv::restore(confirm=FALSE)"

ENTRYPOINT ["R", "--no-save", "--no-restore", "--no-environ", "--slave", "-f", "main.R", "--args"]
CMD ["--taskId", "someid", "--serviceUri", "https://tercen.com", "--token", "sometoken"]