FROM  fpco/stack-build
WORKDIR /app
COPY . .
RUN stack setup
RUN make
CMD ["./glados"]
