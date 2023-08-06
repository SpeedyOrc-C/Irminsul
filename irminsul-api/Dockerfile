FROM haskell:9.4.5

WORKDIR /opt/irminsul
COPY . /opt/irminsul

RUN cabal update
RUN cabal build
RUN cabal install

EXPOSE 50000

CMD ["irminsul-api"]
 