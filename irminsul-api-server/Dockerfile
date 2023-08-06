FROM haskell:9.4.5

WORKDIR /opt/irminsul

# COPY cabal_config /root/.cabal/config
RUN cabal update
COPY irminsul-api-server.cabal /opt/irminsul/irminsul-api-server.cabal

RUN cabal build --only-dependencies

COPY . /opt/irminsul
RUN cabal install

EXPOSE 50000

CMD ["irminsul-api-server"]
 
