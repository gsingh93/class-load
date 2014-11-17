FROM ubuntu

# Remove debconf warnings about "Unable to initialize frontend"
ENV DEBIAN_FRONTEND noninteractive

# Temporary fix for https://github.com/haskell/cabal/issues/1883
ENV LANG C.UTF-8

# Install Haskell and Cabal
RUN apt-get update
RUN apt-get install -y software-properties-common
RUN add-apt-repository -y ppa:hvr/ghc
RUN apt-get update
RUN apt-get install -y cabal-install-1.20 ghc-7.8.3

ENV PATH ~/.cabal/bin:/opt/cabal/1.20/bin:/opt/ghc/7.8.3/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin

# Install yesod dependencies
RUN apt-get install -y zlib1g-dev
RUN cabal update
RUN cabal install alex happy yesod-bin

# So we can have multiple terminals
RUN apt-get install -y tmux

# Yesod development server
EXPOSE 3000
