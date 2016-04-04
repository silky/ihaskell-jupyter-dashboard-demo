FROM        ubuntu:14.04
MAINTAINER  Noon van der Silk <noonsilk@gmail.com>


# Deps for building IHaskell and Jupyter
RUN apt-get update &&           \
        apt-get install -y      \
            python-dev          \
            python-setuptools   \
            libmagic-dev        \
            libtinfo-dev        \
            libzmq3-dev         \
            libcairo2-dev       \
            libpango1.0-dev     \
            libblas-dev         \
            liblapack-dev       \
            gcc                 \
            g++                 \
            libncurses5-dev


# Install Jupyter and Jupyter Dashboards
RUN easy_install -U pip && pip install -U jupyter &&            \
    pip install jupyter_dashboards &&                           \
    jupyter dashboards install --user --symlink --overwrite &&  \
    jupyter dashboards activate


# Set up a working directory for IHaskell
RUN mkdir /ihaskell
WORKDIR /ihaskell


# Copy in relevant parts of IHaskell
COPY ext/IHaskell/src               src
COPY ext/IHaskell/main              main
COPY ext/IHaskell/html              html
COPY ext/IHaskell/LICENSE           LICENSE
COPY ext/IHaskell/stack.yaml        stack.yaml
COPY ext/IHaskell/ihaskell.cabal    ihaskell.cabal
COPY ext/IHaskell/ipython-kernel    ipython-kernel
COPY ext/IHaskell/ghc-parser        ghc-parser
COPY ext/IHaskell/ihaskell-display  ihaskell-display


# Install stack from the FPComplete repositories.
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 575159689BEFB442 && \
    echo 'deb http://download.fpcomplete.com/ubuntu trusty main' > /etc/apt/sources.list.d/fpco.list && \
    apt-get update && \
    apt-get install -y stack


# Set up stack and install IHaskell binaries. Note that we
# have to upgrade stack to catch a recent bugfix where hpack
# and modules with dashed names are used.
RUN stack upgrade --git


# Set the env to use our upgraded stack and whatnot.
ENV PATH /stack-bin:/root/.local/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin


# Begin installing things. Note that we need to use the same resolver for
# every stack command; otherwise we will not having everything in the same
# environment when we run the notebook.
RUN mkdir /stack-bin
RUN stack setup --resolver lts-5.11


# Install stackage libraries that we want to work with
# in the notebook itself. Add any other ones you are
# interested in here.
RUN stack install wreq        --resolver lts-5.11
RUN stack install lens-aeson  --resolver lts-5.11
RUN stack install tagsoup     --resolver lts-5.11


# Install our own local libraries and then IHaskell.
RUN mkdir lib
COPY lib lib
COPY lib-stack.yaml lib-stack.yaml
RUN stack install --local-bin-path /stack-bin --stack-yaml lib-stack.yaml --resolver lts-5.11


# Do this last; it seems to fail finding `IHaskell.Display` unless
# I do this. I don't know why, and it makes me sad.
# TODO: Figure out why this is so.
RUN stack install --local-bin-path /stack-bin --resolver lts-5.11


# Prepare the running environment
RUN mkdir /notebooks
RUN ihaskell install
ENTRYPOINT stack exec --resolver lts-5.11               \
               -- jupyter notebook                      \
                --NotebookApp.port=8889                 \
                --NotebookApp.ip='*'                    \
                --NotebookApp.notebook_dir=/notebooks
EXPOSE 8889
