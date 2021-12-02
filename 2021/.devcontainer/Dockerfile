# See here for image contents: https://github.com/microsoft/vscode-dev-containers/tree/v0.192.0/containers/ubuntu/.devcontainer/base.Dockerfile

# [Choice] Ubuntu version: bionic, focal
ARG VARIANT="focal"
FROM mcr.microsoft.com/vscode/devcontainers/base:0-${VARIANT}

# ** [Optional] Uncomment this section to install additional packages. **
RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
    && apt-get -y install --no-install-recommends \
    iproute2 \
    lsb-release \
    procps \
    wget \
    && rm -rf /var/apt/cache

RUN curl -fsSL https://starship.rs/install.sh -o /install-starship.sh \
    && chmod +x /install-starship.sh \
    && /install-starship.sh --yes \
    && echo 'eval "$(starship init bash)"' >> /home/vscode/.bashrc

ARG UCM_DIR=/opt/unison-language
ARG UCM_ARCHIVE_NAME=ucm-linux.tar.gz
ARG UCM_ARCHIVE_URL=https://github.com/unisonweb/unison/releases/download/latest/$UCM_ARCHIVE_NAME

RUN mkdir -p $UCM_DIR \
    && wget -P $UCM_DIR $UCM_ARCHIVE_URL \
    && tar -C $UCM_DIR -xzf $UCM_DIR/$UCM_ARCHIVE_NAME \
    && ln -s $UCM_DIR/ucm /usr/bin/ \
    && rm $UCM_DIR/$UCM_ARCHIVE_NAME
