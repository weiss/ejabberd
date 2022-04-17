FROM debian:11-slim
ARG VERSION
ARG TARGETOS
ARG TARGETARCH
ENV LC_ALL=C.UTF-8 \
    LANG=en_US.UTF-8 \
    LANGUAGE=en_US.UTF-8 \
    REPLACE_OS_VARS=true \
    HOME=/opt/ejabberd \
    VERSION=${VERSION:-latest}
LABEL org.opencontainers.image.description="Robust and scalable Jabber / XMPP Instant Messaging platform" \
    org.opencontainers.image.source="https://github.com/processone/ejabberd" \
    org.opencontainers.image.title="ejabberd" \
    org.opencontainers.image.url="https://www.ejabberd.im/" \
    org.opencontainers.image.version=$VERSION

# Install required dependencies
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
    openssl \
    && rm -rf /var/cache/apt \
    && rm -rf /var/lib/apt/lists/*

# Install ejabberd
WORKDIR /opt/
COPY ejabberd-$VERSION-1-$TARGETOS-$TARGETARCH.run installer.run
RUN XMPPHOST=localhost ./installer.run \
    && rm ./installer.run

COPY ctl.sh /bin/ejabberdctl

HEALTHCHECK \
    --interval=5s \
    --timeout=5s \
    --start-period=5s \
    --retries=120 \
    CMD /bin/ejabberdctl status

# Setup runtime environment
USER ejabberd
VOLUME ["/opt/ejabberd/database","/opt/ejabberd/conf","/opt/ejabberd/logs","/opt/ejabberd/upload"]
EXPOSE 1883 4369-4399 5222 5269 5280 5443

ENTRYPOINT ["/bin/ejabberdctl"]
CMD ["foreground"]
