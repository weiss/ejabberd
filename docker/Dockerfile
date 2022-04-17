FROM debian:11-slim AS build
ARG VERSION
ARG TARGETOS
ARG TARGETARCH

RUN apt-get update \
    && apt-get install -y \
    openssl

COPY ejabberd-$VERSION-1-$TARGETOS-$TARGETARCH.run installer.run
RUN XMPPHOST=localhost ./installer.run

FROM debian:11-slim
ARG VERSION
ENV LC_ALL=C.UTF-8 \
    LANG=en_US.UTF-8 \
    LANGUAGE=en_US.UTF-8 \
    REPLACE_OS_VARS=true \
    HOME=/opt/ejabberd
LABEL org.opencontainers.image.description="Robust, Scalable and Extensible Realtime Platform" \
    org.opencontainers.image.source="https://github.com/processone/ejabberd" \
    org.opencontainers.image.title="ejabberd" \
    org.opencontainers.image.url="https://www.ejabberd.im/" \
    org.opencontainers.image.version=$VERSION

COPY ctl.sh /bin/ejabberdctl
COPY --from=build /etc/passwd /etc/passwd
COPY --from=build /opt /opt

HEALTHCHECK \
    --interval=5s \
    --timeout=5s \
    --start-period=5s \
    --retries=120 \
    CMD /bin/ejabberdctl status

USER ejabberd
VOLUME ["/opt/ejabberd/conf", \
    "/opt/ejabberd/database", \
    "/opt/ejabberd/logs", \
    "/opt/ejabberd/upload"]
EXPOSE 1883 4369-4399 5222 5269 5280 5443

ENTRYPOINT ["/bin/ejabberdctl"]
CMD ["foreground"]
