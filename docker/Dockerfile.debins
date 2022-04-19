FROM debian:11-slim AS build
ARG VERSION
ARG TARGETOS
ARG TARGETARCH
ENV HOME=/opt/ejabberd

RUN apt-get update \
    && apt-get install -y \
    openssl

COPY ejabberd-$VERSION-1-$TARGETOS-$TARGETARCH.run installer.run
RUN XMPPHOST=localhost ./installer.run

COPY tools/ejabberdctl.bc $HOME
RUN sed -i "s|var/log/ejabberd|opt/ejabberd/logs|g" $HOME/ejabberdctl.bc
RUN echo "source /opt/ejabberd/ejabberdctl.bc" > $HOME/.bashrc

RUN sed -i 's|as_current_user) |as_current_user) exec |g' /opt/ejabberd-$VERSION/bin/ejabberdctl

COPY tools/captcha.sh /opt/ejabberd-$VERSION/lib/ejabberd-$VERSION/priv/bin/
COPY tools/captcha-ng.sh /opt/ejabberd-$VERSION/lib/ejabberd-$VERSION/priv/bin/

FROM debian:11-slim
ARG VERSION
ENV LC_ALL=C.UTF-8 \
    LANG=en_US.UTF-8 \
    LANGUAGE=en_US.UTF-8 \
    REPLACE_OS_VARS=true \
    HOME=/opt/ejabberd
LABEL org.opencontainers.image.description="Robust, Scalable and Extensible Realtime Platform" \
    org.opencontainers.image.url="https://www.ejabberd.im/"

COPY docker/ctl.sh /usr/local/bin/ejabberdctl
COPY --from=build /etc/passwd /etc/passwd
COPY --from=build /opt /opt

HEALTHCHECK \
    --interval=5s \
    --timeout=5s \
    --start-period=5s \
    --retries=120 \
    CMD /usr/local/bin/ejabberdctl status

WORKDIR $HOME
USER ejabberd
VOLUME ["/opt/ejabberd/conf", \
    "/opt/ejabberd/database", \
    "/opt/ejabberd/logs", \
    "/opt/ejabberd/upload"]
EXPOSE 1883 4369-4399 5222 5269 5280 5443

ENTRYPOINT ["/usr/local/bin/ejabberdctl"]
CMD ["foreground"]
