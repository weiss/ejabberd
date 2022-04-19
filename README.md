ejabberd Community Edition
==========================

[![CI](https://github.com/processone/ejabberd/actions/workflows/ci.yml/badge.svg)](https://github.com/processone/ejabberd/actions/workflows/ci.yml)
[![Coverage Status](https://coveralls.io/repos/github/processone/ejabberd/badge.svg?branch=master "Coverage in coveralls.io")](https://coveralls.io/github/processone/ejabberd?branch=master)
[![Translation status](https://hosted.weblate.org/widgets/ejabberd/-/ejabberd-po/svg-badge.svg "Translation status in Weblate")](https://hosted.weblate.org/projects/ejabberd/ejabberd-po/)
[![Hex version](https://img.shields.io/hexpm/v/ejabberd.svg "Hex version")](https://hex.pm/packages/ejabberd)

ejabberd is an open-source,
robust, scalable and extensible realtime platform built using Erlang/OTP,
including XMPP Server, MQTT Broker and SIP Service.
Check a detailed
[feature list](https://docs.ejabberd.im/admin/introduction/).


Install
-------

There are several ways to install ejabberd:
* Compile yourself from source code, see [COMPILE.md][compile]
* Install using graphical installer from [ProcessOne download page][p1dl]
* Install using console-text installer from [ejabberd GitHub Releases][ghr]
* Install using [Homebrew][hb]
* Install using your [Operating System package][osp]
* Pull a container image from [ejabberd Github Package][ghp], see [CONTAINER.md][container]
* Pull a container image from [ejabberd Docker Hub][dhecs], see [CONTAINER.md][container]

[compile]: https://github.com/badlop/ejabberd/blob/3781new1/COMPILE.md
[p1dl]: https://www.process-one.net/en/ejabberd/downloads/
[ghr]: https://github.com/processone/ejabberd/releases
[hb]: https://docs.ejabberd.im/admin/installation/#homebrew
[osp]: https://docs.ejabberd.im/admin/installation/#operating-system-packages
[ghp]: https://github.com/processone/ejabberd/pkgs/container/ejabberd
[container]: https://github.com/badlop/ejabberd/blob/3781new1/CONTAINER.md
[dhecs]: https://hub.docker.com/r/ejabberd/ecs


Translation
-----------

Using any gettext editor, you can improve the translation files found in
`priv/msgs/*.po`, and then submit your changes.

Alternatively, a simple way to improve translations is using our Weblate project:
https://hosted.weblate.org/projects/ejabberd/ejabberd-po/


Links
-----

- Documentation: https://docs.ejabberd.im
- Community site: https://www.ejabberd.im
- ejabberd commercial offering and support: https://www.process-one.net/en/ejabberd
