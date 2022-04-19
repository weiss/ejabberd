ejabberd Community Edition
==========================

[![CI](https://github.com/processone/ejabberd/actions/workflows/ci.yml/badge.svg)](https://github.com/processone/ejabberd/actions/workflows/ci.yml)
[![Coverage Status](https://coveralls.io/repos/github/processone/ejabberd/badge.svg?branch=master "Coverage in coveralls.io")](https://coveralls.io/github/processone/ejabberd?branch=master)
[![Translation status](https://hosted.weblate.org/widgets/ejabberd/-/ejabberd-po/svg-badge.svg "Translation status in Weblate")](https://hosted.weblate.org/projects/ejabberd/ejabberd-po/)
[![Hex version](https://img.shields.io/hexpm/v/ejabberd.svg "Hex version")](https://hex.pm/packages/ejabberd)

ejabberd is a
robust, scalable and extensible realtime platform,
including XMPP Server, MQTT Broker and SIP Service.
Check a detailed
[feature list](https://docs.ejabberd.im/admin/introduction/).

Quickstart guide
----------------

### 0. Requirements

To compile ejabberd you need:

 - GNU Make.
 - GCC.
 - Libexpat ≥ 1.95.
 - Libyaml ≥ 0.1.4.
 - Erlang/OTP ≥ 19.3.
 - OpenSSL ≥ 1.0.0.
 - Zlib ≥ 1.2.3, for Stream Compression support (XEP-0138). Optional.
 - PAM library. Optional. For Pluggable Authentication Modules (PAM).
 - ImageMagick's Convert program and Ghostscript fonts. Optional. For CAPTCHA
   challenges.
 - Elixir ≥ 1.10.3. Optional. Alternative to build ejabberd

If your system splits packages in libraries and development headers, you must
install the development packages also.

### 1. Compile and install on *nix systems

To compile ejabberd, execute the following commands.  The first one is only
necessary if your source tree didn't come with a `configure` script (In this
case you need autoconf installed).

    ./autogen.sh
    ./configure
    make

To install ejabberd, run this command with system administrator rights (root
user):

    sudo make install

These commands will:

- Install the configuration files in `/etc/ejabberd/`
- Install ejabberd binary, header and runtime files in `/lib/ejabberd/`
- Install the administration script: `/sbin/ejabberdctl`
- Install ejabberd documentation in `/share/doc/ejabberd/`
- Create a spool directory: `/var/lib/ejabberd/`
- Create a directory for log files: `/var/log/ejabberd/`


### 2. Start ejabberd

You can use the `ejabberdctl` command line administration script to
start and stop ejabberd. For example:

    ejabberdctl start


For detailed information please refer to the
[ejabberd Documentation](https://docs.ejabberd.im)


### 3. Use ejabberd locally

Alternatively, you can setup ejabberd without installing in your system:

    ./configure --with-rebar=rebar3
    make dev

Or, if you have Elixir available and plan to develop Elixir code:

    ./configure --with-rebar=mix
    make dev

Check the full list of targets:

    make help


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
