PROJECT = natpmp
PROJECT_DESCRIPTION = NAT-PMP client
PROJECT_VERSION = 0.5.2

DEPS = inet_cidr inet_ext

dep_inet_cidr = git https://github.com/benoitc/inet_cidr 0.2.0
dep_inet_ext = git https://github.com/benoitc/inet_ext 0.3.2

include erlang.mk

app:: rebar.config
