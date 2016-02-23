PROJECT = natpmp
PROJECT_DESCRIPTION = NAT-PMP client
PROJECT_VERSION = 0.3.1

DEPS = inet_cidr

dep_inet_cidr = git https://github.com/benoitc/inet_cidr 0.2.0

include erlang.mk

app:: rebar.config
