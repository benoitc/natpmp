# natpmp

Implement the NAT-PMP protocol, typically supported by Apple routers and open source
routers such as DD-WRT and Tomato.
See https://tools.ietf.org/html/rfc6886

## Usage

### Get External IP address

```erlang
1> GatewayIP = "10.0.1.1",
2> natpmp:get_external_address(GatewayIp).
{ok,"192.168.1.50"}
```

This will return the external IP.

### Map an External IP Address

```erlang
GatewayIP = "10.0.1.1",
Protocol = tcp,
InternalPort = 80,
ExternalPortRequest = 8080,
Lifetime = 3600,

{ok, Since, InternalPort, ExternalPortRequest, MappingLifetime} = \
    natpmp:add_port_mapping(GatewayIP, Protocol, InternalPort, ExternalPortRequest, Lifetime).
```

This map the port 80 to the port 8080.

> Note: pass 0 to the ExternalPortRequest to ask to the router to create a dynamic port.

### Remove a port mapping

```erlang
ok = natpmp:delete_port_mapping(GatewayIp, Protocol, InternalPort, ExternalPort)
```

### Discover the gateway

```erlang
1> {ok, Gateway} = natpmp:discover().
{ok,{10,0,1,1}}
```

### Get internal and external address

```erlang
1> {ok, Gateway} = natpmp:discover().
{ok,{10,0,1,1}}
2> natpmp:get_internal_address(Gateway).
"10.0.1.6"
3> natpmp:get_external_address(Gateway).
{ok,"192.168.1.50"}
```

## Contribute

For issues, comments or feedback please create an [issue](https://github.com/benoitc/natpmp/issues).
