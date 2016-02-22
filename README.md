# natpmp

Implement the NAT-PMP protocol, typically supported by Apple routers and open source
routers such as DD-WRT and Tomato.
See http://tools.ietf.org/html/draft-cheshire-nat-pmp-03

## Usage

### Get External IP address

```erlang
1> GatewayIP = "10.0.1.1",
2> natpmp:get_external_ip(GatewayIp).
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

{ok, Since, InternalPort, ExternalPortRequest, MappingLifetile} = \
    natpmp:add_port_mapping(GatewayIP, tcp, 80, 0).
```

This map the port 80 to the port 8080.

> Note: pass 0 to the ExternalPortRequest to ask to the router to create a dynamic port.

### Remove a port mapping

```erlang
ok = natpmp:delete_port_mapping(GatewayIp, Protocol, InternalPort, ExternalPort)
```

## Contribute

For issues, comments or feedback please create an [issue](https://github.com/benoitc/natpmp/issues).
