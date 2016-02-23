{application, natpmp, [
	{description, "NAT-PMP client"},
	{vsn, "0.4.0"},
	{modules, ['natpmp']},
	{registered, []},
	{applications, [kernel,stdlib,inet_cidr,inet_ext]}
]}.