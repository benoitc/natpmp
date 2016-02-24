{application, natpmp, [
	{description, "NAT-PMP client"},
	{vsn, "0.5.1"},
	{modules, ['natpmp']},
	{registered, []},
	{applications, [kernel,stdlib,inet_cidr,inet_ext]}
]}.