{application, natpmp, [
	{description, "NAT-PMP client"},
	{vsn, "0.4.1"},
	{modules, ['natpmp']},
	{registered, []},
	{applications, [kernel,stdlib,inet_cidr,inet_ext]}
]}.