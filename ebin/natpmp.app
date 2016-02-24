{application, natpmp, [
	{description, "NAT-PMP client"},
	{vsn, "0.5.2"},
	{modules, ['natpmp']},
	{registered, []},
	{applications, [kernel,stdlib,inet_cidr,inet_ext]}
]}.