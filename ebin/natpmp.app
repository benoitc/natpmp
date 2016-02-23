{application, natpmp, [
	{description, "NAT-PMP client"},
	{vsn, "0.2.0"},
	{modules, ['natpmp']},
	{registered, []},
	{applications, [kernel,stdlib,inet_cidr]}
]}.