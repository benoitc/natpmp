{application, natpmp, [
	{description, "NAT-PMP client"},
	{vsn, "0.3.4"},
	{modules, ['natpmp']},
	{registered, []},
	{applications, [kernel,stdlib,inet_cidr]}
]}.