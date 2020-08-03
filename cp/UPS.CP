;ups.cp

record	ups
	.include 'def:ups.def'

record	ordhdr
	.include 'def:rd044a.def'

record	vars
	opnok	,d1
	entry	,a30
	inxctl	,d1
	cngctl	,d1
	whatno	,d2
	read	,d1,0
	write	,d1,1
	lokctl	,d1
	xord	,d6
	switch	,d1
	v	,d1
.proc
	
.end

