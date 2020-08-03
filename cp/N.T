	using otaxfl select
	('MNN','WIN','SDN','IAN'),	NON_TAX = 1
	(),				NON_TAX = 0
	endusing

	USING OSCAC SELECT
	('1 '),	IF(.NOT. NON_TAX) OTAXFL = 'MNT'
	('10'),	IF(.NOT. NON_TAX) OTAXFL = 'MNT'
	('WC' ),	IF(.NOT. NON_TAX) OTAXFL = 'MNT'
	ENDUSING
