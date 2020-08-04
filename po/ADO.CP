.subroutine Ado		;ADOBE READER
	f_name	,a

.include 'def:registry.def'

record
	r_path	,a200

record	vars
	ln	,d4
	a1	,a1
	reghndl	,i4
	errflg	,d6
	path	,a100
	rdr	,a150
;
.proc
	errflg = %RegStartSession(reghndl)
	errflg = %RegGetString (reghndl, HKEY_LOCAL_MACHINE, 
&	'SOFTWARE\Adobe\Acrobat Reader\6.0\installPath\',,path) 

	ln = %trim(path)
	if (ln .le. 1)
		begin
		errflg = %RegGetString (reghndl, HKEY_LOCAL_MACHINE, 
&	'SOFTWARE\Adobe\Acrobat Reader\7.0\installPath\',,path) 
		ln = %trim(path)
		end

	if (ln .le. 1)
		begin
		errflg = %RegGetString (reghndl, HKEY_LOCAL_MACHINE, 
&	'SOFTWARE\Adobe\Acrobat Reader\8.0\installPath\',,path) 
		ln = %trim(path)
		end

	if (ln .le. 1)
		begin
		errflg = %RegGetString (reghndl, HKEY_LOCAL_MACHINE, 
&	'SOFTWARE\Adobe\Acrobat Reader\9.0\installPath\',,path) 
		ln = %trim(path)
		end

	if (ln .le. 1)
		begin
		errflg = %RegGetString (reghndl, HKEY_LOCAL_MACHINE, 
&	'SOFTWARE\Adobe\Acrobat Reader\10.0\installPath\',,path) 
		ln = %trim(path)
		end

	if (ln .le. 1)
		begin
		errflg = %RegGetString (reghndl, HKEY_LOCAL_MACHINE, 
&	'SOFTWARE\Adobe\Acrobat Reader\11.0\installPath\',,path) 
		ln = %trim(path)
		end

	if (ln .le. 1)
		begin
		errflg = %RegGetString (reghndl, HKEY_LOCAL_MACHINE, 
&	'SOFTWARE\Adobe\Acrobat Reader\5.0\installPath\',,path) 
		ln = %trim(path)
		end

	
	rdr = path(1,ln) + '\AcroRd32.exe ' + f_name
	xcall spawn(rdr, D_NOWAIT)
	xreturn
.end


