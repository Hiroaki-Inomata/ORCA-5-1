\set ON_ERROR_STOP
--
-- プログラムオプション
--
-- Create Date : 2008/01/22         --
--
create	table	tbl_prgoption	(
	HOSPNUM		numeric(2,0) NOT NULL,
	PRGID     	char(25),
	KBNCD      	char(16),
	OPTION	  	varchar(5000),
	ADDKBN		char(1),
	TERMID	varchar(16),
	OPID	varchar(16),
	CREYMD	char(8),
	UPYMD	char(8),
	UPHMS	char(6),
	Constraint "tbl_prgoption_key" primary	key(
		HOSPNUM,
		PRGID,
		KBNCD
	)
);
