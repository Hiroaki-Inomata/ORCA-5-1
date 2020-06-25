\set ON_ERROR_STOP
--
-- オンライン返戻管理テーブル
--
-- Create Date : 2008/12/12         --
--
create	table	tbl_henrei_kanri	(
	HOSPNUM	smallint,
	TEISYUTUSAKI	char(1),
	SYORIYM	char(6),
	RECV_TOTAL_CNT	integer,
	RECV_PATIENT_CNT	integer,
	RECV_GO_CNT	integer,
	RECV_GO_TEN	bigint,
	TERMID	varchar(64),
	OPID	varchar(16),
	CREYMD	char(8),
	UPYMD	char(8),
	UPHMS	char(6),
	Constraint "tbl_henrei_kanri_primary_key" primary key(
		HOSPNUM,
		TEISYUTUSAKI,
		SYORIYM
	)
);
