--                                    --
--  主科テーブルの変更                --
--                                    --
-- 項目の追加                         --
--     保険番号       HKNNUM          --
--     システム主科   SRYKA_AUTO      --
--                                    --
-- Create Date : 2006/03/08           --
--                                    --

-- tbl_syuka 削除               --

drop table tbl_syuka;

-- tbl_syuka 作成               --
create	table	tbl_syuka	(
	HOSPID	    char(24),
	NYUGAIKBN	char(1),
	PTID	    numeric(10,0),
	TEN_KBN	    char(1),
	SRYYM	    char(6),
	TEISYUTUSAKI	char(1),
	HKNJANUM	char(8),
	HOJOKBN	    char(1),
	RECESYUBETU	char(4),
	SRYKA   	char(2),
	HKNNUM  	char(3),
	SRYKA_INPUT	char(2),
	SRYKA_AUTO	char(2),
	HKNCOMBI_1	numeric(4,0),
	HKNCOMBI_2	numeric(4,0),
	HKNCOMBI_3	numeric(4,0),
	HKNCOMBI_4	numeric(4,0),
	HKNCOMBI_5	numeric(4,0),
	HKNCOMBI_6	numeric(4,0),
	HKNCOMBI_7	numeric(4,0),
	HKNCOMBI_8	numeric(4,0),
	HKNCOMBI_9	numeric(4,0),
	HKNCOMBI_10	numeric(4,0),
	TENSU     	numeric(9,0),
	KAISU      	numeric(3,0),
	NISSU      	numeric(3,0),
	EDDAY	    numeric(2,0),
	MISEIKYU_FLG	char(2),
	DLT_FLG	    char(1),
	TERMID	    varchar(16),
	OPID	    varchar(16),
	CREYMD	    char(8),
	UPYMD	    char(8),
	UPHMS	    char(6),
	constraint tbl_syuka_primary_key primary	key(
		HOSPID,
		NYUGAIKBN,
		PTID,
		TEN_KBN,
		SRYYM,
		TEISYUTUSAKI,
		HKNJANUM,
		HOJOKBN,
		RECESYUBETU,
		SRYKA
	)
);
CREATE INDEX idx_syuka_sryym ON tbl_syuka USING btree ("hospid","nyugaikbn","sryym");

