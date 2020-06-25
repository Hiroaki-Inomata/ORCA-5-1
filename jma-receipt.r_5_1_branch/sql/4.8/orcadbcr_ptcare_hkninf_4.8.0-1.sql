--                                    --
-- 患者介護保険情報テーブルの作成       --
--                                    --
-- Create Date : 2014/01/28           --
--                                    --

CREATE TABLE tbl_ptcare_hkninf (
	HOSPNUM		smallint NOT NULL,
	PTID		bigint,
        TEKSTYMD	character varying(8)  NOT NULL,
        TEKEDYMD	character varying(8),
        HKNJANUM	character varying(6),
        HIHKNJANUM	character varying(10),
	TERMID		character varying(16),
	OPID		character varying(16),
	CREYMD		character varying(8),
	UPYMD		character varying(8),
	UPHMS		character varying(6)
);

ALTER TABLE ONLY tbl_ptcare_hkninf
    ADD CONSTRAINT tbl_ptcare_hkninf_primary_key PRIMARY KEY (HOSPNUM,PTID,TEKSTYMD);
