--                                    --
-- 患者地域連携テーブルの作成       --
--                                    --
-- Create Date : 2012/11/22           --
--                                    --

CREATE TABLE tbl_ptnum_public (
	HOSPNUM		smallint NOT NULL,
	PTID		bigint,
        PATIENT_ID_1    character varying(20),
	TERMID		character varying(16),
	OPID		character varying(16),
	CREYMD		character varying(8),
	UPYMD		character varying(8),
	UPHMS		character varying(6)
);

ALTER TABLE ONLY tbl_ptnum_public
    ADD CONSTRAINT tbl_ptnum_public_primary_key PRIMARY KEY (HOSPNUM,PTID);
