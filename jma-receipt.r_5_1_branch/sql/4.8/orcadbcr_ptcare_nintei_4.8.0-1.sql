--                                    --
-- 患者介護認定者情報テーブルの作成   --
--                                    --
-- Create Date : 2014/01/28           --
--                                    --

CREATE TABLE tbl_ptcare_nintei (
	HOSPNUM		smallint NOT NULL,
	PTID		bigint,
        TEKSTYMD	character varying(8)  NOT NULL,
        TEKEDYMD	character varying(8),
        CAREJOTAICD	character varying(2),
        NINTEIYMD	character varying(8),
	TERMID		character varying(16),
	OPID		character varying(16),
	CREYMD		character varying(8),
	UPYMD		character varying(8),
	UPHMS		character varying(6)
);

ALTER TABLE ONLY tbl_ptcare_nintei
    ADD CONSTRAINT tbl_ptcare_nintei_primary_key PRIMARY KEY (HOSPNUM,PTID,TEKSTYMD);
