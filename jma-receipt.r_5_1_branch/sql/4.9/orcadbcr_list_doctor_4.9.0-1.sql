--                                    --
--  診療科担当医テーブルの作成       --
--                                    --
-- Create Date : 2016/01/06           --
--                                    --

CREATE TABLE tbl_list_doctor (
	HOSPNUM	       smallint NOT NULL,
	SRYKA	       character varying(2)  NOT NULL,
	RENNUM	       smallint default 0,
	DRCD          character varying(5),
	TERMID        character varying(16),
	OPID          character varying(16),
	CREYMD        character varying(8),
	UPYMD         character varying(8),
	UPHMS         character varying(6)
);

ALTER TABLE ONLY tbl_list_doctor
    ADD CONSTRAINT tbl_list_doctor_primary_key PRIMARY KEY (HOSPNUM,SRYKA,RENNUM);
