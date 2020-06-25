--                                    --
-- パラメタテーブルの作成       --
--                                    --
-- Create Date : 2014/05/27           --
--                                    --
-- TBL_PRARA 削除 --

drop table tbl_para;

--                                    --
-- TBL_PRARA 作成 --


CREATE TABLE tbl_para (
	HOSPNUM		smallint NOT NULL,
	GYOUMUID	character varying(4)  NOT NULL,
	TERMID		character varying(64) NOT NULL,
	FILEMEI		character varying(20) NOT NULL,
	EDANUM		smallint DEFAULT 0,
        DATA_REC        character varying(5000)
);

ALTER TABLE ONLY tbl_para
    ADD CONSTRAINT tbl_para_primary_key PRIMARY KEY (hospnum, gyoumuid, termid, filemei, edanum);
