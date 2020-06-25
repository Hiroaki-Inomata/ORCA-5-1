--                                    --
-- ＳＰＡ一時データテーブルの作成       --
--                                    --
-- Create Date : 2014/05/27           --
--                                    --

CREATE TABLE tbl_spa_tmp (
	HOSPNUM		smallint NOT NULL,
	GYOUMUID	character varying(4)  NOT NULL,
	TERMID		character varying(64) NOT NULL,
	FILEMEI		character varying(20) NOT NULL,
	EDANUM		smallint DEFAULT 0,
        DATA_REC        character varying (1000000)
);

ALTER TABLE ONLY tbl_spa_tmp
    ADD CONSTRAINT tbl_spa_tmp_primary_key PRIMARY KEY (HOSPNUM,GYOUMUID,TERMID,FILEMEI,EDANUM);
