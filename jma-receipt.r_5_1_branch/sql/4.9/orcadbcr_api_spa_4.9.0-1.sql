--                                    --
--  ＡＰＩ一時データテーブルの作成       --
--                                    --
-- Create Date : 2015/07/XX           --
--                                    --

CREATE TABLE tbl_api_para (
	HOSPNUM		smallint NOT NULL,
	GYOUMUID	character varying(8)  NOT NULL,
	TERMID		character varying(64) NOT NULL,
	FILEMEI		character varying(20) NOT NULL,
	EDANUM		integer   default 0,
	KARTE_UID	character varying(36),
	PTNUM  		character varying(20),
        DATA_REC        character varying (5000)
);

ALTER TABLE ONLY tbl_api_para
    ADD CONSTRAINT tbl_api_para_primary_key PRIMARY KEY (HOSPNUM,GYOUMUID,TERMID,FILEMEI,EDANUM);
