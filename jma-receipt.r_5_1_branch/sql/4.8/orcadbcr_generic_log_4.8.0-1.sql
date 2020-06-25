--                                    --
-- 一般名記載ログテーブルの作成       --
--                                    --
-- Create Date : 2013/03/19           --
--                                    --

CREATE TABLE tbl_generic_log (
	HOSPNUM		smallint NOT NULL,
	PTID		bigint,
	SRYYMD		character(8),
	PRINTYMD	character(8),
	PRINTHMS	character(6),
	RECIPENUM	smallint NOT NULL,
	SYORIKBN 	smallint NOT NULL,
	DENPNUM  	integer DEFAULT 0,
	ZAINUM   	integer DEFAULT 0,
	SRYCD 		character(9),
	YAKKAKJNCD	character varying(12),
	GENERICNAME	character varying(200)
);

ALTER TABLE ONLY tbl_generic_log
    ADD CONSTRAINT tbl_generic_log_primary_key PRIMARY KEY (HOSPNUM,PTID,SRYYMD,PRINTYMD,PRINTHMS,RECIPENUM);
