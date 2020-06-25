--                                    --
-- 医薬品イメージテーブルの作成       --
--                                    --
-- Create Date : 2013/08/29           --
--                                    --

CREATE TABLE tbl_med_image (
	HOSPNUM		smallint NOT NULL,
	SRYCD 		character(9) NOT NULL,
	IMAGEKBN	smallint NOT NULL,
	IMAGEDATA	character varying(500000),
	OPID		character varying(16),
	UPYMD		character varying(8)
);

ALTER TABLE ONLY tbl_med_image
    ADD CONSTRAINT tbl_med_image_primary_key PRIMARY KEY (HOSPNUM,SRYCD);
