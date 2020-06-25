--                            --
-- 医薬品分類テーブルの作成   --
--                            --
-- Create Date : 2016/03/02   --
--                            --

CREATE TABLE tbl_generic_class (
    yakkakjncd character varying(12) NOT NULL,
    yukostymd  character varying(8)  NOT NULL,
    yukoedymd  character varying(8)  NOT NULL,
    kouhatu    smallint,
    upymd      character varying(8)
);

ALTER TABLE ONLY tbl_generic_class
    ADD CONSTRAINT tbl_generic_class_primary_key PRIMARY KEY (yakkakjncd, yukostymd, yukoedymd);
