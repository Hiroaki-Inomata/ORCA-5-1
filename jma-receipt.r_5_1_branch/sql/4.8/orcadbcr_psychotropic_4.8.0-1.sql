--                                    --
-- 向精神薬成分コードテーブルの作成   --
--                                    --
-- Create Date : 2015/12/09           --
--                                    --

CREATE TABLE tbl_psychotropic (
    code       character varying(7) NOT NULL,
    yukostymd  character varying(8) NOT NULL,
    yukoedymd  character varying(8) NOT NULL,
    class      character varying(1),
    groupcode  character varying(7),
    upymd      character varying(8)
);

ALTER TABLE ONLY tbl_psychotropic
    ADD CONSTRAINT tbl_psychotropic_primary_key PRIMARY KEY (code,yukostymd,yukoedymd);
