--                                    --
-- 最低薬価マスタテーブルの作成       --
--                                    --
-- Create Date : 2012/02/29           --
--                                    --

CREATE TABLE tbl_generic_price (
    yakkakjncd character varying(9) NOT NULL,
    yukostymd  character varying(8) NOT NULL,
    yukoedymd  character varying(8) NOT NULL,
    price      numeric(11,2) DEFAULT 0,
    creymd     character varying(8),
    upymd      character varying(8)
);

ALTER TABLE ONLY tbl_generic_price
    ADD CONSTRAINT tbl_generic_price_primary_key PRIMARY KEY (yakkakjncd,yukostymd,yukoedymd);
