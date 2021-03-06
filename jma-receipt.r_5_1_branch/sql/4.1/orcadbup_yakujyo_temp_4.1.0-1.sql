\set ON_ERROR_STOP

--                                --
-- ORCA DataBase Table Definition --
--                                --
-- 薬剤情報テンポラリ             --
--                                --
-- Table Name  : TBL_YAKUJYO_TEMP --
-- Update      : 2007/12/10       --
-- EFFECT  varchar(400) -> (2000) --
-- CAUTION varchar(850) -> (4000) --

drop table tbl_yakujyo_temp;

create table tbl_yakujyo_temp (
    termid character(64) NOT NULL,
    srycd character(9) NOT NULL,
    yjcd character(12) NOT NULL,
    rennum numeric(2,0) DEFAULT 0 NOT NULL,
    shape character varying(360),
    color character varying(360),
    mark character varying(360),
    effect character varying(2000),
    caution character varying(4000),
    make_company character varying(400),
    sale_company character varying(400),
    photo1_filename character varying(256),
    photo2_filename character varying(256),
    photo3_filename character varying(256),
    creymd character(8),
    hospnum numeric(2,0) NOT NULL
);

ALTER TABLE ONLY tbl_yakujyo_temp
    ADD CONSTRAINT tbl_yakujyo_temp_primary_key PRIMARY KEY (hospnum, termid, srycd, yjcd, rennum);
