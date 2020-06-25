\set ON_ERROR_STOP

--                                --
-- ORCA DataBase Table Definition --
--                                --
-- ÌôºÞ¾ðÊó                       --
--                                --
-- Table Name  : TBL_YAKUJYO      --
-- Update      : 2007/12/10       --
-- EFFECT  varchar(400) -> (2000) --
-- CAUTION varchar(850) -> (4000) --

alter table tbl_yakujyo drop constraint tbl_yakujyo_primary_key;
alter table tbl_yakujyo rename to tbl_yakujyowk;

CREATE TABLE tbl_yakujyo (
    srycd character(9) NOT NULL,
    teknum numeric(2,0) DEFAULT 0 NOT NULL,
    name character varying(170),
    effect character varying(2000),
    caution character varying(4000),
    photo character varying(256),
    color character varying(360),
    shape character varying(360),
    mark character varying(360),
    termid character varying(32),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6),
    hospnum numeric(2,0) NOT NULL
);

insert into tbl_yakujyo  select * from tbl_yakujyowk;

ALTER TABLE ONLY tbl_yakujyo
    ADD CONSTRAINT tbl_yakujyo_primary_key PRIMARY KEY (hospnum, srycd, teknum);

drop table tbl_yakujyowk ;
