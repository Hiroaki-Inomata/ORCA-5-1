--
-- tbl_ptsrh
-- hknnum,kohnum,totalten �ɲ�
--

\set ON_ERROR_STOP

drop table tbl_ptsrh ; 

CREATE TABLE tbl_ptsrh (
    hospnum		smallint,
    termid		character(64),
    ptid		bigint,
    ptnum		character varying(20),
    kananame		character varying(100),
    sex			character(1),
    birthday		character(8),
    jkysnum		character varying(20),
    nyugaikbn		character(1),
    hknnum		character(3),
    kohnum1		character(3),
    kohnum2		character(3),
    totalten		bigint,
    srycd		character(9),
    sryname		character varying(100),
    srykananame		character varying(100),
    sryseq		smallint,
    memo		character varying(200),
    datakbn		character(1),
    creymd		character(8),
    upymd		character(8),
    uphms		character(6)
);


