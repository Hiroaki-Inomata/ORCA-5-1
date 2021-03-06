--
-- tbl_srysrh
-- zaikaisu,suryototal,srysyuryo1-5 �ɲ�
--

\set ON_ERROR_STOP

drop table tbl_srysrh ; 

CREATE TABLE tbl_srysrh (
    hospnum smallint,
    termid character(64),
    ptid bigint,
    zainum integer,
    nyugaikbn character(1),
    sryka character(2),
    hkncombinum smallint,
    srykbn character(2),
    srysyukbn character(3),
    sryymd character(8),
    zaikaisu  integer ,
    suryototal numeric(12,5),  
    rennum smallint ,
    srycd1 character(9),
    srysuryo1 numeric(10,5),
    srycd2 character(9),
    srysuryo2 numeric(10,5),
    srycd3 character(9),
    srysuryo3 numeric(10,5),
    srycd4 character(9),
    srysuryo4 numeric(10,5),
    srycd5 character(9),
    srysuryo5 numeric(10,5),
    acctkbn character(1),
    creymd character(8),
    upymd character(8),
    uphms character(6)
) with oids;



