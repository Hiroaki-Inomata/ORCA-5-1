--                                      
-- ÀÁµá´ÉÍı¾È²ñ
--                                   
-- ¹àÌÜÄÉ²Ã
--   ¥ì¥»¥×¥È¿ÇÎÅ²Ê
--   ¿³ºº»ÙÊ§µ¡´Ø
--   ¥ì¥»¥×¥È¼ïÊÌ
--   Êİ¸±¼ÔÈÖ¹æ
--   Êä½õ¶èÊ¬
--   ¸øÈñ£É£Ä¡Ê¸øÈñÃ±ÆÈ¤Î¤ß¡Ë 
--   Å¬ÍÑ³«»ÏÇ¯·îÆü
--   Ê¬³ä¶èÊ¬  
--   ¸øÈñ£É£Ä£±¡Á£´
--   ¸øÈñÈÖ¹æ£³¡Á£´
--   ºîÀ®ÆüÉÕ
--   ¹¹¿·ÆüÉÕ
--   ¹¹¿·»ş´Ö
--   ¥Ç¡¼¥¿¶èÊ¬
--   Ï¢ÈÖ
--                                   
-- Create Date : 2016/11/22

\set ON_ERROR_STOP

drop table if exists tbl_skysrh;
CREATE TABLE tbl_skysrh (
    hospnum smallint,
    termid character(64),
    ptid bigint,
    nyugaikbn character(1),
    sryym character(6),
    receka character(2),
    teisyutusaki smallint,
    recesyubetu character(4),
    hknjanum character(8),
    hojokbn_key character(1),
    kohid_key bigint,
    tekstymd character(8),
    receipt_kbn character(10),
    hknid bigint,
    hknnum character(3),
    kohid1 bigint,
    kohnum1 character(3),
    kohid2 bigint,
    kohnum2 character(3),
    kohid3 bigint,
    kohnum3 character(3),
    kohid4 bigint,
    kohnum4 character(3),
    totalten bigint,
    tokki1 character varying(2),
    tokki2 character varying(2),
    tokki3 character varying(2),
    tokki4 character varying(2),
    tokki5 character varying(2),
    tokkinm1 character varying(12),
    tokkinm2 character varying(12),
    tokkinm3 character varying(12),
    tokkinm4 character varying(12),
    tokkinm5 character varying(12),
    etc_ykzftn1 integer,
    skykbn_receden character(1),
    etccreateflg smallint,
    datakbn character(1),
    seqnum integer,
    creymd character(8),
    upymd character(8),
    uphms character(6)
) with oids;

create index idx_skysrh_ptid on tbl_skysrh(hospnum,termid,ptid,seqnum) ;

