--                                              --
-- ビュー再作成                                 --
-- view_q002   照会２                           --
--                                              --
-- Create Date : 2009/06/26                     --
--                                              --
\set ON_ERROR_STOP


CREATE VIEW view_q002 AS
    SELECT a.hospnum,
           a.ptid,
           a.nyugaikbn,
           a.sryka,
           a.srykbn,
           a.srysyukbn,
           b.sryymd,
           a.srycd1,
           a.srycd2,
           a.srycd3,
           a.srycd4,
           a.srycd5 
      FROM tbl_sryact a,
           tbl_jyurrk b 
     WHERE (((((a.hospnum = b.hospnum) 
       AND (a.ptid = b.ptid)) 
       AND (a.nyugaikbn = b.nyugaikbn)) 
       AND (a.sryka = b.sryka)) 
       AND (((((((((((((((a.zainum = b.zainum1) 
        OR (a.zainum = b.zainum2)) 
        OR (a.zainum = b.zainum3)) 
        OR (a.zainum = b.zainum4)) 
        OR (a.zainum = b.zainum5)) 
        OR (a.zainum = b.zainum6)) 
        OR (a.zainum = b.zainum7)) 
        OR (a.zainum = b.zainum8)) 
        OR (a.zainum = b.zainum9)) 
        OR (a.zainum = b.zainum10)) 
        OR (a.zainum = b.zainum11)) 
        OR (a.zainum = b.zainum12)) 
        OR (a.zainum = b.zainum13)) 
        OR (a.zainum = b.zainum14)) 
        OR (a.zainum = b.zainum15))) UNION ALL SELECT a.hospnum,
           a.ptid,
           '1' AS nyugaikbn,
           b.sryka,
           b.srykbn,
           cast('   ' as char(3)) as srysyukbn,
           a.sryymd,
           b.srycd AS srycd1,
           b.srycd AS srycd2,
           b.srycd AS srycd3,
           b.srycd AS srycd4,
           b.srycd AS srycd5 
      FROM tbl_nrrksrh a,
           tbl_nsrysrh b 
     WHERE (((a.hospnum = b.hospnum) 
       AND (a.ptid = b.ptid)) 
       AND (a.zainum = b.zainum));

COMMENT 
        ON VIEW view_q002 IS '患者照会ビュー2';
