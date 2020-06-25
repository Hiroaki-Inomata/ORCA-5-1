--                                              --
-- ビュー再作成＆新規作成                       --
-- view_q003   照会３（新規）                   --
-- view_q004   照会４（新規）                   --
-- view_q001   照会１                           --
-- view_q002   照会２                           --
--                                              --
-- Create Date : 2008/06/30                     --
--                                              --
\set ON_ERROR_STOP

create  view view_q003 as
	select	hospnum,ptid,nyugaikbn,sryka,srycd1,srycd2,srycd3,srycd4,srycd5 from tbl_sryact
union all
	select	hospnum,ptid,'1' as nyugaikbn ,sryka,srycd as srycd1 ,'' as srycd2 , '' as srycd3, '' as srycd4,'' as srycd5     from tbl_nsrysrh
;

COMMENT 
        ON VIEW view_q003 IS '患者照会ビュー3';

create  view view_q004 as
	select	hospnum,ptid,nyugaikbn,sryka,hkncombinum,sryymd from tbl_jyurrk
union all
	select	a.hospnum,a.ptid,'1' as nyugaikbn ,b.sryka,b.hkncombinum,a.sryymd      from tbl_nrrksrh a,tbl_nsrysrh b where a.hospnum = b.hospnum and a.ptid = b.ptid and a.zainum = b.zainum
;

COMMENT 
        ON VIEW view_q003 IS '患者照会ビュー4';



CREATE VIEW view_q001 AS
    SELECT tbl_hkncombi.hospnum,
           tbl_hkncombi.ptid,
           tbl_hkncombi.hkncombinum,
           view_q004.nyugaikbn,
           view_q004.sryka,
           view_q004.sryymd,
           tbl_pthkninf.hknid,
           tbl_pthkninf.hknnum,
           tbl_pthkninf.hknjanum,
           tbl_pthkninf.honkzkkbn,
           tbl_pthkninf.kigo,
           tbl_pthkninf.num,
           tbl_ptkohinf.kohid,
           tbl_ptkohinf.kohnum,
           tbl_ptkohinf.paykbn,
           tbl_ptkohinf.ftnjanum 
      FROM (((tbl_hkncombi JOIN view_q004 
        ON ((((tbl_hkncombi.hospnum = view_q004.hospnum) 
       AND (tbl_hkncombi.ptid = view_q004.ptid)) 
       AND (tbl_hkncombi.hkncombinum = to_number((view_q004.hkncombinum)::text,
           '9999'::text))))) 
      LEFT JOIN tbl_pthkninf 
        ON (((((tbl_hkncombi.hospnum = tbl_pthkninf.hospnum) 
       AND (tbl_hkncombi.ptid = tbl_pthkninf.ptid)) 
       AND (tbl_hkncombi.hknid = tbl_pthkninf.hknid)) 
       AND (tbl_hkncombi.hknnum = tbl_pthkninf.hknnum)))) 
      LEFT JOIN tbl_ptkohinf 
        ON ((((tbl_hkncombi.hospnum = tbl_ptkohinf.hospnum) 
       AND (tbl_hkncombi.ptid = tbl_ptkohinf.ptid)) 
       AND (((((tbl_hkncombi.koh1id = tbl_ptkohinf.kohid) 
       AND (tbl_hkncombi.koh1hknnum = tbl_ptkohinf.kohnum)) 
        OR ((tbl_hkncombi.koh2id = tbl_ptkohinf.kohid) 
       AND (tbl_hkncombi.koh2hknnum = tbl_ptkohinf.kohnum))) 
        OR ((tbl_hkncombi.koh3id = tbl_ptkohinf.kohid) 
       AND (tbl_hkncombi.koh3hknnum = tbl_ptkohinf.kohnum))) 
        OR ((tbl_hkncombi.koh4id = tbl_ptkohinf.kohid) 
       AND (tbl_hkncombi.koh4hknnum = tbl_ptkohinf.kohnum)))))) UNION SELECT tbl_hkncombi.hospnum,
           tbl_hkncombi.ptid,
           tbl_hkncombi.hkncombinum,
           view_q004.nyugaikbn,
           view_q004.sryka,
           view_q004.sryymd,
           tbl_pthkninf.hknid,
           tbl_pthkninf.hknnum,
           tbl_pthkninf.hknjanum,
           tbl_pthkninf.honkzkkbn,
           tbl_pthkninf.kigo,
           tbl_pthkninf.num,
           tbl_ptkohinf.kohid,
           tbl_ptkohinf.kohnum,
           tbl_ptkohinf.paykbn,
           tbl_ptkohinf.ftnjanum 
      FROM ((view_q004 JOIN tbl_hkncombi 
        ON ((((view_q004.hospnum = tbl_hkncombi.hospnum) 
       AND (view_q004.ptid = tbl_hkncombi.ptid)) 
       AND (to_number((view_q004.hkncombinum)::text,
           '9999'::text) = tbl_hkncombi.hkncombinum)))) 
      LEFT JOIN tbl_pthkninf 
        ON (((((tbl_hkncombi.hospnum = tbl_pthkninf.hospnum) 
       AND (tbl_hkncombi.ptid = tbl_pthkninf.ptid)) 
       AND (tbl_hkncombi.hknid = tbl_pthkninf.hknid)) 
       AND (tbl_hkncombi.hknnum = tbl_pthkninf.hknnum)))),
           tbl_ptkohinf 
     WHERE (((((view_q004.hospnum = tbl_ptkohinf.hospnum) 
       AND (view_q004.ptid = tbl_ptkohinf.ptid)) 
       AND (((((((((((tbl_ptkohinf.kohnum = '960'::bpchar) 
        OR (tbl_ptkohinf.kohnum = '961'::bpchar)) 
        OR (tbl_ptkohinf.kohnum = '962'::bpchar)) 
        OR (tbl_ptkohinf.kohnum = '963'::bpchar)) 
        OR (tbl_ptkohinf.kohnum = '964'::bpchar)) 
        OR (tbl_ptkohinf.kohnum = '965'::bpchar)) 
        OR (tbl_ptkohinf.kohnum = '966'::bpchar)) 
        OR (tbl_ptkohinf.kohnum = '967'::bpchar)) 
        OR (tbl_ptkohinf.kohnum = '968'::bpchar)) 
        OR (tbl_ptkohinf.kohnum = '978'::bpchar)) 
        OR (tbl_ptkohinf.kohnum = '979'::bpchar))) 
       AND (view_q004.sryymd >= tbl_ptkohinf.tekstymd)) 
       AND (view_q004.sryymd <= tbl_ptkohinf.tekedymd));

COMMENT 
        ON VIEW view_q001 IS '患者照会ビュー1';

CREATE VIEW view_q002 AS
    SELECT a.hospnum,
           a.ptid,
           a.nyugaikbn,
           a.sryka,
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
        OR (a.zainum = b.zainum15)))
union all
	select	a.hospnum,a.ptid,'1' as nyugaikbn ,b.sryka,a.sryymd,b.srycd as srycd1 ,'' as srycd2 , '' as srycd3, '' as srycd4,'' as srycd5     from tbl_nrrksrh a , tbl_nsrysrh b where a.hospnum = b.hospnum and a.ptid = b.ptid and a.zainum = b.zainum 
;
COMMENT 
        ON VIEW view_q002 IS '患者照会ビュー2';

