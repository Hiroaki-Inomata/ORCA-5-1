--                                              --
-- ビュー再作成                                 --
-- view_q001   照会１                           --
--                                              --
-- Create Date : 2009/01/15                     --
--                                              --
\set ON_ERROR_STOP

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
       AND (tbl_hkncombi.hkncombinum = to_number((view_q004.hkncombinum),
           '9999'))))) 
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
       AND (to_number((view_q004.hkncombinum),
           '9999') = tbl_hkncombi.hkncombinum)))) 
      LEFT JOIN tbl_pthkninf 
        ON (((((tbl_hkncombi.hospnum = tbl_pthkninf.hospnum) 
       AND (tbl_hkncombi.ptid = tbl_pthkninf.ptid)) 
       AND (tbl_hkncombi.hknid = tbl_pthkninf.hknid)) 
       AND (tbl_hkncombi.hknnum = tbl_pthkninf.hknnum)))),
           tbl_ptkohinf 
     WHERE view_q004.hospnum = tbl_ptkohinf.hospnum
       AND view_q004.ptid = tbl_ptkohinf.ptid
       AND tbl_ptkohinf.kohnum in ( '956', '957', '958', '960','961','962','963','964','965','966','967','968','969','977','978','979')
       AND view_q004.sryymd >= tbl_ptkohinf.tekstymd 
       AND view_q004.sryymd <= tbl_ptkohinf.tekedymd;

COMMENT 
        ON VIEW view_q001 IS '患者照会ビュー1';

