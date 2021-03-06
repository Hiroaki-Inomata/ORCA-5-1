\set ON_ERROR_STOP

CREATE VIEW view_q001 AS
    SELECT tbl_hkncombi.hospnum,
           tbl_hkncombi.ptid,
           tbl_hkncombi.hkncombinum,
           tbl_jyurrk.nyugaikbn,
           tbl_jyurrk.sryka,
           tbl_jyurrk.sryymd,
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
      FROM (((tbl_hkncombi JOIN tbl_jyurrk 
        ON ((((tbl_hkncombi.hospnum = tbl_jyurrk.hospnum) 
       AND (tbl_hkncombi.ptid = tbl_jyurrk.ptid)) 
       AND (tbl_hkncombi.hkncombinum = to_number((tbl_jyurrk.hkncombinum)::text,
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
       AND (tbl_hkncombi.koh4hknnum = tbl_ptkohinf.kohnum))))))
    union
    SELECT tbl_hkncombi.hospnum,
           tbl_hkncombi.ptid,
           tbl_hkncombi.hkncombinum,
           tbl_jyurrk.nyugaikbn,
           tbl_jyurrk.sryka,
           tbl_jyurrk.sryymd,
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
      FROM tbl_jyurrk  JOIN  tbl_hkncombi
        ON tbl_jyurrk.hospnum     = tbl_hkncombi.hospnum
       AND tbl_jyurrk.ptid        = tbl_hkncombi.ptid
       AND to_number((tbl_jyurrk.hkncombinum)::text,'9999'::text)
                                  = tbl_hkncombi.hkncombinum
      LEFT JOIN tbl_pthkninf 
        ON (tbl_hkncombi.hospnum = tbl_pthkninf.hospnum) 
       AND (tbl_hkncombi.ptid = tbl_pthkninf.ptid) 
       AND (tbl_hkncombi.hknid = tbl_pthkninf.hknid) 
       AND (tbl_hkncombi.hknnum = tbl_pthkninf.hknnum)
      , tbl_ptkohinf
     WHERE tbl_jyurrk.hospnum = tbl_ptkohinf.hospnum
      AND  tbl_jyurrk.ptid    = tbl_ptkohinf.ptid
      AND  tbl_ptkohinf.kohnum in ('960','961','962','963','964','965','966','967','978','979')
      AND  tbl_jyurrk.sryymd >= tbl_ptkohinf.tekstymd
      AND  tbl_jyurrk.sryymd <= tbl_ptkohinf.tekedymd;

