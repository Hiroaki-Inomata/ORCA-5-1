CREATE VIEW view_q004 AS
    SELECT tbl_jyurrk.hospnum,
           tbl_jyurrk.ptid,
           tbl_jyurrk.nyugaikbn,
           tbl_jyurrk.sryka,
           tbl_jyurrk.hkncombinum,
           tbl_jyurrk.sryymd 
      FROM tbl_jyurrk 
     UNION ALL SELECT a.hospnum,
           a.ptid,
           '1' AS nyugaikbn,
           b.sryka,
           b.hkncombinum,
           a.sryymd 
      FROM tbl_nrrksrh a,
           tbl_nsrysrh b 
     WHERE (((a.hospnum = b.hospnum) 
       AND (a.ptid = b.ptid)) 
       AND (a.zainum = b.zainum));


COMMENT ON VIEW view_q004 IS '患者照会ビュー4';

