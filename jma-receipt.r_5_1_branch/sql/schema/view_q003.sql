CREATE VIEW view_q003 AS
    SELECT tbl_sryact.hospnum,
           tbl_sryact.ptid,
           tbl_sryact.nyugaikbn,
           tbl_sryact.sryka,
           tbl_sryact.srykbn,
           tbl_sryact.srysyukbn,
           tbl_sryact.srycd1,
           tbl_sryact.srycd2,
           tbl_sryact.srycd3,
           tbl_sryact.srycd4,
           tbl_sryact.srycd5 
      FROM tbl_sryact 
     UNION ALL SELECT tbl_nsrysrh.hospnum,
           tbl_nsrysrh.ptid,
           '1' AS nyugaikbn,
           tbl_nsrysrh.sryka,
           tbl_nsrysrh.srykbn,
           '   '::bpchar AS srysyukbn,
           tbl_nsrysrh.srycd AS srycd1,
           tbl_nsrysrh.srycd AS srycd2,
           tbl_nsrysrh.srycd AS srycd3,
           tbl_nsrysrh.srycd AS srycd4,
           tbl_nsrysrh.srycd AS srycd5 
      FROM tbl_nsrysrh;


COMMENT ON VIEW view_q003 IS '患者照会ビュー3';

