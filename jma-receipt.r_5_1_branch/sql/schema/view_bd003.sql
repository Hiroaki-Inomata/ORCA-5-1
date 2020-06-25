CREATE VIEW view_bd003 AS
    SELECT a.hospnum,
           a.nyugaikbn,
           a.ptid,
           a.denpnum,
           a.denpedanum,
           a.skymoney,
           a.nyuhen_money,
           a.nyuhen_ymd,
           b.sryymd 
      FROM (tbl_syumei a JOIN tbl_syunou_main b 
     USING (hospnum,
           nyugaikbn,
           ptid,
           denpnum));


COMMENT ON VIEW view_bd003 IS '¼ýÇ¼¥Ó¥å¡¼3';

