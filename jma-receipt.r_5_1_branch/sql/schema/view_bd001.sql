CREATE VIEW view_bd001 AS
    SELECT a.hospnum,
           a.ptid,
           b.ptnum,
           c.name,
           c.sex,
           c.birthday,
           a.sryka,
           a.denpprtymd,
           a.skymoney,
           a.nyukin_total 
      FROM tbl_syunou_main a,
           tbl_ptnum b,
           tbl_ptinf c 
     WHERE ((((((((a.hospnum = b.hospnum) 
       AND (a.ptid = b.ptid)) 
       AND (a.hospnum = c.hospnum)) 
       AND (a.ptid = c.ptid)) 
       AND (a.denpjtikbn <> '3'::bpchar)) 
       AND (a.denpjtikbn <> '7'::bpchar)) 
       AND (a.createkbn <> '3'::bpchar)) 
       AND (c.tstptnumkbn <> '1'::bpchar));


COMMENT ON VIEW view_bd001 IS '¼ýÇ¼¥Ó¥å¡¼';

