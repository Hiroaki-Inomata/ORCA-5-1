----------------------
-- 入院患者照会ビューSQL見直し
-- 2013/5/9
----------------------

DROP VIEW  view_i001 ;

CREATE VIEW view_i001 AS
    SELECT a.hospnum,
           a.ptid,
           b.rrknum,
           b.rrkedanum,
           b.maxedanum as rrkedanum_max,
           c.ptnum,
           a.kananame,
           a.name,
           a.sex,
           a.birthday,
           a.home_post,
           a.home_adrs,
           a.home_banti,
           a.deathkbn,
           b.btunum,
           b.brmnum,
           b.nyuinka,
           b.nyuinymd,
           b.taiinymd,
           b.taiincd,
           b.tennyuymd,
           b.tenstuymd,
           a.tstptnumkbn 
      FROM tbl_ptnyuinrrk b,
           tbl_ptnyuinrrk d,
           tbl_ptinf a,
           tbl_ptnum c 
     WHERE b.hospnum = a.hospnum
       AND b.ptid = a.ptid
       AND b.hospnum = c.hospnum
       AND b.ptid = c.ptid
       AND b.hospnum = d.hospnum
       AND b.ptid = d.ptid
       AND a.rrknum <> 0
       AND a.rrkedanum <> 0
       AND a.rrknum = d.rrknum
       AND a.rrkedanum = d.rrkedanum
       AND d.kensaku_dispkbn = '1'::bpchar
       AND b.jtikbn <> '5'::bpchar
       AND b.jtikbn <> '6'::bpchar
;
