--                            --  
--   患者照会ビュー           --
--                            --
--   2005/02/28
--   １患者の入院履歴         --
--   を全て抽出するように修正 --

drop view view_i001;
create view view_i001 as SELECT a.hospid, a.ptid, b.rrknum, b.rrkedanum, c.ptnum, a.kananame, a.name, a.sex, a.birthday, a.home_post, a.home_adrs, a.home_banti, b.btunum, b.brmnum, b.nyuinka, b.nyuinymd, b.taiinymd, a.tstptnumkbn FROM tbl_ptnyuinrrk b,tbl_ptnyuinrrk d,tbl_ptinf a, tbl_ptnum c
WHERE
      b.hospid = a.hospid
 AND  b.ptid   = a.ptid
 AND  b.hospid = c.hospid
 AND  b.ptid   = c.ptid
 AND  b.hospid = d.hospid
 AND  b.ptid   = d.ptid
 AND  a.rrknum  <> '0'::"numeric"
 AND  a.rrkedanum <> '0'::"numeric"
 AND  a.rrknum = d.rrknum
 AND  a.rrkedanum = d.rrkedanum
 AND  d.kensaku_dispkbn = '1'::bpchar
 AND  b.rrkedanum = (
    select max(rrkedanum) from tbl_ptnyuinrrk
    where b.hospid   = hospid
    AND  b.ptid      = ptid
    AND  b.rrknum    = rrknum
    group by hospid,ptid,rrknum
)
 AND  b.jtikbn not in ('5','6');
;


