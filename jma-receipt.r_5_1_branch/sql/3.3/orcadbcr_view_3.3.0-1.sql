--                                    --
--  入院患者照会ビューの変更          --
--    最大枝番、転入日、転出日追加    --
--    抽出条件変更                    --
--                                    --

\set ON_ERROR_STOP

CREATE VIEW "view_i001" as SELECT a.hospid,
           a.ptid,
           b.rrknum,
           b.rrkedanum,
           e.rrkedanum_max,
           c.ptnum,
           a.kananame,
           a.name,
           a.sex,
           a.birthday,
           a.home_post,
           a.home_adrs,
           a.home_banti,
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
           (select hospid,ptid,rrknum,max(rrkedanum ) as rrkedanum_max from tbl_ptnyuinrrk group by hospid,ptid,rrknum
           ) e,
           tbl_ptinf a,
           tbl_ptnum c 
     WHERE b.hospid = a.hospid
       AND b.ptid = a.ptid 
       AND b.hospid = c.hospid 
       AND b.ptid = c.ptid 
       AND b.hospid = d.hospid 
       AND b.ptid = d.ptid 
       AND a.rrknum <> '0'::"numeric" 
       AND a.rrkedanum <> '0'::"numeric" 
       AND a.rrknum = d.rrknum 
       AND a.rrkedanum = d.rrkedanum 
       AND d.kensaku_dispkbn = '1'::bpchar 
       AND b.jtikbn <> '5'::bpchar 
       AND b.jtikbn <> '6'::bpchar 
       AND b.hospid = e.hospid 
       AND b.ptid = e.ptid 
       AND b.rrknum = e.rrknum;

COMMENT ON VIEW "view_i001" IS '入院患者照会ビュー';

