--                                    --
--  収納テーブル変更に伴う処理        --
--                                    --

\set ON_ERROR_STOP

DROP   VIEW "view_bd002" ;

CREATE VIEW "view_bd002" as SELECT a.*,
           b.ptnum 
      FROM tbl_syunou a,
           tbl_ptnum b,
           tbl_ptinf c 
     WHERE (((((((b.hospid = a.hospid) 
       AND (b.ptid = a.ptid)) 
       AND (b.hospid = c.hospid)) 
       AND (b.ptid = c.ptid)) 
       AND (a.denpjtikbn <> '3'::bpchar)) 
       AND (a.createkbn <> '3'::bpchar)) 
       AND (c.tstptnumkbn <> '1'::bpchar));

COMMENT ON VIEW "view_bd002" IS '収納ビュー2';

