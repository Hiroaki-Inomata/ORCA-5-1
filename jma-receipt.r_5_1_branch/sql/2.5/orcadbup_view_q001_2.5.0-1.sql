--                           --  
--   ビュー再作成            --
--                           --
--   作成 2004/10/27         --

drop view view_Q001;

create view  view_Q001 as (
select tbl_hkncombi.hospid,
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
from   tbl_hkncombi
join   tbl_jyurrk on 
       (tbl_hkncombi.hospid=tbl_jyurrk.hospid
and    tbl_hkncombi.ptid  =tbl_jyurrk.ptid
and    tbl_hkncombi.hkncombinum=to_number(tbl_jyurrk.hkncombinum,'9999')
)
left join tbl_pthkninf on (
       tbl_hkncombi.hospid=tbl_pthkninf.hospid
and    tbl_hkncombi.ptid  =tbl_pthkninf.ptid
and    tbl_hkncombi.hknid =tbl_pthkninf.hknid
and    tbl_hkncombi.hknnum=tbl_pthkninf.hknnum )
left join tbl_ptkohinf on (
     (     tbl_hkncombi.hospid    =tbl_ptkohinf.hospid
and        tbl_hkncombi.ptid      =tbl_ptkohinf.ptid)
and  ((   (tbl_hkncombi.koh1id    =tbl_ptkohinf.kohid)
      and (tbl_hkncombi.koh1hknnum=tbl_ptkohinf.kohnum))
    or  ( (tbl_hkncombi.koh2id    =tbl_ptkohinf.kohid)
      and (tbl_hkncombi.koh2hknnum=tbl_ptkohinf.kohnum))
    or  ( (tbl_hkncombi.koh3id    =tbl_ptkohinf.kohid)
      and (tbl_hkncombi.koh3hknnum=tbl_ptkohinf.kohnum))
    or  ( (tbl_hkncombi.koh4id    =tbl_ptkohinf.kohid)
      and (tbl_hkncombi.koh4hknnum=tbl_ptkohinf.kohnum)))
)
);

