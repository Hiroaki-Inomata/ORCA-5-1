\set ON_ERROR_STOP
--
-- 患者入院履歴テーブル構造変更
--
-- 入院履歴最大枝番号(MAXEDANUM)追加
-- 入院食事区分(SKJKBN)追加
-- 選定入院区分(SENTEIKBN)追加
-- 
-- Create Date : 2006/12/19           --
--
--  項目追加                        --
alter table TBL_PTNYUINRRK
   add column MAXEDANUM  numeric(03);
alter table TBL_PTNYUINRRK
   add column SKJKBN     character(1);
alter table TBL_PTNYUINRRK
   add column SENTEIKBN  character(1);


--  ゼロ設定                       --
alter table TBL_SYUNOU
   alter GRP_HAKHOUFLG set default 0;


--  最大枝番号セット
update tbl_ptnyuinrrk set maxedanum = (select max(b.rrkedanum) from tbl_ptnyuinrrk b where tbl_ptnyuinrrk.hospid = b.hospid and tbl_ptnyuinrrk.ptid = b.ptid and tbl_ptnyuinrrk.rrknum = b.rrknum group by b.hospid,b.ptid,b.rrknum);

--  食事区分セット
update tbl_ptnyuinrrk set skjkbn = '1' where (jtikbn <> '5') and (jtikbn <> '6');

--  選定入院区分セット
update tbl_ptnyuinrrk set senteikbn = '1' where (jtikbn <> '5') and (jtikbn <> '6');
