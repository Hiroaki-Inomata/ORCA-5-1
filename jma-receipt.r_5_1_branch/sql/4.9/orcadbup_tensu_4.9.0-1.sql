--                                      
-- 点数テーブル
--                                    
-- 非侵襲的血行動態モニタリング加算追加
-- 凍結保存同種組織加算追加
-- 点数表区分番号追加
-- 労災算定不可追加
-- 四肢加算（労災）追加
--                                   
-- Create Date : 2016/03/01      

\set ON_ERROR_STOP

alter table tbl_tensu add column hisinsyumonitorksn smallint default 0;
alter table tbl_tensu add column touketuhozonksn smallint default 0;
alter table tbl_tensu add column kubunbangou character varying(30);
alter table tbl_tensu add column rosaikbn smallint default 0;
alter table tbl_tensu add column sisiksn smallint default 0;
update tbl_tensu set kubunbangou = '';
