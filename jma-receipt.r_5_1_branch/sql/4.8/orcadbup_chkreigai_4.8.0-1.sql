--                                      
-- チェック例外
--                                    
-- エラー番号削除
-- 確認番号をcharに変更
-- チェック区分追加
-- エラーコード追加
-- エラーメッセージ追加
-- 病名追加
-- 条件区分追加
-- 適用コード追加
-- 適用病名追加
-- 適用病名条件区分追加
                                  
-- Create Date : 2011/8/29

\set ON_ERROR_STOP
alter table tbl_chkreigai drop   COLUMN errkbn;

alter table tbl_chkreigai add    COLUMN chknum2 char(2);
update tbl_chkreigai set chknum2 = trim(to_char(chknum,'00'));
alter table tbl_chkreigai drop   COLUMN chknum;
alter table tbl_chkreigai rename COLUMN chknum2 to chknum;

alter table tbl_chkreigai add COLUMN chkkbn char(1);
alter table tbl_chkreigai add COLUMN errcd  char(6);
alter table tbl_chkreigai add COLUMN errmsg character varying(200);
alter table tbl_chkreigai add COLUMN byomei character varying(160);
alter table tbl_chkreigai add COLUMN compkbn char(1);
alter table tbl_chkreigai add COLUMN tekcd   char(9);
alter table tbl_chkreigai add COLUMN tekbyomei varchar(160);
alter table tbl_chkreigai add COLUMN tekcompkbn char(1);
update tbl_chkreigai set chkkbn = '', errcd = '',errmsg = '',byomei='',compkbn='',tekcd='',tekbyomei='',tekcompkbn=''; 
update tbl_chkreigai set chkkbn = '1',errcd = '080001' where chknum = '08'; 
update tbl_chkreigai set chkkbn = '2',errcd = '090001' where chknum = '09'; 

-- 前回データチェックエラー情報初期化
delete  from tbl_chk004 where kanricd in ('1200','1201') ;

