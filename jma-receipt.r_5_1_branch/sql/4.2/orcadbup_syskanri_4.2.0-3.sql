--                                    --
-- システム管理データ更新             --
-- 医療区分表示情報期間変更           --
--                                    --
--                                    --
-- Create Date : 2008/03/17           --
--                                    --
\set ON_ERROR_STOP

update tbl_syskanri set edyukymd = '20080331'where kanricd = '5014' and styukymd = '00000000' ;
delete from tbl_syskanri where kanricd = '5014' and styukymd = '20080401' ;

insert into tbl_syskanri
select 
'5014',
'*',
'20080401',
'99999999',
'00000000',
'',
'',
'20080318',
'20080318',
'000000',
hospnum
from tbl_sysbase ;




