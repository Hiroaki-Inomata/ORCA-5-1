--                                    --
--  �����ƥ�����ơ��֥�              --
--  0042 ��ɼ�ֹ���������ɲ�         --
--  ��0041����ܹԡ�                  --
--                                    --
-- Create Date : 2004/07/29           --
--                                    --
insert into tbl_syskanri
(
 kanricd,
 kbncd,
 styukymd,
 edyukymd,
 kanritbl,
 termid,
 opid,
 creymd,
 upymd,
 uphms
) values
(
 '0042',
 '*',
 '00000000',
 '99999999',
 ' ',
 ' ',
 ' ',
 ' ',
 ' ',
 ' ')
;
update tbl_syskanri set kanritbl=(select substr(kanritbl,9,8) from tbl_syskanri where kanricd = '0041' and kbncd = '*') where kanricd = '0042' and kbncd = '*';

