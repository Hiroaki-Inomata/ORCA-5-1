--                                   --
-- 点数マスタの設定                  --
--                                   --
-- Create Date : 2005/05/10          --
--                                   --

-- 入院用システム予約コード作成      --
-- 099999917 療養担当手当（北海道限定）--

insert into tbl_tensu (srycd,yukostymd,yukoedymd,srykbn,srysyukbn,yukoketa,name,kanayukoketa,kananame,tensikibetu,ten,tanicd,datakbn,
chuksncd,chuksntsuban,kgnage,jgnage,ageksnkgn1,ageksnjgn1,ageksnsrycd1,ageksnkgn2,ageksnjgn2,ageksnsrycd2,ageksnkgn3,
ageksnjgn3,ageksnsrycd3,ageksnkgn4,ageksnjgn4,ageksnsrycd4,cdkbn_kbn,kokuchi_kbn,idokanren,yakkakjncd,syomeikanren,
chgymd,haisiymd,keikasochiymd,creymd,upymd,uphms)
values
('099999917','00000000','99999999','99','990',6,'療養担当手当',12,'リョウヨウタントウテアテ',3,10,'000',1,
'0000','0','00','00','00','00','         ','00','00','         ','00','00','         ','00','00','         ',' ',' ','         ',
'            ','         ','        ','        ','        ','20050401','        ','000000');
