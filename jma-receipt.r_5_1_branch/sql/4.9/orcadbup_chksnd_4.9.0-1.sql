-- \set ON_ERROR_STOP

SET client_encoding = 'EUC_JP';
alter table tbl_chksnd add  tekbyokbn character varying(1);
alter table tbl_chksnd alter COLUMN rennum type integer;
delete from tbl_chksnd a where cdkbn = '0' and exists (select * from tbl_chksnd where hospnum = a.hospnum and chkkbn = a.chkkbn and srycd = a.srycd and yukostymd = a.yukostymd and yukoedymd  = a.yukoedymd and cdkbn = '1' );
update tbl_chksnd a set cdkbn = '0' where chkkbn in ('1','2') and cdkbn = '1' and nyugaikbn = '0' and not exists (select * from tbl_chksnd where hospnum = a.hospnum and chkkbn = a.chkkbn and srycd = a.srycd and yukostymd = a.yukostymd and yukoedymd  = a.yukoedymd and trim(byomei) <> '');
insert into tbl_chksnd (
chkkbn         ,
srycd          ,
yukostymd      ,
yukoedymd      ,
cdkbn          ,
rennum         ,
cd             ,
byomei         ,
compkbn        ,
byomeihenflg   ,
nyugaikbn      ,
termid         ,
opid           ,
creymd         ,
upymd          ,
uphms          ,
hospnum        ,
utagaikbn      ,
grpchkkbn      ,
grpid          ,
hosoku         ,
hosoku_compkbn ,
tekbyokbn      )
select
chkkbn         ,
srycd          ,
yukostymd      ,
yukoedymd      ,
'0'            ,
1              ,
''             ,
''             ,
''             ,
''             ,
nyugaikbn      ,
''             ,
''             ,
''             ,
''             ,
''             ,
hospnum        ,
''             ,
grpchkkbn      ,
''             ,
''             ,
''             ,
tekbyokbn
from tbl_chksnd
where cdkbn = '1'
group by 
chkkbn         ,
srycd          ,
yukostymd      ,
yukoedymd      ,
nyugaikbn      ,
hospnum        ,
grpchkkbn      ,
tekbyokbn
;

update tbl_chksnd set tekbyokbn = '' where chkkbn in ('1','2')  ;

update tbl_chksnd a set rennum = b.rennum
from (select hospnum, chkkbn, srycd, yukostymd, yukoedymd, cdkbn,count(*) as rennum from tbl_chksnd where cdkbn = '1' group by hospnum, chkkbn, srycd, yukostymd, yukoedymd, cdkbn ) b
where a.hospnum = b.hospnum and a.chkkbn = b.chkkbn and a.srycd = b.srycd and a.yukostymd = b.yukostymd and a.yukoedymd = b.yukoedymd and a.cdkbn = '0'
 ;

