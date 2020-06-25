\set ON_ERROR_STOP

SET client_encoding = 'EUC_JP';
alter table tbl_mstkanri drop column if exists NAME ;
alter table tbl_mstkanri add column NAME character varying(64);

update tbl_mstkanri set NAME = '';
update tbl_mstkanri set NAME = '点数マスタ' WHERE KANRICD = 'ORCADB00';
update tbl_mstkanri set NAME = '保険番号マスタ' WHERE KANRICD = 'ORCADB01';
update tbl_mstkanri set NAME = 'システム管理マスタ' WHERE KANRICD = 'ORCADB02';
update tbl_mstkanri set NAME = '病名マスタ' WHERE KANRICD = 'ORCADB03';
update tbl_mstkanri set NAME = 'チェックマスタ' WHERE KANRICD = 'ORCADB04';
update tbl_mstkanri set NAME = '一般老人置換マスタ' WHERE KANRICD = 'ORCADB05';
update tbl_mstkanri set NAME = '検査分類マスタ' WHERE KANRICD = 'ORCADB06';
update tbl_mstkanri set NAME = '相互作用マスタ' WHERE KANRICD = 'ORCADB07';
update tbl_mstkanri set NAME = '症状措置マスタ' WHERE KANRICD = 'ORCADB08';
update tbl_mstkanri set NAME = '保険者マスタ' WHERE KANRICD = 'ORCADB09';
update tbl_mstkanri set NAME = '住所マスタ' WHERE KANRICD = 'ORCADB10';
update tbl_mstkanri set NAME = '適応病名マスタ' WHERE KANRICD = 'ORCADB11';
update tbl_mstkanri set NAME = '一般名マスタ' WHERE KANRICD = 'ORCADB12';
update tbl_mstkanri set NAME = '包括チェックマスタ' WHERE KANRICD = 'ORCADB13';
update tbl_mstkanri set NAME = '包括診療コードマスタ' WHERE KANRICD = 'ORCADB14';
update tbl_mstkanri set NAME = '統計メモマスタ' WHERE KANRICD = 'ORCADB15';
update tbl_mstkanri set NAME = 'オプションマスタ' WHERE KANRICD = 'ORCADB16';
update tbl_mstkanri set NAME = '感染症判定マスタ' WHERE KANRICD = 'ORCADB17';
update tbl_mstkanri set NAME = '入院基本料マスタ' WHERE KANRICD = 'ORCADB18';
update tbl_mstkanri set NAME = '入院料加算チェックマスタ' WHERE KANRICD = 'ORCADB19';
update tbl_mstkanri set NAME = '入院レセプト記載略称マスタ' WHERE KANRICD = 'ORCADB20';
update tbl_mstkanri set NAME = '同義語マスタ' WHERE KANRICD = 'ORCADB21';
update tbl_mstkanri set NAME = '電子点数表マスタ' WHERE KANRICD = 'ORCADB22';
update tbl_mstkanri set NAME = '労働基準監督署マスタ' WHERE KANRICD = 'ORCADB23';
update tbl_mstkanri set NAME = '最低薬価マスタ' WHERE KANRICD = 'ORCADB24';
update tbl_mstkanri set NAME = '介護保険者マスタ' WHERE KANRICD = 'ORCADB25';
update tbl_mstkanri set NAME = '選定療養費一般コード振替マスタ' WHERE KANRICD = 'ORCADB26';
update tbl_mstkanri set NAME = '入院料置換マスタ' WHERE KANRICD = 'ORCADB27';
