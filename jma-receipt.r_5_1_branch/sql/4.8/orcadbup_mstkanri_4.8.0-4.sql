\set ON_ERROR_STOP

SET client_encoding = 'EUC_JP';
alter table tbl_mstkanri drop column if exists NAME ;
alter table tbl_mstkanri add column NAME character varying(64);

update tbl_mstkanri set NAME = '';
update tbl_mstkanri set NAME = '�����ޥ���' WHERE KANRICD = 'ORCADB00';
update tbl_mstkanri set NAME = '�ݸ��ֹ�ޥ���' WHERE KANRICD = 'ORCADB01';
update tbl_mstkanri set NAME = '�����ƥ�����ޥ���' WHERE KANRICD = 'ORCADB02';
update tbl_mstkanri set NAME = '��̾�ޥ���' WHERE KANRICD = 'ORCADB03';
update tbl_mstkanri set NAME = '�����å��ޥ���' WHERE KANRICD = 'ORCADB04';
update tbl_mstkanri set NAME = '����Ϸ���ִ��ޥ���' WHERE KANRICD = 'ORCADB05';
update tbl_mstkanri set NAME = '����ʬ��ޥ���' WHERE KANRICD = 'ORCADB06';
update tbl_mstkanri set NAME = '��ߺ��ѥޥ���' WHERE KANRICD = 'ORCADB07';
update tbl_mstkanri set NAME = '�ɾ����֥ޥ���' WHERE KANRICD = 'ORCADB08';
update tbl_mstkanri set NAME = '�ݸ��ԥޥ���' WHERE KANRICD = 'ORCADB09';
update tbl_mstkanri set NAME = '����ޥ���' WHERE KANRICD = 'ORCADB10';
update tbl_mstkanri set NAME = 'Ŭ����̾�ޥ���' WHERE KANRICD = 'ORCADB11';
update tbl_mstkanri set NAME = '����̾�ޥ���' WHERE KANRICD = 'ORCADB12';
update tbl_mstkanri set NAME = '�������å��ޥ���' WHERE KANRICD = 'ORCADB13';
update tbl_mstkanri set NAME = '�����ť����ɥޥ���' WHERE KANRICD = 'ORCADB14';
update tbl_mstkanri set NAME = '���ץ��ޥ���' WHERE KANRICD = 'ORCADB15';
update tbl_mstkanri set NAME = '���ץ����ޥ���' WHERE KANRICD = 'ORCADB16';
update tbl_mstkanri set NAME = '������Ƚ��ޥ���' WHERE KANRICD = 'ORCADB17';
update tbl_mstkanri set NAME = '�����������ޥ���' WHERE KANRICD = 'ORCADB18';
update tbl_mstkanri set NAME = '�������û������å��ޥ���' WHERE KANRICD = 'ORCADB19';
update tbl_mstkanri set NAME = '�����쥻�ץȵ���ά�Υޥ���' WHERE KANRICD = 'ORCADB20';
update tbl_mstkanri set NAME = 'Ʊ����ޥ���' WHERE KANRICD = 'ORCADB21';
update tbl_mstkanri set NAME = '�Ż�����ɽ�ޥ���' WHERE KANRICD = 'ORCADB22';
update tbl_mstkanri set NAME = 'ϫƯ�����Ľ�ޥ���' WHERE KANRICD = 'ORCADB23';
update tbl_mstkanri set NAME = '���������ޥ���' WHERE KANRICD = 'ORCADB24';
update tbl_mstkanri set NAME = '����ݸ��ԥޥ���' WHERE KANRICD = 'ORCADB25';
update tbl_mstkanri set NAME = '������������̥����ɿ��إޥ���' WHERE KANRICD = 'ORCADB26';
update tbl_mstkanri set NAME = '�������ִ��ޥ���' WHERE KANRICD = 'ORCADB27';
