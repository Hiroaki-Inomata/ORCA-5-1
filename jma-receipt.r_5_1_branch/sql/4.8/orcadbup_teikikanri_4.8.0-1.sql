---------------------
-- �������ط��ơ��֥����°���ѹ�
-- ����������
---------------------

\set ON_ERROR_STOP

-- tbl_teikikanri --
ALTER TABLE tbl_teikikanri alter column page    type integer ;
ALTER TABLE tbl_teikikanri alter column kensu   type integer ;
ALTER TABLE tbl_teikikanri alter column hospnum type smallint;

