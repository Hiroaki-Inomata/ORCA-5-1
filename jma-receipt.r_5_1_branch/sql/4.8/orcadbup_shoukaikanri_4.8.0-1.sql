---------------------
-- ���ԾȲ񸡺��������ơ��֥�����ɲ�
-- ɽ���ʬ
---------------------

\set ON_ERROR_STOP

-- tbl_teikirrk --
ALTER TABLE tbl_shoukaikanri add column titlekbn char(1);
update tbl_shoukaikanri set titlekbn = '1';
