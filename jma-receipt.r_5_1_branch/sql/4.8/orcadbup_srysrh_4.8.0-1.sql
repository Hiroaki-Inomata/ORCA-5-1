---------------------
-- ���Ź԰پȲ�ơ��֥�����ɲ�
-- ����ʬ
---------------------

\set ON_ERROR_STOP

-- tbl_teikirrk --
ALTER TABLE tbl_srysrh add column hokatsukbn char(1);
update tbl_srysrh set hokatsukbn ='0';
