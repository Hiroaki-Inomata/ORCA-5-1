--                                      
-- �����ơ��֥�
--                                    
-- ������ॳ���ɤΥ���ǥå��������
--                                   
-- Create Date : 2016/05/26      

\set ON_ERROR_STOP

CREATE INDEX
    idx_tensu_yakkakjncd
ON
    tbl_tensu
USING
    btree (yakkakjncd)
;
