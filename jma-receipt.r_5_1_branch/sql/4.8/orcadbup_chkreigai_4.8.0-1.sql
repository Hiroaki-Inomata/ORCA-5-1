--                                      
-- �����å��㳰
--                                    
-- ���顼�ֹ���
-- ��ǧ�ֹ��char���ѹ�
-- �����å���ʬ�ɲ�
-- ���顼�������ɲ�
-- ���顼��å������ɲ�
-- ��̾�ɲ�
-- ����ʬ�ɲ�
-- Ŭ�ѥ������ɲ�
-- Ŭ����̾�ɲ�
-- Ŭ����̾����ʬ�ɲ�
                                  
-- Create Date : 2011/8/29

\set ON_ERROR_STOP
alter table tbl_chkreigai drop   COLUMN errkbn;

alter table tbl_chkreigai add    COLUMN chknum2 char(2);
update tbl_chkreigai set chknum2 = trim(to_char(chknum,'00'));
alter table tbl_chkreigai drop   COLUMN chknum;
alter table tbl_chkreigai rename COLUMN chknum2 to chknum;

alter table tbl_chkreigai add COLUMN chkkbn char(1);
alter table tbl_chkreigai add COLUMN errcd  char(6);
alter table tbl_chkreigai add COLUMN errmsg character varying(200);
alter table tbl_chkreigai add COLUMN byomei character varying(160);
alter table tbl_chkreigai add COLUMN compkbn char(1);
alter table tbl_chkreigai add COLUMN tekcd   char(9);
alter table tbl_chkreigai add COLUMN tekbyomei varchar(160);
alter table tbl_chkreigai add COLUMN tekcompkbn char(1);
update tbl_chkreigai set chkkbn = '', errcd = '',errmsg = '',byomei='',compkbn='',tekcd='',tekbyomei='',tekcompkbn=''; 
update tbl_chkreigai set chkkbn = '1',errcd = '080001' where chknum = '08'; 
update tbl_chkreigai set chkkbn = '2',errcd = '090001' where chknum = '09'; 

-- ����ǡ��������å����顼��������
delete  from tbl_chk004 where kanricd in ('1200','1201') ;

