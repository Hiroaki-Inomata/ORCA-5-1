--                                                         --
-- ���Ŵѻ�ˡ����ά�Υơ��֥�                              --
--                                                         --
-- �����������оݼ�������ش�����   (188000110)            --
-- �����������оݼ�������ش�����   (188000510)            --
-- �Ҳ������������оݼ�������ش�����   (188000610)        --
--                                                         --
--                                              --
-- Create Date : 2010/08/02                     --
--                                              --
\set ON_ERROR_STOP

delete from tbl_nyuinryakusho where srycd = '188000110';
delete from tbl_nyuinryakusho where srycd = '188000510';
delete from tbl_nyuinryakusho where srycd = '188000610';

COPY "tbl_nyuinryakusho" FROM stdin;
188000110	20100401	99999999	�ѻ�������				        	        	      
188000510	20100401	99999999	�ѻ�������				        	        	      
188000610	20100401	99999999	�ѻ�������				        	        	      
\.
