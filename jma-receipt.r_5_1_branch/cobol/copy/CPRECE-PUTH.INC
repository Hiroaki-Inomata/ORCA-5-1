      *******************************************************************
      * Project code name "ORCA"
      * ����ɸ��쥻�ץȥ��եȡ�JMA standard receipt software��
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will be useful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditions described
      * in the JMA Open Source License. You should have received a copy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      ************************************************************
      *         �쥻�ץ����ֽ�ܺ����� (CPRECE-PUTH.INC)         *
      *         �ȣţ���                                         *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  RECE-PUTH-REC.
      *���ŵ��أɣ�
            02  RECE-PUTH-HOSPNUM      PIC 9(2).
      *���������ʸ��⸩����
            02  RECE-PUTH-PREFKBN-PRTJYUN
                                       PIC X(1).
      *�����������ݸ��ԡ�
            02  RECE-PUTH-HKNJANUM-PRTJYUN
                                       PIC X(1).
      *���ŵ��ؤν�����ݸ���
            02  RECE-PUTH-HKNJANUM     PIC X(8).
      *���ŵ��ؤν�����ݸ��԰�������
            02  RECE-PUTH-HKNJANUM-PRTKBN
                                       PIC X(1).
      *����������ʣ������⡢����������
            02  RECE-PUTH-PRTJYUN-TBL  OCCURS  2.
      *������������
                03  RECE-PUTH-SRT-01   PIC X(2).
      *���̹�������
                03  RECE-PUTH-KKHKBN   PIC X(2).
      *Ĺ����Ĺ������������
                03  RECE-PUTH-CHOUKI   PIC X(1).
      *�����ǡ��߰���
                03  RECE-PUTH-ZAITAKU  PIC X(1).
      *�������
                03  RECE-PUTH-KOUGAKU-TEN
                                       PIC 9(7).
      *�ǽ����Ͻ�
                03  RECE-PUTH-SRT-LAST PIC X(1).
      *ͭ������ǯ����
            02  RECE-PUTH-STYUKYMD     PIC X(8).
      *ͭ����λǯ����
            02  RECE-PUTH-EDYUKYMD     PIC X(8).
      *ü���ɣ�
            02  RECE-PUTH-TERMID       PIC X(64).
      *���ڥ졼���ɣ�
            02  RECE-PUTH-OPID         PIC X(16).
      *����ǯ����
            02  RECE-PUTH-CREYMD       PIC X(8).
      *����ǯ����
            02  RECE-PUTH-UPYMD        PIC X(8).
      *��������
            02  RECE-PUTH-UPHMS        PIC X(6).
      *
      *�祭�������ѹ���
            02  RECE-PUTH-UPSTYUKYMD   PIC X(08).
            02  RECE-PUTH-UPEDYUKYMD   PIC X(08).
