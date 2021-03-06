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
      *    �쥻�ץ����ٽ�     COPY (CPQRECEPRT.INC)              *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  04.05.01    NACL-ƣ��    09/06/18  ���ݥ쥻�ץ����ֽ��б�
      *                                     ������ʧ���ء������ǥե饰���ɲ�
      *
      *  04.07.01    NACL-ƣ��    12/02/13  �����ȹ��ؤ�ľ������ȿ���
      *                                     ��ʧ���������б�
      *************************************************************
      *01  QUERY-RECE08-REC.
      *���ŵ����ֹ�
           03  QUERY-RECE08-HOSPNUM        PIC 9(2).
      *Ģɼ�ɣ�
           03  QUERY-RECE08-PRTID          PIC X(04).
      *����ǯ��
           03  QUERY-RECE08-RECEYM         PIC X(06).
      *������
           03  QUERY-RECE08-CREYMD         PIC X(08).
      *��������
           03  QUERY-RECE08-CREHMS         PIC X(06).
      *������ʬ
           03  QUERY-RECE08-NYUGAIKBN      PIC X(01).
      *������ʧ����
           03  QUERY-RECE08-TEISYUTUSAKI   PIC X(01).
      *�����ǡ��߰���
           03  QUERY-RECE08-ZAITAKU        PIC X(01).
      *��ե�����
           03  QUERY-RECE08-DRCD           PIC X(05).
      *����̿��Ų�
           03  QUERY-RECE08-DRCD-SRYKA     PIC X(02).
      *����
           03  QUERY-RECE08-SORT-1         PIC X(10).
      *���Ų�
           03  QUERY-RECE08-SRYKA          PIC X(02).
      *�������ݸ����ֹ�        
           03  QUERY-RECE08-HKNJANUM-S     PIC X(08).
      *���
           03  QUERY-RECE08-KENSU          PIC 9(06).
      *�ڡ���
           03  QUERY-RECE08-PAGE           PIC 9(06).  
