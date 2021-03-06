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
      *    ����                 COPY (CPPTINF.INC)     *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      * 01.00.01     MCC-����     02/06/06  ɬ�׹��ܤθ�ľ��
      ************************************************************
      *01  QUERY-PTINF-REC.
      *����
           03  QUERY-PTINF-KEY.
      *���ŵ����ֹ�
               05  QUERY-PTINF-HOSPNUM         PIC  9(02).
      *���ԣɣ�
               05  QUERY-PTINF-PTID            PIC  9(10).
      *���ʻ�̾
           03  QUERY-PTINF-KANANAME            PIC  X(100).
      *������̾
           03  QUERY-PTINF-NAME                PIC  X(100).
      *����
           03  QUERY-PTINF-SEX                 PIC  X(01).
      *��ǯ����
           03  QUERY-PTINF-BIRTHDAY            PIC  X(08).
      *�����͹���ֹ�
           03  QUERY-PTINF-HOME-POST           PIC  X(07).
      *����ݽ���
           03  QUERY-PTINF-HOME-ADRS           PIC  X(200).
      *�������������
           03  QUERY-PTINF-HOME-BANTI          PIC  X(200).
      *����������ֹ�
           03  QUERY-PTINF-HOME-TEL1           PIC  X(15).
      *�ƥ��ȴ��Զ�ʬ
           03  QUERY-PTINF-TSTPTNUMKBN         PIC  X(1).
      *�����ֹ�
           03  QUERY-PTINF-PTNUM               PIC  X(20).
      *���
           03  QUERY-PTINF-TOTAL               PIC  9(10).
      *
      *�ե꡼�ӣѣ�ʸ��WHERE���
           03  QUERY-PTINF-SQL                 PIC  X(15000).
      *DBESCAPEENCODING
           03  QUERY-PTINF-DBSTR               PIC  X(500).
