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
      *    �ãӣ��ѣģ�                   (CPCSV-INFO.INC)       *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  CSV-INFO-REC.
      *
          03  CSV-INFO-KEY.
      *
               05  CSV-INFO-HOSPNUM        PIC 9(2).
      *�ˣţ�
               05  CSV-INFO-TBL-KEY        PIC X(08).
      *����ǯ��
               05  CSV-INFO-SRYYM          PIC X(06).
      *������ɣ�
               05  CSV-INFO-SHELLID        PIC X(20).
      *Ϣ��
               05  CSV-INFO-RENNUM         PIC 9(03).
      *������Ϣ��
               05  CSV-INFO-SHORI-RENNUM   PIC 9(03).
      *����Ϣ��
               05  CSV-INFO-SEQ-NO         PIC 9(06).
      *������ʬ
           03  CSV-INFO-NYUGAIKBN          PIC X(01).
      *����ǯ����
           03  CSV-INFO-SKYYMD             PIC X(08).
      *������ʧ����
           03  CSV-INFO-TEISYUTUSAKI       PIC X(01).
      *�Уԣɣ�
           03  CSV-INFO-PTID               PIC 9(10).
      *�ãӣ֥ǡ���
           03  CSV-INFO-CSVDATA            PIC X(2000).
      *������
           03  CSV-INFO-CREYMD             PIC X(08).
      *��������
           03  CSV-INFO-CREHMS             PIC X(06).
