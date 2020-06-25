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
      *****************************************************************
      *  ���ԥ쥻�ץ�ʬ��ơ��֥�       (CPPT-HDIST-DAY.INC)
      *****************************************************************
      *  ��������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *
      *01  PT-HDIST-DAY-REC.
           03  PT-HDIST-DAY-KEY.
      *���ŵ��أɣ�
               05  PT-HDIST-DAY-HOSPNUM    PIC 9(2).
      *���ԣɣ�
               05  PT-HDIST-DAY-PTID       PIC 9(10).
      *������ʬ
               05  PT-HDIST-DAY-NYUGAIKBN  PIC X(01).
      *����ǯ��
               05  PT-HDIST-DAY-SRYYM      PIC X(06).
      *��ʬ��
               05  PT-HDIST-DAY-DAY        PIC X(02).
      *�쥻�ץȶ�ʬ
               05  PT-HDIST-DAY-RECEIPT-KBN
                                           PIC X(01).
      *�ݸ��ȹ礻����ʣ����� �������
           03  PT-HDIST-DAY-HKNCOMBI-INF.
               05  PT-HDIST-DAY-HKNCOMBI-OCC   OCCURS 2.
                   07  PT-HDIST-DAY-HKNCOMBI-T OCCURS 20.
      *�ݸ��ȹ礻
                       09  PT-HDIST-DAY-HKNCOMBI
                                           PIC 9(04).
      *����ե饰
                       09  PT-HDIST-DAY-DLTFLG
                                           PIC X(01).
      *ü���ɣ�
           03  PT-HDIST-DAY-TERMID         PIC X(16).
      *���ڡ��졼���ɣ�
           03  PT-HDIST-DAY-OPID           PIC X(16).
      *����ǯ����
           03  PT-HDIST-DAY-CREYMD         PIC X(08).
      *����ǯ����
           03  PT-HDIST-DAY-UPYMD          PIC X(08).
      *��������
           03  PT-HDIST-DAY-UPHMS          PIC X(06).