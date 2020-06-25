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
      *    ����饤����������    COPY (CPHENREI-BODY.INC)        *
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  04.06.01    NACL-ƣ��    10/10/12  ����ñ�ȤΥ쥻ʬΥ�б�
      *                                     kohid �ɲ�
      *  04.06.02    NACL-ƣ��    10/12/20  �ݸ��Ԥ���������б�
      *  04.06.03    NACL-ƣ��    12/10/26  �õ����ࣰ����Ϸʻ�˵ڤ�
      *                                     ������Ϸ��ˤΥ쥻�ץ��б�
      *                                     receipt_kbn �ɲ�
      *
      *  04.08.01    NACL-ƣ��    16/08/08  �ҳ�����ʬ�������оݤȸ����оݡ�
      *                                     �쥻�ץȤ�ʬ���б�
      *                                     disaster s_disaster 1-10 �ɲ�
      *
      *  04.08.02    NACL-ƣ��    16/06/27  ���ŵ��إ������ѹ����Υ쥻��
      *                                     �ǡ������κ����б�
      *                                      �ʣ���������ѹ��Τߡ�
      ************************************************************
      *01  HENREI-BODY-REC.
           03  HENREI-B-HOSPNUM    PIC 9(2).
           03  HENREI-B-TEISYUTUSAKI
                                   PIC X(1).
           03  HENREI-B-TEISYUTUSAKI2
                                   PIC X(1).
           03  HENREI-B-HOSPCD     PIC X(7).
           03  HENREI-B-SYORIYM    PIC X(6).
           03  HENREI-B-RECEIPT-NO PIC X(6).
           03  HENREI-B-PT-RENNUM  PIC 9(4).
      *
           03  HENREI-B-SELECT-FLG PIC 9(1).
      *
           03  HENREI-B-RECESYUBETU
                                   PIC X(4).
           03  HENREI-B-NYUGAIKBN  PIC X(1).
           03  HENREI-B-SRYYM      PIC X(6).
           03  HENREI-B-NAME       PIC X(40).
           03  HENREI-B-SEX        PIC X(1).
           03  HENREI-B-BIRTHDAY   PIC X(8).
           03  HENREI-B-PTNUM      PIC X(20).
           03  HENREI-B-PTID       PIC 9(10).
           03  HENREI-B-HKNJANUM   PIC X(8).
           03  HENREI-B-KIGO       PIC X(40).
           03  HENREI-B-NUM        PIC X(40).
           03  HENREI-B-RECEIPT-KBN
                                   PIC X(10).
           03  HENREI-B-KO-AREA.
               05  HENREI-B-KO-OCC OCCURS  4.
                   07  HENREI-B-FTNNUM PIC X(8).
                   07  HENREI-B-JKYNUM PIC X(7).
           03  HENREI-B-DISASTER   PIC X(2).
      *
           03  HENREI-B-SEARCH-NO  PIC X(30).
           03  HENREI-B-CHECK-NO   PIC X(100).
           03  HENREI-B-HR-CODE    PIC X(5).
           03  HENREI-B-HR-MSG     PIC X(1000).
           03  HENREI-B-HENREI-INFO1
                                   PIC X(20).
           03  HENREI-B-HENREI-INFO2
                                   PIC X(3000).
           03  HENREI-B-ERRCD      PIC X(03).
      *
           03  HENREI-B-SEIKYU-AREA.
               05  HENREI-B-SEIKYU-OCC OCCURS  10.
                   07  HENREI-B-S-SYORIYMD     PIC X(8).
                   07  HENREI-B-S-MAKE-FLG     PIC 9(01).
                   07  HENREI-B-S-HENREIYM     PIC X(06).
                   07  HENREI-B-S-HENREI-SIJI-FLG
                                               PIC 9(01).
      *
                   07  HENREI-B-S-TEISYUTUSAKI PIC X(01).
                   07  HENREI-B-S-NYUGAIKBN    PIC X(01).
                   07  HENREI-B-S-RECEKA       PIC X(02).
                   07  HENREI-B-S-RECESYUBETU  PIC X(04).
                   07  HENREI-B-S-HKNJANUM     PIC X(08).
                   07  HENREI-B-S-HOJOKBN-KEY  PIC X(01).
                   07  HENREI-B-S-KOHID-KEY    PIC 9(10).
                   07  HENREI-B-S-TEKSTYMD     PIC X(08).
                   07  HENREI-B-S-RECEIPT-KBN  PIC X(10).
      *
                   07  HENREI-B-S-KIGO         PIC X(40).
                   07  HENREI-B-S-DISASTER     PIC X(02).
      *ü���ɣ�
           03  HENREI-B-TERMID     PIC X(64).
      *���ڡ��졼���ɣ�
           03  HENREI-B-OPID       PIC X(16).
      *����ǯ����
           03  HENREI-B-CREYMD     PIC X(08).
      *����ǯ����
           03  HENREI-B-UPYMD      PIC X(08).
      *��������
           03  HENREI-B-UPHMS      PIC X(06).
      *�����ѹ���
           03  HENREI-B-XX-AREA.
      *������ʧ���ظ����ѹ���
               05  HENREI-B-XXTEISYUTUSAKI
                                   PIC X(1).
