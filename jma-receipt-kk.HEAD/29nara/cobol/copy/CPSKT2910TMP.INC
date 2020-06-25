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
      *    �����������     COPY (CPSKT2910TMP.INC)            *
      *
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01     ����      05/04/13  �����������������绻���ƽ��פ���
      *                                   �������ơ��֥��OCCURS�򣳤��ѹ�
      *  01.00.02     ����      10/05/21  Ĺ������Ģɼ�ˤ��뤿��KEY�ɲ�
      *  01.00.03     ����      19/04/25  �������򥳥��Ⱥ��
      ************************************************************
       01  SKT2910TMP-REC.
           03      SKT2910TMP-KEY.
             05    SKT2910TMP-SYUTURYOKU          PIC  X(01).
             05    SKT2910TMP-HKNJANUM            PIC  X(06).
             05    SKT2910TMP-ZAITAKU             PIC  X(01).
             05    SKT2910TMP-TYOKI               PIC  X(01).
             05    SKT2910TMP-KYURATE             PIC  9(02).
           03      SKT2910TMP-PAGE-FLG1           PIC  9(01).
           03      SKT2910TMP-PAGE-FLG2           PIC  9(01).
           03      SKT2910TMP-SEIKYU-TBL          OCCURS  11.
             05    SKT2910TMP-NYUGAI-TBL          OCCURS  2.
               07  SKT2910TMP-KENSU               PIC  9(06).
               07  SKT2910TMP-NISSU               PIC  9(06).
               07  SKT2910TMP-TENSU               PIC  9(10).
               07  SKT2910TMP-ITBFTN              PIC  9(10).
             05    SKT2910TMP-SKENSU              PIC  9(06).
             05    SKT2910TMP-SNISSU              PIC  9(06).
             05    SKT2910TMP-SKINGK              PIC  9(10).
             05    SKT2910TMP-SFTNGK              PIC  9(10).
           03      SKT2910TMP-KOH-TBL             OCCURS 3.
             05    SKT2910TMP-KOH-FLG             PIC  9(01).
             05    SKT2910TMP-K-NYUGAI-TBL        OCCURS 2.
               07  SKT2910TMP-K-KENSU             PIC  9(06).
               07  SKT2910TMP-K-NISSU             PIC  9(06).
               07  SKT2910TMP-K-TENSU             PIC  9(10).
               07  SKT2910TMP-K-ITBFTN            PIC  9(10).