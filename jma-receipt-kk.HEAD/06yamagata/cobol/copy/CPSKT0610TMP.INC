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
      *    �����������     COPY (CPSKT0610TMP.INC)            *
      *
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������   ����       ����
      *  01.00.01    ����     11/03/24   ����������ʿ�β��Ͽ��б����оݼ��̽��ס�
      *  01.00.02    ����     12/05/10   Ģɼ�ͼ����ѹ���ȼ������
      *  01.00.03    ���     18/10/12   �������򥳥��Ⱥ��
      ************************************************************
       01  SKT0610TMP-REC.
           03      SKT0610TMP-KEY.
             05    SKT0610TMP-SYUTURYOKU            PIC  9(01).
             05    SKT0610TMP-HKNJANUM              PIC  X(06).
             05    SKT0610TMP-NYUGAIKBN             PIC  X(01).
             05    SKT0610TMP-GENMEN                PIC  X(05).
           03      SKT0610TMP-DATA.
             05    SKT0610TMP-SEIKYU-TBL            OCCURS    13.
               07  SKT0610TMP-KENSU                 PIC  9(06).
               07  SKT0610TMP-NISSU                 PIC  9(06).
               07  SKT0610TMP-TENSU                 PIC  9(10).
               07  SKT0610TMP-ITBFTN                PIC  9(10).
               07  SKT0610TMP-SKENSU                PIC  9(06).
               07  SKT0610TMP-SNISSU                PIC  9(06).
               07  SKT0610TMP-SKINGK                PIC  9(10).
               07  SKT0610TMP-SFTNGK                PIC  9(10).
             05    SKT0610TMP-KOH-TBL  OCCURS 20.
               07  SKT0610TMP-KOHNUM                PIC  X(03).
               07  SKT0610TMP-F-KENSU               PIC  9(06).
               07  SKT0610TMP-F-NISSU               PIC  9(06).
               07  SKT0610TMP-F-TENSU               PIC  9(10).
               07  SKT0610TMP-F-ITBFTN              PIC  9(10).
               07  SKT0610TMP-F-SKENSU              PIC  9(06).
               07  SKT0610TMP-F-SNISSU              PIC  9(06).
               07  SKT0610TMP-F-SKINGK              PIC  9(10).
               07  SKT0610TMP-F-SFTNGK              PIC  9(10).
