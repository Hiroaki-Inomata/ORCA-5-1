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
      *    �����������     COPY (CPSKT0520TMP.INC)              *
      *
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������     ����      ����
      *  01.00.01    ����       11/03/24  ����������ʿ�β��Ͽ��б����оݼ��̽��ס�
      *  01.00.02    ���       19/12/09  �������򥳥��Ⱥ��
      ************************************************************
       01  SKT0520TMP-REC.
           03  SKT0520TMP-KEY.
               05  SKT0520TMP-SYUTURYOKU            PIC  9(01).
               05  SKT0520TMP-HKNJANUM              PIC  X(06).
               05  SKT0520TMP-KYURATE               PIC  9(03).
               05  SKT0520TMP-ZAITAKU               PIC  X(01).
               05  SKT0520TMP-SRYYM                 PIC  X(06).
               05  SKT0520TMP-GENMEN                PIC  X(05).
           03  SKT0520TMP-HKN-AREA  OCCURS  11.
               05  SKT0520TMP-NYUGAI-TBL  OCCURS  2.
                   07  SKT0520TMP-KENSU             PIC  9(05).
                   07  SKT0520TMP-NISSU             PIC  9(05).
                   07  SKT0520TMP-TENSU             PIC  9(09).
                   07  SKT0520TMP-ITBFTN            PIC  9(09).
               05  SKT0520TMP-SKENSU                PIC  9(05).
               05  SKT0520TMP-SNISSU                PIC  9(05).
               05  SKT0520TMP-SKINGK                PIC  9(09).
               05  SKT0520TMP-SFTNGK                PIC  9(09).
           03  SKT0520TMP-KOH-AREA.
               05  SKT0520TMP-KOH-NYUGAI-TBL OCCURS 2.
                   07  SKT0520TMP-K-KENSU            PIC  9(05).
                   07  SKT0520TMP-K-NISSU            PIC  9(05).
                   07  SKT0520TMP-K-TENSU            PIC  9(09).
                   07  SKT0520TMP-K-ITBFTN           PIC  9(09).
               05  SKT0520TMP-K-SKENSU               PIC  9(05).
               05  SKT0520TMP-K-SNISSU               PIC  9(05).
               05  SKT0520TMP-K-SKINGK               PIC  9(09).
               05  SKT0520TMP-K-SFTNGK               PIC  9(09).
