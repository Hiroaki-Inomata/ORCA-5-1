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
      *    �����������     COPY (CPSKT0810TMP.INC)              *
      *
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      * 01.00.01       Ĺ��   08/04/26    Ģɼ�ͼ���ȼ������
      * 01.00.02       ����   11/03/24    ����������ʿ�β��Ͽ��б����оݼ��̽��ס�
      * 01.00.03       ���   18/10/09    ���ץ����Ȥ���
      ************************************************************
       01  SKT0810TMP-REC.
           03  SKT0810TMP-KEY.
               05  SKT0810TMP-SYUTURYOKU            PIC  9(01).
               05  SKT0810TMP-HKNJANUM1             PIC  X(06).
               05  SKT0810TMP-KBN                   PIC  X(01).
               05  SKT0810TMP-PREFKBN               PIC  X(01).
               05  SKT0810TMP-ZAITAKU               PIC  X(01).
               05  SKT0810TMP-GENMEN                PIC  X(05).
               05  SKT0810TMP-SRYYM                 PIC  9(06).
           03  SKT0810TMP-HKN-AREA  OCCURS  11.
               05  SKT0810TMP-NYUGAI-TBL  OCCURS  2.
                   07  SKT0810TMP-KENSU             PIC  9(05).
                   07  SKT0810TMP-NISSU             PIC  9(05).
                   07  SKT0810TMP-TENSU             PIC  9(09).
                   07  SKT0810TMP-ITBFTN            PIC  9(07).
               05  SKT0810TMP-S-KENSU               PIC  9(05).
               05  SKT0810TMP-S-NISSU               PIC  9(05).
               05  SKT0810TMP-S-KINGK               PIC  9(09).
               05  SKT0810TMP-S-FTNGK               PIC  9(09).
           03  SKT0810TMP-KOH-AREA  OCCURS  20.
               05  SKT0810TMP-KOHNUM                PIC  X(03).
               05  SKT0810TMP-KOH-NYUGAI-TBL OCCURS 2.
                   07  SKT0810TMP-KKENSU            PIC  9(05).
                   07  SKT0810TMP-KNISSU            PIC  9(05).
                   07  SKT0810TMP-KTENSU            PIC  9(09).
                   07  SKT0810TMP-KITBFTN           PIC  9(07).
               05  SKT0810TMP-S-KKENSU              PIC  9(05).
               05  SKT0810TMP-S-KNISSU              PIC  9(05).
               05  SKT0810TMP-S-KKINGK              PIC  9(09).
               05  SKT0810TMP-S-KFTNGK              PIC  9(09).
