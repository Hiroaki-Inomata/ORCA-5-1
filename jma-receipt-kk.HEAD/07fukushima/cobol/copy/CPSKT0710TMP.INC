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
      *    �����������     COPY (CPSKT0710TMP.INC)              *
      *
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    Ĺ��    08/04/29    Ģɼ�ͼ���ȼ������
      *  01.00.02    ����    08/10/09    ����̾�ɲ�
      *  01.00.03    ����    11/03/15    ����������ʿ�β��Ͽ̡��оݼ��̽��ס�
      *  01.00.04    ����    18/10/10    �������򥳥��Ⱥ��
      ************************************************************
       01  SKT0710TMP-REC.
           03  SKT0710TMP-KEY.
               05  SKT0710TMP-SYUTURYOKU           PIC  9(01).
               05  SKT0710TMP-HKNJANUM             PIC  X(06).
               05  SKT0710TMP-HKNJANAME            PIC  X(20).
               05  SKT0710TMP-ZAITAKU              PIC  X(01).
               05  SKT0710TMP-SRYYM                PIC  9(06).
               05  SKT0710TMP-GENMEN               PIC  X(05).
           03  SKT0710TMP-SEIKYU-TBL     OCCURS  14.
               05  SKT0710TMP-NYUGAI-TBL  OCCURS  2.
                   07  SKT0710TMP-KENSU            PIC  9(06).
                   07  SKT0710TMP-NISSU            PIC  9(06).
                   07  SKT0710TMP-TENSU            PIC  9(10).
                   07  SKT0710TMP-YKZFTN           PIC  9(10).
                   07  SKT0710TMP-ITBFTN           PIC  9(10).
               05  SKT0710TMP-S-KENSU              PIC  9(06).
               05  SKT0710TMP-S-NISSU              PIC  9(06).
               05  SKT0710TMP-S-KINGAKU            PIC  9(10).
               05  SKT0710TMP-S-FTNGAKU            PIC  9(10).
           03  SKT0710TMP-KOH-TBL      OCCURS  20.
               05  SKT0710TMP-KOHNUM               PIC  X(03).
               05  SKT0710TMP-KOHNAME              PIC  X(10).
               05  SKT0710TMP-R-NYUGAI-TBL  OCCURS  2.
                   07  SKT0710TMP-KKENSU           PIC  9(06).
                   07  SKT0710TMP-KNISSU           PIC  9(06).
                   07  SKT0710TMP-KTENSU           PIC  9(10).
                   07  SKT0710TMP-KYKZFTN          PIC  9(10).
                   07  SKT0710TMP-KITBFTN          PIC  9(10).
               05  SKT0710TMP-S-KKENSU             PIC  9(06).
               05  SKT0710TMP-S-KNISSU             PIC  9(06).
               05  SKT0710TMP-S-KKINGAKU           PIC  9(10).
               05  SKT0710TMP-S-KFTNGAKU           PIC  9(10).
           03  SKT0710TMP-ROUJIN                   PIC  9(10).
