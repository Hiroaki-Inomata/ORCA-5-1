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
      *    �����������     COPY (CPSKT0210TMP.INC)              *
      *
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    ����     08/09/04   ����̾���ɲ�
      *  01.00.02    ����     10/09/14   ����ǯ���ɲ�
      *  01.00.03    ����     11/03/24   ����������ʿ�β��Ͽ��б����оݼ��̽��ס�
      *  01.00.04    ���     19/12/09   �������򥳥��Ⱥ��
      ************************************************************
       01  SKT0210TMP-REC.
           03  SKT0210TMP-KEY.
               05  SKT0210TMP-SYUTURYOKU           PIC  9(01).
               05  SKT0210TMP-HKNJANUM             PIC  X(06).
               05  SKT0210TMP-ZAITAKU              PIC  X(01).
               05  SKT0210TMP-GENMEN               PIC  X(05).
           03  SKT0210TMP-SRYYM                    PIC  X(06).
           03  SKT0210TMP-SEIKYU-TBL     OCCURS  14.
               05  SKT0210TMP-NYUGAI-TBL  OCCURS  2.
                   07  SKT0210TMP-KENSU            PIC  9(06).
                   07  SKT0210TMP-NISSU            PIC  9(06).
                   07  SKT0210TMP-TENSU            PIC  9(10).
                   07  SKT0210TMP-YKZFTN           PIC  9(10).
                   07  SKT0210TMP-ITBFTN           PIC  9(10).
               05  SKT0210TMP-S-KENSU              PIC  9(06).
               05  SKT0210TMP-S-NISSU              PIC  9(06).
               05  SKT0210TMP-S-KINGAKU            PIC  9(10).
               05  SKT0210TMP-S-FTNGAKU            PIC  9(10).
           03  SKT0210TMP-KOH-TBL      OCCURS  20.
               05  SKT0210TMP-KOHNUM               PIC  X(03).
               05  SKT0210TMP-KOHNAME              PIC  X(16).
               05  SKT0210TMP-R-NYUGAI-TBL  OCCURS  2.
                   07  SKT0210TMP-KKENSU           PIC  9(06).
                   07  SKT0210TMP-KNISSU           PIC  9(06).
                   07  SKT0210TMP-KTENSU           PIC  9(10).
                   07  SKT0210TMP-KYKZFTN          PIC  9(10).
                   07  SKT0210TMP-KITBFTN          PIC  9(10).
               05  SKT0210TMP-S-KKENSU             PIC  9(06).
               05  SKT0210TMP-S-KNISSU             PIC  9(06).
               05  SKT0210TMP-S-KKINGAKU           PIC  9(10).
               05  SKT0210TMP-S-KFTNGAKU           PIC  9(10).
           03  SKT0210TMP-GKENSU                   PIC  9(05).
           03  SKT0210TMP-GRKENSU                  PIC  9(05).
           03  SKT0210TMP-KYURATE-TBL  OCCURS  2.
               05  SKT0210TMP-KYURATE              PIC  9(01).
