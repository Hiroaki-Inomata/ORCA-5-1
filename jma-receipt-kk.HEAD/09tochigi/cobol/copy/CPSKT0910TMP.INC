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
      *    �����������     COPY (CPSKT0910TMP.INC)              *
      *
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������  ����        ����
      *  01.00.01    ����    2008/07/07  H20ǯ4��������ϵ��ͼ��ǰ���
      *  01.00.02    ����    2008/10/09  ����̾�ɲ�
      *  01.00.03    ����    2011/03/24  ����������ʿ�β��Ͽ��б����оݼ��̽��ס�
      *  01.00.04    ���    2018/10/12  �������򥳥��Ⱥ��
      ************************************************************
       01  SKT0910TMP-REC.
           03  SKT0910TMP-KEY.
               05  SKT0910TMP-SYUTURYOKU            PIC  9(01).
               05  SKT0910TMP-HKNJANUM1             PIC  X(12).
               05  SKT0910TMP-KBN                   PIC  X(04).
               05  SKT0910TMP-PREFKBN               PIC  X(01).
               05  SKT0910TMP-ZAITAKU               PIC  X(01).
               05  SKT0910TMP-GENMEN                PIC  X(05).
           03  SKT0910TMP-HKN-AREA  OCCURS  11.
               05  SKT0910TMP-NYUGAI-TBL  OCCURS  2.
                   07  SKT0910TMP-KENSU             PIC  9(05).
                   07  SKT0910TMP-NISSU             PIC  9(05).
                   07  SKT0910TMP-TENSU             PIC  9(09).
                   07  SKT0910TMP-ITBFTN            PIC  9(07).
               05  SKT0910TMP-S-KENSU               PIC  9(05).
               05  SKT0910TMP-S-NISSU               PIC  9(05).
               05  SKT0910TMP-S-KINGK               PIC  9(09).
               05  SKT0910TMP-S-FTNGK               PIC  9(09).
           03  SKT0910TMP-KOH-AREA  OCCURS  20.
               05  SKT0910TMP-KOHNUM                PIC  X(03).
               05  SKT0910TMP-KOHNAME               PIC  X(10).
               05  SKT0910TMP-KOH-NYUGAI-TBL OCCURS 2.
                   07  SKT0910TMP-KKENSU            PIC  9(05).
                   07  SKT0910TMP-KNISSU            PIC  9(05).
                   07  SKT0910TMP-KTENSU            PIC  9(09).
                   07  SKT0910TMP-KITBFTN           PIC  9(07).
               05  SKT0910TMP-S-KKENSU              PIC  9(05).
               05  SKT0910TMP-S-KNISSU              PIC  9(05).
               05  SKT0910TMP-S-KKINGK              PIC  9(09).
               05  SKT0910TMP-S-KFTNGK              PIC  9(09).
