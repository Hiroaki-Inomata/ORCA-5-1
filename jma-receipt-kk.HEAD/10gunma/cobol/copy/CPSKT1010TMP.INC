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
      *    ����ɽ����ե�����                                    *
      *                                                          *
      *                        ��ʬ��  1:���̡�2:ʡ��            *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������  ����        ����
      *  01.00.01    ����    2009/08/25  SKT1010TMP-TYOKI�ɲ�
      *  01.00.02    ����    2011/03/24  ����������ʿ�β��Ͽ��б����оݼ��̽��ס�
      *  01.00.03    ����    2011/04/08  �ҳ��б������ѹ�
      *  01.00.04    ���    2018/10/02  ���ץ����Ȥ���
      ************************************************************
       01  SKT1010TMP-REC.
           03  SKT1010TMP-KEY.
               05  SKT1010TMP-SYUTURYOKU     PIC  9(01).
               05  SKT1010TMP-HKNJANUM       PIC  X(06).
               05  SKT1010TMP-ZAITAKU        PIC  9(01).
               05  SKT1010TMP-KYURATE-KEY    PIC  9(03).
               05  SKT1010TMP-SRYYM          PIC  9(06).
               05  SKT1010TMP-PREFKBN        PIC  X(01).
               05  SKT1010TMP-TYOKI          PIC  X(01).
               05  SKT1010TMP-GENMEN         PIC  X(05).
               05  SKT1010TMP-ADRS           PIC  X(40).
           03  SKT1010TMP-KYURATE            PIC  9(02).
           03  SKT1010TMP-FLG1               PIC  9(01).
           03  SKT1010TMP-FLG2               PIC  9(01).
           03  SKT1010TMP-FLG3               PIC  9(01).
           03  SKT1010TMP-IPAN-TBL    OCCURS  9.
               05  SKT1010TMP-IPAN-NYUGAI-TBL    OCCURS  2.
                   07 SKT1010TMP-KENSU       PIC  9(06).
                   07 SKT1010TMP-KKENSU      PIC  9(06).
                   07 SKT1010TMP-FKENSU      PIC  9(06).
                   07 SKT1010TMP-NISSU       PIC  9(06).
                   07 SKT1010TMP-TENSU       PIC  9(10).
                   07 SKT1010TMP-YKZFTN      PIC  9(10).
                   07 SKT1010TMP-ITBFTN      PIC  9(10).
               05  SKT1010TMP-S-KENSU        PIC  9(06).
               05  SKT1010TMP-S-NISSU        PIC  9(06).
               05  SKT1010TMP-S-KINGAKU      PIC  9(10).
               05  SKT1010TMP-S-FTNGAKU      PIC  9(10).
           03  SKT1010TMP-TAI-TBL    OCCURS  10.
               05  SKT1010TMP-TAI-NYUGAI-TBL    OCCURS  2.
                   07 SKT1010TMP-TKENSU      PIC  9(06).
                   07 SKT1010TMP-TKKENSU     PIC  9(06).
                   07 SKT1010TMP-TFKENSU     PIC  9(06).
                   07 SKT1010TMP-TNISSU      PIC  9(06).
                   07 SKT1010TMP-TTENSU      PIC  9(10).
                   07 SKT1010TMP-TYKZFTN     PIC  9(10).
                   07 SKT1010TMP-TITBFTN     PIC  9(10).
               05  SKT1010TMP-S-TKENSU       PIC  9(06).
               05  SKT1010TMP-S-TNISSU       PIC  9(06).
               05  SKT1010TMP-S-TKINGAKU     PIC  9(10).
               05  SKT1010TMP-S-TFTNGAKU     PIC  9(10).
           03  SKT1010TMP-RJN-TBL    OCCURS  6.
               05  SKT1010TMP-RJN-NYUGAI-TBL    OCCURS  2.
                   07 SKT1010TMP-RKENSU      PIC  9(06).
                   07 SKT1010TMP-RKKENSU     PIC  9(06).
                   07 SKT1010TMP-RFKENSU     PIC  9(06).
                   07 SKT1010TMP-RNISSU      PIC  9(06).
                   07 SKT1010TMP-RTENSU      PIC  9(10).
                   07 SKT1010TMP-RITBFTN     PIC  9(10).
               05  SKT1010TMP-S-RKENSU       PIC  9(06).
               05  SKT1010TMP-S-RNISSU       PIC  9(06).
               05  SKT1010TMP-S-RKINGAKU     PIC  9(10).
               05  SKT1010TMP-S-RFTNGAKU     PIC  9(10).
           03  SKT1010TMP-GKENSU             PIC  9(03).
           03  SKT1010TMP-GTKENSU            PIC  9(03).
           03  SKT1010TMP-GRKENSU            PIC  9(03).
      *
