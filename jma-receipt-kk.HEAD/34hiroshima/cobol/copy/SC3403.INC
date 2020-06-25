      *******************************************************************
      * Project code name "ORCA"
      * 日医標準レセプトソフト（JMA standard receipt software）
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
      * 修正履歴
      *   修正者       日付      内容
      *   中川        06/08/22   備考欄へ３項目追加
      *   内田        06/11/22   老人負担者、受給者等追加
      *   井上        19/01/10  修正履歴コメント削除
      ************************************************************
        01  SC03.
            03  SC03-PTNUM              PIC X(20).
            03  SC03-HONKZKKBN          PIC X(02)    OCCURS  10 TIMES.
            03  SC03-SEIYMD             PIC X(22).
            03  SC03-SRYYM              PIC X(16).
            03  SC03-CITYNAME           PIC X(24).
            03  SC03-KOHKBN             PIC X(02)    OCCURS  3 TIMES.
            03  SC03-HOSPCDN            PIC X(20).
            03  SC03-ADRS               PIC X(100).
            03  SC03-HOSPNAME           PIC X(100).
            03  SC03-KAISETU            PIC X(80).
            03  SC03-MEISAI-TBL.
                05  SC03-RJNFTNJANUM-TBL.
                    07  SC03-RJNFTNJANUM  PIC X(02)  OCCURS  4 TIMES.
                05  SC03-RJNJKYSNUM-TBL.
                    07  SC03-RJNJKYSNUM PIC X(02)    OCCURS  7 TIMES.
                05  SC03-FTNJANUM2-TBL      OCCURS  2 TIMES.
                    07  SC03-FTNJANUM2  PIC X(02)    OCCURS  2 TIMES.
                05  SC03-FTNJANUM4-TBL      OCCURS  2 TIMES.
                    07  SC03-FTNJANUM4  PIC X(02)    OCCURS  4 TIMES.
                05  SC03-JKYSNUM-TBL        OCCURS  2 TIMES.
                    07  SC03-JKYSNUM    PIC X(02)    OCCURS  7 TIMES.
                05  SC03-NAME           PIC X(30).
                05  SC03-GENGO          PIC X(04)    OCCURS  4 TIMES.
                05  SC03-SEX            PIC X(02)    OCCURS  2 TIMES.
                05  SC03-BIRTHDAYY      PIC X(04).
                05  SC03-BIRTHDAYM      PIC X(04).
                05  SC03-BIRTHDAYD      PIC X(04).
                05  SC03-KIGO           PIC X(20).
                05  SC03-NUM            PIC X(20).
                05  SC03-HKNJANUM-TBL.
                    07  SC03-HKNJANUM   PIC X(02)    OCCURS  8 TIMES.
                05  SC03-NISSU          PIC X(02).
                05  SC03-KYURATE        PIC X(02)    OCCURS  14 TIMES.
                05  SC03-TENSU          PIC X(07)    OCCURS  3 TIMES.
                05  SC03-ITBFTN         PIC X(07)    OCCURS  3 TIMES.
                05  SC03-TYOKI          PIC X(02).
                05  SC03-KEIKA          PIC X(02).
                05  SC03-KOUHAN         PIC X(06).
                05  SC03-BIKO1          PIC X(20).
                05  SC03-BIKO2          PIC X(04).
                05  SC03-BIKO3          PIC X(02).
                05  SC03-BIKO4          PIC X(02).
