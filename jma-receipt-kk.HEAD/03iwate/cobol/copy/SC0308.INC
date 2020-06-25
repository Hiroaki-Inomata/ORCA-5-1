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
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者   日付        内容
      *  01.00.01    萩野     16/10/07    特記事項の要素数追加
      *  01.00.02    井上     18/10/11    修正履歴コメント削除
      *  01.00.03    吉川     19/04/25    元号改正対応
      ********************************************************************
        01  SC08.
            03  SC08-SRYYM      PIC X(16).
            03  SC08-HOSPCDN    PIC X(20).
            03  SC08-HKNKBN     PIC X(02)    OCCURS  4 TIMES.
            03  SC08-HEIYOKBN   PIC X(02)    OCCURS  2 TIMES.
            03  SC08-HONKZKKBN  PIC X(02)    OCCURS  5 TIMES.
            03  SC08-FTNJANUM   PIC X(16)   OCCURS  3 TIMES.
            03  SC08-JKYSNUM    PIC X(14)   OCCURS  3 TIMES.
            03  SC08-HKNJANUM   PIC X(16).
            03  SC08-KIGO       PIC X(34).
            03  SC08-NUM        PIC X(34).
            03  SC08-ADRS       PIC X(100).
            03  SC08-HOSPNAME   PIC X(100).
            03  SC08-KAISETU    PIC X(40).
            03  SC08-KYURATE    PIC X(02)    OCCURS  4 TIMES.
            03  SC08-KANANAME   PIC X(40).
            03  SC08-NAME       PIC X(30).
            03  SC08-PTNUM      PIC X(20).
            03  SC08-BIRTHDAY   PIC X(08).
            03  SC08-SEX        PIC X(02)    OCCURS  2 TIMES.
            03  SC08-GENGO      PIC X(02)    OCCURS  4 TIMES.
            03  SC08-TOKKI-TBL          OCCURS 3.
                05  SC08-TOKKI    PIC X(08).
            03  SC08-BYOMEI-TBL.
                05  SC08-BYOMEI   PIC X(54)  OCCURS  5 TIMES.
                05  SC08-SRYYMD   PIC X(26)  OCCURS  5 TIMES.
                05  SC08-TENKI1   PIC X(02)  OCCURS  5 TIMES.
                05  SC08-TENKI2   PIC X(02)  OCCURS  5 TIMES.
                05  SC08-TENKI3   PIC X(02)  OCCURS  5 TIMES.
                05  SC08-TEKIYO   PIC X(52)  OCCURS  35 TIMES.
            03  SC08-KOHKBN     PIC X(02)    OCCURS  5 TIMES.
            03  SC08-MEISAI-TBL.
                05  SC08-NISSU    PIC X(02)    OCCURS  3 TIMES.
                05  SC08-TENSU    PIC X(07)    OCCURS  3 TIMES.
                05  SC08-ITBFTN   PIC X(07)    OCCURS  3 TIMES.
                05  SC08-SNISSU   PIC X(02)    OCCURS  3 TIMES.
                05  SC08-RYOYOHI  PIC X(07)    OCCURS  3 TIMES.
                05  SC08-FTNGK    PIC X(07)    OCCURS  3 TIMES.
            03  SC08-BIRTHDAY2  PIC X(22).
