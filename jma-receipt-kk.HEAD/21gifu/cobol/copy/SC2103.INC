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
      * 01.00.01     吉川     2015/02/16  患者負担額のバイト数増加
      * 01.00.02     門間     2017/04/10  備考欄の記載追加
      * 01.00.03     萩野     2018/10/11  修正履歴コメント削除
      ********************************************************************
        01  SC03.
            02  SC03-PRTYM    PIC X(16).
            02  SC03-HOSPCDN  PIC X(20).
            02  SC03-SEIYMD   PIC X(22).
            02  SC03-SRYKA    PIC X(2).
            02  SC03-ADRS PIC X(100).
            02  SC03-HOSPNAME PIC X(100).
            02  SC03-KAISETUNAME  PIC X(60).
            02  SC03-TEL  PIC X(26).
            02  SC03-SRYTBL           OCCURS  10 TIMES.
              03  SC03-JKYSNUM1 PIC X(6).
              03  SC03-JKYSNUM2 PIC X(22).
              03  SC03-NAME PIC X(20).
              03  SC03-NYUGAIKBN
                      PIC X(2)    OCCURS  2 TIMES.
              03  SC03-HKNKBN
                      PIC X(2)    OCCURS  4 TIMES.
              03  SC03-KOREI    PIC X(1).
              03  SC03-TYOKI    PIC X(1).
              03  SC03-TENSU
                      PIC X(7)    OCCURS  3 TIMES.
              03  SC03-KOHTENSU PIC X(7).
              03  SC03-YKZFTN   PIC X(6).
              03  SC03-ITBFTN   PIC X(7).
              03  SC03-SKINGK   PIC X(6).
              03  SC03-SFTNGK   PIC X(6).
              03  SC03-BIKO PIC X(16).
              03  SC03-BIKO2 PIC X(16).
            02  SC03-SUM-TBL.
              03  SC03-GKENSU
                      PIC X(2)    OCCURS  3 TIMES.
              03  SC03-GTENSU
                      PIC X(9)    OCCURS  3 TIMES.
              03  SC03-GKOHTENSU    PIC X(7).
              03  SC03-GYKZFTN  PIC X(7).
              03  SC03-GITBFTN  PIC X(9).
              03  SC03-GSKENSU  PIC X(2).
              03  SC03-GSKINGK  PIC X(7).
              03  SC03-GSFTNGK  PIC X(7).
              03  SC03-KOHSYUBETU
                      PIC X(2)    OCCURS  4 TIMES.
