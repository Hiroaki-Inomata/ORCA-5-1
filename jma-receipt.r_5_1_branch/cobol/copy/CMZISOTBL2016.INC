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
      *----------------------------------------------------------------*
      *    在宅の処方せん切替えコードテーブル
      *                            　平成２８年４月改正から            *
      *                         COPY (CMZISOTBL2016.INC)               *
      *----------------------------------------------------------------*
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.08.00    NACL-多々納  17/02/XX  実績等加算追加
      *****************************************************************
      *
       01  TBL-ZISOSIN-AREA.
      *在宅がん医総合
      *    （在支診等）（処方せんあり）
           03  FILLER              PIC X(14)   VALUE   "2110 114007610".
      *    （在支診等）（処方せんなし）
           03  FILLER              PIC X(14)   VALUE   "2120 114007710".
      *
      *    （機能強化した在支診等）（病床あり）（処方せんあり）
           03  FILLER              PIC X(14)   VALUE   "2210 114019510".
      *    （機能強化した在支診等）（病床あり）（処方せんなし）
           03  FILLER              PIC X(14)   VALUE   "2220 114019610".
      *
      *    （機能強化した在支診等）（病床なし）（処方せんあり）
           03  FILLER              PIC X(14)   VALUE   "2310 114019710".
      *    （機能強化した在支診等）（病床なし）（処方せんなし）
           03  FILLER              PIC X(14)   VALUE   "2320 114019810".
      *
      *在医総管
      *    （機能強化在支診等）（病床有・難病月２回・１人）
           03  FILLER              PIC X(14)   VALUE   "01 1 114030710".
      *    （機能強化在支診等）（病床有・難病月２回・２〜９人）
           03  FILLER              PIC X(14)   VALUE   "01 2 114030810".
      *    （機能強化在支診等）（病床有・難病月２回・以外）
           03  FILLER              PIC X(14)   VALUE   "01 3 114030910".
      *
      *    （機能強化在支診等）（病床有・月２回・１人）
           03  FILLER              PIC X(14)   VALUE   "01 1 114031010".
      *    （機能強化在支診等）（病床有・月２回・２〜９人）
           03  FILLER              PIC X(14)   VALUE   "01 2 114031110".
      *    （機能強化在支診等）（病床有・月２回・以外）
           03  FILLER              PIC X(14)   VALUE   "01 3 114031210".
      *
      *    （機能強化在支診等）（病床有・月１回・１人）
           03  FILLER              PIC X(14)   VALUE   "01 1 114031310".
      *    （機能強化在支診等）（病床有・月１回・２〜９人）
           03  FILLER              PIC X(14)   VALUE   "01 2 114031410".
      *    （機能強化在支診等）（病床有・月１回・以外）
           03  FILLER              PIC X(14)   VALUE   "01 3 114031510".
      *****
      *    （機能強化在支診等）（病床無・難病月２回・１人）
           03  FILLER              PIC X(14)   VALUE   "02 1 114031610".
      *    （機能強化在支診等）（病床無・難病月２回・２〜９人）
           03  FILLER              PIC X(14)   VALUE   "02 2 114031710".
      *    （機能強化在支診等）（病床無・難病月２回・以外）
           03  FILLER              PIC X(14)   VALUE   "02 3 114031810".
      *
      *    （機能強化在支診等）（病床無・月２回・１人）
           03  FILLER              PIC X(14)   VALUE   "02 1 114031910".
      *    （機能強化在支診等）（病床無・月２回・２〜９人）
           03  FILLER              PIC X(14)   VALUE   "02 2 114032010".
      *    （機能強化在支診等）（病床無・月２回・以外）
           03  FILLER              PIC X(14)   VALUE   "02 3 114032110".
      *
      *    （機能強化在支診等）（病床無・月１回・１人）
           03  FILLER              PIC X(14)   VALUE   "02 1 114032210".
      *    （機能強化在支診等）（病床無・月１回・２〜９人）
           03  FILLER              PIC X(14)   VALUE   "02 2 114032310".
      *    （機能強化在支診等）（病床無・月１回・以外）
           03  FILLER              PIC X(14)   VALUE   "02 3 114032410".
      *****
      *    （在支診等）（在支援・難病月２回・１人）
           03  FILLER              PIC X(14)   VALUE   "03 1 114032510".
      *    （在支診等）（在支援・難病月２回・２〜９人）
           03  FILLER              PIC X(14)   VALUE   "03 2 114032610".
      *    （在支診等）（在支援・難病月２回・以外）
           03  FILLER              PIC X(14)   VALUE   "03 3 114032710".
      *
      *    （在支診等）（在支援・月２回・１人）
           03  FILLER              PIC X(14)   VALUE   "03 1 114032810".
      *    （在支診等）（在支援・月２回・２〜９人）
           03  FILLER              PIC X(14)   VALUE   "03 2 114032910".
      *    （在支診等）（在支援・月２回・以外）
           03  FILLER              PIC X(14)   VALUE   "03 3 114033010".
      *
      *    （在支診等）（在支援・月１回・１人）
           03  FILLER              PIC X(14)   VALUE   "03 1 114033110".
      *    （在支診等）（在支援・月１回・２〜９人）
           03  FILLER              PIC X(14)   VALUE   "03 2 114033210".
      *    （在支診等）（在支援・月１回・以外）
           03  FILLER              PIC X(14)   VALUE   "03 3 114033310".
      *****
      *    （在支診等以外）（難病月２回・１人）
           03  FILLER              PIC X(14)   VALUE   "04 1 114033410".
      *    （在支診等以外）（難病月２回・２〜９人）
           03  FILLER              PIC X(14)   VALUE   "04 2 114033510".
      *    （在支診等以外）（難病月２回・以外）
           03  FILLER              PIC X(14)   VALUE   "04 3 114033610".
      *
      *    （在支診等以外）（月２回・１人）
           03  FILLER              PIC X(14)   VALUE   "04 1 114033710".
      *    （在支診等以外）（月２回・２〜９人）
           03  FILLER              PIC X(14)   VALUE   "04 2 114033810".
      *    （在支診等以外）（月２回・以外）
           03  FILLER              PIC X(14)   VALUE   "04 3 114033910".
      *
      *    （在支診等以外）（在支援・月１回・１人）
           03  FILLER              PIC X(14)   VALUE   "04 1 114034010".
      *    （在支診等以外）（在支援・月１回・２〜９人）
           03  FILLER              PIC X(14)   VALUE   "04 2 114034110".
      *    （在支診等以外）（在支援・月１回・以外）
           03  FILLER              PIC X(14)   VALUE   "04 3 114034210".
      *
      *
      *特医総管
      *    （機能強化在支診等）（病床有・難病月２回・１人）
           03  FILLER              PIC X(14)   VALUE   "11 1 114035510".
      *    （機能強化在支診等）（病床有・難病月２回・２〜９人）
           03  FILLER              PIC X(14)   VALUE   "11 2 114035610".
      *    （機能強化在支診等）（病床有・難病月２回・以外）
           03  FILLER              PIC X(14)   VALUE   "11 3 114035710".
      *
      *    （機能強化在支診等）（病床有・月２回・１人）
           03  FILLER              PIC X(14)   VALUE   "11 1 114035810".
      *    （機能強化在支診等）（病床有・月２回・２〜９人）
           03  FILLER              PIC X(14)   VALUE   "11 2 114035910".
      *    （機能強化在支診等）（病床有・月２回・以外）
           03  FILLER              PIC X(14)   VALUE   "11 3 114036010".
      *
      *    （機能強化在支診等）（病床有・月１回・１人）
           03  FILLER              PIC X(14)   VALUE   "11 1 114036110".
      *    （機能強化在支診等）（病床有・月１回・２〜９人）
           03  FILLER              PIC X(14)   VALUE   "11 2 114036210".
      *    （機能強化在支診等）（病床有・月１回・以外）
           03  FILLER              PIC X(14)   VALUE   "11 3 114036310".
      *****
      *    （機能強化在支診等）（病床無・難病月２回・１人）
           03  FILLER              PIC X(14)   VALUE   "12 1 114036410".
      *    （機能強化在支診等）（病床無・難病月２回・２〜９人）
           03  FILLER              PIC X(14)   VALUE   "12 2 114036510".
      *    （機能強化在支診等）（病床無・難病月２回・以外）
           03  FILLER              PIC X(14)   VALUE   "12 3 114036610".
      *
      *    （機能強化在支診等）（病床無・月２回・１人）
           03  FILLER              PIC X(14)   VALUE   "12 1 114036710".
      *    （機能強化在支診等）（病床無・月２回・２〜９人）
           03  FILLER              PIC X(14)   VALUE   "12 2 114036810".
      *    （機能強化在支診等）（病床無・月２回・以外）
           03  FILLER              PIC X(14)   VALUE   "12 3 114036910".
      *
      *    （機能強化在支診等）（病床無・月１回・１人）
           03  FILLER              PIC X(14)   VALUE   "12 1 114037010".
      *    （機能強化在支診等）（病床無・月１回・２〜９人）
           03  FILLER              PIC X(14)   VALUE   "12 2 114037110".
      *    （機能強化在支診等）（病床無・月１回・以外）
           03  FILLER              PIC X(14)   VALUE   "12 3 114037210".
      *****
      *    （在支診等）（在支援・難病月２回・１人）
           03  FILLER              PIC X(14)   VALUE   "13 1 114037310".
      *    （在支診等）（在支援・難病月２回・２〜９人）
           03  FILLER              PIC X(14)   VALUE   "13 2 114037410".
      *    （在支診等）（在支援・難病月２回・以外）
           03  FILLER              PIC X(14)   VALUE   "13 3 114037510".
      *
      *    （在支診等）（在支援・月２回・１人）
           03  FILLER              PIC X(14)   VALUE   "13 1 114037610".
      *    （在支診等）（在支援・月２回・２〜９人）
           03  FILLER              PIC X(14)   VALUE   "13 2 114037710".
      *    （在支診等）（在支援・月２回・以外）
           03  FILLER              PIC X(14)   VALUE   "13 3 114037810".
      *
      *    （在支診等）（在支援・月１回・１人）
           03  FILLER              PIC X(14)   VALUE   "13 1 114037910".
      *    （在支診等）（在支援・月１回・２〜９人）
           03  FILLER              PIC X(14)   VALUE   "13 2 114038010".
      *    （在支診等）（在支援・月１回・以外）
           03  FILLER              PIC X(14)   VALUE   "13 3 114038110".
      *****
      *    （在支診等以外）（難病月２回・１人）
           03  FILLER              PIC X(14)   VALUE   "14 1 114038210".
      *    （在支診等以外）（難病月２回・２〜９人）
           03  FILLER              PIC X(14)   VALUE   "14 2 114038310".
      *    （在支診等以外）（難病月２回・以外）
           03  FILLER              PIC X(14)   VALUE   "14 3 114038410".
      *
      *    （在支診等以外）（月２回・１人）
           03  FILLER              PIC X(14)   VALUE   "14 1 114038510".
      *    （在支診等以外）（月２回・２〜９人）
           03  FILLER              PIC X(14)   VALUE   "14 2 114038610".
      *    （在支診等以外）（月２回・以外）
           03  FILLER              PIC X(14)   VALUE   "14 3 114038710".
      *
      *    （在支診等以外）（月１回・１人）
           03  FILLER              PIC X(14)   VALUE   "14 1 114038810".
      *    （在支診等以外）（月１回・２〜９人）
           03  FILLER              PIC X(14)   VALUE   "14 2 114038910".
      *    （在支診等以外）（月１回・以外）
           03  FILLER              PIC X(14)   VALUE   "14 3 114039010".
      *
      *
       01  TBL-ZISOSIN-AREA-R      REDEFINES   TBL-ZISOSIN-AREA.
           03  TBL-ZISOSIN-OCC     OCCURS   78
                                   INDEXED  TBL-ZSIDX.
      *        同一識別（１桁目　0：在医総管、1：特医総管）
      *                          2:在宅がん医総
               05  TBL-ZS-GRP          PIC X(02).
      *        1:処方せんあり、2:処方せんなし
               05  TBL-ZS-KBN          PIC X(01).
      *        1:同一建物１人、2:同一建物２人から９人、3:１０人〜
               05  TBL-ZS-KBN2         PIC X(01).
      *
               05  TBL-ZS-YOBI         PIC X(01).
      *
               05  TBL-ZS-SRYCD        PIC X(09).
      *
      *
      *H29.2
       01  TBL-ZISOKSN-AREA.
      *在医総管(在支診等)
      *（１人）緩和ケア充実
           03  FILLER              PIC X(14)   VALUE   "0 11 114034570".
      *        実績加算１
           03  FILLER              PIC X(14)   VALUE   "0 21 114034870".
      *        実績加算２
           03  FILLER              PIC X(14)   VALUE   "0 31 114035170".
      * （２人〜９人）緩和ケア充実
           03  FILLER              PIC X(14)   VALUE   "0 12 114034670".
      *        実績加算１
           03  FILLER              PIC X(14)   VALUE   "0 22 114034970".
      *        実績加算２
           03  FILLER              PIC X(14)   VALUE   "0 32 114035270".
      * （１０人〜）緩和ケア充実
           03  FILLER              PIC X(14)   VALUE   "0 13 114034770".
      *        実績加算１
           03  FILLER              PIC X(14)   VALUE   "0 23 114035070".
      *        実績加算２
           03  FILLER              PIC X(14)   VALUE   "0 33 114035370".
      *特医総管(在支診等)
      *（１人）緩和ケア充実
           03  FILLER              PIC X(14)   VALUE   "1 11 114039370".
      *        実績加算１
           03  FILLER              PIC X(14)   VALUE   "1 21 114039670".
      *        実績加算２
           03  FILLER              PIC X(14)   VALUE   "1 31 114039970".
      *（２人〜９人）緩和ケア充実
           03  FILLER              PIC X(14)   VALUE   "1 12 114039470".
      *        実績加算１
           03  FILLER              PIC X(14)   VALUE   "1 22 114039770".
      *        実績加算２
           03  FILLER              PIC X(14)   VALUE   "1 32 114040070".
      *（１０人〜）緩和ケア充実
           03  FILLER              PIC X(14)   VALUE   "1 13 114039570".
      *        実績加算１
           03  FILLER              PIC X(14)   VALUE   "1 23 114039870".
      *        実績加算２
           03  FILLER              PIC X(14)   VALUE   "1 33 114040170".
      *在宅がん医総合
      *        緩和ケア充実
           03  FILLER              PIC X(14)   VALUE   "2 1  114040270".
      *        実績加算１
           03  FILLER              PIC X(14)   VALUE   "2 2  114040370".
      *        実績加算２
           03  FILLER              PIC X(14)   VALUE   "2 3  114040470".
      *
       01  TBL-ZISOKSN-AREA-R      REDEFINES   TBL-ZISOKSN-AREA.
           03  TBL-ZISOKSN-OCC     OCCURS   21
                                   INDEXED  TBL-ZKSIDX.
      *        同一識別（１桁目　0：在医総管、1：特医総管）
      *                          2:在宅がん医総
               05  TBL-ZKS-GRP          PIC X(02).
      *        1:緩和ケア充実、2:実績加算１、3:実績加算２
               05  TBL-ZKS-KBN          PIC X(01).
      *        1:同一建物１人、2:同一建物２人から９人、3:１０人〜
               05  TBL-ZKS-KBN2         PIC X(01).
      *
               05  TBL-ZKS-YOBI         PIC X(01).
      *
               05  TBL-ZKS-SRYCD        PIC X(09).
      *
      *
