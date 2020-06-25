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
      *-----------------------------------------------------------------*
      *    向精神薬長期処方成分コードテーブル COPY (CMPSYCHOTROPIC2.INC)
      *-----------------------------------------------------------------*
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     NACL-伊藤    19/01/31  向精神薬長期処方の対象薬剤
      *****************************************************************
      *
       01  TBL-PSYCHO2-CODE-AREA.
      *
      * 抗不安薬
      * 01
      *    オキサゾラム
           03  FILLER  PIC X(35)   VALUE
               "1124013 00000000 99999999 1124013".
      *    クロキサゾラム
           03  FILLER  PIC X(35)   VALUE
               "1124014 00000000 99999999 1124014".
      *    クロラゼプ酸二カリウム
           03  FILLER  PIC X(35)   VALUE
               "1124015 00000000 99999999 1124015".
      *    ジアゼパム
           03  FILLER  PIC X(35)   VALUE
               "1124017 00000000 99999999 1124017".
           03  FILLER  PIC X(35)   VALUE
               "1124701 00000000 99999999 1124017".
      *    フルジアゼパム
           03  FILLER  PIC X(35)   VALUE
               "1124019 00000000 99999999 1124019".
      *    ブロマゼパム
           03  FILLER  PIC X(35)   VALUE
               "1124020 00000000 99999999 1124020".
           03  FILLER  PIC X(35)   VALUE
               "1124700 00000000 99999999 1124020".
      *    メダゼパム
           03  FILLER  PIC X(35)   VALUE
               "1124021 00000000 99999999 1124021".
      *    ロラゼパム
           03  FILLER  PIC X(35)   VALUE
               "1124022 00000000 99999999 1124022".
      * 11
      *    アルプラゾラム
           03  FILLER  PIC X(35)   VALUE
               "1124023 00000000 99999999 1124023".
      *    フルタゾラム
           03  FILLER  PIC X(35)   VALUE
               "1124024 00000000 99999999 1124024".
      *    メキサゾラム
           03  FILLER  PIC X(35)   VALUE
               "1124025 00000000 99999999 1124025".
      *    フルトプラゼパム
           03  FILLER  PIC X(35)   VALUE
               "1124027 00000000 99999999 1124027".
      *    クロルジアゼポキシド
           03  FILLER  PIC X(35)   VALUE
               "1124028 00000000 99999999 1124028".
      *    ロフラゼプ酸エチル
           03  FILLER  PIC X(35)   VALUE
               "1124029 00000000 99999999 1124029".
      *    クロチアゼパム
           03  FILLER  PIC X(35)   VALUE
               "1179012 00000000 99999999 1179012".
      *    エチゾラム
           03  FILLER  PIC X(35)   VALUE
               "1179025 00000000 99999999 1179025".
      * 睡眠薬
      *    エスタゾラム
           03  FILLER  PIC X(35)   VALUE
               "1124001 00000000 99999999 1124001".
      *    フルラゼパム塩酸塩
           03  FILLER  PIC X(35)   VALUE
               "1124002 00000000 99999999 1124002".
      * 21
      *    ニトラゼパム
           03  FILLER  PIC X(35)   VALUE
               "1124003 00000000 99999999 1124003".
      *    ニメタゼパム
           03  FILLER  PIC X(35)   VALUE
               "1124004 00000000 99999999 1124004".
      *    ハロキサゾラム
           03  FILLER  PIC X(35)   VALUE
               "1124005 00000000 99999999 1124005".
      *    トリアゾラム
           03  FILLER  PIC X(35)   VALUE
               "1124007 00000000 99999999 1124007".
      *    フルニトラゼパム
           03  FILLER  PIC X(35)   VALUE
               "1124008 00000000 99999999 1124008".
      *    ブロチゾラム
           03  FILLER  PIC X(35)   VALUE
               "1124009 00000000 99999999 1124009".
      *    ロルメタゼパム
           03  FILLER  PIC X(35)   VALUE
               "1124010 00000000 99999999 1124010".
      *    クアゼパム
           03  FILLER  PIC X(35)   VALUE
               "1124030 00000000 99999999 1124030".
      *    リルマザホン塩酸塩水和物
           03  FILLER  PIC X(35)   VALUE
               "1129006 00000000 99999999 1129006".
      *    ゾピクロン
           03  FILLER  PIC X(35)   VALUE
               "1129007 00000000 99999999 1129007".
      * 31
      *    ゾルピデム酒石酸塩
           03  FILLER  PIC X(35)   VALUE
               "1129009 00000000 99999999 1129009".
      *    エスゾピクロン
           03  FILLER  PIC X(35)   VALUE
               "1129010 00000000 99999999 1129010".
      * 抗てんかん薬
      *    クロナゼパム
           03  FILLER  PIC X(35)   VALUE
               "1139003 00000000 99999999 1139003".
      *    クロバザム
           03  FILLER  PIC X(35)   VALUE
               "1139006 00000000 99999999 1139006".
      * 麻酔導入薬・鎮静薬
      *    ミダゾラム（注射）
      **   03  FILLER  PIC X(35)   VALUE
      **       "1124401 00000000 99999999 1124401".
      **   03  FILLER  PIC X(35)   VALUE
      **       "1139401 00000000 99999999 1124401".
      *
       01  TBL-PSYCHO2-CODE-AREA-R REDEFINES   TBL-PSYCHO2-CODE-AREA.
           03  TBL-PSYCHO2-OCC     OCCURS  34  INDEXED   TBL-PSY2.
               05  TBL-PSYCHO2-CODE        PIC X(07).
               05  TBL-PSYCHO2-YOBI1       PIC X(01).
               05  TBL-PSYCHO2-STYMD       PIC X(08).
               05  TBL-PSYCHO2-YOBI2       PIC X(01).
               05  TBL-PSYCHO2-EDYMD       PIC X(08).
               05  TBL-PSYCHO2-YOBI3       PIC X(01).
               05  TBL-PSYCHO2-GROUP       PIC X(07).
               05  TBL-PSYCHO2-YOBI        PIC X(02).
      *
