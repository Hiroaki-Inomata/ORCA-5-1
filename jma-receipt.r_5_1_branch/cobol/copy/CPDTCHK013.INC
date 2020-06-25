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
      *    データチェック中間ファイル     COPY (CPDTCHK013.INC)  *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  DTCHK013-REC.
           03  DTCHK013-KEY.
               05  DTCHK013-PTID               PIC 9(10).
               05  DTCHK013-TANICD             PIC 9(03).
               05  DTCHK013-SRYCD              PIC X(09).
               05  DTCHK013-SHONUM             PIC 9(03).
               05  DTCHK013-SRYYMD             PIC X(08).
               05  DTCHK013-SRYSTYMD           PIC X(08).
               05  DTCHK013-SRYEDYMD           PIC X(08).
               05  DTCHK013-MSTKBN             PIC X(01).
      *    単位名称
           03  DTCHK013-TANINAME               PIC X(12).
      *    単位当たりの回数上限
           03  DTCHK013-LIMIT                  PIC 9(05).
      *    月数（単位コード901（月数フリー設定の場合）
           03  DTCHK013-TSUKISU                PIC 9(02).
      *    特例設定
           03  DTCHK013-TOKUREI                PIC 9(01).
      *    単位当たりの回数合計
           03  DTCHK013-KAISU-TOTAL            PIC 9(05).
           03  DTCHK013-DAY-G.
               05  DTCHK013-DAY                PIC 9(05)
                                               OCCURS 31.
      *    診療年月（週）の回数
           03  DTCHK013-KAISU1                 PIC 9(05).
      *    診療年月（週）外の回数
           03  DTCHK013-KAISU2                 PIC 9(05).
      *    診療年月（週）外最終算定日
           03  DTCHK013-OUTYMD                 PIC X(08).
           03  DTCHK013-NYUINYMD               PIC X(08).
           03  DTCHK013-TAIINYMD               PIC X(08).
           03  DTCHK013-SRYYM                  PIC X(06).
           03  DTCHK013-WEEK                   PIC 9(01).
           03  DTCHK013-JGNCNTERR              PIC 9(01).
           03  DTCHK013-NYUGAIKBN              PIC X(01).
      *    診療年月の保険情報
           03  DTCHK013-HKNCOMBI-MAX           PIC 9(03).
           03  DTCHK013-HKNCOMBI               PIC 9(04)
                                               OCCURS 40.
