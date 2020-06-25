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
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.00     @@@@-田中    11/04/04  新規作成
      * 01.00.00     @@@@-田中    11/09/29  集計対象日を範囲指定に変更
      ********************************************************************
       01  D600.
      *    ジョブメッセージ
           03  D600-JOBMSG            PIC X(60).
      *    頁数
           03  D600-PAGE              PIC X(03).
      *    集計対象日付
           03  D600-SYSYMD            PIC X(22).
      *    見出し
           03  D600-MIDASI            PIC X(10).
      *    入金返金日
           03  D600-NYUHEN-YMD        PIC X(50).
      *    入外区分
           03  D600-NYUGAIKBN         PIC X(04).
      *    診療科
           03  D600-SRYKA             PIC X(20).
      *    入院料（項目名称）
           03  D600-NYUINRYO          PIC X(08).
      *    療養手当て（項目名称）
           03  D600-TRYO              PIC X(08).
      *    連番号テーブル
           03  D600-RENNUM-TBL        OCCURS 21 TIMES.
      *    連番
               05  D600-RENNUM        PIC X(07).
      *    患者番号テーブル
           03  D600-PTNUM-TBL         OCCURS 21 TIMES.
      *    患者番号（１０桁以下）
               05  D600-PTNUM         PIC X(10).
      *    患者番号（１１桁以上）
               05  D600-PTNUM2        PIC X(20).
      *    患者氏名テーブル
           03  D600-NAME-TBL          OCCURS 21 TIMES.
      *    患者氏名
               05  D600-NAME          PIC X(18).
      *    性別テーブル
           03  D600-SEX-TBL           OCCURS  21 TIMES.
      *    性別
               05  D600-SEX           PIC X(02).
      *    年齢テーブル
           03  D600-AGE-TBL           OCCURS  21 TIMES.
      *    年齢
               05  D600-AGE           PIC X(03).
      *    本人家族テーブル
           03  D600-HKNKBN-TBL        OCCURS  21 TIMES.
      *    本人家族
               05  D600-HKNKBN        PIC X(02).
      *    保険種類テーブル
           03  D600-HKNS-TBL          OCCURS  21 TIMES.
      *    保険種類
               05  D600-HKNS          PIC X(08).
      *    保険点数テーブル
           03  D600-HKNTEN-TBL        OCCURS  21 TIMES.
      *    保険点数
               05  D600-HKNTEN        PIC X(07).
      *    診療区分テーブル
           03  D600-TEN-HK-TBL        OCCURS  16 TIMES.
      *    保険点数テーブル（診療区分別）
               05  D600-TEN-TBL       OCCURS  21 TIMES.
      *    保険点数（診療区分別）
                   07  D600-TEN       PIC X(07).
      *    保険給付額テーブル
           03  D600-HKNFTN-TBL        OCCURS  21 TIMES.
      *    保険給付額
               05  D600-HKNFTN        PIC X(08).
      *    自費負担額テーブル
           03  D600-JIHIFTN-TBL       OCCURS  21 TIMES.
      *    自費負担額
               05  D600-JIHIFTN       PIC X(08).
      *    窓口請求額テーブル
           03  D600-SEIKYU-TBL        OCCURS  21 TIMES.
      *    窓口請求額
               05  D600-SEIKYU        PIC X(08).
      *    窓口領収額テーブル
           03  D600-RYOSYU-TBL        OCCURS  21 TIMES.
      *    窓口領収額
               05  D600-RYOSYU        PIC X(08).
      *    保険点数―合計（７桁）
           03  D600-G-HKNTEN          PIC X(07).
      *    保険点数―合計（８桁）
           03  D600-GG-HKNTEN         PIC X(08).
      *    保険点数―合計テーブル（診療区分別）
           03  D600-G-TEN-HK-TBL      OCCURS  16 TIMES.
      *    保険点数―合計（診療区分別）（７桁）
               05  D600-G-TEN         PIC X(07).
      *    保険点数―合計テーブル（診療区分別）
           03  D600-GG-TEN-HK-TBL     OCCURS  16 TIMES.
      *    保険点数―合計（診療区分別）（８桁）
               05  D600-GG-TEN        PIC X(08).
      *    保険給付額―合計（８桁）
           03  D600-G-HKNFTN          PIC X(08).
      *    保険給付額―合計（９桁）
           03  D600-GG-HKNFTN         PIC X(09).
      *    自費負担額―合計（８桁）
           03  D600-G-JIHIFTN         PIC X(08).
      *    自費負担額―合計（９桁）
           03  D600-GG-JIHIFTN        PIC X(09).
      *    窓口請求額―合計（８桁）
           03  D600-G-SEIKYU          PIC X(08).
      *    窓口請求額―合計（９桁）
           03  D600-GG-SEIKYU         PIC X(09).
      *    窓口領収額―合計（８桁）
           03  D600-G-RYOSYU          PIC X(08).
      *    窓口領収額―合計（９桁）
           03  D600-GG-RYOSYU         PIC X(09).
