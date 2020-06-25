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
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     @@@@-門間    11/01/07  コメント履歴削除
      ********************************************************************
        01  KOHS-DATA.
      *    市町村名
            03  KOHS-CITYNAME                 PIC X(80).
      *    公費名
            03  KOHS-KOHNAME-TBL.
                05  KOHS-KOHNAME              PIC X(10) OCCURS 15.
      *    ID(患者番号)
            03  KOHS-PTNUM-TBL.
                05  KOHS-PTNUM                PIC X(18) OCCURS 15.
      *    氏名
            03  KOHS-NAME-TBL.
                05  KOHS-NAME                 PIC X(20) OCCURS 15.
      *    フリガナ
            03  KOHS-FURIGANA-TBL.
                05  KOHS-FURIGANA             PIC X(24) OCCURS 15.
      *    負担者番号／受給者番号
            03  KOHS-FTNJA-JKYS-TBL OCCURS 15.
                05  KOHS-FTNJANUM             PIC X(28).
                05  KOHS-JKYSNUM              PIC X(28).
      *    診療年月
            03  KOHS-SRYYM-TBL.
                05  KOHS-SRYYM                PIC X(12) OCCURS 15.
      *    性別
            03  KOHS-SEX-TBL.
                05  KOHS-SEX                  PIC X(02) OCCURS 15.
      *    入外区分
            03  KOHS-NYUGAIKBN-TBL.
                05  KOHS-NYUGAIKBN            PIC X(02) OCCURS 15.
      *    保険者番号
            03  KOHS-HKNJANUM-TBL.
                05  KOHS-HKNJANUM             PIC X(16) OCCURS 15.
      *    記号・番号
            03  KOHS-KIGONUM-TBL OCCURS 15.
                05  KOHS-KIGO                 PIC X(16).
                05  KOHS-NUM                  PIC X(16).
      *    実日数
            03  KOHS-NISSU-TBL.
                05  KOHS-NISSU                PIC X(04) OCCURS 15.
      *    点数
            03  KOHS-TENSU-TBL.
                05  KOHS-TENSU                PIC X(18) OCCURS 15.
      *    本人負担額
            03  KOHS-ITBFTN-TBL.
                05  KOHS-ITBFTN               PIC X(18) OCCURS 15.
      *    食事回数
            03  KOHS-SKJKAISU-TBL.
                05  KOHS-SKJKAISU             PIC X(04) OCCURS 15.
      *    食事療養費―請求額
            03  KOHS-SKJSKY-TBL.
                05  KOHS-SKJSKY               PIC X(18) OCCURS 15.
      *    食事療養費―標準負担額
            03  KOHS-SKJFTN-TBL.
                05  KOHS-SKJFTN               PIC X(18) OCCURS 15.
      *    診療科名
            03  KOHS-SRYKANAME-TBL.
                05  KOHS-SRYKANAME            PIC X(10) OCCURS 15.
      *    給付割合
            03  KOHS-KYURATE-TBL.
                05  KOHS-KYURATE              PIC X(04) OCCURS 15.
      *    他法公費(法別番号)
            03  KOHS-TAHO-TBL.
                05  KOHS-TAHO                 PIC X(22) OCCURS 15.
