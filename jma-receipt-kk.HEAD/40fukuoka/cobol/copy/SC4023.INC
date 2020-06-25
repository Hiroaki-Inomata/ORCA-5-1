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
      *    特定疾病療養受領証保持者の
      *    自己負担分相当額申請書     （福岡県）   COPY (SC4023.INC)
      *
      *  作成日付    作業者        記述
      *  09/02/12    吉川          新規作成
      ********************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者  日付        内容
      *  01.00.01    吉川    2009/03/05  口座名義人追加の為の
      *                                  開設者名バイト数増加
      *  01.00.02    吉川    2009/08/20  2回目の入院期間を追加
      ********************************************************************
       01  SC23.
      *    請求日付
           03  SC23-SEIYMD               PIC X(22).
      *    市町村名
           03  SC23-CITYNAME             PIC X(30).
      *    市町村番号（公費負担者番号）
           03  SC23-CITYNUM              PIC X(16).
      *    診療年月
           03  SC23-SRYYM                PIC X(16).
      *    医療機関コード
           03  SC23-HOSPCDN              PIC X(20).
      *    医療機関所在地
           03  SC23-ADRS                 PIC X(100).
      *    医療機関名
           03  SC23-HOSPNAME             PIC X(100).
      *    開設者名
           03  SC23-KAISETU              PIC X(100).
      *    銀行名
           03  SC23-GINKOU               PIC X(20).
      *    支店名
           03  SC23-SITEN                PIC X(20).
      *    口座区分
           03  SC23-KOUZAKBN             PIC X(02)  OCCURS 2.
      *    口座番号
           03  SC23-KOUZANUM             PIC X(10).
      *    合計請求額
           03  SC23-GSEIKYUGK            PIC X(10).
      *    請求エリア
            03  SC23-SEIKYU-AREA  OCCURS 4.
      *    保険者番号
                05  SC23-HKNJANUM        PIC X(16).
      *    記号・番号
                05  SC23-KIGONUM         PIC X(36).
      *    受給者番号
                05  SC23-JKYSNUM         PIC X(14).
      *    氏名
                05  SC23-NAME            PIC X(20).
      *    入院期間
                05  SC23-NYUINKIKAN      PIC X(14)  OCCURS 2.
      *    入外テーブル
                05  SC23-NYUGAI-TBL  OCCURS 2.
      *    日数
                    07  SC23-NISSU       PIC X(02).
      *    給付外金額
                    07  SC23-KYUFUGAIGK  PIC X(09).
      *    請求金額
                    07  SC23-SEIKYUGK    PIC X(09).
