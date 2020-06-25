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
      *****************************************************************
      *  QRコード(被保険者証)格納用               (CPQRCDHKN.INC) 
      *****************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  QRCDHKN-REC.
      *
      *キー
           03  QRCDHKN-KEY.
      *医療機関ＩＤ
               05  QRCDHKN-HOSPNUM         PIC 9(2).
      *受付年月日
               05  QRCDHKN-REGISTYMD       PIC X(08).
      *受付ＩＤ
               05  QRCDHKN-REGISTID       PIC 9(05).
      *受付時間
           03  QRCDHKN-REGISTTIME          PIC X(06).
      *制度区分(必須)
           03  QRCDHKN-SEIDOKBN            PIC X(03).
      *保険者番号(必須)
           03  QRCDHKN-HKNJANUM            PIC X(08).
      *記号番号(必須)
           03  QRCDHKN-KIGONUM             PIC X(40).
      *本人家族区分(必須)
           03  QRCDHKN-HONKZKKBN           PIC X(01).
      *カナ氏名(任意)（全角）
           03  QRCDHKN-KANANAME            PIC X(100).
      *性別(必須)
           03  QRCDHKN-SEX                 PIC X(01).
      *生年月日(必須)
           03  QRCDHKN-BIRTHDAY            PIC X(07).
      *-------------> ここまで固定長
      *保険者独自の管理番号(任意)
           03  QRCDHKN-KANRINUM            PIC X(100).
      *漢字氏名(任意)
           03  QRCDHKN-NAME                PIC X(100).
      *給付割合(任意)
           03  QRCDHKN-KYURATE             PIC X(02).
      *再発行コード(任意)
           03  QRCDHKN-SAIHAKKU            PIC X(50).
      *-------------> ここまで入力情報
      *患者ＩＤ
           03  QRCDHKN-PTID                PIC 9(10).
      *記号
           03  QRCDHKN-KIGO                PIC  X(80).
      *番号
           03  QRCDHKN-NUM                 PIC  X(80).
      *生年月日(西暦)
           03  QRCDHKN-BIRTHDAYS           PIC X(08).
      *エラーコード
           03  QRCDHKN-ERRCD               PIC X(02).
      *更新済フラグ
           03  QRCDHKN-MOD-FLG             PIC 9(01).
      *受付済フラグ
           03  QRCDHKN-MOD-FLG2            PIC 9(01).
      *削除区分
           03  QRCDHKN-DELKBN              PIC X(01).
      *端末ＩＤ
           03  QRCDHKN-TERMID              PIC X(64).
      *オペーレータＩＤ
           03  QRCDHKN-OPID                PIC X(16).
      *作成年月日
           03  QRCDHKN-CREYMD              PIC X(08).
      *更新年月日
           03  QRCDHKN-UPYMD               PIC X(08).
      *更新時間
           03  QRCDHKN-UPHMS               PIC X(06).
