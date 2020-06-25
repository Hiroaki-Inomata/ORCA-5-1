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
      *  主科テーブル       (CPSYUKA.INC)
      *****************************************************************
      *  修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.02.01    NACL-藤原    06/08/11  患者番号を追加
      *
      *  04.06.01    NACL-藤原    10/10/12  公費単独のレセ分離対応
      *                                     kohid 追加
      *  04.06.02    NACL-藤原    12/10/26  特記事項０７（老併）及び
      *                                     ０８（老健）のレセプト対応
      *                                     receipt_kbn 追加 
      *****************************************************************
      *
      *01  SYUKA-REC.
           03  SYUKA-KEY.
      *医療機関ＩＤ
               05  SYUKA-HOSPNUM        PIC 9(2).
      *入外区分
               05  SYUKA-NYUGAIKBN     PIC X(01).
      *患者ＩＤ
               05  SYUKA-PTID          PIC 9(10).
      *会計区分
               05  SYUKA-TEN-KBN       PIC X(01).
      *診療年月
               05  SYUKA-SRYYM         PIC X(06).
      *審査支払機関
               05  SYUKA-TEISYUTUSAKI  PIC X(01).
      *保険者番号
               05  SYUKA-HKNJANUM      PIC X(08).
      *補助区分
               05  SYUKA-HOJOKBN       PIC X(01).
      *公費ＩＤ
               05  SYUKA-KOHID         PIC 9(10).
      *分割区分
               05  SYUKA-RECEIPT-KBN   PIC X(10).
      *レセプト種別
               05  SYUKA-RECESYUBETU   PIC X(04).
      *診療科
               05  SYUKA-SRYKA         PIC X(02).
      *保険番号（主保険）
           03  SYUKA-HKNNUM            PIC X(03).
      *診療科（入力）
           03  SYUKA-SRYKA-INPUT       PIC X(02).
      *診療科（システム）
           03  SYUKA-SRYKA-AUTO        PIC X(02).
      *保険組合せ番号
           03  SYUKA-HKNCOMBI-AREA.
               05  SYUKA-HKNCOMBI      PIC 9(04)   OCCURS  10.
      *請求点数合計
           03  SYUKA-TENSU             PIC S9(09).
      *受診回数合計(入院外)
           03  SYUKA-KAISU             PIC 9(03).
      *通算日数合計(入院)
           03  SYUKA-NISSU             PIC 9(03).
      *終了日
           03  SYUKA-EDDAY             PIC 9(02).
      *未請求フラグ
           03  SYUKA-MISEIKYU-FLG      PIC X(02).
      *削除フラグ
           03  SYUKA-DLT-FLG           PIC X(01).
      *端末ＩＤ
           03  SYUKA-TERMID            PIC X(16).
      *オペーレータＩＤ
           03  SYUKA-OPID              PIC X(16).
      *作成年月日
           03  SYUKA-CREYMD            PIC X(08).
      *更新年月日
           03  SYUKA-UPYMD             PIC X(08).
      *更新時間
           03  SYUKA-UPHMS             PIC X(06).
      *検索用項目
           03  SYUKA-PTNUM             PIC X(20).
