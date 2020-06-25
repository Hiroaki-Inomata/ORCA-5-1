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
      *    オンライン返戻明細    COPY (CPHENREI-BODY.INC)        *
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.06.01    NACL-藤原    10/10/12  公費単独のレセ分離対応
      *                                     kohid 追加
      *  04.06.02    NACL-藤原    10/12/20  保険者からの返戻対応
      *  04.06.03    NACL-藤原    12/10/26  特記事項０７（老併）及び
      *                                     ０８（老健）のレセプト対応
      *                                     receipt_kbn 追加
      *
      *  04.08.01    NACL-藤原    16/08/08  災害該当分（非減免対象と減免対象）
      *                                     レセプトの分割対応
      *                                     disaster s_disaster 1-10 追加
      *
      *  04.08.02    NACL-藤原    16/06/27  医療機関コード変更時のレセ電
      *                                     データ等の作成対応
      *                                      （１日からの変更のみ）
      ************************************************************
      *01  HENREI-BODY-REC.
           03  HENREI-B-HOSPNUM    PIC 9(2).
           03  HENREI-B-TEISYUTUSAKI
                                   PIC X(1).
           03  HENREI-B-TEISYUTUSAKI2
                                   PIC X(1).
           03  HENREI-B-HOSPCD     PIC X(7).
           03  HENREI-B-SYORIYM    PIC X(6).
           03  HENREI-B-RECEIPT-NO PIC X(6).
           03  HENREI-B-PT-RENNUM  PIC 9(4).
      *
           03  HENREI-B-SELECT-FLG PIC 9(1).
      *
           03  HENREI-B-RECESYUBETU
                                   PIC X(4).
           03  HENREI-B-NYUGAIKBN  PIC X(1).
           03  HENREI-B-SRYYM      PIC X(6).
           03  HENREI-B-NAME       PIC X(40).
           03  HENREI-B-SEX        PIC X(1).
           03  HENREI-B-BIRTHDAY   PIC X(8).
           03  HENREI-B-PTNUM      PIC X(20).
           03  HENREI-B-PTID       PIC 9(10).
           03  HENREI-B-HKNJANUM   PIC X(8).
           03  HENREI-B-KIGO       PIC X(40).
           03  HENREI-B-NUM        PIC X(40).
           03  HENREI-B-RECEIPT-KBN
                                   PIC X(10).
           03  HENREI-B-KO-AREA.
               05  HENREI-B-KO-OCC OCCURS  4.
                   07  HENREI-B-FTNNUM PIC X(8).
                   07  HENREI-B-JKYNUM PIC X(7).
           03  HENREI-B-DISASTER   PIC X(2).
      *
           03  HENREI-B-SEARCH-NO  PIC X(30).
           03  HENREI-B-CHECK-NO   PIC X(100).
           03  HENREI-B-HR-CODE    PIC X(5).
           03  HENREI-B-HR-MSG     PIC X(1000).
           03  HENREI-B-HENREI-INFO1
                                   PIC X(20).
           03  HENREI-B-HENREI-INFO2
                                   PIC X(3000).
           03  HENREI-B-ERRCD      PIC X(03).
      *
           03  HENREI-B-SEIKYU-AREA.
               05  HENREI-B-SEIKYU-OCC OCCURS  10.
                   07  HENREI-B-S-SYORIYMD     PIC X(8).
                   07  HENREI-B-S-MAKE-FLG     PIC 9(01).
                   07  HENREI-B-S-HENREIYM     PIC X(06).
                   07  HENREI-B-S-HENREI-SIJI-FLG
                                               PIC 9(01).
      *
                   07  HENREI-B-S-TEISYUTUSAKI PIC X(01).
                   07  HENREI-B-S-NYUGAIKBN    PIC X(01).
                   07  HENREI-B-S-RECEKA       PIC X(02).
                   07  HENREI-B-S-RECESYUBETU  PIC X(04).
                   07  HENREI-B-S-HKNJANUM     PIC X(08).
                   07  HENREI-B-S-HOJOKBN-KEY  PIC X(01).
                   07  HENREI-B-S-KOHID-KEY    PIC 9(10).
                   07  HENREI-B-S-TEKSTYMD     PIC X(08).
                   07  HENREI-B-S-RECEIPT-KBN  PIC X(10).
      *
                   07  HENREI-B-S-KIGO         PIC X(40).
                   07  HENREI-B-S-DISASTER     PIC X(02).
      *端末ＩＤ
           03  HENREI-B-TERMID     PIC X(64).
      *オペーレータＩＤ
           03  HENREI-B-OPID       PIC X(16).
      *作成年月日
           03  HENREI-B-CREYMD     PIC X(08).
      *更新年月日
           03  HENREI-B-UPYMD      PIC X(08).
      *更新時間
           03  HENREI-B-UPHMS      PIC X(06).
      *検索用項目
           03  HENREI-B-XX-AREA.
      *審査支払機関検索用項目
               05  HENREI-B-XXTEISYUTUSAKI
                                   PIC X(1).

