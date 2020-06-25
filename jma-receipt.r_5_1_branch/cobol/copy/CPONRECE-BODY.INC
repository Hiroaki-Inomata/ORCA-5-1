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
      *    オンラインレセ電明細マスタ    COPY (CPONRECE-BODY.INC)*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.06.01    NACL-藤原    10/10/12  公費単独のレセ分離対応
      *                                     kohid 追加
      *  04.06.02    NACL-藤原    12/10/26  特記事項０７（老併）及び
      *                                     ０８（老健）のレセプト対応
      *                                     receipt_kbn 追加 
      *
      *  04.08.01    NACL-藤原    16/06/27  医療機関コード変更時のレセ電
      *                                     データ等の作成対応
      *                                      （１日からの変更のみ）
      ************************************************************
      *01  ONRECE-BODY-REC.
           03  ONRECE-B-HOSPNUM    PIC 9(2).
           03  ONRECE-B-TEISYUTUSAKI
                                   PIC X(1).
           03  ONRECE-B-HOSPCD     PIC X(7).
           03  ONRECE-B-SRYYM      PIC X(6).
           03  ONRECE-B-UCNT       PIC 9(3).
           03  ONRECE-B-SCNT       PIC 9(3).
           03  ONRECE-B-RENNUM     PIC 9(3).
           03  ONRECE-B-RECEIPT-NO PIC 9(6).
      *    オンラインレセ電実施日
           03  ONRECE-B-SYORIYMD   PIC X(8).
           03  ONRECE-B-UKEYMD     PIC X(8).
           03  ONRECE-B-SKYYMD     PIC X(8).
           03  ONRECE-B-RECV-FLG   PIC 9(1).
           03  ONRECE-B-AMOUNT-FLG PIC 9(1).
           03  ONRECE-B-NYUGAIKBN  PIC X(1).
           03  ONRECE-B-RECESYUBETU
                                   PIC X(4).
           03  ONRECE-B-SRYYM-B    PIC X(6).
           03  ONRECE-B-NAME       PIC X(40).
           03  ONRECE-B-SEX        PIC X(1).
           03  ONRECE-B-BIRTHDAY   PIC X(8).
           03  ONRECE-B-PTNUM      PIC X(20).
           03  ONRECE-B-PTID       PIC 9(10).
           03  ONRECE-B-HKNJANUM   PIC X(8).
           03  ONRECE-B-KIGO       PIC X(40).
           03  ONRECE-B-NUM        PIC X(40).
           03  ONRECE-B-HKNNUM     PIC X(03).
           03  ONRECE-B-TEISYUTUSAKI-B
                                   PIC X(1).
           03  ONRECE-B-RECEKA     PIC X(02).
           03  ONRECE-B-HOJOKBN    PIC X(01).
           03  ONRECE-B-KOHID      PIC 9(10).
           03  ONRECE-B-TEKSTYMD   PIC X(08).
           03  ONRECE-B-RECEIPT-KBN
                                   PIC X(10).
           03  ONRECE-B-H-TTL-TEN  PIC 9(8).
           03  ONRECE-B-KO-AREA.
              05  ONRECE-B-KO-A        OCCURS  4.
                07  ONRECE-B-FTNNUM    PIC X(8).
                07  ONRECE-B-JKYNUM    PIC X(7).
                07  ONRECE-B-K-TTL-TEN PIC 9(8).
           03  ONRECE-B-KAKUTEI-KBN
                                   PIC X(1).
           03  ONRECE-B-ERR-KBN    PIC X(2).
           03  ONRECE-B-SEIKYU-INFO
                                   PIC X(40).
           03  ONRECE-B-U-ERR-CODE PIC X(4).
           03  ONRECE-B-U-ERR-MSG-1
                                   PIC X(400).
           03  ONRECE-B-U-ERR-MSG-2
                                   PIC X(512).
           03  ONRECE-B-U-ERR-SRYKBN
                                   PIC X(2).
           03  ONRECE-B-U-ERR-INFO PIC X(400).
           03  ONRECE-B-U-TEN-NG   PIC 9(8).
           03  ONRECE-B-U-TEN-OK   PIC 9(8).
           03  ONRECE-B-U-HENREI-FLG
                                   PIC X(01).
           03  ONRECE-B-U-YOBI1    PIC X(10). 
           03  ONRECE-B-U-YOBI2    PIC X(10). 
           03  ONRECE-B-END-FLG    PIC 9(1).
      *端末ＩＤ
           03  ONRECE-B-TERMID     PIC X(64).
      *オペーレータＩＤ
           03  ONRECE-B-OPID       PIC X(16).
      *作成年月日
           03  ONRECE-B-CREYMD     PIC X(08).
      *更新年月日
           03  ONRECE-B-UPYMD      PIC X(08).
      *更新時間
           03  ONRECE-B-UPHMS      PIC X(06).
      *更新用項目 
           03  ONRECE-B-VIRTUAL.
              05  ONRECE-B-UCNT-V  PIC 9(3).
              05  ONRECE-B-SCNT-V  PIC 9(3).
              05  ONRECE-B-XXTEISYUTUSAKI
                                   PIC X(1).

