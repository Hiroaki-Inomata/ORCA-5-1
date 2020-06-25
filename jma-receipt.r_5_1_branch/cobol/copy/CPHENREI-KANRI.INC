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
      *    オンライン返戻管理   COPY (CPHENREI-KANRI.INC)        *
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.06.01    NACL-藤原    10/12/20  保険者からの返戻対応
      *
      *  04.08.01    NACL-藤原    16/06/27  医療機関コード変更時のレセ電
      *                                     データ等の作成対応
      *                                      （１日からの変更のみ）
      ************************************************************
      *01  HENREI-KANRI-REC.
      *医療機関番号
           03  HENREI-K-HOSPNUM    PIC 9(2).
      *提出先
           03  HENREI-K-TEISYUTUSAKI
                                   PIC X(1).
      *提出先
           03  HENREI-K-TEISYUTUSAKI2
                                   PIC X(1).
      *医療機関コード（REレコードの診療年月に該当）
           03  HENREI-K-HOSPCD     PIC X(7).
      *審査側が処理した年月(HIレコードの年月)
           03  HENREI-K-SYORIYM    PIC X(6).
      *
           03  HENREI-K-RECV-TOTAL-CNT
                                   PIC 9(5).
      *
           03  HENREI-K-RECV-PATIENT-CNT
                                   PIC 9(5).
      *
           03  HENREI-K-RECV-GO-CNT 
                                   PIC 9(5).
      *
           03  HENREI-K-RECV-GO-TEN
                                   PIC 9(10).
      *端末ＩＤ
           03  HENREI-K-TERMID     PIC X(64).
      *オペーレータＩＤ
           03  HENREI-K-OPID       PIC X(16).
      *作成年月日
           03  HENREI-K-CREYMD     PIC X(08).
      *更新年月日
           03  HENREI-K-UPYMD      PIC X(08).
      *更新時間
           03  HENREI-K-UPHMS      PIC X(06).
