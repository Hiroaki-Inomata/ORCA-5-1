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
      *    オンラインレセ電管理マスタ   COPY (CPONRECE-KANRI.INC)*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.08.01    NACL-藤原    16/06/27  医療機関コード変更時のレセ電
      *                                     データ等の作成対応
      *                                      （１日からの変更のみ）
      ************************************************************
      *01  ONRECE-KANRI-REC.
      *医療機関番号
           03  ONRECE-K-HOSPNUM    PIC 9(2).
      *提出先
           03  ONRECE-K-TEISYUTUSAKI
                                   PIC X(1).
      *医療機関コード
           03  ONRECE-K-HOSPCD     PIC X(7).
      *診療年月
           03  ONRECE-K-SRYYM      PIC X(6).
      *受付回数
           03  ONRECE-K-UCNT       PIC 9(3).
      *請求回数
           03  ONRECE-K-SCNT       PIC 9(3).
      *個別レセ電実施時の回数(暫定 初回は 1)
           03  ONRECE-K-RENNUM     PIC 9(3).
      *オンラインレセ電実施日
           03  ONRECE-K-SYORIYMD   PIC X(8).
      *
           03  ONRECE-K-UKEYMD     PIC X(8).
      *
           03  ONRECE-K-SKYYMD     PIC X(8).
      *受付事務点検ファイル格納フラグ（格納：１）
           03  ONRECE-K-RECV-FLG   PIC 9(1).
      *請求確定ファイル格納フラグ（格納：１）
            03  ONRECE-K-AMOUNT-FLG PIC 9(1).
      *エラーなしで送信した件数
           03  ONRECE-K-SEND-PATIENT-CNT
                                   PIC 9(5).
      *エラーありで送信しなかった件数
           03  ONRECE-K-NOSEND-PATIENT-CNT
                                   PIC 9(5).
      *合計件数(上の総和)
           03  ONRECE-K-SEND-TOTAL-CNT 
                                   PIC 9(5).
      *端末ＩＤ
           03  ONRECE-K-TERMID     PIC X(64).
      *オペーレータＩＤ
           03  ONRECE-K-OPID       PIC X(16).
      *作成年月日
           03  ONRECE-K-CREYMD     PIC X(08).
      *更新年月日
           03  ONRECE-K-UPYMD      PIC X(08).
      *更新時間
           03  ONRECE-K-UPHMS      PIC X(06).
      *更新用項目 
           03  ONRECE-K-VIRTUAL.
              05  ONRECE-K-UCNT-V  PIC 9(3).
              05  ONRECE-K-SCNT-V  PIC 9(3).
