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
      ******************************************************************
      *    ＡＰＩレスポンス保険者情報         COPY (CPINSPROV2RES.INC)
      ******************************************************************
        01  XML-INSPROGETRES.
            02  INSPRORES-CONTEXT            PIC S9(9)   BINARY.
            02  INSPRORES-OBJECT             PIC X(36).
            02  INSPRORES-MODE               PIC S9(9)   BINARY.
            02  INSPRORES-RECORDNAME         PIC X(128).
            02  INSPRORES-APPOINTRES.
      *       実施日
              03  INSPRORES-INFORMATION-DATE    PIC X(10).
      *       実施時間
              03  INSPRORES-INFORMATION-TIME    PIC X(8).
      *       返却コード
              03  INSPRORES-API-RESULT          PIC X(2).
      *       返却メッセージ
              03  INSPRORES-API-RESULT-MSG      PIC X(100).
      *
              03  INSPRORES-RESKEY              PIC X(100).
      *       保険者番号一覧
              03  INSPRORES-INSURANCE-INFOMATION.
                  05  INSPRORES-INSURANCE-OCC   OCCURS   2500.
      *               保険者番号
                      07  INSPRORES-INSPRO-NUMBER    PIC X(08).
      *               保険者名
                      07  INSPRORES-INSPRO-NAME      PIC X(200).
      *               保険者名略称１
                      07  INSPRORES-INSPRO-NAME1     PIC X(100).
      *               保険者名略称２
                      07  INSPRORES-INSPRO-NAME2     PIC X(40).
      *               保険者名略称３
                      07  INSPRORES-INSPRO-NAME3     PIC X(40).
      *               保険番号
                      07  INSPRORES-INS-NUMBER       PIC X(03).
      *               保険番号名称
                      07  INSPRORES-INS-NUMBER-NAME  PIC X(20).
      *               郵便番号
                      07  INSPRORES-INSE-ZIP-CODE      PIC X(7).
      *               住所
                      07  INSPRORES-INS-ADDRESSES1    PIC X(200).
      *               番地番号
                      07  INSPRORES-INS-ADDRESSES2    PIC X(200).
      *               電話番号
                      07  INSPRORES-INS-HPONE         PIC X(15).
      *               記号
                      07  INSPRORES-INS-SYMBOL        PIC X(80).
      *               給付割合（本人・外来）
                      07  INSPRORES-HON-GAIKYURATE    PIC X(03).
      *               給付割合（本人・入院）
                      07  INSPRORES-HON-NYUKYURATE    PIC X(03).
      *               給付割合（家族・外来）
                      07  INSPRORES-KZK-GAIKYURATE    PIC X(03).
      *               給付割合（家族・入院）
                      07  INSPRORES-KZK-NYUKYURATE    PIC X(03).
      *               異動区分
                      07  INSPRORES-CHANGE-MEMO       PIC X(10).
      *               異動年月日
                      07  INSPRORES-CHANGE-DATE       PIC X(10).
      *
      *
