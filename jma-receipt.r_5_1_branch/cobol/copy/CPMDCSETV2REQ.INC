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
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      ******************************************************************
      *    ＡＰＩ　セット更新情報 (xml2)  COPY (CPMDCSETV2REQ.INC)
      ******************************************************************
        01  XML-MEDICALSETREQ.
            02  MDCREQ-CONTEXT          PIC S9(9)   BINARY.
            02  MDCREQ-OBJECT           PIC X(36).
            02  MDCREQ-MODE             PIC S9(9)   BINARY.
            02  MDCREQ-RECORDNAME       PIC X(128).
            02  MDCREQ-MEDICALREQ.
      *        リクエスト番号
               03  MDCREQ-REQUEST-NUMBER   PIC X(2).
      *        基準日
               03  MDCREQ-BASE-DATE        PIC X(10).
      *        セットコード
               03  MDCREQ-SETCD            PIC X(06).
      *        セット名称
               03  MDCREQ-SETCD-NAME       PIC X(80).
      *        有効開始年月日
               03  MDCREQ-START-DATE       PIC X(10).
      *        有効終了年月日
               03  MDCREQ-END-DATE         PIC X(10).
      *        入外区分（Ｉ：入院）
               03  MDCREQ-OUTPATIENT-CLASS PIC X(01).
      *        診療情報
               03  MDCREQ-MDC-INFO-G.
                  05  MDCREQ-MDC-INFO          OCCURS   40.
      *             診療種別区分
                    09  MDCREQ-MDC-CLASS             PIC X(3).
      *             診療種別名称
                    09  MDCREQ-MDC-CLASS-NAME        PIC X(40).
      *             回数
                    09  MDCREQ-MDC-CLASS-NUMBER      PIC X(03).
      *
                    09  MDCREQ-PRESCRIPTION-INFO    OCCURS   40.
      *                 診療コード
                        11  MDCREQ-PRSCRPT-CODE          PIC X(9).
      *                 診療名称
                        11  MDCREQ-PRSCRPT-NAME          PIC X(140).
      *                 数量
                        11  MDCREQ-PRSCRPT-NUMBER        PIC X(11).
      *                 自費金額
                        11  MDCREQ-PRSCRPT-MONEY         PIC X(07).
      *                 コメント埋め込み値
                        11  MDCREQ-PRSCRPT-ATAI-G.
                            13  MDCREQ-PRSCRPT-ATAI      PIC X(10)
                                                         OCCURS   5.
      *                 継続コメント指示区分
                        11  MDCREQ-PRSCRPT-CONTKBN       PIC X(01).
      *                 内服種類数指示区分
                        11  MDCREQ-PRSCRPT-NAIFKBN       PIC X(01).
      *
