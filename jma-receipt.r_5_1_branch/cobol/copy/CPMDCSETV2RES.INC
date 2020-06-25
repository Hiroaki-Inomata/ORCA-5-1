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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      ******************************************************************
      *    ＡＰＩ　セット更新情報 (xml2)  COPY (CPMDCSETV2RES.INC)
      ******************************************************************
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
        01  XML-MEDICALSETRES.
            02  MDCRES-CONTEXT             PIC S9(9)   BINARY.
            02  MDCRES-OBJECT              PIC X(36).
            02  MDCRES-MODE                PIC S9(9)   BINARY.
            02  MDCRES-RECORDNAME          PIC X(128).
            02  MDCRES-MEDICALRES.
      *        実施日
               03  MDCRES-INFORMATION-DATE  PIC X(10).
      *        実施時間
               03  MDCRES-INFORMATION-TIME  PIC X(8).
      *        返却コード
               03  MDCRES-API-RESULT        PIC X(3).
      *        返却メッセージ
               03  MDCRES-API-RESULT-MSG    PIC X(100).
      *
               03  MDCRES-RESKEY            PIC X(100).
      *
      *        基準日
               03  MDCRES-BASE-DATE        PIC X(10).
      *        セットコード
               03  MDCRES-SETCD            PIC X(06).
      *        セット名称
               03  MDCRES-SETCD-NAME       PIC X(80).
      *        有効開始年月日
               03  MDCRES-START-DATE       PIC X(10).
      *        有効終了年月日
               03  MDCRES-END-DATE         PIC X(10).
      *        診療情報
               03  MDCRES-MDC-INFO-G.
                  05  MDCRES-MDC-INFO          OCCURS   40.
      *             診療種別区分
                    09  MDCRES-MDC-CLASS             PIC X(3).
      *             診療種別名称
                    09  MDCRES-MDC-CLASS-NAME        PIC X(40).
      *             回数
                    09  MDCRES-MDC-CLASS-NUMBER      PIC X(03).
      *
                    09  MDCRES-PRESCRIPTION-INFO    OCCURS   40.
      *                 診療コード
                        11  MDCRES-PRSCRPT-CODE          PIC X(9).
      *                 診療名称
                        11  MDCRES-PRSCRPT-NAME          PIC X(140).
      *                 数量
                        11  MDCRES-PRSCRPT-NUMBER        PIC X(11).
      *                 自費金額
                        11  MDCRES-PRSCRPT-MONEY         PIC X(07).
      *                 コメント埋め込み値
                        11  MDCRES-PRSCRPT-ATAI-G.
                            13  MDCRES-PRSCRPT-ATAI      PIC X(10)
                                                         OCCURS   5.
      *                 継続コメント指示区分
                        11  MDCRES-PRSCRPT-CONTKBN       PIC X(01).
      *                 内服種類数指示区分
                        11  MDCRES-PRSCRPT-NAIFKBN       PIC X(01).
      *
      *        データメッセージ
               03  MDCRES-MEDICAL-MSG1-G.
                   05  MDCRES-MEDICAL-MSG1-TBL     OCCURS   50.
      *                エラーコード
                       07  MDCRES-MEDI-RESULT        PIC X(4).
      *                エラーメッセージ
                       07  MDCRES-MEDI-RESULT-MSG    PIC X(200).
      *                エラー剤番
                       07  MDCRES-MEDI-POSITION       PIC X(03).
      *                エラー行番号
                       07  MDCRES-MEDI-ITEM-POSITION  PIC X(02).
      *                エラーコード
                       07  MDCRES-MEDI-CODE           PIC X(09).
               03  MDCRES-WARNING-MSG-G.
      *            ワーニングメッセージ
                   05  MDCRES-MEDI-WARNING-TBL   OCCURS  50.
      *               ワーニングコード
                      07  MDCRES-MEDI-WARNING       PIC X(04).
      *               ワーニングメッセージ
                      07  MDCRES-WARNING-MSG        PIC X(200).
      *               ワーニング剤
                      07  MDCRES-WRG-POSITION       PIC X(03).
      *               ワーニング行番号
                      07  MDCRES-WRG-ITEM-POSITION  PIC X(02).
      *               ワーニングコード
                      07  MDCRES-WRG-MEDI-CODE      PIC X(09).
