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
      *---------------------------------------------------------------*
      *    シェルＩＤ対応ＪＯＢ名称テーブル   COPY (CMJOBNAME.INC)    *
      *---------------------------------------------------------------*
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-太田     02/06/27  患者照会追加
      *  01.00.02    MCC-太田     02/12/19  定期請求追加
      *  01.00.03    NACL-太田    03/10/14  入院患者照会追加
      *  01.00.04    NACL-藤原    03/12/03  統計データ作成追加
      *
      *  04.05.01    NACL-藤原    09/10/01  統計ＣＳＶ出力追加
      *  05.00.01    NACL-伊藤    18/02/02  連携基盤追加
      *****************************************************************
       01  MNAME-DATA-REC.
           03  FILLER               PIC X(08)   VALUE   "RECEPT1".
           03  FILLER               PIC X(40)   VALUE   "レセプト作成".
      *
           03  FILLER               PIC X(08)   VALUE   "RECEPT2".
           03  FILLER               PIC X(40)   VALUE   "レセプト印刷".
      *
           03  FILLER               PIC X(08)   VALUE   "ORCBSD1 ".
           03  FILLER               PIC X(40)   VALUE
                                    "データチェック".
      *
           03  FILLER               PIC X(08)   VALUE   "ORCBSE1 ".
           03  FILLER               PIC X(40)   VALUE   "データ出力".
      *
           03  FILLER               PIC X(08)   VALUE   "ORCBSF1 ".
           03  FILLER               PIC X(40)   VALUE   "外部媒体変換".
      *
           03  FILLER               PIC X(08)   VALUE   "MSTMNT  ".
           03  FILLER               PIC X(40)   VALUE   "マスタ更新".
      *
           03  FILLER               PIC X(08)   VALUE   "SOKATU".
           03  FILLER               PIC X(40)   VALUE
                                    "総括・公費請求書".
      *
           03  FILLER               PIC X(08)   VALUE   "TENSU".
           03  FILLER               PIC X(40)   VALUE
                                    "点数マスタ一覧".
      *
           03  FILLER               PIC X(08)   VALUE   "TENSU2".
           03  FILLER               PIC X(40)   VALUE
                                    "経過措置医薬品置換マスタ一覧".
      *
           03  FILLER               PIC X(08)   VALUE   "DAILY".
           03  FILLER               PIC X(40)   VALUE   "日次統計".
      *
           03  FILLER               PIC X(08)   VALUE   "MONTHLY".
           03  FILLER               PIC X(40)   VALUE   "月次統計".
      *
           03  FILLER               PIC X(08)   VALUE   "REPRINT".
           03  FILLER               PIC X(40)   VALUE
                                    "再印刷処理".
           03  FILLER               PIC X(08)   VALUE   "ORCBQ01".
           03  FILLER               PIC X(40)   VALUE
                                    "患者照会".
           03  FILLER               PIC X(08)   VALUE   "TEIKI1".
           03  FILLER               PIC X(40)   VALUE
                                    "定期請求−収納データ作成".
           03  FILLER               PIC X(08)   VALUE   "TEIKI2".
           03  FILLER               PIC X(40)   VALUE
                                    "定期請求−請求書印刷".
           03  FILLER               PIC X(08)   VALUE   "ORCBI201".
           03  FILLER               PIC X(40)   VALUE
                                    "入院患者照会".
      *
           03  FILLER               PIC X(08)   VALUE   "MONTHLY1".
           03  FILLER               PIC X(40)   VALUE
                                                "月次統計データ作成".
      *
           03  FILLER               PIC X(08)   VALUE   "CHKMST".
           03  FILLER               PIC X(40)   VALUE
                                                "チェックマスタ一覧".
      *
           03  FILLER               PIC X(08)   VALUE   "ORCBS01".
           03  FILLER               PIC X(40)   VALUE
                                                "支払証明書".
      *
           03  FILLER               PIC X(08)   VALUE   "DAILY1".
           03  FILLER               PIC X(40)   VALUE
                                                "日次統計データ作成".
      *
           03  FILLER               PIC X(08)   VALUE   "TEIKI3".
           03  FILLER               PIC X(40)   VALUE
                                    "定期請求−請求削除".
           03  FILLER               PIC X(08)   VALUE   "YKJMST".
           03  FILLER               PIC X(40)   VALUE
                                    "薬剤情報マスタ".
           03  FILLER               PIC X(08)   VALUE   "ORHCML09".
           03  FILLER               PIC X(40)   VALUE
                                    "自院病名一覧".
           03  FILLER               PIC X(08)   VALUE   "COLLECT".
           03  FILLER               PIC X(40)   VALUE
                                    "データ収集".
      *
           03  FILLER               PIC X(08)   VALUE   "PUTCSV".
           03  FILLER               PIC X(40)   VALUE
                                    "統計ＣＳＶ出力".
      *
           03  FILLER               PIC X(08)   VALUE   "INFECTIO".
           03  FILLER               PIC X(40)   VALUE
                                    "感染症サーベイ".
      *
           03  FILLER               PIC X(08)   VALUE   "RENKEI1".
           03  FILLER               PIC X(40)   VALUE
                                    "連携基盤".
      *
           03  FILLER               PIC X(08)   VALUE   "MISRECE1".
           03  FILLER               PIC X(40)   VALUE
                                    "診療情報用レセ電".
      *
           03  FILLER               PIC X(08)   VALUE   "MISJMAMR".
           03  FILLER               PIC X(40)   VALUE
                                    "診療情報フォーマット".
      *
           03  FILLER               PIC X(08)   VALUE   "MIGRATE1".
           03  FILLER               PIC X(40)   VALUE
                                    "マスタデータ移行".
      *
       01  MNAME-DATA-REC-R         REDEFINES   MNAME-DATA-REC.
           03  MNAME-TBL-OCC        OCCURS      30  INDEXED TBL-IDX.
               05  MNAME-SHELLID    PIC X(08).
               05  MNAME-JOBNAME    PIC X(40).
      *
       01  MNAME-MAX                PIC 9(04)   VALUE   30.
      *
