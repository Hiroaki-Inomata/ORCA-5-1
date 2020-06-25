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
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 05.00.00     NACL-多々納  19/01/XX  複数科保険対応
      *****************************************************************
      ******************************************************************
      *    ＡＰＩ診療内容編集サブパラメタ（一体化）
      *                                         (CPORAPI021SUB2V3.INC)
      ******************************************************************
       01  ORAPI021SUB2V3AREA.
      *    処理区分
           03  ORAPISUB2-SYORIKBN      PIC X(01).
      *    テーブル件数
           03  ORAPISUB2-WKSRYACT-MAX  PIC 9(04).
      *
      *返却
           03  ORAPISUB2-HENSYU-AREA.
      *        診療内容返却
               05  ORAPISUB2-MEDICAL-INF-G.
                 07  ORAPISUB2-MDC-INFO          OCCURS   120.
      *!!!!
      *H31.01
      *             保険・診療科切替
                    09  ORAPISUB2-CHENGE-REC.
      *                データ区分（DEP：複数診療科、COM:保険組合せ番号のレコード）
                        11  ORAPISUB2-MDC-DATA-KBN         PIC X(03).
      *                 保険組合せ番号
                        11  ORAPISUB2-MDC-COMBINATION      PIC X(04).
      *                 保険の種類名称
                        11  ORAPISUB2-MDC-MAIN-INS-NAME
                                                       PIC X(20).
                        11  ORAPISUB2-PUBLIC-INSURANCE-INFO.
      *                     公費の種類名称
                            13  ORAPISUB2-INSURANCE-NAME  PIC X(20)
                                                       OCCURS  4 TIMES.
      *                 診療科
                        11  ORAPISUB2-MDC-DEP-CODE         PIC X(02).
                        11  ORAPISUB2-MDC-DEP-CODE-NAME    PIC X(20).
      *                 ドクター
                        11  ORAPISUB2-MDC-DR-CODE          PIC X(05).
                        11  ORAPISUB2-MDC-DR-CODE-NAME     PIC X(80).
      *!!!!
      *             診療区分
                    09  ORAPISUB2-MDC-CLASS-KBN         PIC X(2).
      *             診療種別区分
                    09  ORAPISUB2-MDC-CLASS             PIC X(3).
      *             診療種別名称
                    09  ORAPISUB2-MDC-CLASS-NAME        PIC X(40).
      *             回数
                    09  ORAPISUB2-MDC-CLASS-NUMBER      PIC X(03).
      *H29.11 入院用
      *            算定日
                    09  ORAPISUB02-MDC-DAY-G.
                       11  ORAPISUB02-MDC-DAY      PIC X(01)
                                                   OCCURS   31.
      *
      *             剤点数
                    09  ORAPISUB2-MDC-POINT             PIC X(08).
      *             剤金額
                    09  ORAPISUB2-MDC-MONEY             PIC X(08).
      *             剤区分（１：包括剤）
                    09  ORAPISUB2-MDC-CODE              PIC X(01).
      *H29.5
      *             包括検査の検査数
                    09  ORAPISUB2-HKTKNSA-CNT           PIC X(02).
      *
      *             剤削除可能連番
                    09  ORAPISUB2-DEL-NUMBER            PIC X(02).
      *             剤削除区分
                    09  ORAPISUB2-DEL-INF               PIC X(01).
      *
                    09  ORAPISUB2-PRSCRPT-INFO-G.
                      10  ORAPISUB2-PRSCRPT-INFO    OCCURS   50.
      *                 診療コード
                        11  ORAPISUB2-PRSCRPT-CODE          PIC X(9).
      *                 診療名称
                        11  ORAPISUB2-PRSCRPT-NAME          PIC X(140).
      *                 数量
                        11  ORAPISUB2-PRSCRPT-NUMBER        PIC X(11).
      *H29.5
      *                 単位コード
                        11  ORAPISUB2-PRSCRPT-TANICD        PIC X(03).
      *                 単位名称
                        11  ORAPISUB2-PRSCRPT-TANICD-NAME   PIC X(24).
      *                 自費金額
                        11  ORAPISUB2-PRSCRPT-MONEY         PIC X(07).
      *                 コメント埋め込み値
                        11  ORAPISUB2-PRSCRPT-ATAI-G.
                            13  ORAPISUB2-PRSCRPT-ATAI      PIC X(10)
                                                         OCCURS   5.
      *H29.5
      *                 フィルム分画数
                        11  ORAPISUB2-PRSCRPT-FILMNUM       PIC X(03).
      *                 一般名記載区分
                        11  ORAPISUB2-PRSCRPT-GENERIC       PIC X(01).
      *                 一般名
                        11  ORAPISUB2-PRSCRPT-GENNAME       PIC X(200).
      *                 継続指示区分
                        11  ORAPISUB2-PRSCRPT-CONTKBN       PIC X(01).
      *                 内服種類数指示区分
                        11  ORAPISUB2-PRSCRPT-NAIFKBN       PIC X(01).
      *H29.5
      *                 自動算定なし区分（在宅療養実績加算）
                        11  ORAPISUB2-PRSCRPT-NOAUTO        PIC X(03).
      *                 自動区分
                        11  ORAPISUB2-PRSCRPT-AUTOKBN       PIC X(01).
      *
      *        院外院内投薬(1:院外、2:院内）
               05  ORAPISUB2-INGAIKBN      PIC X(01).
      *    エラー区分
           03  ORAPISUB2-ERRCD         PIC X(04).
           03  ORAPISUB2-ERRMSG        PIC X(100).
      *
