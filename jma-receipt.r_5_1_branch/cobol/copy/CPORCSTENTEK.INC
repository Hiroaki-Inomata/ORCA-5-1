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
      *    入院点滴集計パラメタ　　COPY (CPORCSCTENTEK.INC)
      *                            使用プログラム 
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     NACL-多々納  03/04/03  中心静脈処理追加
      * 04.07.00     NACL-多々納  11/11/XX  同日再入院追加
      ************************************************************
       01  ORCSTENTEKAREA.
      *
      *入力
           03  ORCSTEN-IN-AREA.
      *処理区分（１：一括更新、２：保険別、３：一括チェック）
               05  ORCSTEN-KBN                 PIC  X(01).
      *患者ＩＤ
               05  ORCSTEN-PTID                PIC  9(10).
      *診療年月
               05  ORCSTEN-SRYYM               PIC  X(06).
      *保険組合
               05  ORCSTEN-HKNCOMBI            PIC 9(04).
      *出力（保険別）
           03  ORCSTEN-OUT-AREA.
      *        薬剤日別テーブル
               05  ORCSTEN-YAKZI-TBL           OCCURS   31.
      *            剤点数
                   07  ORCSTEN-ZAITEN          PIC  9(08).
      *            診療科
                   07  ORCSTEN-YK-SRYKA        PIC  X(02).
      *            合計液量
                   07  ORCSTEN-CHUSYARYO       PIC  9(07)V9(05).
      *
                   07  ORCSTEN-MEISAI-OCC      OCCURS   50.
      *                診療コード
                       09  ORCSTEN-SRYCD       PIC X(09).
      *                数量
                       09  ORCSTEN-SURYO       PIC 9(05)V9(05).
      *                残量廃棄コメント
                       09  ORCSTEN-HAIK-SRYCD  PIC X(09).
      *                廃棄数量
                       09  ORCSTEN-HAIK-SURYO   PIC 9(05)V9(05).
      *                コメント
                       09  ORCSTEN-COMMNT-G        OCCURS  5.
                           11  ORCSTEN-COM-SRYCD       PIC X(09).
                           11  ORCSTEN-COM-MEI         PIC X(80).
      *
      *        器材剤別テーブル
               05  ORCSTEN-KIZAI-TBL-G.
                   07  ORCSTEN-KIZI-ZAI-OCC        OCCURS   50.
      *                器材点数
                       09  ORCSTEN-KIZ-ZAITEN          PIC  9(08).
                       09  ORCSTEN-KIZ-MEI-OCC         OCCURS   10.
      *                    器材診療コード
                           11  ORCSTEN-KIZ-SRYCD       PIC X(09).
      *                    器材数量
                           11  ORCSTEN-KIZ-SURYO       PIC 9(05)V9(05).
      *                    コメント
                           11  ORCSTEN-KIZ-COMMNT-G        OCCURS  5.
                               13  ORCSTEN-KIZ-COM-SRYCD   PIC X(09).
                               13  ORCSTEN-KIZ-COM-MEI     PIC X(80).
      *                器材日数
                       09  ORCSTEN-KIZ-DAY-G.
                           11  ORCSTEN-KIZ-DAY         PIC 9(03)
                                                       OCCURS  31.
      *                    同日算定回数
                           11  ORCSTEN-KIZ-DAY2-G      OCCURS  31.
                               13  ORCSTEN-KIZ-DAY2    PIC 9(03)
                                                       OCCURS  2.
      *                    診療科
                           11  ORCSTEN-KIZ-SRYKA       PIC X(02)
                                                       OCCURS  31.
      *
      *点滴手技料算定
           03  ORCSTEN-DAY-AREA.
      *        未算定日
               05  ORCSTEN-MIDAY-AREA.
                   07  ORCSTEN-MI-DAY          PIC 9(01)
                                               OCCURS   31.
               05  ORCSTEN-MIIVHDAY-AREA.
      *            中心静脈
                   07  ORCSTEN-MIIVH-DAY       PIC 9(01)
                                               OCCURS   31.
      *        複数算定日
               05  ORCSTEN-FUKUDAY-AREA.
      *            点滴
                   07  ORCSTEN-FUKU-AREA.
                       09  ORCSTEN-FUKU-DAY        PIC 9(01)
                                               OCCURS   31.
      *            動脈
                   07  ORCSTEN-DOU-AREA.
                       09  ORCSTEN-DOU-DAY         PIC 9(01)
                                               OCCURS   31.
      *            中心静脈
                   07  ORCSTEN-CHU-AREA.
                       09  ORCSTEN-CHU-DAY         PIC 9(01)
                                               OCCURS   31.
      *同日再入院対応点数
           03  ORCSTEN-DOUJITU-AREA.
      *        薬剤日別テーブル
               05  ORCSTEN-DOU-TBL2          OCCURS   31.
      *            薬剤１日点数
                   07  ORCSTEN-DOU-ZAITEN          PIC  9(08)
                                                   OCCURS   2.
      *
      *
      *エラーコード（１：会計照会更新エラー
      *              ２：受診履歴更新エラー）
           03  ORCSTEN-ERRCD           PIC 9(01).
      *更新フラグ（１：変更あり）
           03  ORCSTEN-HENFLG          PIC 9(01).
      *更新フラグ２（１：削除エラー）
           03  ORCSTEN-HENFLG2         PIC 9(01).
      *
