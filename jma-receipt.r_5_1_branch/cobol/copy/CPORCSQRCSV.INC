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
      *----------------------------------------------------------*
      *    処方箋ＱＲデータ出力パラメタ    COPY (CPORCSQRCSV.INC)
      *                                          (ORCHC02Q で使用）
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.06.00    NACL-多々納  11/02/21  用法コメント対応
      *  04.06.00    NACL-多々納  12/03/01  一般コード追加（H24.4）
      *  04.06.00    NACL-多々納  12/09/27  医師・麻薬施用コード等追加
      *  04.07.00    NACL-多々納  15/01/21  ＣＳＶデータ名等追加
      *  04.08.00    NACL-多々納  18/03/XX  備考種別追加
      ************************************************************
      *
       01  ORCSQRCSVAREA.
      *    1:共通情報、2:剤情報、3:終了
           03  ORCSQRCSV-KBN               PIC X(01).
      *
      *    共通情報
           03  ORCSQRCSV-IN01-G.
      *        県番号
               05  ORCSQRCSV-PREFNUM       PIC  9(02).
      *        医療機関コード
               05  ORCSQRCSV-HOSPCD        PIC  9(07).
      *        医療機関種別
               05  ORCSQRCSV-HOSPSBT       PIC  9(01).
      *        医療機関名称
               05  ORCSQRCSV-HOSPNAME      PIC  X(120).
      *        医療機関郵便
               05  ORCSQRCSV-HOSPPOST      PIC  X(08).
      *        医療機関住所
               05  ORCSQRCSV-HOSPADRS      PIC  X(100).
      *        医療機関電話番号
               05  ORCSQRCSV-HOSPTEL       PIC  X(13).
      *        医療機関FAX
               05  ORCSQRCSV-HOSPFAX       PIC  X(13).
      *        診療科
               05  ORCSQRCSV-RECESRYKA     PIC  X(02).
               05  ORCSQRCSV-SRYKANAME     PIC  X(40).
      *        医師情報
      *        フリガナ
               05  ORCSQRCSV-DR-KANANAME   PIC  X(40).
      *        氏名
               05  ORCSQRCSV-DR-NAME       PIC  X(40).
      *        医師コード
               05  ORCSQRCSV-DR-CD         PIC  X(15).
      *        麻薬施用コード
               05  ORCSQRCSV-MAYAKULICENSE PIC  X(15).
      *        患者情報
               05  ORCSQRCSV-PTNUM         PIC  X(15).
               05  ORCSQRCSV-NAME          PIC  X(40).
               05  ORCSQRCSV-KANANAME      PIC  X(40).
               05  ORCSQRCSV-SEX           PIC  X(01).
               05  ORCSQRCSV-BIRTHDAY      PIC  X(08).
               05  ORCSQRCSV-ADRS          PIC  X(100).
               05  ORCSQRCSV-TEL           PIC  X(13).
      *        患者保険情報
               05  ORCSQRCSV-HKNCOMBINUM   PIC  X(04).
      *        患者公費ＩＤ
               05  ORCSQRCSV-KOHID-G       OCCURS   4.
                   07  ORCSQRCSV-KOHID         PIC 9(10).
                   07  ORCSQRCSV-KOHHKNNUM     PIC X(03).
      *        麻毒フラグ
               05  ORCSQRCSV-MAYAKU        PIC  9(01).
      *        一部負担区分
               05  ORCSQRCSV-BIKO-FTN      PIC  X(01).
      *        残薬確認指示
               05  ORCSQRCSV-ZANSIJI       PIC  X(01).
      *H30.4
      *        分割指示　回数
               05  ORCSQRCSV-BUNKATU-MAX   PIC  9(01).
      *        分割指示　回
               05  ORCSQRCSV-BUNKATU-CNT   PIC  9(01).
      *
      *    備考内容
           03  ORCSQRCSV-BIKOU-AREA.
               05  ORCSQRCSV-BIKOU-TBL       OCCURS   20.
                   07  ORCSQRCSV-BIKOU       PIC X(100).
      *            備考 種別
                   07  ORCSQRCSV-BIKOU-KBN   PIC X(02).
      *
      *    ＱＲ用剤毎の内容
           03  ORCSQRCSV-NAIYOU-AREA.
               05  ORCSQRCSV-NAIYOU-TBL       OCCURS   50.
                   07  ORCSQRCSV-ZAIKATA          PIC X(01).
                   07  ORCSQRCSV-KAISU            PIC 9(03).
      *            分割総投与日数
                   07  ORCSQRCSV-GOKKAISU         PIC 9(03).
                   07  ORCSQRCSV-NAIYOU-TBL       OCCURS   50.
                       09  ORCSQRCSV-SRYCD            PIC X(09).
                       09  ORCSQRCSV-TNS-NAME         PIC X(100).
                       09  ORCSQRCSV-SURYOU           PIC 9(05)V9(05).
                       09  ORCSQRCSV-YKZKBN           PIC X(01).
                       09  ORCSQRCSV-TANINAME         PIC X(12).
                       09  ORCSQRCSV-KISAIKBN         PIC X(01).
                       09  ORCSQRCSV-KANZANCHI        PIC 9(05)V9(5).
      *                用法コメント区分
                       09  ORCSQRCSV-YCOMMNTKBN       PIC X(01).
      *                一般名コード
                       09  ORCSQRCSV-YAKKAKJNCD       PIC X(13).
      *H26.5
      *                内服１回量
                       09  ORCSQRCSV-SURYOU-SG        PIC 9(05)V9(05).
      *                内服服用回数
                       09  ORCSQRCSV-KAISU-SG         PIC 9(03).
      *H26.12
      *                CSV用　薬剤名称
                       09  ORCSQRCSV-CSV-NAME         PIC X(100).
      *
      *    ファイル名組み立て項目
           03  ORCSQRCSV-SYSTIME       PIC 9(08).
      *    エラーコード
           03  ORCSQRCSV-ERRCD         PIC 9(01).
      *    リターンコード(ＱＲ画像バージョン)
           03  ORCSQRCSV-RET-VERSION        PIC 9(02).
      *    リターンコード(ＱＲ画像ファイル数)
           03  ORCSQRCSV-RET-COUNT          PIC 9(02).
      *    qrencode return-code
           03  ORCSQRCSV-RET                PIC X(01).
      *H27.1
      *    ＣＳＶファイル名称
           03  ORCSQRCSV-CSVFILE            PIC X(100).
      *
