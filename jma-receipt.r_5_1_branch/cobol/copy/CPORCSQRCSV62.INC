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
      *    お薬手帳ＱＲデータ出力パラメタ    COPY (CPORCSQRCSV62.INC)
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *
       01  ORCSQRCSV62AREA.
      *    帳票名
           03  ORCSQRCSV62-FORM-ID         PIC X(06).
      *    処理回数
           03  ORCSQRCSV62-CNT             PIC 9(03).
      *
      *    共通情報
           03  ORCSQRCSV62-IN01-G.
      *        診療年月日
               05  ORCSQRCSV62-SRYYMD        PIC  X(08).
      *        県番号
               05  ORCSQRCSV62-PREFNUM       PIC  9(02).
      *        医療機関コード
               05  ORCSQRCSV62-HOSPCD        PIC  9(07).
      *        医療機関種別
               05  ORCSQRCSV62-HOSPSBT       PIC  9(01).
      *        医療機関名称
               05  ORCSQRCSV62-HOSPNAME      PIC  X(120).
      *        医療機関郵便
               05  ORCSQRCSV62-HOSPPOST      PIC  X(08).
      *        医療機関住所
               05  ORCSQRCSV62-HOSPADRS      PIC  X(800).
      *        医療機関電話番号
               05  ORCSQRCSV62-HOSPTEL       PIC  X(13).
      *        診療科
      *        05  ORCSQRCSV62-RECESRYKA     PIC  X(02).
      *        05  ORCSQRCSV62-SRYKANAME     PIC  X(40).
      *        医師情報
      *        ドクターコード
               05  ORCSQRCSV62-DRCD          PIC  X(05).
      *        フリガナ
               05  ORCSQRCSV62-DR-KANANAME   PIC  X(40).
      *        氏名
               05  ORCSQRCSV62-DR-NAME       PIC  X(40).
      *        患者情報
               05  ORCSQRCSV62-NAME          PIC  X(40).
               05  ORCSQRCSV62-KANANAME      PIC  X(40).
               05  ORCSQRCSV62-SEX           PIC  X(02).
               05  ORCSQRCSV62-BIRTHDAY      PIC  X(08).
      *
      *    ＱＲ用剤毎の内容
           03  ORCSQRCSV62-NAIYOU-AREA.
               05  ORCSQRCSV62-NAIYOU-TBL       OCCURS   50.
                   07  ORCSQRCSV62-SRYKBN           PIC X(02).
                   07  ORCSQRCSV62-SRYSYUKBN        PIC X(03).
                   07  ORCSQRCSV62-ZAIKATA          PIC X(01).
                   07  ORCSQRCSV62-KAISU            PIC 9(03).
                   07  ORCSQRCSV62-NAIYOU-TBL       OCCURS   100.
                       09  ORCSQRCSV62-SRYCD            PIC X(09).
                       09  ORCSQRCSV62-TNS-NAME         PIC X(100).
                       09  ORCSQRCSV62-SURYOU           PIC 9(05)V9(05).
                       09  ORCSQRCSV62-YKZKBN           PIC X(01).
                       09  ORCSQRCSV62-TANINAME         PIC X(12).
      *                内服点剤区分
                       09  ORCSQRCSV62-NAIHENKBN        PIC X(01).
      *                用法コメント区分
                       09  ORCSQRCSV62-YCOMMNTKBN       PIC X(01).
      *
      *    備考内容
           03  ORCSQRCSV62-BIKOU-AREA.
               05  ORCSQRCSV62-BIKOU-TBL       OCCURS   10.
                   07  ORCSQRCSV62-BIKOU       PIC X(200).
      *
      *    ファイル名組み立て項目
           03  ORCSQRCSV62-SYSTIME         PIC 9(08).
      *
      *    ＱＲＣＳＶファイル名
           03  ORCSQRCSV62-CSV-FILENAME    PIC X(256).
      *    ＱＲ出力区分
           03  ORCSQRCSV62-QR-KBN          PIC X(01).
      *    ＱＲＣＳＶ処理区分
           03  ORCSQRCSV62-CSV-KBN         PIC X(01).
      *    発行画面区分
           03  ORCSQRCSV62-PGKBN           PIC X(01).
      *
      *出力
      *    エラーコード
           03  ORCSQRCSV62-ERRCD           PIC 9(01).
      *
      *    リターンコード(ＱＲ画像バージョン)
           03  ORCSQRCSV62-RET-VERSION     PIC 9(02).
      *    リターンコード(ＱＲ画像ファイル数)
           03  ORCSQRCSV62-RET-COUNT       PIC 9(02).
      *    qrencode return-code
           03  ORCSQRCSV62-RET             PIC X(01).
      *
