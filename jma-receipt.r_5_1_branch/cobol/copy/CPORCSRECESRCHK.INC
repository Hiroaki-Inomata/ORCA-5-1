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
      ************************************************************
      * オンライン診療料等に関わる管理料・指導料の算定履歴チェック
      * （レセプト用）
      * COPY  (CPORCSRECESRCHK.INC)
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  05.00.02    NACL-藤原    20/04/08  排尿自立支援加算、外来排尿自
      *                                     立指導料の算定履歴チェック追加
      ************************************************************
       01  ORCSRECESRCHKAREA.
      *入力値
      *    処理区分    空白：オンライン診療料等に関わる算定履歴チェック
      *                １  ：排尿自立に関わる算定履歴チェック
           03  LNK-RECESRCHK-SYORI         PIC X(01).
      *    患者ＩＤ
           03  LNK-RECESRCHK-PTID          PIC 9(10).
      *    処理年月（診療年月）
           03  LNK-RECESRCHK-SRYYM         PIC X(06).
      *    チェック区分
           03  LNK-RECESRCHK-CHKKBN        PIC X(01).
      *    チェック番号
           03  LNK-RECESRCHK-CHKNO         PIC 9(02).
      *    入外区分
           03  LNK-RECESRCHK-NYUGAIKBN     PIC X(01).
      *    基準日（初回入院日）
           03  LNK-RECESRCHK-SRYYMD        PIC X(08)   OCCURS  2.
      *返却項目
           03  LNK-RECESRCHK-HENKYAKU-AREA.
      *算定履歴情報
      *  直近算定年月
               05  LNK-RECESRCHK-SANTEIYM  PIC X(06).
      *  算定診療コード
               05  LNK-RECESRCHK-SRYCD     PIC X(09).
      *  初回算定日
               05  LNK-RECESRCHK-FSANTEIYMD
                                           PIC X(08).
      *  当月初回算定日
               05  LNK-RECESRCHK-MSANTEID  PIC X(02).
      *  該当コメントコード
               05  LNK-RECESRCHK-COMCD     PIC X(09).
      *
      *  通算回数
               05  LNK-RECESRCHK-KAISU     PIC 9(02).
      *
      *復帰コード １：未設定エラー
      *           ２：処理年月エラー
      *           ３：患者ＩＤエラー
      *           ４：チェック区分エラー
      *           ５：チェック番号エラー
      *           ６：入外区分エラー
      *           ７：基準日（初回入院日）エラー
      *           ８：処理区分エラー
      *           ９：算定履歴情報編集なし
      *           １０：初回入院日なし
           03  LNK-RECESRCHK-RC            PIC S9(9) BINARY.
      *
