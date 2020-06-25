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
      *    診療行為算定歴チェック　   COPY (CPORCSC91.INC)
      *                              (ORCSC91 で使用）
      *
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     NACL-多々納  02/09/02  併用算定分を表示
      * 04.07.00     NACL-多々納  11/11/29  入院期間分追加
      ************************************************************
      *
       01  ORCSC91AREA.
      *入力
      *    1:入力分   2:自動追加分 3：チェック分
      *    9:入院期間 8:継続入院期間（継続初回入院日から）
           03  LNK-SC91-KBN            PIC X(01).
      *    訂正フラグ（１：訂正）
           03  LNK-SC91-SYORIFLG       PIC 9(01).
      *    診療コード
           03  LNK-SC91-SRYCD          PIC X(09).
      *    現在ＳＰＡＩＤＸ（1:入力分の時）
           03  LNK-SC91-GMN-IDX        PIC 9(04).
      *    回数
           03  LNK-SC91-KAISU          PIC 9(03).
      *    数量
           03  LNK-SC91-SURYO          PIC 9(03).
      *    訂正時の診療コード
           03  LNK-SC91-TEISEI-AREA.
               05  LNK-SC91-TEISEI-OCC      OCCURS   400.
                   07  LNK-SC91-TEI-SRYCD   PIC X(09).
                   07  LNK-SC91-TEI-KAISU   PIC 9(03).
      *    労災のリハビリ（０：チェックあり、１：３月以内なし）
           03  LNK-SC91-CHKFLG         PIC 9(01).
      *    入院期間算定回数
           03  LNK-SC91-NYUCNT         PIC 9(03).
      *
      *外来まとめ算定履歴領域
           03  LNK-SC91-KT01-AREA.
               05  LNK-SC91-KT01FLG         PIC X(01).
               05  LNK-SC91-KT01-OCC        OCCURS   100.
                   07  LNK-SC91-KT01-SRYCD  PIC X(09).
                   07  LNK-SC91-KT01-DAY-G.
                       09  LNK-SC91-KT01-DAY     PIC 9(03)
                                                  OCCURS   31.
      *
      *出力
      *    最大件数
           03  LNK-SC91-MAXCNT         PIC 9(04).
      *    現在件数
           03  LNK-SC91-CNT            PIC 9(04).
      *
      *    最大件数(日）
           03  LNK-SC91-DAYMAXCNT      PIC 9(04).
      *    現在件数（日）
           03  LNK-SC91-DAYCNT         PIC 9(04).
      *    訂正後回数
           03  LNK-SC91-ATO-CNT        PIC 9(04).
      *
      *    最大件数(週）
           03  LNK-SC91-WEEKMAXCNT     PIC 9(04).
      *    現在件数（週）
           03  LNK-SC91-WEEKCNT        PIC 9(04).
      *
      *    入院日前件数
           03  LNK-SC91-NYUZCNT        PIC 9(03).
      *
      *    エラーコード
           03  LNK-SC91-ERRCD          PIC X(04).
      *    エラー診療メッセージ
           03  LNK-SC91-ERRMSG         PIC X(200).
      *    エラーコード２
           03  LNK-SC91-ERRCD2         PIC X(04).
      *
