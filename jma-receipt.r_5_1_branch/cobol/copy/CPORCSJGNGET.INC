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
      *    公費月上限額取得用パラメタ　　　 COPY (CPORCSJGNGET.INC)
      *                          使用プログラム  (ORCSJGNGET)
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-門脇    06/03/20  他医療機関等一部負担累計追加
      *  01.00.02    NACL-門脇    09/07/10  他医療機関等一部負担累計
      *                                     編集フラグ追加
      *  04.07.01    NACL-門脇    11/09/01  システム管理２０１０機能追加対応
      *                                     公費適用開始日追加
      *                                     都道府県番号追加
      *                                     対象区分デフォルト設定
      *                                     編集フラグ
      *                                     対象区分追加
      *  04.07.02    NACL-門脇    15/01/26  平成２７年１月改正対応
      *                                     （都道府県独自難病対応）
      ************************************************************
       01  LNK-ORCSJGNGET-REC.
      *
      *医療機関ＩＤ
           03  LNK-ORCSJGNGET-HOSPNUM       PIC 9(2).
      *入外区分
           03  LNK-ORCSJGNGET-NYUGAIKBN     PIC  X(01).
      *患者ＩＤ
           03  LNK-ORCSJGNGET-PTID          PIC  9(10).
      *公費保険番号
           03  LNK-ORCSJGNGET-KOHHKNNUM     PIC  X(03).
      *公費ＩＤ
           03  LNK-ORCSJGNGET-KOHID         PIC  9(10).
      *診療月
           03  LNK-ORCSJGNGET-SRYYM         PIC  X(06).
      *入院請求終了日
           03  LNK-ORCSJGNGET-SKYEDYMD      PIC  X(08).
      *公費適用開始日
           03  LNK-ORCSJGNGET-TEKSTYMD      PIC  X(08).
      *都道府県番号
           03  LNK-ORCSJGNGET-PREFNUM       PIC  X(02).
      *対象区分デフォルト設定
           03  LNK-ORCSJGNGET-SETKBN        PIC  9(01).
      *難病・小児慢性適用区分対応フラグデフォルト設定
           03  LNK-ORCSJGNGET-SETKBN2       PIC  9(01).
      *編集フラグ
      *　（空白）：上限額入力の対象チェックを行わない
      *　　　　　　　　　　　　　　→　対象区分と上限額を返却
      *　（１　）：上限額入力の対象チェックを行う
      *　　　　　　　地方公費　　　→　対象区分を返却
      *　　　　　　　地方公費以外　→　エラーを返却
      *　（２　）：上限額入力の対象チェックを行う
      *　　　　　　　地方公費　　　→　対象区分と上限額を返却
      *　　　　　　　地方公費以外　→　エラーを返却
           03  LNK-ORCSJGNGET-HENFLG        PIC  X(01).
      *
      *返却値
      *
           03  LNK-ORCSJGNGET-OUT-AREA.
      *外来上限額
             05  LNK-ORCSJGNGET-GAIJGNGAK   PIC  9(07).
      *入院上限額
             05  LNK-ORCSJGNGET-NYUJGNGAK   PIC  9(07).
      *他医療機関等一部負担累計（024,079:食事療養上限額）
             05  LNK-ORCSJGNGET-TAFTNRUIKEI PIC  9(07).
      *他医療機関等一部負担累計編集フラグ
             05  LNK-ORCSJGNGET-TAFTNFLG    PIC  9(01).
      *対象区分
      *　（０）：上限額入力の対象外
      *　（１）：上限額入力の対象（地方公費）　外来上限額・入院上限額
      *　（２）：上限額入力の対象（地方公費）　入外上限額・他一部負担累計
      *　（９）：上限額入力の対象（地方公費以外）
             05  LNK-ORCSJGNGET-TAISYOKBN   PIC  9(01).
      *難病・小児慢性適用区分対応フラグ
             05  LNK-ORCSJGNGET-TEKIYOFLG   PIC  9(01).
      *
      *復帰コード
      *
           03  LNK-ORCSJGNGET-RC            PIC  9(02).
      *
