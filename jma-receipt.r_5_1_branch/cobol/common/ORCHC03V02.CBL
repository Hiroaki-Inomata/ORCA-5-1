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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCHC03V02.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : 外来請求書兼領収書　印刷（ＨＣＭ０３Ｖ０２）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  04/01/29    NACL−小豆沢  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-小豆沢  04/01/29  保険適用外の記載と消費税対応
      *  01.00.02    NACL-太田    04/03/17  （再）の文字の編集を削除
      *  01.00.03    NACL-多々納  04/07/15  複数科・保険組合せ対応
      *  01.00.04    NACL-多々納  05/01/11  前回未収額の複数科・保険対応
      *  01.00.05    NACL-小豆沢  05/05/25  労災・自賠責の患者請求分は明細を
      *                                     表示するように変更
      *  01.00.06    NACL-多々納  06/05/15  MONFUNC 対応
      *  01.00.07    NACL-太田    06/08/22  パラメタテーブルより編集データ
      *                                     を参照する区分を追加
      *                                     月まとめ請求書発行機能追加
      *  03.03.00    NACL-多々納  06/10/02  返金対応追加
      *  03.04.01    NACL-森脇    07/01/17  医療機関名称、住所セット変更
      *  03.05.00    NACL-多々納  07/04/XX  グループ診療対応
      *  03.05.00    NACL-多々納  07/05/22  再印刷処理追加
      *  03.05.00    NACL-太田    07/08/03  月別発行実日数記載追加
      *  03.05.01    NACL-多々納  07/08/06  訂正時合計対応
      *  04.01.00    NACL-太田    07/10/22  公務災害対応
      *  04.02.00    NACL-太田    08/03/06  改正対応
      *  04.03.00    NACL-太田    08/09/17  改正対応
      *  04.04.00    NACL-森脇    09/01/14  プログラムオプション対応
      *  04.05.00    NACL-太田    09/07/24  ORCHCN03V03に統合
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    請求書パラメタ
           COPY    "CPORCHC03.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCHC03AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           MOVE    "V2"            TO      ORCHC03-SIZE
           CALL    "ORCHC03V03"    USING   SPA-AREA
                                           ORCHC03AREA
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
