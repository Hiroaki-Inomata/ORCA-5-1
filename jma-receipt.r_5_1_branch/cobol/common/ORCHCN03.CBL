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
       PROGRAM-ID.             ORCHCN03.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院
      *  コンポーネント名  : 入院請求書兼領収書　印刷
      *  管理者            : 
      *  作成日付    作業者        記述
      *  07/10/18    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-太田    03.03.13  入院請求書複数枚一括印刷対応
      *  01.00.02    NACL-太田    03.04.28  自費分負担金額に消費税が
      *                                     落ちていたのを修正
      *  01.00.03    NACL-太田    03.07.01  出力先プリンタサブ追加
      *  01.00.04    NACL-太田    03.08.26  労災自賠責編集修正
      *  01.00.05    NACL-太田    03.10.10  食事負担編集修正
      *  01.00.06    NACL-太田    03.10.21  金額編集領域を拡張
      *  01.00.07    NACL-小豆沢  05.05.25  労災・自賠責の患者請求分は明細を
      *                                     表示するように変更
      *  02.08.00    NACL-太田    06.02.15  再印刷、プレビュー対応
      *  03.04.01    NACL-森脇    07/01/17  医療機関名称、住所セット変更
      *  03.04.02    NACL-森脇    07/01/30  自費消費税H19年4月対応
      *  03.05.00    NACL-太田    07.05.17  グループ診療対応
      *  04.01.00    NACL-太田    07/10/22  公務災害対応
      *  04.02.00    NACL-太田    08/03/07  改正対応
      *  04.03.00    NACL-太田    08/09/17  改正対応
      *  04.04.00    NACL-太田    09/01/14  プログラムオプション対応
      *  04.05.00    NACL-太田    09/07/15  ORCHCN03V03に統合
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
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
      *    請求書兼領収書共通パラメタ
           COPY    "CPORCHCN03.INC".
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           ORCHCN03AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           MOVE    "V1"            TO      ORCHCN03-SIZE
           CALL    "ORCHCN03V03"   USING   ORCHCN03AREA
                                           SPA-AREA
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
