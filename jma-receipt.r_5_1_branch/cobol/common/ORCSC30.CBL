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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCSC30.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 診療行為チェック、算定（注射）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     MCC−多々納  01/04/19  １診療行為内で、点滴は１回とする
      * 01.00.02     MCC−多々納  01/05/16  点滴注射の判定を診療種別とする
      *  01.00.03    MCC-多々納   02/03/20  改正毎にサブプロを分離
      *  04.04.00    NACL-多々納  09/03/23  外来まとめ対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
      *WORKING-STORAGE             SECTION.
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
      *    診療行為算定用共通パラメタ
           COPY    "CPORCSC00.INC".
      *
       01  WRK-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //WRK-//.
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           ORCSC00AREA
           WRK-SCR-REC
           SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
      *
           EVALUATE    TRUE
               WHEN    SPA-SRYYMD          <=  "20020331"
                   CALL    "ORCSC30200203"     USING
                                               MCPAREA
                                               ORCSC00AREA
                                               WRK-SCR-REC
                                               SPA-AREA
      *
               WHEN   (SPA-SRYYMD          >=  "20020401" )  AND
                      (SPA-SRYYMD          <   "20050401" )
                   CALL    "ORCSC30200204"     USING
                                               MCPAREA
                                               ORCSC00AREA
                                               WRK-SCR-REC
                                               SPA-AREA
      *
               WHEN   (SPA-SRYYMD          >=  "20050401" )  AND
                      (SPA-SRYYMD          <   "20060401" )
                   CALL    "ORCSC30200504"     USING
                                               MCPAREA
                                               ORCSC00AREA
                                               WRK-SCR-REC
                                               SPA-AREA
      *
               WHEN   (SPA-SRYYMD          >=  "20060401" )  AND
                      (SPA-SRYYMD          <   "20080401" )
                   CALL    "ORCSC30200604"     USING
                                               MCPAREA
                                               ORCSC00AREA
                                               WRK-SCR-REC
                                               SPA-AREA
      *
               WHEN   (SPA-SRYYMD          >=  "20080401" )  AND
                      (SPA-SRYYMD          <   "20100401" )
                   CALL    "ORCSC30200804"     USING
                                               MCPAREA
                                               ORCSC00AREA
                                               WRK-SCR-REC
                                               SPA-AREA
      *
               WHEN   (SPA-SRYYMD          >=  "20100401" )  AND
                      (SPA-SRYYMD          <   "20160401" )
                   CALL    "ORCSC30201004"     USING
                                               MCPAREA
                                               ORCSC00AREA
                                               WRK-SCR-REC
                                               SPA-AREA
      *
               WHEN   (SPA-SRYYMD          >=  "20160401" )  AND
                      (SPA-SRYYMD          <   "99999999" )
                   CALL    "ORCSC30201604"     USING
                                               MCPAREA
                                               ORCSC00AREA
                                               WRK-SCR-REC
                                               SPA-AREA
           END-EVALUATE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
