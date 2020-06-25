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
       PROGRAM-ID.                 ORCSC80.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 診療行為チェック、算定（その他）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.00    MCC-多々納   02/03/18  改正毎にサブプロを分離
      *  01.00.01    NACL-多々納  04/03/16  Ｈ１６年４月改正追加
      *  01.00.02    NACL-多々納  06/03/16  Ｈ１８年４月改正追加
      *  04.02.00    NACL-多々納  08/03/XX  Ｈ２０年４月改正追加
      *  04.05.00    NACL-多々納  10/03/XX  Ｈ２２年４月改正追加
      *  04.07.00    NACL-多々納  12/03/XX  Ｈ２４年４月改正追加
      *  04.07.00    NACL-多々納  12/05/15  Ｈ２５年４月追加
      *  04.07.00    NACL-多々納  14/03/XX  Ｈ２６年４月追加
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
                   CALL    "ORCSC80200203"     USING
                                               MCPAREA
                                               ORCSC00AREA
                                               WRK-SCR-REC
                                               SPA-AREA
      *
               WHEN   (SPA-SRYYMD          >=  "20020401" )  AND
                      (SPA-SRYYMD          <   "20040401" )
                   CALL    "ORCSC80200204"     USING
                                               MCPAREA
                                               ORCSC00AREA
                                               WRK-SCR-REC
                                               SPA-AREA
      *
               WHEN   (SPA-SRYYMD          >=  "20040401" )  AND
                      (SPA-SRYYMD          <   "20050401" )
                   CALL    "ORCSC80200404"     USING
                                               MCPAREA
                                               ORCSC00AREA
                                               WRK-SCR-REC
                                               SPA-AREA
      *
               WHEN   (SPA-SRYYMD          >=  "20050401" )  AND
                      (SPA-SRYYMD          <   "20060401" )
                   CALL    "ORCSC80200504"     USING
                                               MCPAREA
                                               ORCSC00AREA
                                               WRK-SCR-REC
                                               SPA-AREA
      *
               WHEN   (SPA-SRYYMD          >=  "20060401" )  AND
                      (SPA-SRYYMD          <   "20080401" )
                   CALL    "ORCSC80200604"     USING
                                               MCPAREA
                                               ORCSC00AREA
                                               WRK-SCR-REC
                                               SPA-AREA
      *
               WHEN   (SPA-SRYYMD          >=  "20080401" )  AND
                      (SPA-SRYYMD          <   "20100401" )
                   CALL    "ORCSC80200804"     USING
                                               MCPAREA
                                               ORCSC00AREA
                                               WRK-SCR-REC
                                               SPA-AREA
               WHEN   (SPA-SRYYMD          >=  "20100401" )  AND
                      (SPA-SRYYMD          <   "20120401" )
                   CALL    "ORCSC80201004"     USING
                                               MCPAREA
                                               ORCSC00AREA
                                               WRK-SCR-REC
                                               SPA-AREA
               WHEN   (SPA-SRYYMD          >=  "20120401" )  AND
                      (SPA-SRYYMD          <   "20130401" )
                   CALL    "ORCSC80201204"     USING
                                               MCPAREA
                                               ORCSC00AREA
                                               WRK-SCR-REC
                                               SPA-AREA
               WHEN   (SPA-SRYYMD          >=  "20130401" )  AND
                      (SPA-SRYYMD          <   "20140401" )
                   CALL    "ORCSC80201304"     USING
                                               MCPAREA
                                               ORCSC00AREA
                                               WRK-SCR-REC
                                               SPA-AREA
               WHEN   (SPA-SRYYMD          >=  "20140401" )  AND
                      (SPA-SRYYMD          <   "20160401" )
                   CALL    "ORCSC80201404"     USING
                                               MCPAREA
                                               ORCSC00AREA
                                               WRK-SCR-REC
                                               SPA-AREA
               WHEN   (SPA-SRYYMD          >=  "20160401" )  AND
                      (SPA-SRYYMD          <   "20180401" )
                   CALL    "ORCSC80201604"     USING
                                               MCPAREA
                                               ORCSC00AREA
                                               WRK-SCR-REC
                                               SPA-AREA
               WHEN   (SPA-SRYYMD          >=  "20180401" )  AND
                      (SPA-SRYYMD          <   "99999999" )
                   CALL    "ORCSC80201804"     USING
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
