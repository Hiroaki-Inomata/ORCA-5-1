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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCSGYOUMUGET.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 業務コード取得
      *  コンポーネント名  : 業務コード取得処理
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/08/08    MCC−竹田　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
001200*
001300 ENVIRONMENT             DIVISION.
001400 CONFIGURATION           SECTION.
001500 INPUT-OUTPUT            SECTION.
001600 FILE-CONTROL.
007000 DATA                    DIVISION.
010200 WORKING-STORAGE         SECTION.
      *
      *    フラグ領域
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  FLG-ARI                                 PIC 9(01).
           03  FLG-MAP                                 PIC 9(01).
           03  WRK-LOCK-MATCH-FLG                      PIC S9(09).
           03  FLG-ALLCHK                              PIC 9(01).
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                                  PIC 9(06).
      *
       01  INDEX-AREA.
           03  IDX                                     PIC S9(03). 
           03  IDY                                     PIC S9(03). 
           03  IDZ                                     PIC S9(03). 
           03  IDZZ                                    PIC S9(03). 
      *
      *
       01  WRK-AREA.
           03  WRK-SRYYMD.
               05  WRK-SRYYM           PIC 9(06).
               05  WRK-SRYDD           PIC 9(02).  
      *シェルＩＤ
           03  WRK-LDNAME              PIC X(40).
           03  WRK-LD                  PIC X(20).
           03  WRK-WINDOW              PIC X(10).
           03  WRK-TERMID              PIC X(64).
           03  WRK-OPID                PIC X(16).
      *
           COPY    "CPLOCKCONST.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      **************************************************************************
      *
       LINKAGE                 SECTION.
      *   排他制御ＤＢ制御サブ
           COPY    "COMMON-SPA".
       01  SGYOUMU-CODE                PIC 9(03).
      *
      **************************************************************************
       PROCEDURE           DIVISION
               USING
               SPA-AREA
               SGYOUMU-CODE.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  CNT-AREA
                                       FLG-AREA
                                       WRK-AREA
                                       INDEX-AREA
      *
           MOVE    ZERO            TO  SGYOUMU-CODE
      *
      *    画面ＩＤより業務ＬＤを検索する
           PERFORM 1101-LD-GET-SEC
      *
           .
       000-PROC-EXT.
           EXIT PROGRAM
           .
      *****************************************************************
      *    業務LD取得　処理
      *****************************************************************
       1101-LD-GET-SEC               SECTION.
      *
      *    IF      MCP-WINDOW      =   "P97"
      *            MOVE  SPA-MOTOPG(1:1)   TO  WRK-WINDOW
      *    ELSE
      *            MOVE  MCP-WINDOW(1:1)   TO  WRK-WINDOW
      *    END-IF
           IF      SPA-SAKIPG      =   "P97"
             IF    SPA-MOTOPG(1:1) =   "I"
                   MOVE  SPA-MOTOPG(1:2)   TO  WRK-WINDOW
             ELSE
                   MOVE  SPA-MOTOPG(1:1)   TO  WRK-WINDOW
             END-IF
           ELSE
             IF    SPA-SAKIPG(1:1)     =   "I"
                   MOVE  SPA-SAKIPG(1:2)   TO  WRK-WINDOW
             ELSE
                   MOVE  SPA-SAKIPG(1:1)   TO  WRK-WINDOW
             END-IF
           END-IF
      *
           MOVE    ZERO                    TO  FLG-ARI
           SET     TBLLOCK-IDX             TO  1
           SEARCH  ORC-LOCKCONST-TX        VARYING   TBLLOCK-IDX
               AT   END
                   MOVE    ZERO               TO  SGYOUMU-CODE 
               WHEN   WRK-WINDOW          =   
                      ORC-LOCKCONST-WINDOW (TBLLOCK-IDX)
                   MOVE    1                  TO  FLG-ARI
                   MOVE    ORC-LOCKCONST-GNUM (TBLLOCK-IDX)
                                              TO  SGYOUMU-CODE 
           END-SEARCH
      *
           IF      FLG-ARI             =   ZERO
                   MOVE  999           TO  SGYOUMU-CODE
           END-IF
      *
           .
       1101-LD-GET-EXT.
           EXIT.
