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
      *
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCGW99.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : ＣＬＡＩＭ接続情報（Ｗ９９）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−藤原　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL−太田   04/01/29  送信アドレスを最大件数を
      *                                     ２０件に拡張
      *                                     一括入力対応
      *  01.00.02    NACL−太田   04/03/03  患者登録送信ボタン等追加
      *  02.07.00    NACL−太田   06/02/15  入院オーダー取込ボタン追加
      *  03.05.00    NACL-多々納  07/05/02  グループ診療対応
      *  03.05.01    NACL-多々納  07/07/17  外来診察料展開追加
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    システム管理情報設定用共通パラメタ
           COPY    "W01COMMON-SPA".
      *
      *    画面用ＳＰＡ
       01  SPA-W99.
           03  SPA-W99-AREA.
               05  SPA-GMN-PAGE        PIC 9(02).
               05  SPA-GMN-LINE        PIC 9(02).
               05  SPA-GMN-MAX         PIC 9(03).
               05  SPA-GMN-CUR         PIC 9(03).
      *
               05  SPA-GMN-AREA.
                   07  SPA-GMN-STYUKYMD       PIC X(09).
                   07  SPA-GMN-EDYUKYMD       PIC X(09).              
      *
                   07  SPA-GMN-UKEBUTTON-G.
                       09  SPA-GMN-UKEBUTTON  PIC X(01) OCCURS 2.
                   07  SPA-GMN-KANBUTTON-G.
                       09  SPA-GMN-KANBUTTON  PIC X(01) OCCURS 2.
                   07  SPA-GMN-SYNBUTTON-G.
                       09  SPA-GMN-SYNBUTTON  PIC X(01) OCCURS 2.
      *                                        
                   07  SPA-GMN-SETUZOKU-G.   
                       09  SPA-GMN-SETUZOKU
                                              PIC X(01).
                       09  SPA-GMN-F1         PIC X(01).
                       09  SPA-GMN-SETUZOKUMEI
                                              PIC X(18).
      *                                        
                   07  SPA-GMN-SCODE-G.
                       09  SPA-GMN-SCODE
                                              PIC X(01).
                       09  SPA-GMN-F1         PIC X(01).
                       09  SPA-GMN-SCODEMEI
                                              PIC X(18).
      *                                        
                   07  SPA-GMN-SETUZOKULST      OCCURS  2.
                       09  SPA-GMN-SETUZOKUL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-SETUZOKUMEIL
                                              PIC X(18).
                   07  SPA-GMN-SETUZOKU-MAX
                                              PIC 9(02).    
      *                                        
                   07  SPA-GMN-SCODELST          OCCURS  3.
                       09  SPA-GMN-SCODEL
                                              PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-SCODEMEIL
                                              PIC X(18).
                   07  SPA-GMN-SCODE-MAX     PIC 9(02).    
      *
                   07  SPA-GMN-LBLNO-G.
                       09  SPA-GMN-LBLNO      PIC X(04)
                                              OCCURS 10.
                   07  SPA-GMN-PTINF-SEND-G.
                       09  SPA-GMN-PTINF-SEND PIC X(01) OCCURS 2.
                   07  SPA-GMN-RECEDENKA-SEND-G.
                       09  SPA-GMN-RECEDENKA-SEND
                                              PIC X(01) OCCURS 2.
                   07  SPA-GMN-BYOMEI-AGG-G.
                       09  SPA-GMN-BYOMEI-AGG
                                              PIC X(01) OCCURS 2.
                   07  SPA-GMN-BYOMEI-SYNC-G.
                       09  SPA-GMN-BYOMEI-SYNC
                                              PIC X(01) OCCURS 2.
                   07  SPA-GMN-NYU-ORDER-G.
                       09  SPA-GMN-NYU-ORDER
                                              PIC X(01) OCCURS 2.
                   07  SPA-GMN-MULTIHOST-G.
                       09  SPA-GMN-MULTIHOST  PIC X(01) OCCURS 2.
                   07  SPA-GMN-RESEND-G.
                       09  SPA-GMN-RESEND     PIC X(01) OCCURS 2.
                   07  SPA-GMN-TBL.            
                       09  SPA-GMN-TBL-G      OCCURS  20.
                           11  SPA-GMN-TADDRESS-G.
                               13  SPA-GMN-SADDRESS
                                              PIC X(40).
                               13  SPA-GMN-SPORT1
                                              PIC X(05).
                               13  SPA-GMN-SPORT2
                                              PIC X(05).
                               13  SPA-GMN-SMEMO
                                              PIC X(40).
      *            外来診察料展開追加
                   07  SPA-GMN-SINSATU-FLG-G.
                       09  SPA-GMN-SINSATU-FLG     PIC X(01).
                       09  SPA-GMN-SINSATU-F1      PIC X(01).
                       09  SPA-GMN-SINSATU-MEI     PIC X(28).
                   07  SPA-GMN-SINSATU-LST-G.
                       09  SPA-GMN-SINSATU-LST     PIC X(30) 
                                                   OCCURS   5.
                   07  SPA-GMN-SINSATU-MAX         PIC 9(04).
      *
               05  SPA-NAIB-AREA.
                   07  SPA-NAI-STYUKYMD       PIC 9(08).
                   07  SPA-NAI-EDYUKYMD       PIC 9(08).
      *             
                   07  SPA-NAI-TBL.
                       09  SPA-NAI-TBLO       OCCURS  20.
                           11  SPA-NAI-TADDRESS-G.
                               13  SPA-NAI-SADDRESS
                                              PIC X(40).
                               13  SPA-NAI-SPORT1
                                              PIC X(05).
                               13  SPA-NAI-SPORT2
                                              PIC X(05).
                               13  SPA-NAI-SMEMO
                                              PIC X(40).
      *
              05  SPA-NAI-SELNO               PIC 9(03).
      *
           COPY    "ENUM-VALUE".
      *
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
      *
      *    カウント領域
      *
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
      *
      *    添字領域
      *
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX0                PIC 9(04).
           03  IDXA                PIC 9(04).
           03  IDXB                PIC 9(04).
      *
      *    一時領域
      *
       01  WRK-AREA.
           03  WRK-Z9              PIC Z9.
           03  WRK-GMN-CUR         PIC 9(03).
           03  WRK-PATH            PIC X(64).
           03  WRK-SELNO           PIC 9(03).
           03  WRK-NUM             PIC 9(02).
           03  WRK-WIDMSG          PIC X(70). 
           03  WRK-YMD             PIC X(09). 
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
       01  WRK-ADDRESS.
           03  WRK-ADDRESS1        PIC X(08).
           03  WRK-ADDRESS2        PIC 9(02).
       01  WRK-PORT.
           03  WRK-PORT1           PIC X(05).
           03  WRK-PORT2           PIC 9(02).
       01  WRK-MEMO.
           03  WRK-MEMO1           PIC X(05).
           03  WRK-MEMO2           PIC 9(02).
      *
       01  WRK-CONS-LINE-MAX       PIC 9(02)   VALUE   20.
       01  WRK-CONS-GMN-LINE-MAX   PIC 9(02)   VALUE   10.
       01  WRK-CONS-PAGE-MAX       PIC 9(02)   VALUE   2.
       01  WRK-KBNCD               PIC 9(02).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSYSKANRI.INC".
      *    ＣＬＡＩＭ医療機関情報
           COPY  "CPSK9000.INC".
      *    ＣＬＡＩＭ接続設定情報
           COPY  "CPSK9001.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *
      *    日付サブルーチン領域
      *
           COPY  "CPORCSDAY.INC".
      *
           COPY  "CPORCSLNK.INC".
      *     
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA101SCRAREA.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-W01KYOUTU
           MOVE    SPA-FREE        TO  SPA-W99
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN    "PUTG"          ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-W99         TO  SPA-FREE
           MOVE    SPA-W01KYOUTU   TO  SPA-WORK 
           MOVE    SPA-AREA        TO  SPA-COMMON
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "WERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF      FLG-END             =   1
                   GO  TO  100-INIT-EXT
               END-IF
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               PERFORM 510-GSRAUTH-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      * 
           MOVE    SPACE                TO  MCP-PUTTYPE
           MOVE    "W99"                TO  MCP-WINDOW
           PERFORM 900-PUT-WINDOW
           MOVE    1                    TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    他のＬＤより
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
           END-IF
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "WID1"
                   PERFORM 330-WID1-SET-SEC
                   GO      TO      300-SCREEN-EXT
           END-EVALUATE
      *
           PERFORM 310-SPASET-SEC
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-WID1-SET-SEC            SECTION.
      *
           IF      SPA-WID1-FLG    =   "OK"
               PERFORM 490-KOUSHIN-SEC
           END-IF
      *
           MOVE    SPACE           TO  SPA-WID1-FLG
           MOVE    SPACE           TO  SPA-WIDCD      
           .
       330-WID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE   SPA-W99-AREA
      *
           MOVE    SPA-W01-STYUKYMDH   TO  SPA-GMN-STYUKYMD
           MOVE    SPA-W01-EDYUKYMDH   TO  SPA-GMN-EDYUKYMD
      *     
           MOVE    "0"                 TO  SPA-GMN-SETUZOKUL    (1)
           MOVE    "しない"            TO  SPA-GMN-SETUZOKUMEIL (1)
           MOVE    "1"                 TO  SPA-GMN-SETUZOKUL    (2)
           MOVE    "する"              TO  SPA-GMN-SETUZOKUMEIL (2)
           MOVE    2                   TO  SPA-GMN-SETUZOKU-MAX 
      *     
           MOVE    "1"                 TO  SPA-GMN-SCODEL      (1)
           MOVE    "ＥＵＣ"            TO  SPA-GMN-SCODEMEIL   (1)
           MOVE    "2"                 TO  SPA-GMN-SCODEL      (2)
           MOVE    "Ｓ−ｊｉｓ"        TO  SPA-GMN-SCODEMEIL   (2)
           MOVE    "3"                 TO  SPA-GMN-SCODEL      (3)
           MOVE    "ＵＴＦ−８"        TO  SPA-GMN-SCODEMEIL   (3)
           MOVE    3                   TO  SPA-GMN-SCODE-MAX 
      * 
           MOVE    SPA-GMN-SETUZOKULST (1)
                                       TO  SPA-GMN-SETUZOKU-G
      *    外来診察料展開
           MOVE    "0 標準変換（再診料のみ）"
                                       TO  SPA-GMN-SINSATU-LST (1)
           MOVE    "1 診察料変換"      TO  SPA-GMN-SINSATU-LST (2)
           MOVE    2                   TO  SPA-GMN-SINSATU-MAX
      * 
           MOVE    SPACE               TO  SYS-9000-REC
           INITIALIZE                      SYS-9000-REC
           MOVE    "9000"              TO  SYS-9000-KANRICD
           MOVE    "*"                 TO  SYS-9000-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-9000-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-9000-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-9000-HOSPNUM
           MOVE    SYS-9000-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-9000-REC
               MOVE    SYS-9000-SETUZOKU   TO  SPA-GMN-SETUZOKU 
      *
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-SETUZOKU-MAX
                 IF        SPA-GMN-SETUZOKU-G(1:1)
                                   =   SPA-GMN-SETUZOKUL (IDY)
                   MOVE    SPA-GMN-SETUZOKULST (IDY)
                                           TO  SPA-GMN-SETUZOKU-G
                   MOVE    SPA-GMN-SETUZOKU-MAX
                                           TO  IDY
                 END-IF                                     
               END-PERFORM
      *
               MOVE    SYS-9000-CODE       TO  SPA-GMN-SCODE  
      *
               PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       SPA-GMN-SCODE-MAX
                 IF        SPA-GMN-SCODE-G(1:1)
                                   =   SPA-GMN-SCODEL (IDY)
                   MOVE    SPA-GMN-SCODELST (IDY)
                                           TO  SPA-GMN-SCODE-G
                   MOVE    SPA-GMN-SCODE-MAX
                                           TO  IDY
                 END-IF                                     
               END-PERFORM
               MOVE    SYS-9000-UKEBUTTON (1)
                                       TO  SPA-GMN-UKEBUTTON (1)  
               MOVE    SYS-9000-UKEBUTTON (2)
                                       TO  SPA-GMN-UKEBUTTON (2)  
               MOVE    SYS-9000-KANBUTTON (1)
                                       TO  SPA-GMN-KANBUTTON (1)  
               MOVE    SYS-9000-KANBUTTON (2)
                                       TO  SPA-GMN-KANBUTTON (2)  
               MOVE    SYS-9000-SYNBUTTON (1)
                                       TO  SPA-GMN-SYNBUTTON (1)  
               MOVE    SYS-9000-SYNBUTTON (2)
                                       TO  SPA-GMN-SYNBUTTON (2)  
      *
               IF    ( SYS-9000-PT-FLG             =   "1" )
                   MOVE    "F"         TO  SPA-GMN-PTINF-SEND (1)
                   MOVE    "T"         TO  SPA-GMN-PTINF-SEND (2)
               ELSE
                   MOVE    "T"         TO  SPA-GMN-PTINF-SEND (1)
                   MOVE    "F"         TO  SPA-GMN-PTINF-SEND (2)
               END-IF
      *
               IF    ( SYS-9000-RECEDEN-FLG        =   "1" )
                   MOVE    "F"         TO  SPA-GMN-RECEDENKA-SEND (1)
                   MOVE    "T"         TO  SPA-GMN-RECEDENKA-SEND (2)
               ELSE
                   MOVE    "T"         TO  SPA-GMN-RECEDENKA-SEND (1)
                   MOVE    "F"         TO  SPA-GMN-RECEDENKA-SEND (2)
               END-IF
      *
               IF    ( SYS-9000-BYOMEI-SYU-FLG     =   "1" )
                   MOVE    "F"         TO  SPA-GMN-BYOMEI-AGG (1)
                   MOVE    "T"         TO  SPA-GMN-BYOMEI-AGG (2)
               ELSE
                   MOVE    "T"         TO  SPA-GMN-BYOMEI-AGG (1)
                   MOVE    "F"         TO  SPA-GMN-BYOMEI-AGG (2)
               END-IF
      *
               IF    ( SYS-9000-BYOMEI-DOUKI-FLG   =   "1" )
                   MOVE    "F"         TO  SPA-GMN-BYOMEI-SYNC (1)
                   MOVE    "T"         TO  SPA-GMN-BYOMEI-SYNC (2)
               ELSE
                   MOVE    "T"         TO  SPA-GMN-BYOMEI-SYNC (1)
                   MOVE    "F"         TO  SPA-GMN-BYOMEI-SYNC (2)
               END-IF
      *
               IF    ( SYS-9000-NYU-ORDER-FLG   =   "1" )
                   MOVE    "F"         TO  SPA-GMN-NYU-ORDER (1)
                   MOVE    "T"         TO  SPA-GMN-NYU-ORDER (2)
               ELSE
                   MOVE    "T"         TO  SPA-GMN-NYU-ORDER (1)
                   MOVE    "F"         TO  SPA-GMN-NYU-ORDER (2)
               END-IF
      *
               IF    ( SYS-9000-MULTIHOST-FLG   =   "1" )
                   MOVE    "F"         TO  SPA-GMN-MULTIHOST (1)
                   MOVE    "T"         TO  SPA-GMN-MULTIHOST (2)
               ELSE
                   MOVE    "T"         TO  SPA-GMN-MULTIHOST (1)
                   MOVE    "F"         TO  SPA-GMN-MULTIHOST (2)
               END-IF
      *
               IF    ( SYS-9000-RESEND-FLG   =   "1" )
                   MOVE    "F"         TO  SPA-GMN-RESEND (1)
                   MOVE    "T"         TO  SPA-GMN-RESEND (2)
               ELSE
                   MOVE    "T"         TO  SPA-GMN-RESEND (1)
                   MOVE    "F"         TO  SPA-GMN-RESEND (2)
               END-IF
      *        外来診察料展開
               MOVE    SYS-9000-SINSATU-FLG    TO  SPA-GMN-SINSATU-FLG
           ELSE
               MOVE    "T" 
                                       TO  SPA-GMN-UKEBUTTON (1)  
               MOVE    "F" 
                                       TO  SPA-GMN-UKEBUTTON (2)  
               MOVE    "T" 
                                       TO  SPA-GMN-KANBUTTON (1)  
               MOVE    "F" 
                                       TO  SPA-GMN-KANBUTTON (2)  
               MOVE    "T" 
                                       TO  SPA-GMN-SYNBUTTON (1)  
               MOVE    "F" 
                                       TO  SPA-GMN-SYNBUTTON (2)  
      *
               MOVE    "T"             TO  SPA-GMN-PTINF-SEND (1)
               MOVE    "F"             TO  SPA-GMN-PTINF-SEND (2)
               MOVE    "T"             TO  SPA-GMN-RECEDENKA-SEND (1)
               MOVE    "F"             TO  SPA-GMN-RECEDENKA-SEND (2)
               MOVE    "T"             TO  SPA-GMN-BYOMEI-AGG (1)
               MOVE    "F"             TO  SPA-GMN-BYOMEI-AGG (2)
               MOVE    "T"             TO  SPA-GMN-BYOMEI-SYNC (1)
               MOVE    "F"             TO  SPA-GMN-BYOMEI-SYNC (2)
               MOVE    "T"             TO  SPA-GMN-NYU-ORDER (1)
               MOVE    "F"             TO  SPA-GMN-NYU-ORDER (2)
               MOVE    "T"             TO  SPA-GMN-MULTIHOST (1)
               MOVE    "F"             TO  SPA-GMN-MULTIHOST (2)
               MOVE    "T"             TO  SPA-GMN-RESEND (1)
               MOVE    "F"             TO  SPA-GMN-RESEND (2)
      *
           END-IF             
      *    外来診察料展開
           EVALUATE    SPA-GMN-SINSATU-FLG
               WHEN    "1"
                   MOVE    SPA-GMN-SINSATU-LST(2)
                                           TO  SPA-GMN-SINSATU-FLG-G
               WHEN    OTHER
                   MOVE    SPA-GMN-SINSATU-LST(1)
                                           TO  SPA-GMN-SINSATU-FLG-G
           END-EVALUATE
      *
           MOVE    SPACE               TO  SYS-9001-REC
           INITIALIZE                      SYS-9001-REC
           MOVE    "9001"              TO  SYS-9001-KANRICD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-9001-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-9001-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-9001-HOSPNUM
           MOVE    SYS-9001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    ZERO                TO  IDX
           PERFORM             UNTIL   FLG-SYSKANRI   =   1
                               OR      IDX            >= 
                                                  WRK-CONS-LINE-MAX
               MOVE    MCPDATA-REC        TO  SYS-9001-REC
               ADD     1              TO  IDX
               MOVE    SYS-9001-ADDRESS 
                                       TO  SPA-GMN-SADDRESS (IDX)  
               MOVE    SYS-9001-PORT1 
                                       TO  SPA-GMN-SPORT1 (IDX)  
               MOVE    SYS-9001-PORT2 
                                       TO  SPA-GMN-SPORT2 (IDX)  
               MOVE    SYS-9001-MEMO 
                                       TO  SPA-GMN-SMEMO (IDX)  
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *                                  
           MOVE    IDX                 TO  SPA-GMN-MAX
      *
           MOVE    1                    TO  SPA-GMN-PAGE
      *
           PERFORM 4200-LBLNO-HEN-SEC
      *
           MOVE    1                    TO  SPA-GMN-CUR
           . 
       31O-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE   "CHANGE"             TO  MCP-PUTTYPE
                   PERFORM 210-BACK
      *    前ページ
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 460-MAEPAGE-SEC
      *    次ページ
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 470-TUGIPAGE-SEC
      *    登録
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 490-TOUROKU-SEC
      *    受付ボタン処理
               WHEN    "CLICKED"       ALSO    "UKEBUTTON1"
               WHEN    "CLICKED"       ALSO    "UKEBUTTON2"
                   PERFORM 430-UKEBUTTON-SEC
      *    患者登録ボタン処理
               WHEN    "CLICKED"       ALSO    "KANBUTTON1"
               WHEN    "CLICKED"       ALSO    "KANBUTTON2"
                   PERFORM 440-KANBUTTON-SEC
      *    診療行為ボタン処理
               WHEN    "CLICKED"       ALSO    "SYNBUTTON1"
               WHEN    "CLICKED"       ALSO    "SYNBUTTON2"
                   PERFORM 450-SYNBUTTON-SEC
      *    患者登録送信ボタン処理
               WHEN    "CLICKED"       ALSO    "PTINF_SEND1"
               WHEN    "CLICKED"       ALSO    "PTINF_SEND2"
                   PERFORM 450-PTINF-SEND-SEC
      *    診療科レセ電送信処理
               WHEN    "CLICKED"       ALSO    "RECEDENKA_SEND1"
               WHEN    "CLICKED"       ALSO    "RECEDENKA_SEND2"
                   PERFORM 450-RECEDENKA-SEND-SEC
      *    病名集約
               WHEN    "CLICKED"       ALSO    "BYOMEI_AGG1"
               WHEN    "CLICKED"       ALSO    "BYOMEI_AGG2"
                   PERFORM 450-BYOMEI-AGG-SEC
      *    病名同期
               WHEN    "CLICKED"       ALSO    "BYOMEI_SYNC1"
               WHEN    "CLICKED"       ALSO    "BYOMEI_SYNC2"
                   PERFORM 450-BYOMEI-SYNC-SEC
      *    入院オーダー取込
               WHEN    "CLICKED"       ALSO    "NYU_ORDER1"
               WHEN    "CLICKED"       ALSO    "NYU_ORDER2"
                   PERFORM 450-NYU-ORDER-SEC
      *    マルチホスト
               WHEN    "CLICKED"       ALSO    "MULTIHOST1"
               WHEN    "CLICKED"       ALSO    "MULTIHOST2"
                   PERFORM 450-MULTIHOST-SEC
      *    再送信
               WHEN    "CLICKED"       ALSO    "RESEND1"
               WHEN    "CLICKED"       ALSO    "RESEND2"
                   PERFORM 450-RESEND-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
      *    EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *    WHEN    "XXXX"
      *        入力個所のセット
      *        PERFORM 400-GMN-INPUT-SEC
      *        入力チェック処理
      *        PERFORM 410-INPUT-CHK-SEC
      *    END-EVALUATE
      *
      *    入力個所のセット
           PERFORM 400-GMN-INPUT-SEC
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       400-GMN-INPUT-SEC           SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-CUR
      *
           .
      *
       400-GMN-INPUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
      ***  IF    ( SPA-ERRCD   NOT =   SPACE )
      *      OR  ( FLG-END     NOT =   ZERO  )
      *        GO  TO  410-INPUT-CHK-EXT
      ***  END-IF
      *
           .
      *
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC            SECTION.
      *
      *    ＣＬＡＩＭ接続区分
           MOVE    W99-SETUZOKU            TO  SPA-GMN-SETUZOKU-G 
      *
      *    送信用コード
           MOVE    W99-SCODE               TO  SPA-GMN-SCODE-G  
      *    外来診察料
           MOVE    W99-SINSATU-FLG         TO  SPA-GMN-SINSATU-FLG-G
      *
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >       WRK-CONS-GMN-LINE-MAX )
      *
               COMPUTE IDX     = ( SPA-GMN-PAGE -  1 )
                               *   WRK-CONS-GMN-LINE-MAX
                               +   IDXA
      *
      *        ＩＰアドレス
               MOVE    W99-ADDRESS (IDXA)  TO  SPA-GMN-SADDRESS (IDX)
      *        ポート１
               MOVE    W99-PORT1   (IDXA)  TO  SPA-GMN-SPORT1   (IDX)
      *        ポート２
               MOVE    W99-PORT2   (IDXA)  TO  SPA-GMN-SPORT2   (IDX)
      *        メモ入力
               MOVE    W99-MEMO    (IDXA)  TO  SPA-GMN-SMEMO    (IDX)
           END-PERFORM
      *
           .
      *
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC              SECTION.
      *
      *    ＣＬＡＩＭ接続区分チェック
           PERFORM 4100-SETUZOKU-CHK-SEC
      *
           IF    ( SPA-GMN-SETUZOKU    =   "1" )
      *        送信用コードチェック
               IF      SPA-ERRCD           =   SPACE
                   PERFORM 4101-CODE-CHK-SEC
               END-IF
           END-IF
      *
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   WRK-CONS-GMN-LINE-MAX )
                     OR  ( SPA-ERRCD   NOT =   SPACE )
      *
               COMPUTE IDX     = ( SPA-GMN-PAGE -  1 )
                               *   WRK-CONS-GMN-LINE-MAX
                               +   IDXA
      *
      *        ＩＰアドレス入力チェック処理 
               IF    ( SPA-ERRCD       =   SPACE )
                   PERFORM 4102-ADDRESS-CHK-SEC
               END-IF
      *
      *        ポート１入力チェック処理
               IF    ( SPA-ERRCD       =   SPACE )
                   PERFORM 4103-PORT1-CHK-SEC
               END-IF
      *
      *        ポート２入力チェック処理
               IF    ( SPA-ERRCD       =   SPACE )
                   PERFORM 4104-PORT2-CHK-SEC
               END-IF
      *
      *        メモ入力チェック処理
               IF    ( SPA-ERRCD       =   SPACE )
                   PERFORM 4105-MEMO-CHK-SEC
               END-IF
           END-PERFORM
      *
      *    外来診察料展開
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4106-SINSATU-CHK-SEC
           END-IF
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *   受付ボタン処理 
      *****************************************************************
       430-UKEBUTTON-SEC           SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           MOVE    W99-UKEBUTTON1         TO  SPA-GMN-UKEBUTTON (1) 
           MOVE    W99-UKEBUTTON2         TO  SPA-GMN-UKEBUTTON (2) 
      *
           MOVE    5                      TO  SPA-GMN-CUR
      *
           .
       430-UKEBUTTON-SEC-EXT.
           EXIT.
      *
      *****************************************************************
      *   患者登録ボタン処理 
      *****************************************************************
       440-KANBUTTON-SEC           SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           MOVE    W99-KANBUTTON1         TO  SPA-GMN-KANBUTTON (1) 
           MOVE    W99-KANBUTTON2         TO  SPA-GMN-KANBUTTON (2) 
      *
           MOVE    7                      TO  SPA-GMN-CUR
      *
           .
       440-KANBUTTON-SEC-EXT.
           EXIT.
      *
      *****************************************************************
      *   診療行為ボタン処理 
      *****************************************************************
       450-SYNBUTTON-SEC           SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           MOVE    W99-SYNBUTTON1         TO  SPA-GMN-SYNBUTTON (1) 
           MOVE    W99-SYNBUTTON2         TO  SPA-GMN-SYNBUTTON (2) 
      *
      *D   MOVE    9                      TO  SPA-GMN-CUR
           MOVE    50                     TO  SPA-GMN-CUR
      *
           .
       450-SYNBUTTON-SEC-EXT.
           EXIT.
      *****************************************************************
      *   患者登録送信ボタン処理
      *****************************************************************
       450-PTINF-SEND-SEC              SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           MOVE    W99-PTINF-SEND1     TO  SPA-GMN-PTINF-SEND (1) 
           MOVE    W99-PTINF-SEND2     TO  SPA-GMN-PTINF-SEND (2) 
      *
           MOVE    103                 TO  SPA-GMN-CUR
      *
           .
       450-PTINF-SEND-EXT.
           EXIT.
      *****************************************************************
      *   診療科レセ電送信処理
      *****************************************************************
       450-RECEDENKA-SEND-SEC          SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           MOVE    W99-RECEDENKA-SEND1 TO  SPA-GMN-RECEDENKA-SEND (1)
           MOVE    W99-RECEDENKA-SEND2 TO  SPA-GMN-RECEDENKA-SEND (2)
      *
           MOVE    105                 TO  SPA-GMN-CUR
      *
           .
       450-RECEDENKA-SEND-EXT.
           EXIT.
      *****************************************************************
      *   病名集約
      *****************************************************************
       450-BYOMEI-AGG-SEC              SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           MOVE    W99-BYOMEI-AGG1     TO  SPA-GMN-BYOMEI-AGG (1)
           MOVE    W99-BYOMEI-AGG2     TO  SPA-GMN-BYOMEI-AGG (2)
      *
           MOVE    107                 TO  SPA-GMN-CUR
      *
           .
       450-BYOMEI-AGG-EXT.
           EXIT.
      *****************************************************************
      *   病名同期
      *****************************************************************
       450-BYOMEI-SYNC-SEC              SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           MOVE    W99-BYOMEI-SYNC1    TO  SPA-GMN-BYOMEI-SYNC (1)
           MOVE    W99-BYOMEI-SYNC2    TO  SPA-GMN-BYOMEI-SYNC (2)
      *
           MOVE    109                 TO  SPA-GMN-CUR
      *
           .
       450-BYOMEI-SYNC-EXT.
           EXIT.
      *****************************************************************
      *   入院オーダー取込
      *****************************************************************
       450-NYU-ORDER-SEC               SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           MOVE    W99-NYU-ORDER1      TO  SPA-GMN-NYU-ORDER (1)
           MOVE    W99-NYU-ORDER2      TO  SPA-GMN-NYU-ORDER (2)
      *
      *
           MOVE    111                 TO  SPA-GMN-CUR
      *
           .
       450-NYU-ORDER-EXT.
           EXIT.
      *****************************************************************
      *   マルチホスト
      *****************************************************************
       450-MULTIHOST-SEC               SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           MOVE    W99-MULTIHOST1      TO  SPA-GMN-MULTIHOST (1)
           MOVE    W99-MULTIHOST2      TO  SPA-GMN-MULTIHOST (2)
      *
      *
           MOVE    113                 TO  SPA-GMN-CUR
      *
           .
       450-MULTIHOST-EXT.
           EXIT.
      *****************************************************************
      *   再送信
      *****************************************************************
       450-RESEND-SEC               SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           MOVE    W99-RESEND1      TO  SPA-GMN-RESEND (1)
           MOVE    W99-RESEND2      TO  SPA-GMN-RESEND (2)
      *
      *
           MOVE    9                TO  SPA-GMN-CUR
      *
           .
       450-RESEND-EXT.
           EXIT.
      *****************************************************************
      *   ＩＰアドレス入力チェック処理 
      *****************************************************************
       4102-ADDRESS-CHK-SEC       SECTION.
      *
           COMPUTE WRK-GMN-CUR            =   9  +  4 * (IDX  - 1)
      *
      *    半角チェック
           IF      SPA-GMN-SADDRESS (IDX) NOT =   SPACE     
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI 
               MOVE    SPA-GMN-SADDRESS (IDX)
                                           TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
               IF      KANACHK-RC          =   ZERO
               AND     KANACHK-SYUBETU     =   "1"
                   MOVE    KANACHK-OUT-INPUT
                                           TO  SPA-GMN-SADDRESS (IDX) 
               ELSE
                   MOVE    "0004"          TO  SPA-ERRCD
                   MOVE    WRK-GMN-CUR     TO  SPA-GMN-CUR
               END-IF
           ELSE
               IF    ( MCP-WIDGET (1:8)    =   "SADDRESS" )
                   MOVE    MCP-WIDGET      TO  WRK-ADDRESS
                   IF    ( WRK-ADDRESS2    =   IDX )
                       MOVE    49          TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
      *
           .
       4102-ADDRESS-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *   ポート１入力チェック処理 
      *****************************************************************
       4103-PORT1-CHK-SEC          SECTION.
      *
           COMPUTE WRK-GMN-CUR         =   10
                                       +   4 * (IDX  - 1)
      *
      *    半角チェック
           IF      SPA-GMN-SPORT1 (IDX) NOT =   SPACE     
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI 
               MOVE    SPA-GMN-SPORT1 (IDX)
                                           TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
               IF      KANACHK-RC          =   ZERO
               AND     KANACHK-SYUBETU     =   "1"
                   MOVE    KANACHK-OUT-INPUT
                                           TO  SPA-GMN-SPORT1 (IDX) 
               ELSE
                   MOVE    "0004"          TO  SPA-ERRCD
                   MOVE    WRK-GMN-CUR     TO  SPA-GMN-CUR
               END-IF
           END-IF    
      *
           .
       4103-PORT1-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *   ポート２入力チェック処理 
      *****************************************************************
       4104-PORT2-CHK-SEC          SECTION.
      *
           COMPUTE WRK-GMN-CUR         =   9 +  4 * (IDX  - 1)
      *
      *    半角チェック
           IF      SPA-GMN-SPORT2 (IDX) NOT =   SPACE     
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI 
               MOVE    SPA-GMN-SPORT2 (IDX)
                                           TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
               IF      KANACHK-RC          =   ZERO
               AND     KANACHK-SYUBETU     =   "1"
                   MOVE    KANACHK-OUT-INPUT
                                           TO  SPA-GMN-SPORT2 (IDX) 
               ELSE
                   MOVE    "0004"              TO  SPA-ERRCD
                   MOVE    WRK-GMN-CUR     TO  SPA-GMN-CUR
               END-IF
           END-IF    
      *
           .
       4104-PORT2-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *   メモ入力チェック処理 
      *****************************************************************
       4105-MEMO-CHK-SEC       SECTION.
      *
           COMPUTE WRK-GMN-CUR             =   12 +  4 * (IDX  - 1)
      *
      *    半角チェック
           IF      SPA-GMN-SMEMO (IDX)     NOT =   SPACE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI 
               MOVE    SPA-GMN-SMEMO(IDX)
                                           TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
               IF      KANACHK-RC          =   ZERO
               AND     KANACHK-SYUBETU     =   "2"
                   MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-SMEMO(IDX) 
               ELSE
                   MOVE    "0005"              TO  SPA-ERRCD
                   MOVE    WRK-GMN-CUR     TO  SPA-GMN-CUR
               END-IF
           END-IF    
      *
           .
       4105-MEMO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来診察料展開区分チェック
      *****************************************************************
       4106-SINSATU-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL ( IDY     >   SPA-GMN-SINSATU-MAX )
                    OR   ( FLG-OK      =   1    )
               IF     ( SPA-GMN-SINSATU-LST (IDY)(1:1)
                                           =    SPA-GMN-SINSATU-FLG )
                   MOVE    SPA-GMN-SINSATU-LST (IDY)
                                           TO  SPA-GMN-SINSATU-FLG-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF     (FLG-OK              =   ZERO)
               MOVE    "0010"              TO  SPA-ERRCD
               MOVE    50                  TO  SPA-GMN-CUR
           END-IF    
           .
       4106-SINSATU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＬＡＩＭ接続区分チェック
      *****************************************************************
       4100-SETUZOKU-CHK-SEC       SECTION.
      *
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL ( IDY >       SPA-GMN-SETUZOKU-MAX )
                    OR   ( SPA-GMN-SETUZOKU
                                   =   SPA-GMN-SETUZOKUL (IDY) )
               CONTINUE
           END-PERFORM
      *
           IF     ( IDY >       SPA-GMN-SETUZOKU-MAX )
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
           ELSE
               MOVE    SPA-GMN-SETUZOKULST (IDY)
                                           TO  SPA-GMN-SETUZOKU-G
           END-IF
           .
       4100-SETUZOKU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    送信用コードチェック
      *****************************************************************
       4101-CODE-CHK-SEC       SECTION.
      *
           IF    ( SPA-GMN-SCODE-G     =   SPACE )
               MOVE    SPA-GMN-SCODELST (1)
                                   TO  SPA-GMN-SCODE-G
           END-IF
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL ( IDY >       SPA-GMN-SCODE-MAX )
                    OR   ( FLG-OK      =   1    )
               IF     ( SPA-GMN-SCODE-G(1:1)
                                       =   SPA-GMN-SCODEL (IDY) )
                   MOVE    SPA-GMN-SCODELST (IDY)
                                           TO  SPA-GMN-SCODE-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF     (FLG-OK              =   ZERO)
               MOVE    "0002"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-CUR
           END-IF    
           .
       4101-CODE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ラベル番号編集処理
      *****************************************************************
       4200-LBLNO-HEN-SEC              SECTION.
      *
           INITIALIZE              SPA-GMN-LBLNO-G
      *
           PERFORM VARYING IDXB    FROM    1   BY  1
                   UNTIL ( IDXB    >   WRK-CONS-GMN-LINE-MAX )
      *
               COMPUTE IDX0    = ( SPA-GMN-PAGE -  1 )
                               *   WRK-CONS-GMN-LINE-MAX
                               +   IDXB
      *
               MOVE    IDX0        TO  WRK-Z9
      *
               INITIALIZE              ORCSKANACHKAREA
               MOVE    "2"         TO  KANACHK-SYORI
               MOVE    WRK-Z9      TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
               IF    ( KANACHK-RC      =   ZERO )
                   MOVE    KANACHK-OUT-INPUT
                                   TO  SPA-GMN-LBLNO (IDXB)
               END-IF
      *
           END-PERFORM
      *
           .
       4200-LBLNO-HEN-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                        SECTION.
      *
           MOVE    "W01"               TO  SPA-SAKIPG
           MOVE    "W99"               TO  SPA-MOTOPG
      *
      *     INITIALIZE                  SPA-W01KYOUTU
      *
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *****************************************************************
      *    前ページ処理
      *****************************************************************
       460-MAEPAGE-SEC                 SECTION.
      *
           IF    ( SPA-GMN-PAGE    >   1 )
      *
               PERFORM 410-INPUT-CHK-SEC
      *
               IF      SPA-ERRCD       =   SPACE
                   COMPUTE SPA-GMN-PAGE    =   SPA-GMN-PAGE    -   1
                   PERFORM 4200-LBLNO-HEN-SEC
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    9               TO  SPA-GMN-CUR
           END-IF
      *
           .
       460-MAEPAGE-EXT.
           EXIT.
      *****************************************************************
      *    次ページ処理
      *****************************************************************
       470-TUGIPAGE-SEC                SECTION.
      *
           IF    ( SPA-GMN-PAGE    <   WRK-CONS-PAGE-MAX )
      *
               PERFORM 410-INPUT-CHK-SEC
      *
               IF      SPA-ERRCD       =   SPACE
                   COMPUTE SPA-GMN-PAGE    =   SPA-GMN-PAGE    +   1
                   PERFORM 4200-LBLNO-HEN-SEC
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    9               TO  SPA-GMN-CUR
           END-IF
      *
           .
       470-TUGIPAGE-EXT.
           EXIT.
      *
      *****************************************************************
      *       登録前処理
      *****************************************************************
       490-TOUROKU-SEC              SECTION.
      *
      *    基本チェック
           PERFORM 410-INPUT-CHK-SEC
      *
      *    関連チェック
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4900-KANRENCHK-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    "1001"          TO  SPA-WIDCD
           END-IF
      *
           .
       490-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *       登録処理
      *****************************************************************
       490-KOUSHIN-SEC           SECTION.
      *
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD        TO  SYS-UPYMD
           MOVE    SMCNDATE-HMS        TO  SYS-UPHMS
      *
      *    労災医療機関情報
           MOVE    SPACE               TO  SYS-9000-REC
           INITIALIZE                      SYS-9000-REC
           MOVE    "9000"                  TO  SYS-9000-KANRICD
           MOVE    "*"                     TO  SYS-9000-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-9000-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-9000-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-9000-HOSPNUM
           MOVE    SYS-9000-REC            TO  MCPDATA-REC
      *****MOVE    "SYSKANRI-KEY"          TO  WRK-PATH
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    SYSKANRI-REC            TO  SYS-9000-REC
           ELSE
               MOVE    SPACE                   TO  SYS-9000-REC
               INITIALIZE                      SYS-9000-REC
               MOVE    "9000"                  TO  SYS-9000-KANRICD
               MOVE    "*"                     TO  SYS-9000-KBNCD
           END-IF
      *
           MOVE    "00000000"              TO  SYS-9000-STYUKYMD
           MOVE    "99999999"              TO  SYS-9000-EDYUKYMD
           MOVE    SPA-GMN-SETUZOKU        TO  SYS-9000-SETUZOKU
           MOVE    SPA-GMN-SCODE           TO  SYS-9000-CODE
           MOVE    SPA-GMN-UKEBUTTON-G     TO  SYS-9000-UKEBUTTON-G
           MOVE    SPA-GMN-KANBUTTON-G     TO  SYS-9000-KANBUTTON-G
           MOVE    SPA-GMN-SYNBUTTON-G     TO  SYS-9000-SYNBUTTON-G
      *    外来診察料展開 
           MOVE    SPA-GMN-SINSATU-FLG     TO  SYS-9000-SINSATU-FLG
      *     
           IF    ( SPA-GMN-PTINF-SEND (2)      =   "T" )
               MOVE    "1"                 TO  SYS-9000-PT-FLG
           ELSE
               MOVE    SPACE               TO  SYS-9000-PT-FLG
           END-IF
      *     
           IF    ( SPA-GMN-RECEDENKA-SEND (2)  =   "T" )
               MOVE    "1"                 TO  SYS-9000-RECEDEN-FLG
           ELSE
               MOVE    SPACE               TO  SYS-9000-RECEDEN-FLG
           END-IF
      *     
           IF    ( SPA-GMN-BYOMEI-AGG (2)      =   "T" )
               MOVE    "1"                 TO  SYS-9000-BYOMEI-SYU-FLG
           ELSE
               MOVE    SPACE               TO  SYS-9000-BYOMEI-SYU-FLG
           END-IF
      *     
           IF    ( SPA-GMN-BYOMEI-SYNC (2)     =   "T" )
               MOVE    "1"                 TO  SYS-9000-BYOMEI-DOUKI-FLG
           ELSE
               MOVE    SPACE               TO  SYS-9000-BYOMEI-DOUKI-FLG
           END-IF
      *     
           IF    ( SPA-GMN-NYU-ORDER (2)       =   "T" )
               MOVE    "1"                 TO  SYS-9000-NYU-ORDER-FLG
           ELSE
               MOVE    SPACE               TO  SYS-9000-NYU-ORDER-FLG
           END-IF
      *     
           IF    ( SPA-GMN-MULTIHOST (2)       =   "T" )
               MOVE    "1"                 TO  SYS-9000-MULTIHOST-FLG
           ELSE
               MOVE    SPACE               TO  SYS-9000-MULTIHOST-FLG
           END-IF
      *     
           IF    ( SPA-GMN-RESEND (2)       =   "T" )
               MOVE    "1"                 TO  SYS-9000-RESEND-FLG
           ELSE
               MOVE    SPACE               TO  SYS-9000-RESEND-FLG
           END-IF
      *     
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    SMCNDATE-YMD        TO  SYS-9000-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYS-9000-UPHMS
           ELSE
               MOVE    SMCNDATE-YMD        TO  SYS-9000-CREYMD
                                               SYS-9000-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYS-9000-UPHMS
           END-IF
      *
           MOVE    SPA-HOSPNUM         TO  SYS-9000-HOSPNUM
           MOVE    SYS-9000-REC        TO  SYSKANRI-REC
      *
      *    ＣＬＡＩＭ接続情報
           MOVE    "99999999"          TO  SYS-STYUKYMD
           MOVE    "00000000"          TO  SYS-EDYUKYMD
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "del11"             TO  MCP-PATHNAME
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF    ( MCP-RC              NOT =   ZERO )
               MOVE    "1003"                  TO  SPA-ERRCD
               GO  TO  490-KOUSHIN-EXT
           END-IF
      *
           MOVE    SYS-9000-REC        TO  SYSKANRI-REC
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "DBINSERT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF    ( MCP-RC              NOT =   ZERO )
               MOVE    "1003"                  TO  SPA-ERRCD
               GO  TO  490-KOUSHIN-EXT
           END-IF
      *
      *    ＣＬＡＩＭ接続設定名称削除処理
           INITIALIZE                      SYS-9001-REC
           MOVE    "9001"                  TO  SYS-9001-KANRICD
           MOVE    "99999999"              TO  SYS-9001-STYUKYMD
           MOVE    "00000000"              TO  SYS-9001-EDYUKYMD
           MOVE    SPA-HOSPNUM             TO  SYS-9001-HOSPNUM
           MOVE    SYS-9001-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"              TO  MCP-FUNC
           MOVE    "tbl_syskanri"          TO  MCP-TABLE
           MOVE    "del11"                 TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "1001"                  TO  SPA-ERRCD
               DISPLAY "SYSKANRI-KEY =>" SYS-9001-KEY 
               GO  TO  490-KOUSHIN-EXT
           END-IF
      *
      *    ＣＬＡＩＭ接続設定登録処理
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL  (IDX >       WRK-CONS-LINE-MAX )
                       OR (SPA-ERRCD   NOT =   SPACE     )
               IF      SPA-GMN-SADDRESS (IDX)
                                       NOT  =   SPACE   
                   INITIALIZE                  SYS-9001-REC
                   MOVE    "9001"           TO  SYS-9001-KANRICD
                   MOVE    IDX              TO  WRK-KBNCD
                   MOVE    WRK-KBNCD        TO  SYS-9001-KBNCD
                   MOVE    SPA-GMN-SADDRESS (IDX)
                                            TO  SYS-9001-ADDRESS 
                   MOVE    SPA-GMN-SPORT1 (IDX)
                                            TO  SYS-9001-PORT1 
                   MOVE    SPA-GMN-SPORT2 (IDX)
                                            TO  SYS-9001-PORT2 
                   MOVE    SPA-GMN-SMEMO (IDX)
                                            TO  SYS-9001-MEMO 
                   MOVE    "00000000"       TO  SYS-9001-STYUKYMD
                   MOVE    "99999999"       TO  SYS-9001-EDYUKYMD
      *     
                   PERFORM 800-MCNDATE-SEC
                   MOVE    SMCNDATE-YMD        TO  SYS-9001-CREYMD
                                                   SYS-9001-UPYMD
                   MOVE    SMCNDATE-HMS        TO  SYS-9001-UPHMS
      *     
                   MOVE    SPA-HOSPNUM         TO  SYS-9001-HOSPNUM
                   MOVE    SYS-9001-REC        TO  MCPDATA-REC
                   MOVE    "DBINSERT"          TO  MCP-FUNC
      *D           MOVE    "SYSKANRI-KEY"      TO  ORC-DBPATH
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
grpsys             CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
      *
                   IF      MCP-RC          NOT =   ZERO 
                       MOVE    "1002"              TO  SPA-ERRCD
                       DISPLAY "2002 SYSKANRI-KEY =>" SYS-9001-KEY 
                   END-IF
               END-IF    
           END-PERFORM
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           PERFORM 210-BACK 
           .
       490-KOUSHIN-EXT.
           EXIT.
      *
      *****************************************************************
      *       入力項目の関連チェック処理
      *****************************************************************
       4900-KANRENCHK-SEC           SECTION.
      *
      *
           .
       4900-KANRENCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
           PERFORM 510-GSRAUTH-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-WIDCD       NOT =   SPACE
               PERFORM 520-WIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "W99    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           INITIALIZE                       W99
      *
           MOVE    SPA-GMN-STYUKYMD         TO  W99-YUKOSTYMD
           MOVE    SPA-GMN-EDYUKYMD         TO  W99-YUKOEDYMD
      *
           MOVE    SPA-GMN-SETUZOKU-G       TO  W99-SETUZOKU
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       SPA-GMN-SETUZOKU-MAX
               MOVE    SPA-GMN-SETUZOKULST (IDY)
                                       TO  W99-SETUZOKUL (IDY)
           END-PERFORM
           MOVE    SPA-GMN-SETUZOKU-MAX
                                       TO  W99-SETUZOKU-COUNT 
      *     
           PERFORM VARYING IDY     FROM    1   BY  1
                  UNTIL   IDY     >       SPA-GMN-SCODE-MAX
               MOVE    SPA-GMN-SCODELST (IDY)
                                       TO  W99-SCODEL (IDY)
           END-PERFORM
           MOVE    SPA-GMN-SCODE-MAX  TO  W99-SCODE-COUNT 
      *
           MOVE    SPA-GMN-SCODE-G    TO  W99-SCODE                                  
      *
           PERFORM VARYING     IDXA   FROM    1   BY  1
                   UNTIL       IDXA   >       WRK-CONS-GMN-LINE-MAX
      *
               COMPUTE IDX     = ( SPA-GMN-PAGE -  1 )
                               *   WRK-CONS-GMN-LINE-MAX
                               +   IDXA
      *
               MOVE    SPA-GMN-SADDRESS (IDX)
                                           TO  W99-ADDRESS (IDXA)
               MOVE    SPA-GMN-SPORT1   (IDX)
                                           TO  W99-PORT1   (IDXA)
               MOVE    SPA-GMN-SPORT2   (IDX)
                                           TO  W99-PORT2   (IDXA)
               MOVE    SPA-GMN-SMEMO    (IDX)
                                           TO  W99-MEMO    (IDXA)
      *
           END-PERFORM
      *
           PERFORM VARYING IDY     FROM    1   BY  1
                  UNTIL   IDY     >       WRK-CONS-GMN-LINE-MAX
      *
               MOVE    SPA-GMN-LBLNO (IDY) TO  W99-LBLNO   (IDY)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-UKEBUTTON (1)   TO  W99-UKEBUTTON1
           MOVE    SPA-GMN-UKEBUTTON (2)   TO  W99-UKEBUTTON2
           MOVE    SPA-GMN-KANBUTTON (1)   TO  W99-KANBUTTON1
           MOVE    SPA-GMN-KANBUTTON (2)   TO  W99-KANBUTTON2
           MOVE    SPA-GMN-SYNBUTTON (1)   TO  W99-SYNBUTTON1
           MOVE    SPA-GMN-SYNBUTTON (2)   TO  W99-SYNBUTTON2
      *
           MOVE    SPA-GMN-PTINF-SEND (1)      TO   W99-PTINF-SEND1
           MOVE    SPA-GMN-PTINF-SEND (2)      TO   W99-PTINF-SEND2
           MOVE    SPA-GMN-RECEDENKA-SEND (1)  TO   W99-RECEDENKA-SEND1
           MOVE    SPA-GMN-RECEDENKA-SEND (2)  TO   W99-RECEDENKA-SEND2
           MOVE    SPA-GMN-BYOMEI-AGG (1)      TO   W99-BYOMEI-AGG1
           MOVE    SPA-GMN-BYOMEI-AGG (2)      TO   W99-BYOMEI-AGG2
           MOVE    SPA-GMN-BYOMEI-SYNC (1)     TO   W99-BYOMEI-SYNC1
           MOVE    SPA-GMN-BYOMEI-SYNC (2)     TO   W99-BYOMEI-SYNC2
           MOVE    SPA-GMN-NYU-ORDER (1)       TO   W99-NYU-ORDER1
           MOVE    SPA-GMN-NYU-ORDER (2)       TO   W99-NYU-ORDER2
           MOVE    SPA-GMN-MULTIHOST (1)       TO   W99-MULTIHOST1
           MOVE    SPA-GMN-MULTIHOST (2)       TO   W99-MULTIHOST2
           MOVE    SPA-GMN-RESEND (1)          TO   W99-RESEND1
           MOVE    SPA-GMN-RESEND (2)          TO   W99-RESEND2
      *    外来診察料展開
           MOVE    SPA-GMN-SINSATU-FLG-G   TO  W99-SINSATU-FLG
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SINSATU-MAX
               MOVE    SPA-GMN-SINSATU-LST (IDX)
                                       TO  W99-SINSATU-FLG-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-SINSATU-MAX TO  W99-SINSATU-FLG-COUNT
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    業務権限非活性処理
      *****************************************************************
       510-GSRAUTH-SEC             SECTION.
      *
           MOVE    WIDGET-NORMAL   TO  W99-B06-STATE
           MOVE    WIDGET-NORMAL   TO  W99-B07-STATE
      *
           IF    ( SPA-GMN-PAGE    =   1 )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W99-B06-STATE
           END-IF
      *
           IF    ( SPA-GMN-PAGE    =   WRK-CONS-PAGE-MAX )
               MOVE    WIDGET-INSENSITIVE
                                   TO  W99-B07-STATE
           END-IF
      *
           .
       51O-GSRAUTH-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "ＣＬＡＩＭ接続区分の入力に誤りがあります"
                                                         TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "送信用コードの入力に誤りがあります"
                                                         TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "半角で入力して下さい"
                                                         TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "半角で入力して下さい"        TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "全角で入力して下さい"        TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE    "数字で入力して下さい"        TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE    "外来診察料展開区分がありません"
                                                         TO  SPA-ERRMSG
               WHEN    "0099"
                   MOVE    "未入力の項目があります"
                                               TO  SPA-ERRMSG
               WHEN    "1001"
                   MOVE    "更新ができませんでした"
                                               TO  SPA-ERRMSG
               WHEN    "1002"
                   MOVE    "削除ができませんでした"
                                               TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  WERR
           MOVE    SPA-ERRCD            TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  WERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "W99"                TO  SPA-MOTOPG
           MOVE    "WERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "WERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ガイドメッセージ表示
      *****************************************************************
       520-WIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-WIDMSG
      *
           EVALUATE    SPA-WIDCD
               WHEN    "1001"
                   MOVE    "ＣＬＡＩＭ設定情報を登録します"
                                                TO  WRK-WIDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  WID1
           INITIALIZE                       WID1
           MOVE    SPA-WIDCD            TO  WID1-ID1CODE
           MOVE    WRK-WIDMSG           TO  WID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "W99"                TO  SPA-MOTOPG
           MOVE    "WID1"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "WID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       520-WIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理    
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF    ( SPA-ERRCD           =   SPACE)
             AND ( SPA-GMN-CUR         =   ZERO )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-CUR
           WHEN    1
               MOVE  "SETUZOKU"        TO   MCP-WIDGET
           WHEN    2
               MOVE  "SCODE"           TO   MCP-WIDGET
           WHEN    3
               MOVE  "UKEBUTTON1"      TO   MCP-WIDGET
           WHEN    4
               MOVE  "UKEBUTTON2"      TO   MCP-WIDGET
           WHEN    5
               MOVE  "KANBUTTON1"      TO   MCP-WIDGET
           WHEN    6
               MOVE  "KANBUTTON2"      TO   MCP-WIDGET
           WHEN    7
               MOVE  "SYNBUTTON1"      TO   MCP-WIDGET
           WHEN    8
               MOVE  "SYNBUTTON2"      TO   MCP-WIDGET
           WHEN    9
               MOVE  "SADDRESS01"      TO   MCP-WIDGET
           WHEN    13
               MOVE  "SADDRESS02"      TO   MCP-WIDGET
           WHEN    17
               MOVE  "SADDRESS03"      TO   MCP-WIDGET
           WHEN    21
               MOVE  "SADDRESS04"      TO   MCP-WIDGET
           WHEN    25
               MOVE  "SADDRESS05"      TO   MCP-WIDGET
           WHEN    29
               MOVE  "SADDRESS06"      TO   MCP-WIDGET
           WHEN    33
               MOVE  "SADDRESS07"      TO   MCP-WIDGET
           WHEN    37
               MOVE  "SADDRESS08"      TO   MCP-WIDGET
           WHEN    41
               MOVE  "SADDRESS09"      TO   MCP-WIDGET
           WHEN    45
               MOVE  "SADDRESS10"      TO   MCP-WIDGET
           WHEN    10
               MOVE  "SPORT01_1"       TO   MCP-WIDGET
           WHEN    14
               MOVE  "SPORT02_1"       TO   MCP-WIDGET
           WHEN    18
               MOVE  "SPORT03_1"       TO   MCP-WIDGET
           WHEN    22
               MOVE  "SPORT04_1"       TO   MCP-WIDGET
           WHEN    26
               MOVE  "SPORT05_1"       TO   MCP-WIDGET
           WHEN    30
               MOVE  "SPORT06_1"       TO   MCP-WIDGET
           WHEN    34
               MOVE  "SPORT07_1"       TO   MCP-WIDGET
           WHEN    38
               MOVE  "SPORT08_1"       TO   MCP-WIDGET
           WHEN    42
               MOVE  "SPORT09_1"       TO   MCP-WIDGET
           WHEN    46
               MOVE  "SPORT10_1"       TO   MCP-WIDGET
           WHEN    11
               MOVE  "SPORT01_2"       TO   MCP-WIDGET
           WHEN    15
               MOVE  "SPORT02_2"       TO   MCP-WIDGET
           WHEN    19
               MOVE  "SPORT03_2"       TO   MCP-WIDGET
           WHEN    23
               MOVE  "SPORT04_2"       TO   MCP-WIDGET
           WHEN    27
               MOVE  "SPORT05_2"       TO   MCP-WIDGET
           WHEN    31
               MOVE  "SPORT06_2"       TO   MCP-WIDGET
           WHEN    35
               MOVE  "SPORT07_2"       TO   MCP-WIDGET
           WHEN    39
               MOVE  "SPORT08_2"       TO   MCP-WIDGET
           WHEN    43
               MOVE  "SPORT09_2"       TO   MCP-WIDGET
           WHEN    47
               MOVE  "SPORT10_2"       TO   MCP-WIDGET
           WHEN    12
               MOVE  "SMEMO01"         TO   MCP-WIDGET
           WHEN    16
               MOVE  "SMEMO02"         TO   MCP-WIDGET
           WHEN    20
               MOVE  "SMEMO03"         TO   MCP-WIDGET
           WHEN    24
               MOVE  "SMEMO04"         TO   MCP-WIDGET
           WHEN    28
               MOVE  "SMEMO05"         TO   MCP-WIDGET
           WHEN    32
               MOVE  "SMEMO06"         TO   MCP-WIDGET
           WHEN    36
               MOVE  "SMEMO07"         TO   MCP-WIDGET
           WHEN    40
               MOVE  "SMEMO08"         TO   MCP-WIDGET
           WHEN    44
               MOVE  "SMEMO09"         TO   MCP-WIDGET
           WHEN    48
               MOVE  "SMEMO10"         TO   MCP-WIDGET
           WHEN    49
               MOVE  "B12"             TO   MCP-WIDGET
      *
           WHEN    50
               MOVE  "SINSATU_FLG"     TO   MCP-WIDGET
      *
           WHEN    101
               MOVE  "PTINF_SEND1"     TO   MCP-WIDGET
           WHEN    102
               MOVE  "PTINF_SEND2"     TO   MCP-WIDGET
           WHEN    103
               MOVE  "RECEDENKA_SEND1" TO   MCP-WIDGET
           WHEN    104
               MOVE  "RECEDENKA_SEND2" TO   MCP-WIDGET
           WHEN    105
               MOVE  "BYOMEI_AGG1"     TO   MCP-WIDGET
           WHEN    106
               MOVE  "BYOMEI_AGG2"     TO   MCP-WIDGET
           WHEN    107
               MOVE  "BYOMEI_SYNC1"    TO   MCP-WIDGET
           WHEN    108
               MOVE  "BYOMEI_SYNC2"    TO   MCP-WIDGET
           WHEN    109
               MOVE  "NYU_ORDER1"      TO   MCP-WIDGET
           WHEN    110
               MOVE  "NYU_ORDER2"      TO   MCP-WIDGET
           WHEN    111
               MOVE  "MULTIHOST1"      TO   MCP-WIDGET
           WHEN    112
               MOVE  "MULTIHOST2"      TO   MCP-WIDGET
           WHEN    113
               MOVE  "RESEND1"         TO   MCP-WIDGET
           WHEN    114
               MOVE  "RESEND2"         TO   MCP-WIDGET
           END-EVALUATE
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       5011-INPUT-CUR-SEC              SECTION.
      *
           EVALUATE    MCP-WIDGET
           WHEN    "SETUZOKU"
               MOVE  1                 TO   SPA-GMN-CUR
           WHEN    "SCODE"
               MOVE  2                 TO   SPA-GMN-CUR
           WHEN    "UKEBUTTON1"
               MOVE  3                 TO   SPA-GMN-CUR
           WHEN    "UKEBUTTON2"
               MOVE  4                 TO   SPA-GMN-CUR
           WHEN    "KANBUTTON1"
               MOVE  5                 TO   SPA-GMN-CUR
           WHEN    "KANBUTTON2"
               MOVE  6                 TO   SPA-GMN-CUR
           WHEN    "SYNBUTTON1"
               MOVE  7                 TO   SPA-GMN-CUR
           WHEN    "SYNBUTTON2"
      *D       MOVE  8                 TO   SPA-GMN-CUR
               MOVE  49                TO   SPA-GMN-CUR
           WHEN    "SINSATU_FLG"
               MOVE  8                 TO   SPA-GMN-CUR
           WHEN    "SADDRESS01"
               MOVE  9                 TO   SPA-GMN-CUR
           WHEN    "SADDRESS02"
               MOVE  13                TO   SPA-GMN-CUR
           WHEN    "SADDRESS03"
               MOVE  17                TO   SPA-GMN-CUR
           WHEN    "SADDRESS04"
               MOVE  21                TO   SPA-GMN-CUR
           WHEN    "SADDRESS05"
               MOVE  25                TO   SPA-GMN-CUR
           WHEN    "SADDRESS06"
               MOVE  29                TO   SPA-GMN-CUR
           WHEN    "SADDRESS07"
               MOVE  33                TO   SPA-GMN-CUR
           WHEN    "SADDRESS08"
               MOVE  37                TO   SPA-GMN-CUR
           WHEN    "SADDRESS09"
               MOVE  41                TO   SPA-GMN-CUR
           WHEN    "SADDRESS10"
               MOVE  45                TO   SPA-GMN-CUR
           WHEN    "SPORT01_1"
               MOVE  10                TO   SPA-GMN-CUR
           WHEN    "SPORT02_1"
               MOVE  14                TO   SPA-GMN-CUR
           WHEN    "SPORT03_1"
               MOVE  18                TO   SPA-GMN-CUR
           WHEN    "SPORT04_1"
               MOVE  22                TO   SPA-GMN-CUR
           WHEN    "SPORT05_1"
               MOVE  26                TO   SPA-GMN-CUR
           WHEN    "SPORT06_1"
               MOVE  30                TO   SPA-GMN-CUR
           WHEN    "SPORT07_1"
               MOVE  34                TO   SPA-GMN-CUR
           WHEN    "SPORT08_1"
               MOVE  38                TO   SPA-GMN-CUR
           WHEN    "SPORT09_1"
               MOVE  42                TO   SPA-GMN-CUR
           WHEN    "SPORT10_1"
               MOVE  46                TO   SPA-GMN-CUR
           WHEN    "SPORT01_2"
               MOVE  11                TO   SPA-GMN-CUR
           WHEN    "SPORT02_2"
               MOVE  15                TO   SPA-GMN-CUR
           WHEN    "SPORT03_2"
               MOVE  19                TO   SPA-GMN-CUR
           WHEN    "SPORT04_2"
               MOVE  23                TO   SPA-GMN-CUR
           WHEN    "SPORT05_2"
               MOVE  27                TO   SPA-GMN-CUR
           WHEN    "SPORT06_2"
               MOVE  31                TO   SPA-GMN-CUR
           WHEN    "SPORT07_2"
               MOVE  35                TO   SPA-GMN-CUR
           WHEN    "SPORT08_2"
               MOVE  39                TO   SPA-GMN-CUR
           WHEN    "SPORT09_2"
               MOVE  43                TO   SPA-GMN-CUR
           WHEN    "SPORT10_2"
               MOVE  47                TO   SPA-GMN-CUR
           WHEN    "SMEMO01"
               MOVE  12                TO   SPA-GMN-CUR
           WHEN    "SMEMO02"
               MOVE  16                TO   SPA-GMN-CUR
           WHEN    "SMEMO03"
               MOVE  20                TO   SPA-GMN-CUR
           WHEN    "SMEMO04"
               MOVE  24                TO   SPA-GMN-CUR
           WHEN    "SMEMO05"
               MOVE  28                TO   SPA-GMN-CUR
           WHEN    "SMEMO06"
               MOVE  32                TO   SPA-GMN-CUR
           WHEN    "SMEMO07"
               MOVE  36                TO   SPA-GMN-CUR
           WHEN    "SMEMO08"
               MOVE  40                TO   SPA-GMN-CUR
           WHEN    "SMEMO09"
               MOVE  44                TO   SPA-GMN-CUR
           WHEN    "SMEMO10"
               MOVE  48                TO   SPA-GMN-CUR
           WHEN    "B12"
               MOVE  49                TO   SPA-GMN-CUR
           END-EVALUATE
      *
           IF    ( MCP-WIDGET      NOT =    "B12" )
               COMPUTE SPA-GMN-CUR     =    SPA-GMN-CUR
                                       +    1
           END-IF
      *
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       5002-HIZUKE-CHK-SEC         SECTION.
      *
           IF      WRK-YMD  (1:7)  NOT NUMERIC
               INSPECT     WRK-YMD      REPLACING   ALL " " BY  "0"
               MOVE    WRK-YMD(1:1)     TO  WRK-WGO
               MOVE    WRK-YMD(2:2)     TO  WRK-WYY
               MOVE    WRK-YMD(5:2)     TO  WRK-WMM
               MOVE    WRK-YMD(8:2)     TO  WRK-WDD
           ELSE
               MOVE    WRK-YMD(1:7)     TO  WRK-WYMD
           END-IF
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY1-AREA
           MOVE    "12"                TO  LNK-DAY1-IRAI
           MOVE    ZERO                TO  LNK-DAY1-YMD
           MOVE    WRK-WYMD            TO  LNK-DAY1-YMD(2:7)
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY1-AREA
           IF      STS-DAY-RC1     NOT =   ZERO
               MOVE    "0001"          TO  SPA-ERRCD
           ELSE
               MOVE    LNK-DAY1-YMD        TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
           END-IF
           .
       5002-HIZUKE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    和暦西暦変換編集処理
      *****************************************************************
       5002-HIZUKE-HEN-SEC          SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE
               MOVE    SPACE               TO  WRK-HGO
               MOVE    WRK-SYMD(3:2)       TO  WRK-HYY
               MOVE    WRK-SYMD(5:2)       TO  WRK-HMM
               MOVE    WRK-SYMD(7:2)       TO  WRK-HDD
               MOVE    WRK-HENYMD          TO  WRK-HENYMDG
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF
      *
           .
       5002-HIZUKE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    マシン日付取得サブ
      *****************************************************************
       800-MCNDATE-SEC         SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           .
      *
       800-MCNDATE-EXT.
           EXIT.
      *****************************************************************
      *    管理マスター読込（キー）
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
      *
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  SYSKANRI-REC
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ　ＲＥＡＤ
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
