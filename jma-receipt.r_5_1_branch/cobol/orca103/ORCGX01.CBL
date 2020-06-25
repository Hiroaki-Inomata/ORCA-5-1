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
       IDENTIFICATION                            DIVISION.
       PROGRAM-ID.                               ORCGX01.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : チェックマスタ情報設定
      *  コンポーネント名  : チェックマスタ情報設定（Ｘ０１）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−藤原　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-多々納   01/07/03  日付入力チェックサブ追加
      *  01.01.01    MCC-藤原     01/09/21  JOINの修正      
      *  01.01.03    MCC-多々納   02/04/26  印刷の追加      
      *  01.01.04    NACL-多々納  02/10/02  添え字を数字へ修正
      *  01.01.05    NACL-多々納  02/11/18  一括入力・診療コード入力修正
      *  01.01.06    NACL-太田    03/11/19  有効期間編集修正
      *  03.05.00    NACL-多々納  07/05/08  グループ診療対応
      *  05.01.00    NACL-多々納  17/09/XX  GINBEE(X08）対応
      *****************************************************************
      *
       ENVIRONMENT                               DIVISION.
       CONFIGURATION                             SECTION.
       INPUT-OUTPUT                              SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    画面活性非活性
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    チェックマスタ情報設定用共通パラメタ
           COPY    "X01COMMON-SPA".
      *
      *    チェックマスタ情報用画面スパ
           COPY    "X01SCR-SPA".
      *
      *    画面用ＳＰＡ
       01  SPA-X01.
           03  SPA-X01-AREA.
               05  SPA-GMN-PAGE        PIC 9(02).
               05  SPA-GMN-LINE        PIC 9(02).
               05  SPA-GMN-MAX         PIC 9(03).
               05  SPA-GMN-CUR         PIC 9(02).
      *
               05  SPA-GMN-AREA.
                   07  SPA-GMN-TBL.
                       09  SPA-GMN-TBLO    OCCURS  100.
                           11  SPA-GMN-TBANGO     PIC 9(03).
                           11  SPA-GMN-TSTYUKYMD  PIC X(09).
                           11  SPA-GMN-TEDYUKYMD  PIC X(09).  
                   07  SPA-GMN-KOUMOKU.
                       09  SPA-GMN-BYOMEI-CHK-FLG    PIC 9(1).
                       09  SPA-GMN-BYOMEICD-CHK-FLG  PIC 9(1).
                       09  SPA-GMN-BYOSTATUS       PIC X(1).
                       09  SPA-GMN-CHKKBN-G.
                           11  SPA-GMN-CHKKBN     PIC X(01).
                           11  SPA-GMN-CHKKBNFL   PIC X(01).
                           11  SPA-GMN-CHKKBNNAME PIC X(30).
                       09  SPA-GMN-SRYCD          PIC X(100). 
                       09  SPA-GMN-NAME           PIC X(100).
                       09  SPA-GMN-KOUMOKU1.
                           11  SPA-GMN-SELNUM     PIC X(03).
                           11  SPA-GMN-STYUKYMD   PIC X(10).
                           11  SPA-GMN-EDYUKYMD   PIC X(10).
                   07  SPA-GMN-CHKKBNLIST         OCCURS  11.
                       09  SPA-GMN-CHKKBNL        PIC X(01).
                       09  SPA-GMN-F1L            PIC X(01).
                       09  SPA-GMN-CHKKBNMEIL     PIC X(30).
                   07  SPA-GMN-CHKKBN-MAX         PIC 9(02).    
      *
               05  SPA-NAIB-AREA.
                   07  SPA-NAI-CHKKBN             PIC X(01).
                   07  SPA-NAI-SRYCD              PIC X(100).
                   07  SPA-NAI-KOUMOKU1.
                       09  SPA-NAI-STYUKYMD       PIC X(08).
                       09  SPA-NAI-EDYUKYMD       PIC X(08).
                       09  SPA-NAI-SELNUM         PIC 9(03).
                       09  SPA-NAI-ID             PIC 9(09).
                       09  SPA-NAI-JYKNKBN            PIC X(01).
                       09  SPA-NAI-BYOCD-G.
                           11  SPA-NAI-BYOCD-OCC      OCCURS  21.
                               13  SPA-NAI-BYOCD      PIC X(07).
                               13  SPA-NAI-BYOCDSEP   PIC X(01).
                   07  SPA-NAI-TBL.
                       09  SPA-NAI-TBLO    OCCURS  100.
                           11  SPA-NAI-TSTYUKYMD  PIC 9(08).
                           11  SPA-NAI-TEDYUKYMD  PIC 9(08). 
                           11  SPA-NAI-NAME       PIC X(100).
                           11  SPA-NAI-TID        PIC 9(09).
                           11  SPA-NAI-TCHOFUKU PIC 9(01).
                   07  SPA-MAE-SRYCD              PIC X(09). 
                   07  SPA-NAI-BYOMEI             PIC X(100).
                   07  SPA-NAI-HOSOKU             PIC X(40).
                   07  SPA-NAI-MOTOPG             PIC X(10). 
      *
              05  SPA-NAI-SELNO                   PIC 9(03).
      *
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-END                            PIC 9(01).
           03  FLG-CHK                            PIC 9(01).
           03  FLG-CHKSND                         PIC 9(01).
           03  FLG-CHKTRD                         PIC 9(01).
           03  FLG-CHK005                         PIC 9(01).
           03  FLG-TENSU                          PIC 9(01).
      *
           03  FLG-OK                             PIC 9(01).
           03  FLG-TOUROKU                        PIC 9(01).
           03  FLG-KANJI                          PIC 9(01).
           03  FLG-KENSAKU                        PIC 9(01).
           03  FLG-ZENKAKU                        PIC 9(01).
           03  FLG-ERR                            PIC 9(01).
           03  FLG-INPUTCD                        PIC 9(01).
           03  FLG-INPUTSET                       PIC 9(01).
           03  FLG-SRYCDCHG                       PIC 9(01).
           03  FLG-USERBYOMEI                     PIC 9(01).
      *
       01  CNT-AREA.
           03  CNT-STR                 PIC 9(05).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                               PIC 9(04).
           03  IDY                               PIC 9(04).
           03  IDX0                              PIC 9(04).
           03  IDX1                              PIC 9(04).
           03  IDX2                              PIC 9(04).
           03  IDXB                              PIC 9(04).
           03  IDXL                              PIC 9(04).
           03  IDX-PNT                           PIC 9(04).
           03  IDX-CMB                           PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SELNO           PIC 9(03).
           03  WRK-MAE-INPUT       PIC X(100).
           03  WRK-OUT-INPUT       PIC X(100).
           03  WRK-YMD             PIC X(10). 
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-ZOGENPTN                PIC X(01).
           03  WRK-ZOGEN                   PIC S9(05).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
      *    入力項目名
           03  WRK-MCP-WIDGET          PIC X(64).
      *
           03  WRK-INPUTCD             PIC X(20).
           03  WRK-CD-MCNT             PIC 9(02).
           03  WRK-CHKKBN              PIC X(01).
           03  WRK-CHKKBN-G            PIC X(50).
           03  WRK-TNS-G.
               05  WRK-TNS-YUKOSTYMD   PIC X(08).
               05  WRK-TNS-SRYCD       PIC X(09).
               05  WRK-TNS-NAME        PIC X(100).
           03  WRK-BYOMEI              PIC X(100).
           03  WRK-ZZ9                 PIC ZZ9.
           03  WRK-CMB-CD                  PIC X(10).
           03  WRK-CMB-ITM-G.
               05  WRK-CMB-ITM             PIC X(200).
           03  WRK-X01-MAE-G.
               05  WRK-X01-MAE-CHKKBN      PIC X(01).
               05  WRK-X01-MAE-SRYCD       PIC X(09).
               05  WRK-X01-MAE-NAME        PIC X(100).
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
      *
       01  CONST-AREA.
           03  CONST-BYOCD-MAX             PIC 9(05)   VALUE   6.
           03  CONST-MKCD-MAX              PIC 9(05)   VALUE   21.
           03  CONST-CDLST-MAX             PIC 9(05)   VALUE   200.
           03  CONST-SETCD-MAX             PIC 9(05)   VALUE   100.
           03  CONST-SRY                   PIC 9(01)   VALUE   0.
           03  CONST-BYO                   PIC 9(01)   VALUE   1.
      *
      *    病名システム予約コードテーブル
           COPY    "CMBYOMEICD.INC".
      *
      *    他業務からの引継ぎ情報
           COPY    "CPSENDX01.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
       01  CHK-REC.
           COPY    "CPCHK.INC".
      *
       01  CHKSND-REC.
           COPY    "CPCHKSND.INC".
      *
       01  CHKTRD-REC.
           COPY    "CPCHKTRD.INC".
      *
       01  CHK005-REC.
           COPY    "CPCHK005.INC".
      *
       01  BYOMEI-REC.
           COPY    "CPBYOMEI.INC".
      *
       01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
      *
       01  INPUTSET-REC.
           COPY    "CPINPUTSET.INC".
      *
       01  SRYCDCHG-REC.
           COPY    "CPSRYCDCHG.INC".
      *
       01  USERBYOMEI-REC.
           COPY    "CPUSERBYOMEI.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *----(01.02.03) LINE ADD START ----------------------------------
      *   画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *----(01.02.03) LINE ADD END   ----------------------------------
      *
      *    一覧印刷変換領域
           COPY    "CPORCHXLST.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
           COPY    "CPORCSBYOMEI.INC".
      *
      *    チェックマスタ業務サブ１（病名欄編集）
           COPY    "CPXSUB01.INC".
      *
      *    ＤＢ検索
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "X01.INC".
           COPY    "X02.INC".
           COPY    "X03.INC".
           COPY    "X04.INC".
           COPY    "X05.INC".
           COPY    "X06.INC".
           COPY    "X07.INC".
           COPY    "X08.INC".
           COPY    "X91.INC".
           COPY    "X97.INC".
           COPY    "X98.INC".
           COPY    "X99.INC".
           COPY    "X100.INC".
           COPY    "XERR.INC".
           COPY    "XID1.INC".
           COPY    "XID2.INC".
      *
       PROCEDURE                                 DIVISION    USING
                                                 MCPAREA
                                                 SPAAREA
                                                 LINKAREA
                                                 SCRAREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-X01KYOUTU 
           MOVE    SPA-FREE        TO  SPA-X01FREE
           MOVE    SPA-X01SONOTA   TO  SPA-X01 
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS  ALSO    MCP-EVENT
           WHEN    "LINK"          ALSO    ANY
               PERFORM 100-INIT-SEC
      *    画面遷移
           WHEN     "PUTG"         ALSO   "CLICKED"
               PERFORM 200-GMNSENI
      *    入力
           WHEN      OTHER
               PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF    ( FLG-END         =   ZERO )
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-X01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-X01         TO  SPA-X01SONOTA
           MOVE    SPA-X01FREE     TO  SPA-FREE
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
           IF    ( SPA-MOTOPG          =   "XERR" )
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF    ( FLG-END         =   1 )
                   GO  TO  100-INIT-EXT
               END-IF
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
      ***  MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    SPACE                TO  MCP-PUTTYPE
           MOVE    "X01    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
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
           IF    ( LINKAREA        NOT =   SPACE )
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
               MOVE    SPA-WORK            TO  SENDX01-AREA
               MOVE    SPACE               TO  LINKAREA
               IF    ( SENDX01-MOTOPG      =   "D04" )
                   PERFORM 310-LNKX01-SEC
                   GO  TO  300-SCREEN-EXT
               END-IF
           END-IF
      *
           EVALUATE    SPA-MOTOPG
           WHEN    "X02"
           WHEN    "X04"
           WHEN    "X05"
      *!!
           WHEN    "X08"
               IF    ( SPA-X01-MOTOPG  NOT =   SPACE )
                   PERFORM 210-BACK
                   GO  TO      300-SCREEN-EXT
               END-IF
               MOVE    SPA-X01-CHKKBN  TO  WRK-CHKKBN
               MOVE    SPA-X01-MAE-G   TO  WRK-X01-MAE-G
               PERFORM 310-SPASET-SEC
               MOVE    WRK-CHKKBN      TO  SPA-GMN-CHKKBN
               MOVE    WRK-X01-MAE-G   TO  SPA-X01-MAE-G
               PERFORM 4105-CHKKBN-CHK-SEC
               MOVE    1           TO  SPA-GMN-PAGE
               MOVE    2           TO  SPA-GMN-CUR 
               GO      TO      300-SCREEN-EXT
           WHEN    "X91"
               PERFORM 493-CLEAR-SEC
               GO      TO      300-SCREEN-EXT
           WHEN    "X98"
      *    診療行為コード処理
               PERFORM 320-X98SPASET-SEC
               GO      TO      300-SCREEN-EXT
           WHEN    "X99"
      *    病名コード処理
               PERFORM 320-X99SPASET-SEC
               GO      TO      300-SCREEN-EXT
           WHEN    "X100"
               GO      TO      300-SCREEN-EXT
           END-EVALUATE
      *
           PERFORM 310-SPASET-SEC
      *
           MOVE    1               TO  SPA-GMN-PAGE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
      *
           INITIALIZE                  SPA-X01-AREA
                                       SPA-X01KYOUTU
      *
           MOVE    "1"                     TO  SPA-GMN-CHKKBNL    (1)
           MOVE    "薬剤と病名"            TO  SPA-GMN-CHKKBNMEIL (1)
           MOVE    "2"                     TO  SPA-GMN-CHKKBNL    (2)
           MOVE    "診療行為と病名"        TO  SPA-GMN-CHKKBNMEIL (2)
           MOVE    "4"                     TO  SPA-GMN-CHKKBNL    (3)
           MOVE    "薬剤と併用禁忌"        TO  SPA-GMN-CHKKBNMEIL (3)
           MOVE    "5"                     TO  SPA-GMN-CHKKBNL    (4)
           MOVE    "診療行為の併用算定（同月内）"
                                           TO  SPA-GMN-CHKKBNMEIL (4)
           MOVE    "6"                     TO  SPA-GMN-CHKKBNL    (5)
           MOVE    "投与禁忌薬剤と病名"    TO  SPA-GMN-CHKKBNMEIL (5)
           MOVE    "7"                     TO  SPA-GMN-CHKKBNL    (6)
           MOVE    "診療行為どうしの算定漏れ"
                                           TO  SPA-GMN-CHKKBNMEIL (6)
           MOVE    "8"                     TO  SPA-GMN-CHKKBNL    (7)
           MOVE    "病名と薬剤"            TO  SPA-GMN-CHKKBNMEIL (7)
           MOVE    "9"                     TO  SPA-GMN-CHKKBNL    (8)
           MOVE    "病名と診療行為"        TO  SPA-GMN-CHKKBNMEIL (8)
           MOVE    "A"                     TO  SPA-GMN-CHKKBNL    (9)
           MOVE    "診療行為の併用算定（同日内）"
                                           TO  SPA-GMN-CHKKBNMEIL (9)
           MOVE    "B"                     TO  SPA-GMN-CHKKBNL    (10)
           MOVE    "診療行為の併用算定（同会計内）"
                                           TO  SPA-GMN-CHKKBNMEIL (10)
      *
           MOVE    10                      TO  SPA-GMN-CHKKBN-MAX
      *
           MOVE    1                       TO  SPA-GMN-CUR
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    他業務より遷移時の処理
      *****************************************************************
       310-LNKX01-SEC           SECTION.
      *
           PERFORM 310-SPASET-SEC
      *
           MOVE    SENDX01-CHKKBN          TO  SPA-GMN-CHKKBN
           PERFORM 4105-CHKKBN-CHK-SEC
      *
           EVALUATE    SPA-GMN-CHKKBN
           WHEN    "8"
           WHEN    "9"
               MOVE    SENDX01-BYOMEI  TO  X01-NAME
               PERFORM 4100-BYOMEI-CHK-SEC
            WHEN   OTHER
               MOVE    SENDX01-SRYCD   TO  SPA-GMN-SRYCD
               PERFORM 4103-SRYCD-CHK-SEC
           END-EVALUATE
      *
           MOVE    "00000000"              TO  SPA-NAI-STYUKYMD
                                               SPA-GMN-STYUKYMD
           MOVE    "99999999"              TO  SPA-NAI-EDYUKYMD
                                               SPA-GMN-EDYUKYMD
      *
           MOVE    SPACE                   TO  SPA-GMN-SELNUM
           MOVE    ZERO                    TO  SPA-NAI-SELNUM
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL ( IDX         >   SPA-GMN-MAX )
                    OR   ( SPA-GMN-SELNUM  NOT =   SPACE )
      *
               IF    ( SENDX01-SRYYMD  >=  SPA-NAI-TSTYUKYMD (IDX))
                AND  ( SENDX01-SRYYMD  <=  SPA-NAI-TEDYUKYMD (IDX))
                   MOVE    IDX                     TO  SPA-NAI-SELNUM
                   MOVE    SPA-NAI-SELNUM          TO  SPA-GMN-SELNUM
                   MOVE    SPA-NAI-TSTYUKYMD (IDX) TO  SPA-NAI-STYUKYMD
                   MOVE    SPA-NAI-TEDYUKYMD (IDX) TO  SPA-NAI-EDYUKYMD
                   MOVE    SPA-GMN-TSTYUKYMD (IDX) TO  SPA-GMN-STYUKYMD
                   MOVE    SPA-GMN-TEDYUKYMD (IDX) TO  SPA-GMN-EDYUKYMD
                   MOVE    SPA-NAI-TID (IDX)       TO  SPA-NAI-ID
               ELSE
                   IF    ( SENDX01-SRYYMD  >   SPA-NAI-TEDYUKYMD (IDX))
                       MOVE    SPA-NAI-TEDYUKYMD (IDX)
                                                   TO  WRK-SYMD
                       MOVE    "1"                 TO  WRK-ZOGENPTN
                       MOVE    1                   TO  WRK-ZOGEN
                       PERFORM 800-CALENDAR-SEC
                       MOVE    WRK-SYMD            TO  WRK-YMD
                       PERFORM 5002-HIZUKE-CHK-SEC
                       MOVE    WRK-SYMD            TO  SPA-NAI-STYUKYMD
                       MOVE    WRK-HENYMDG         TO  SPA-GMN-STYUKYMD
                   END-IF
      *
                   IF    ( SENDX01-SRYYMD  <   SPA-NAI-TEDYUKYMD (IDX))
                       MOVE    SPA-NAI-TEDYUKYMD (IDX)
                                                   TO  WRK-SYMD
                       MOVE    "1"                 TO  WRK-ZOGENPTN
                       MOVE    -1                  TO  WRK-ZOGEN
                       PERFORM 800-CALENDAR-SEC
                       MOVE    WRK-SYMD            TO  WRK-YMD
                       PERFORM 5002-HIZUKE-CHK-SEC
                       MOVE    WRK-SYMD            TO  SPA-NAI-EDYUKYMD
                       MOVE    WRK-HENYMDG         TO  SPA-GMN-EDYUKYMD
                   END-IF
      *
               END-IF
      *
           END-PERFORM
      *
           MOVE    SENDX01-MOTOPG      TO  SPA-NAI-MOTOPG
           MOVE    SENDX01-BYOMEI      TO  SPA-NAI-BYOMEI
           MOVE    SENDX01-HOSOKU      TO  SPA-NAI-HOSOKU
      *
           MOVE    1                   TO  FLG-TOUROKU
           PERFORM 4102-KIHON-CHK-SEC
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-KAKUTEI-SEC
           END-IF
      *
           .
       310-LNKX01-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為コードセット処理
      *****************************************************************
       320-X98SPASET-SEC          SECTION.
      *
           MOVE    SPACE               TO  SPA-MAE-SRYCD
      *
           IF    ( SPA-X98-SRYCD (1)   NOT =   SPACE  )
      ******** MOVE    SPA-X98-SRYCD       TO  X01-SRYCD
               MOVE    SPA-X98-SRYCD (1)   TO  SPA-GMN-SRYCD
               PERFORM 4103-SRYCD-CHK-SEC
           ELSE
               MOVE    2                   TO  SPA-GMN-CUR
           END-IF  
           .
       320-X98SPASET-EXT.
           EXIT.
      *****************************************************************
      *    病名セット処理
      *****************************************************************
       320-X99SPASET-SEC          SECTION.
      *
           IF    ( SPA-X99-BYOCD (1)       =   SPACE  )
               MOVE    2           TO  SPA-GMN-CUR
           ELSE
      *
           INITIALIZE                  SPA-GMN-BYOSTATUS
                                       SPA-GMN-SRYCD
                                       SPA-NAI-BYOCD-G
                                       SPA-GMN-NAME
                                       SPA-GMN-KOUMOKU1
                                       SPA-NAI-KOUMOKU1
                                       SPA-GMN-MAX
      *
               MOVE    SPA-X99-BYOSTATUS   TO  SPA-GMN-BYOSTATUS
               MOVE    SPA-X99-BYOMEI      TO  SPA-GMN-NAME
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   CONST-MKCD-MAX )
      *
                   MOVE    SPA-X99-BYOCD (IDX0)
                                   TO  SPA-NAI-BYOCD (IDX0)
      *
               END-PERFORM
      *
               MOVE    ZERO        TO  SPA-GMN-BYOMEICD-CHK-FLG
               MOVE    ZERO        TO  SPA-GMN-BYOMEI-CHK-FLG
               PERFORM 41011-BYOMEI-CHK005-SEC
               IF    ( SPA-NAI-SELNO   =   ZERO )
                   PERFORM 41011-BYOMEICD-SEC
               END-IF
           END-IF
      *
           .
       320-X99SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                           SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT       ALSO      MCP-WIDGET
      *    戻る
           WHEN    "CLICKED"       ALSO    "B01"
               PERFORM  210-BACK
      *    クリア
           WHEN    "CLICKED"       ALSO    "B02"
               PERFORM 493-CLEAR-SEC
      *    前回ＣＤ
           WHEN    "CLICKED"       ALSO    "B03"
               PERFORM 430-ZENKAICD-SEC
      *    リスト
           WHEN    "CLICKED"       ALSO    "B04"
               PERFORM  411-PRINT-SEC
      *    検索
           WHEN    "CLICKED"       ALSO    "B09"
               IF    ( SPA-GMN-CHKKBN      =   "8" OR "9" )
                   INITIALIZE              SPA-X01-USERBYOMEI-G
                   MOVE    SPACE               TO  WRK-BYOMEI
                   PERFORM  410-KENSAKU-SEC
               ELSE
                   MOVE    SPACE               TO  WRK-INPUTCD
                   PERFORM  409-KENSAKU-SEC
               END-IF
      *    ＣＤ表示
           WHEN    "CLICKED"       ALSO    "B10"
               PERFORM  410-X100-SENI-SEC
      *    確定
           WHEN    "CLICKED"       ALSO    "B12"
               PERFORM  410-KAKUTEI-SEC
           END-EVALUATE
           .
      *
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                             SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    選択番号
           WHEN    "ACTIVATE"       ALSO    "SELNUM"
               PERFORM  4101-SELNUM-CHK-SEC
      *    一括入力分
           WHEN    "ACTIVATE"       ALSO    ANY
               PERFORM 400-ENTRY-SYORI-SEC
      *    行選択
           WHEN    ANY             ALSO    "TENSU_LIST"
               PERFORM 41015-LIST1SEL-SEC
           END-EVALUATE
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           IF    ( SPA-X01-MOTOPG      =   SPACE )
               MOVE    "M02"           TO  SPA-SAKIPG
           ELSE
               MOVE    SPA-X01-MOTOPG  TO  SPA-SAKIPG
           END-IF
           MOVE    "X01"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    番号選択　入力処理
      *****************************************************************
       4101-SELNUM-CHK-SEC       SECTION.
      *
           INITIALIZE                  SPA-GMN-KOUMOKU1
                                       SPA-NAI-KOUMOKU1
           MOVE    ZERO                TO  SPA-NAI-SELNO
      *
           MOVE    X01-SELNUM          TO  SPA-GMN-SELNUM
      *H29.9
      *    診療行為の併用算定対応
           IF    (  SPA-GMN-CHKKBN     =   "5" OR  "A"  OR  "B" )
               MOVE    ZERO            TO  SPA-NAI-SELNUM
               MOVE    SPACE           TO  SPA-GMN-SELNUM
               MOVE    12              TO  SPA-GMN-CUR
               GO  TO  4101-SELNUM-CHK-EXT
           END-IF
      *
           PERFORM 41012-SELNUM-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4101-SELNUM-CHK-EXT
           END-IF
      *
           IF      SPA-NAI-SELNUM      =   ZERO
               MOVE    ZERO            TO  SPA-NAI-SELNUM
               MOVE    SPACE           TO  SPA-GMN-SELNUM
               MOVE    4               TO  SPA-GMN-CUR
               IF    ( SPA-GMN-CHKKBN  =   "8" OR "9" )
                   PERFORM 4101-BYOMEI-CHK-SEC
                   IF    ( SPA-ERRCD       =   SPACE )
                    OR   ( SPA-ERRCD       =   "0016" )
                       MOVE    ZERO    TO  SPA-GMN-BYOMEI-CHK-FLG
                   END-IF
               END-IF
               GO  TO  4101-SELNUM-CHK-EXT
           END-IF
      *
           MOVE    SPA-NAI-SELNUM      TO  SPA-NAI-SELNO
      *
           PERFORM 41011-SELNUM-HENSYU-SEC
           .
       4101-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号編集処理
      *****************************************************************
       41011-SELNUM-HENSYU-SEC           SECTION.
      *
           MOVE    SPA-GMN-TSTYUKYMD(SPA-NAI-SELNO)
                                       TO  SPA-GMN-STYUKYMD
           MOVE    SPA-NAI-TSTYUKYMD(SPA-NAI-SELNO)
                                       TO  SPA-NAI-STYUKYMD
           MOVE    SPA-GMN-TEDYUKYMD(SPA-NAI-SELNO)
                                       TO  SPA-GMN-EDYUKYMD
           MOVE    SPA-NAI-TEDYUKYMD(SPA-NAI-SELNO)
                                       TO  SPA-NAI-EDYUKYMD
           MOVE    SPA-NAI-TID (SPA-NAI-SELNO)
                                       TO  SPA-NAI-ID
      *
           IF    ( SPA-GMN-CHKKBN      =   "8" OR  "9" )
               PERFORM 900-CHK005-KEY-SEL-SEC
               MOVE    CHK005-BYOMEI   TO  SPA-GMN-NAME
               MOVE    CHK005-BYOMEIHENFLG
                                       TO  SPA-GMN-BYOSTATUS
               MOVE    CHK005-CD       TO  SPA-NAI-BYOCD-G
               MOVE    CHK005-COMPKBN  TO  SPA-NAI-JYKNKBN
               PERFORM 41011-BYOMEICD-SEC
           ELSE
               MOVE    SPA-NAI-NAME   (SPA-NAI-SELNO)
                                       TO  SPA-GMN-NAME
           END-IF
      *
           MOVE    4                   TO  SPA-GMN-CUR
           .
       41011-SELNUM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    番号選択　入力処理
      *****************************************************************
       41012-SELNUM-CHK-SEC       SECTION.
      *
           INITIALIZE                     ORCSNUMAREA
           MOVE    SPA-GMN-SELNUM      TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN     NOT =   SPACE )   OR
                  (SNUM-SYOKBN     NOT =   SPACE )
               MOVE    3                   TO  SPA-GMN-CUR
               MOVE    "0001"              TO  SPA-ERRCD
               GO      TO      41012-SELNUM-CHK-EXT
           ELSE
               MOVE    SNUM-OUTNUM     TO  SPA-NAI-SELNUM
           END-IF
      *
           IF      SPA-NAI-SELNUM      =   ZERO
               GO  TO  41012-SELNUM-CHK-EXT
           ELSE
      *        選択番号チェック
               IF      SPA-NAI-SELNUM      >=  1
               AND     SPA-NAI-SELNUM      <=  SPA-GMN-MAX   
                   CONTINUE
               ELSE
                   MOVE    3                   TO  SPA-GMN-CUR
                   MOVE    "0001"              TO  SPA-ERRCD
                   GO  TO  41012-SELNUM-CHK-EXT
               END-IF
           END-IF
      *
           .
       41012-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一括分　入力処理
      *****************************************************************
       400-ENTRY-SYORI-SEC       SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           MOVE    ZERO                TO  FLG-TOUROKU
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           .
       400-ENTRY-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC       SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      410-INPUT-CHK-EXT
           END-IF
      *
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面をＳＰＡセット処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC       SECTION.
      *
           MOVE    X01-CHKKBN          TO  SPA-GMN-CHKKBN-G
      *
           IF    ( SPA-GMN-CHKKBN      =   "8" OR "9" )
               CONTINUE
           ELSE
      *
               MOVE    X01-SRYCD       TO  SPA-GMN-SRYCD
      *
      *        全角
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"             TO  KANACHK-SYORI
               MOVE    SPA-GMN-SRYCD   TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               IF     (KANACHK-RC          =   ZERO ) AND
                      (KANACHK-SYUBETU     =   2    )
                   MOVE    SPACE               TO  SPA-GMN-SRYCD
                   MOVE    X01-SRYCD(1:8)      TO  SPA-GMN-SRYCD
               ELSE
                   MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-SRYCD
               END-IF
           END-IF
      *
           MOVE    X01-STYUKYMD        TO  SPA-GMN-STYUKYMD
           MOVE    X01-EDYUKYMD        TO  SPA-GMN-EDYUKYMD
      *
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC       SECTION.
      *
      *    チェック区分
           PERFORM  4105-CHKKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           IF    (  SPA-GMN-CHKKBN     =   "8" OR  "9" )
      *        病名コード
               IF    (  WRK-MCP-WIDGET =   "SRYCD" OR  "NAME" OR "B03" )
                   IF    (  WRK-MCP-WIDGET =   "SRYCD" )
                       PERFORM  4100-BYOMEICD-CHK-SEC
                       IF     ( SPA-ERRCD      NOT =   SPACE )
                           GO  TO      4102-KIHON-CHK-EXT
                       END-IF
                   END-IF
                   IF    (  WRK-MCP-WIDGET =   "NAME" )
                    OR   (  WRK-MCP-WIDGET =   "B03" )
                       PERFORM  4100-BYOMEI-CHK-SEC
                       IF     ( SPA-ERRCD      NOT =   SPACE )
                           GO  TO      4102-KIHON-CHK-EXT
                       END-IF
                   END-IF
               ELSE
                   IF    ( SPA-GMN-BYOMEICD-CHK-FLG    =   1 )
                    AND  ( SPA-NAI-SELNUM              =   ZERO )
                       PERFORM  4101-BYOMEICD-CHK-SEC
                       IF     ( SPA-ERRCD      NOT =   SPACE )
                           GO  TO      4102-KIHON-CHK-EXT
                       END-IF
                   END-IF
                   IF    ( SPA-GMN-BYOMEI-CHK-FLG    =   1 )
                    AND  ( SPA-NAI-SELNUM            =   ZERO )
                       PERFORM  4101-BYOMEI-CHK-SEC
                       IF     ( SPA-ERRCD      NOT =   SPACE )
                           GO  TO      4102-KIHON-CHK-EXT
                       END-IF
                   END-IF
               END-IF
           ELSE
      *        診療行為コード
               PERFORM  4103-SRYCD-CHK-SEC
               IF     ( SPA-ERRCD      NOT =   SPACE )
                   GO  TO      4102-KIHON-CHK-EXT
               END-IF
           END-IF
      *
      *H29.9
      *    診療行為の併用算定対応
           IF    (  SPA-GMN-CHKKBN     =   "5" OR  "A"  OR  "B" )
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
      *    選択番号
           PERFORM  41012-SELNUM-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
      *    有効開始年月日
           PERFORM  4102-STYUKYMD-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    有効終了年月日
           PERFORM 4103-EDYUKYMD-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *  　チェック区分チェック
      *****************************************************************
       4105-CHKKBN-CHK-SEC             SECTION.
      *
           IF      SPA-GMN-CHKKBN      =   SPA-NAI-CHKKBN
               GO      TO      4105-CHKKBN-CHK-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-CHKKBN-MAX
               IF      SPA-GMN-CHKKBN      =   SPA-GMN-CHKKBNLIST(IDX)
                                                                (1:1)
                   MOVE    SPA-GMN-CHKKBNLIST(IDX)
                                           TO  SPA-GMN-CHKKBN-G
                   MOVE    SPA-GMN-CHKKBN-MAX
                                           TO  IDX
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0006"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-GMN-CUR
               GO      TO      4105-CHKKBN-CHK-EXT
           END-IF
           IF      SPA-GMN-CHKKBN      =   "3"
               MOVE    "0010"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-GMN-CUR
               GO      TO      4105-CHKKBN-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-CHKKBN-G    TO  WRK-CHKKBN-G
      *
           IF    ( SPA-NAI-CHKKBN      =   "1" OR "2" OR "3" OR "4"
                                        OR "5" OR "6" OR "7" OR "A"
                                        OR "B" )
            AND  ( SPA-GMN-CHKKBN      =   "8" OR "9" )
      *
               PERFORM 493-CLEAR-SEC
               MOVE    ZERO            TO   SPA-GMN-CUR
           END-IF
      *
           IF    ( SPA-NAI-CHKKBN      =   "8" OR "9" )
            AND  ( SPA-GMN-CHKKBN      =   "1" OR "2" OR "3" OR "4"
                                        OR "5" OR "6" OR "7" OR "A"
                                        OR "B" )
      *
               PERFORM 493-CLEAR-SEC
               MOVE    ZERO          TO   SPA-GMN-CUR
           END-IF
      *
           MOVE    WRK-CHKKBN-G      TO   SPA-GMN-CHKKBN-G
      *
           INITIALIZE                  SPA-GMN-TBL
           INITIALIZE                  SPA-NAIB-AREA
           MOVE    ZERO                TO  SPA-GMN-MAX
      *
           MOVE    SPA-GMN-CHKKBN    TO   SPA-NAI-CHKKBN
      *
           .
       4105-CHKKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    有効開始年月日チェック処理
      *****************************************************************
       4102-STYUKYMD-CHK-SEC          SECTION.
      *
      *
           IF      SPA-GMN-STYUKYMD    =   SPACE
               MOVE    ZERO                TO  SPA-NAI-EDYUKYMD
               IF     (SPA-GMN-EDYUKYMD    NOT =   SPACE      ) OR
                      (WRK-MCP-WIDGET      =   "STYUKYMD" ) OR
                      (WRK-MCP-WIDGET      =   "EDYUKYMD" ) OR
                      (FLG-TOUROKU         =   1          )
                   MOVE    "00000000"         TO  SPA-GMN-STYUKYMD
               END-IF
               GO      TO      4102-STYUKYMD-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-STYUKYMD   TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           IF      SPA-ERRCD            =   SPACE
               MOVE    WRK-SYMD             TO  SPA-NAI-STYUKYMD 
               MOVE    WRK-HENYMDG          TO  SPA-GMN-STYUKYMD
           ELSE
               MOVE    4                  TO  SPA-GMN-CUR
               MOVE    ZERO               TO  SPA-NAI-STYUKYMD
               GO  TO  4102-STYUKYMD-CHK-EXT
           END-IF
      *
           .
       4102-STYUKYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    有効終了年月日チェック処理
      *****************************************************************
       4103-EDYUKYMD-CHK-SEC          SECTION.
      *
      *
           IF      SPA-GMN-EDYUKYMD    =   SPACE
               MOVE    ZERO                TO  SPA-NAI-EDYUKYMD
               IF     (WRK-MCP-WIDGET      =   "EDYUKYMD" ) OR
                      (FLG-TOUROKU         =   1          )
                   MOVE    "99999999"         TO  SPA-GMN-EDYUKYMD
                   MOVE    ALL "9"            TO  SPA-NAI-EDYUKYMD
               END-IF
               GO      TO      4103-EDYUKYMD-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-EDYUKYMD    TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           IF      SPA-ERRCD           =   SPACE
               MOVE    WRK-SYMD            TO  SPA-NAI-EDYUKYMD 
               MOVE    WRK-HENYMDG         TO  SPA-GMN-EDYUKYMD
           ELSE
               MOVE    5                  TO  SPA-GMN-CUR
               GO  TO  4103-EDYUKYMD-CHK-EXT
           END-IF
      *
           .
       4103-EDYUKYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *  　診療行為コードチェック
      *****************************************************************
       4103-SRYCD-CHK-SEC                         SECTION.
      *
           PERFORM 4104-SRYCD-CHK-SEC
      *
           .
       4103-SRYCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *  　診療行為コードチェック
      *****************************************************************
       4104-SRYCD-CHK-SEC                         SECTION.
      *
      *    検索へ
           IF      SPA-GMN-SRYCD(1:1)  =   "/"
               MOVE    SPACE               TO  WRK-INPUTCD
               PERFORM 409-KENSAKU-SEC
               GO                TO  4104-SRYCD-CHK-EXT
           END-IF
      *
           IF     (SPA-GMN-SRYCD       =   SPACE   )  AND
                  (WRK-MCP-WIDGET      =   "SRYCD" )
               MOVE    "0005"          TO  SPA-ERRCD
               MOVE    2               TO  SPA-GMN-CUR
               GO      TO      4104-SRYCD-CHK-EXT
           END-IF
      *
           IF      SPA-GMN-SRYCD       =   SPA-MAE-SRYCD
               GO      TO      4104-SRYCD-CHK-EXT
           END-IF
      *
           INITIALIZE                  SPA-GMN-KOUMOKU1
                                       SPA-GMN-TBL
                                       SPA-NAI-KOUMOKU1
                                       SPA-NAI-SRYCD
                                       SPA-GMN-NAME
      *
           MOVE    ZERO            TO  SPA-NAI-SELNO
                                       SPA-GMN-MAX
      *
           IF      SPA-GMN-SRYCD       =   SPACE
               GO      TO      4104-SRYCD-CHK-EXT
           END-IF
      *
           IF    ( SPA-GMN-SRYCD(1:1)  =   "P" OR "S" )
               PERFORM 41011-SETCODE-CHK-SEC
               IF    ( FLG-KENSAKU         =   1 )
                OR   ( SPA-ERRCD       NOT =   SPACE )
                   GO      TO      4104-SRYCD-CHK-EXT
               END-IF
           ELSE
      *
      *        入力コードチェック
               PERFORM 4010-SRYCD-CHK-SEC
               IF    ( FLG-KENSAKU         =   1 )
                   GO      TO      4104-SRYCD-CHK-EXT
               END-IF
      *
      *        点数マスタ読み込み
               PERFORM 900-TENSU-KEY4-SEL-SEC
               IF    ( FLG-TENSU           =   1 )
                   MOVE    "0005"          TO  SPA-ERRCD
                   MOVE    2               TO  SPA-GMN-CUR
                   GO  TO  4104-SRYCD-CHK-EXT
               END-IF
      *
      *        最終履歴の名称をセットする
               PERFORM UNTIL ( FLG-TENSU   =   1 )
      *
                   IF     (TNS-SRYCD(1:1)      =   "1") AND
                          (TNS-ROUTEKKBN       =   2  )
                       MOVE    SPACE       TO  SPA-GMN-NAME
                       STRING  "［老］"        DELIMITED   BY  SIZE
                               TNS-NAME        DELIMITED   BY  SIZE
                       INTO    SPA-GMN-NAME
                       END-STRING
                   ELSE
                       MOVE    TNS-NAME    TO  SPA-GMN-NAME
                   END-IF
      *
                   PERFORM 900-TENSU-KEY4-FET-SEC
      *
               END-PERFORM
      *
      *
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *
               EVALUATE    SPA-GMN-CHKKBN
               WHEN    "1"
               WHEN    "3"
               WHEN    "4"
               WHEN    "6"
                   IF      SPA-GMN-SRYCD (1:1) NOT =   "6"
                       MOVE    "0008"      TO  SPA-ERRCD
                       MOVE    2           TO  SPA-GMN-CUR
                       GO  TO  4104-SRYCD-CHK-EXT
                   END-IF
               WHEN    "2"
               WHEN    "5"
               WHEN    "A"
               WHEN    "B"
                   IF      SPA-GMN-SRYCD (1:1)     =   "6"
                       MOVE    "0008"      TO  SPA-ERRCD
                       MOVE    2           TO  SPA-GMN-CUR
                       GO  TO  4104-SRYCD-CHK-EXT
                   END-IF
               END-EVALUATE
           END-IF
      *
           MOVE    SPA-GMN-SRYCD       TO  SPA-NAI-SRYCD
           MOVE    SPA-GMN-SRYCD       TO  SPA-MAE-SRYCD
      *
           EVALUATE    SPA-GMN-CHKKBN
           WHEN    "1"
           WHEN    "2"
           WHEN    "6"
               PERFORM 41041-SRYCD-CHKSND-SEC
           WHEN    "3"
           WHEN    "4"
               PERFORM 41041-SRYCD-CHKMST-SEC
      *
           WHEN    "5"
           WHEN    "A"
           WHEN    "B"
               PERFORM 41041-SRYCD-CHKLAST-SEC
               GO  TO  4104-SRYCD-CHK-EXT
           WHEN    "7"
               PERFORM 41041-SRYCD-CHKTRD-SEC
           END-EVALUATE
      *
          PERFORM 41041-CHKKBNLIST-INIT-SEC
      *
           .
       4104-SRYCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為コード編集処理（チェックマスタより）
      *****************************************************************
       41041-SRYCD-CHKMST-SEC              SECTION.
      *
           PERFORM 900-CHK-KEY12-SEL-SEC
      *
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL      (IDX     =  100  )  OR
                              (FLG-CHK =   1   )
               ADD     1                   TO  IDX 
               MOVE    IDX                 TO  SPA-GMN-TBANGO
                                                          (IDX)
               MOVE    CHK-YUKOSTYMD       TO  SPA-NAI-TSTYUKYMD
                                                          (IDX)
               MOVE    CHK-YUKOSTYMD       TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC 
               MOVE    WRK-HENYMDG         TO  SPA-GMN-TSTYUKYMD
                                                              (IDX)
               MOVE    CHK-YUKOEDYMD       TO  SPA-NAI-TEDYUKYMD
                                                             (IDX)
               MOVE    CHK-YUKOEDYMD       TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG         TO  SPA-GMN-TEDYUKYMD
                                                              (IDX)
      *
               MOVE    CHK-SRYCD           TO  WRK-TNS-SRYCD
               MOVE    CHK-YUKOSTYMD       TO  WRK-TNS-YUKOSTYMD
               PERFORM 41041-SRYNAME-HEN-SEC
               MOVE    WRK-TNS-NAME        TO  SPA-NAI-NAME (IDX)
      *
               MOVE    IDX                 TO  SPA-GMN-MAX
      *
               PERFORM 900-CHK-KEY12-FET-SEC
      *
           END-PERFORM
      *
      *    カーソルクロース
           MOVE    "tbl_chk"           TO  MCP-TABLE
           MOVE    "key12"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       41041-SRYCD-CHKMST-EXT.
           EXIT.
      *****************************************************************
      *    診療行為コード編集処理（５、Ａ、Ｂ）
      *****************************************************************
       41041-SRYCD-CHKLAST-SEC              SECTION.
      *
      *
           PERFORM 900-CHK-KEY23-SEL-SEC
      *
           MOVE    ZERO                TO  IDX
           IF      FLG-CHK             =   ZERO
               ADD     1                   TO  IDX 
               MOVE    IDX                 TO  SPA-GMN-TBANGO
                                                          (IDX)
               MOVE    CHK-YUKOSTYMD       TO  SPA-NAI-TSTYUKYMD
                                                          (IDX)
               MOVE    CHK-YUKOSTYMD       TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC 
               MOVE    WRK-HENYMDG         TO  SPA-GMN-TSTYUKYMD
                                                              (IDX)
               MOVE    CHK-YUKOEDYMD       TO  SPA-NAI-TEDYUKYMD
                                                             (IDX)
               MOVE    CHK-YUKOEDYMD       TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG         TO  SPA-GMN-TEDYUKYMD
                                                              (IDX)
      *
               MOVE    CHK-SRYCD           TO  WRK-TNS-SRYCD
               MOVE    CHK-YUKOSTYMD       TO  WRK-TNS-YUKOSTYMD
               PERFORM 41041-SRYNAME-HEN-SEC
               MOVE    WRK-TNS-NAME        TO  SPA-NAI-NAME (IDX)
      *
               MOVE    IDX                 TO  SPA-GMN-MAX
           END-IF
      *    カーソルクロース
           MOVE    "tbl_chk"           TO  MCP-TABLE
           MOVE    "key23"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           MOVE    SPACE                   TO  SPA-GMN-SELNUM
           MOVE    ZERO                    TO  SPA-NAI-SELNUM
           MOVE    12                      TO  SPA-GMN-CUR
           .
       41041-SRYCD-CHKLAST-EXT.
           EXIT.
      *****************************************************************
      *    診療行為コード編集処理（チェックマスタ２より）
      *****************************************************************
       41041-SRYCD-CHKSND-SEC              SECTION.
      *
           PERFORM 900-CHKSND-KEY3-SEL-SEC
      *
           MOVE    ZERO            TO  IDX
           PERFORM UNTIL      (IDX     =  100  )  OR
                              (FLG-CHKSND =   1   )
               ADD     1                   TO  IDX 
               MOVE    IDX                 TO  SPA-GMN-TBANGO
                                                          (IDX)
               MOVE    CHKSND-YUKOSTYMD    TO  SPA-NAI-TSTYUKYMD
                                                          (IDX)
               MOVE    CHKSND-YUKOSTYMD    TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG         TO  SPA-GMN-TSTYUKYMD
                                                          (IDX)
               MOVE    CHKSND-YUKOEDYMD    TO  SPA-NAI-TEDYUKYMD
                                                          (IDX)
               MOVE    CHKSND-YUKOEDYMD    TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG         TO  SPA-GMN-TEDYUKYMD
                                                          (IDX)
      *
               IF    ( CHKSND-SRYCD (1:1)  =   "P" OR "S" )
                   MOVE    SPA-GMN-NAME    TO  SPA-NAI-NAME (IDX)
               ELSE
                   MOVE    CHKSND-SRYCD    TO  WRK-TNS-SRYCD
                   MOVE    CHKSND-YUKOSTYMD TO WRK-TNS-YUKOSTYMD
                   PERFORM 41041-SRYNAME-HEN-SEC
                   MOVE    WRK-TNS-NAME    TO  SPA-NAI-NAME (IDX)
               END-IF
      *
               MOVE    IDX                 TO  SPA-GMN-MAX
      *
               PERFORM 900-CHKSND-KEY3-FET-SEC
      *
           END-PERFORM
      *
      *    カーソルクロース
           MOVE    "tbl_chksnd"         TO  MCP-TABLE
           MOVE    "key3"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       41041-SRYCD-CHKSND-EXT.
           EXIT.
      *****************************************************************
      *    診療行為コード編集処理（チェックマスタ３より）
      *****************************************************************
       41041-SRYCD-CHKTRD-SEC              SECTION.
      *
           PERFORM 900-CHKTRD-KEY12-SEL-SEC
      *
           MOVE    ZERO            TO  IDX
           PERFORM UNTIL      (IDX     =  100  )  OR
                              (FLG-CHKTRD =   1   )
               ADD     1                   TO  IDX 
               MOVE    IDX                 TO  SPA-GMN-TBANGO
                                                          (IDX)
               MOVE    CHKTRD-YUKOSTYMD    TO  SPA-NAI-TSTYUKYMD
                                                          (IDX)
               MOVE    CHKTRD-YUKOSTYMD    TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG         TO  SPA-GMN-TSTYUKYMD
                                                          (IDX)
               MOVE    CHKTRD-YUKOEDYMD    TO  SPA-NAI-TEDYUKYMD
                                                          (IDX)
               MOVE    CHKTRD-YUKOEDYMD    TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG         TO  SPA-GMN-TEDYUKYMD
                                                          (IDX)
      *
               MOVE    CHKTRD-SRYCD        TO  WRK-TNS-SRYCD
               MOVE    CHKTRD-YUKOSTYMD    TO  WRK-TNS-YUKOSTYMD
               PERFORM 41041-SRYNAME-HEN-SEC
               MOVE    WRK-TNS-NAME        TO  SPA-NAI-NAME (IDX)
      *
               MOVE    IDX                 TO  SPA-GMN-MAX
      *
               PERFORM 900-CHKTRD-KEY12-FET-SEC
      *
           END-PERFORM
      *
      *    カーソルクロース
           MOVE    "tbl_chktrd"         TO  MCP-TABLE
           MOVE    "key12"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       41041-SRYCD-CHKTRD-EXT.
           EXIT.
      *****************************************************************
      *    チェック区分リスト初期処理
      *****************************************************************
       41041-CHKKBNLIST-INIT-SEC             SECTION.
      *
           PERFORM 410411-CHOFUKU-CHK-SEC
      *
           MOVE    SPACE                       TO  SPA-GMN-SELNUM
           MOVE    ZERO                        TO  SPA-NAI-SELNUM
      *
           IF    ( SPA-GMN-MAX     =   ZERO )
               MOVE    4                       TO  SPA-GMN-CUR
           ELSE
               MOVE    SPA-GMN-MAX             TO  SPA-NAI-SELNUM
                                                   SPA-GMN-SELNUM
                                                   SPA-NAI-SELNO
                                                   IDX
               MOVE    SPA-NAI-TSTYUKYMD (IDX) TO  SPA-NAI-STYUKYMD
               MOVE    SPA-NAI-TEDYUKYMD (IDX) TO  SPA-NAI-EDYUKYMD
               MOVE    SPA-GMN-TSTYUKYMD (IDX) TO  SPA-GMN-STYUKYMD
               MOVE    SPA-GMN-TEDYUKYMD (IDX) TO  SPA-GMN-EDYUKYMD
               MOVE    SPA-NAI-TID (IDX)       TO  SPA-NAI-ID
      *
               MOVE    12              TO  SPA-GMN-CUR
           END-IF
      *
           .
       41041-CHKKBNLIST-INIT-EXT.
           EXIT.
      *****************************************************************
      *    期間重複チェック処理
      *****************************************************************
       410411-CHOFUKU-CHK-SEC              SECTION.
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL ( IDX >   SPA-GMN-MAX  )
      *
               MOVE    0   TO  SPA-NAI-TCHOFUKU (IDX)
      *
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL ( IDY >   SPA-GMN-MAX )
      *
                   IF    ( IDX     =   IDY )
                       CONTINUE
                   ELSE
                       IF    ( SPA-NAI-TEDYUKYMD (IDY)
                                   <  SPA-NAI-TSTYUKYMD (IDX))
                       OR    ( SPA-NAI-TSTYUKYMD (IDY)
                                   >  SPA-NAI-TEDYUKYMD (IDX))
                           CONTINUE
                       ELSE
                           MOVE    1   TO  SPA-NAI-TCHOFUKU (IDX)
                       END-IF
                   END-IF
      *
               END-PERFORM
      *
           END-PERFORM
      *
           .
       410411-CHOFUKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療行為名称編集処理
      *****************************************************************
       41041-SRYNAME-HEN-SEC               SECTION.
      *
           MOVE    SPACE               TO  WRK-TNS-NAME
      *
           PERFORM 900-TENSU-KEY-SEL-SEC
           IF    ( FLG-TENSU           =   ZERO )
               IF     (TNS-SRYCD(1:1)      =   "1") AND
                      (TNS-ROUTEKKBN       =   2  )
                   STRING  "［老］"    DELIMITED   BY  SIZE
                           TNS-NAME    DELIMITED   BY  SIZE
                   INTO    WRK-TNS-NAME
                   END-STRING
               ELSE
                   MOVE    TNS-NAME  TO  WRK-TNS-NAME
               END-IF
           END-IF
      *
           .
       41041-SRYNAME-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入力コードチェック処理
      *****************************************************************
       4010-SRYCD-CHK-SEC                  SECTION.
      *
           MOVE    ZERO               TO  FLG-KENSAKU
      *
           MOVE    ZERO               TO  WRK-CD-MCNT
           MOVE    ZERO               TO  FLG-KANJI
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-GMN-SRYCD       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-SRYCD
           IF      KANACHK-RC      NOT =   ZERO
      *        全角半角混在エラー
               MOVE    "0007"              TO  SPA-ERRCD
               GO      TO    4010-SRYCD-CHK-EXT
           ELSE
               MOVE    KANACHK-MAX         TO  WRK-CD-MCNT
      *        全角
               IF      KANACHK-SYUBETU     =   2
                   MOVE    1               TO  FLG-KANJI
               ELSE
                   MOVE    ZERO            TO  FLG-KANJI
               END-IF
           END-IF
      *
      *    半角で９文字の時、点数マスタチェックへ
           IF     (WRK-CD-MCNT         =   9  )  AND
                  (FLG-KANJI           =   ZERO )  AND
                  (SPA-GMN-SRYCD (1:9) IS  NUMERIC  )
               GO      TO    4010-SRYCD-CHK-EXT
           END-IF
      *
           MOVE    1                   TO  FLG-KENSAKU
           MOVE    SPA-GMN-SRYCD       TO  WRK-INPUTCD
           PERFORM 409-KENSAKU-SEC
      *
           .
       4010-SRYCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為コードチェック
      *****************************************************************
       41011-SETCODE-CHK-SEC               SECTION.
      *
           IF    ( SPA-GMN-SRYCD(1:1)  =   "S" )
               IF    ( SPA-GMN-CHKKBN  =   "1" )
                   CONTINUE
               ELSE
                   MOVE    "0021"      TO  SPA-ERRCD
                   MOVE    2           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-GMN-SRYCD(1:1)  =   "P" )
                   IF    ( SPA-GMN-CHKKBN  =   "2" )
                       CONTINUE
                   ELSE
                       MOVE    "0022"      TO  SPA-ERRCD
                       MOVE    2           TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               MOVE    SPA-GMN-SRYCD   TO   WRK-TNS-SRYCD
               PERFORM 900-INPUTCD-KEY2-SEL-SEC
               IF    ( FLG-INPUTCD     =   ZERO )
                   MOVE    ICD-DSPNAME     TO  SPA-GMN-NAME
               ELSE
                   MOVE    SPA-GMN-SRYCD   TO  WRK-INPUTCD
                   MOVE    1               TO  FLG-KENSAKU
                   PERFORM 409-KENSAKU-SEC
               END-IF
           END-IF
      *
           .
       41011-SETCODE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    行選択処理
      *****************************************************************
       41015-LIST1SEL-SEC       SECTION.
      *
      *    行選択
           MOVE    ZERO                TO  WRK-SELNO
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL ( IDX     >   SPA-GMN-MAX )
      *
               IF    ( X01-SELECT (IDX)    =   "T" )
      *
                   IF    ( IDX             =   SPA-NAI-SELNO )
                       CONTINUE
                   ELSE
                       MOVE    IDX         TO  WRK-SELNO
                       MOVE    SPA-GMN-MAX TO  IDX
                   END-IF
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-SELNO           >   ZERO )
               MOVE    WRK-SELNO           TO  SPA-NAI-SELNO 
           END-IF
      *H29.9
      *    診療行為の併用算定対応
           IF    (  SPA-GMN-CHKKBN     =   "5" OR  "A"  OR  "B" )
               MOVE    ZERO            TO  SPA-NAI-SELNUM
               MOVE    SPACE           TO  SPA-GMN-SELNUM
               MOVE    12              TO  SPA-GMN-CUR
               GO  TO  41015-LIST1SEL-EXT
           END-IF
      *
           MOVE    SPA-NAI-SELNO      TO  SPA-GMN-SELNUM
                                          SPA-NAI-SELNUM 
      *
           PERFORM 41011-SELNUM-HENSYU-SEC
           .
       41015-LIST1SEL-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    病名コードチェック処理
      *****************************************************************
       4100-BYOMEICD-CHK-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN-BYOSTATUS
                                       SPA-GMN-SRYCD
                                       SPA-NAI-BYOCD-G
                                       SPA-GMN-NAME
                                       SPA-GMN-KOUMOKU1
                                       SPA-NAI-KOUMOKU1
                                       SPA-GMN-MAX
      *
           MOVE    X01-SRYCD       TO  SPA-GMN-SRYCD
      *
           MOVE    1               TO  SPA-GMN-BYOMEICD-CHK-FLG
           MOVE    ZERO            TO  SPA-GMN-BYOMEI-CHK-FLG
      *
           IF    ( SPA-GMN-SRYCD   =   SPACE )
               MOVE    6           TO  SPA-GMN-CUR
               GO  TO  4100-BYOMEICD-CHK-EXT
           END-IF
      *
           PERFORM 4101-BYOMEICD-CHK-SEC
           IF    ( SPA-ERRCD       =   SPACE )
            OR   ( SPA-ERRCD       =   "0016" )
               MOVE    ZERO        TO  SPA-GMN-BYOMEICD-CHK-FLG
      *
               PERFORM 41011-BYOMEI-CHK005-SEC
           END-IF
      *
           .
       4100-BYOMEICD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病名コードチェック処理
      *****************************************************************
       4101-BYOMEICD-CHK-SEC           SECTION.
      *
      *    検索へ
           IF    ( SPA-GMN-SRYCD(1:1)  =   "/" )
               INITIALIZE              SPA-X01-USERBYOMEI-G
               MOVE    SPACE       TO  WRK-BYOMEI
               PERFORM 410-KENSAKU-SEC
               GO  TO  4101-BYOMEICD-CHK-EXT
           END-IF
      *
           INITIALIZE                  XSUB01-LNK
      *
           MOVE    SPA-GMN-SRYCD   TO  WRK-MAE-INPUT
           PERFORM 800-KANA-CHK-SEC
           MOVE    WRK-OUT-INPUT   TO  SPA-GMN-SRYCD
      *
           UNSTRING    SPA-GMN-SRYCD   DELIMITED   BY  ","
           INTO        XSUB01-BYOCD (1)
                       XSUB01-BYOCD (2)
                       XSUB01-BYOCD (3)
                       XSUB01-BYOCD (4)
                       XSUB01-BYOCD (5)
                       XSUB01-BYOCD (6)
           END-UNSTRING
      *
           MOVE    1               TO  XSUB01-SINGLE
           CALL    "ORCGXSUB01"    USING   SPA-AREA
                                           XSUB01-LNK
      *
           MOVE    SPACE           TO  SPA-NAI-BYOCD-G
           PERFORM VARYING IDXB    FROM    1   BY  1
                   UNTIL ( IDXB    >   CONST-BYOCD-MAX )
      *
               IF    ( XSUB01-O-TOP (IDXB) =   1 )
                   PERFORM VARYING IDX0    FROM    1   BY  1
                           UNTIL ( IDX0    >   CONST-MKCD-MAX )
                       MOVE    XSUB01-O-RECECD (IDXB IDX0)
                                   TO  SPA-NAI-BYOCD (IDX0)
                   END-PERFORM
                   MOVE    CONST-BYOCD-MAX
                                   TO  IDXB
               END-IF
      *
           END-PERFORM
      *
           MOVE    XSUB01-O-BYOSTATUSALL
                                        TO  SPA-GMN-BYOSTATUS
      *
           MOVE    XSUB01-O-BYOMEIALL  TO  SPA-GMN-NAME
      *
           EVALUATE    XSUB01-O-RC
           WHEN    1
               MOVE    "0015"          TO  SPA-ERRCD
           WHEN    2
               MOVE    "0016"          TO  SPA-ERRCD
           WHEN    3
               MOVE    "0017"          TO  SPA-ERRCD
           WHEN    4
               IF    ( XSUB01-BYOCD (1)  NOT =   SPACE )
                AND  ( XSUB01-BYOCD (2)      =   SPACE )
                AND  ( XSUB01-BYOCD (3)      =   SPACE )
                AND  ( XSUB01-BYOCD (4)      =   SPACE )
                AND  ( XSUB01-BYOCD (5)      =   SPACE )
                AND  ( XSUB01-BYOCD (6)      =   SPACE )
                   MOVE    XSUB01-BYOCD (1)
                                       TO  WRK-INPUTCD
                   PERFORM 900-USERBYOMEI-KEY11-SEL-SEC
                   IF    ( FLG-USERBYOMEI  =   ZERO )
                       INITIALIZE          SPA-X01-USERBYOMEI-G
                       MOVE    1       TO  SPA-X01-INDEX
                       MOVE    XSUB01-BYOCD (1)
                                       TO  WRK-BYOMEI
                       PERFORM 410-KENSAKU-SEC
                       GO  TO  4101-BYOMEICD-CHK-EXT
                    ELSE
                        MOVE    "0018" TO  SPA-ERRCD
                    END-IF
               ELSE
                        MOVE    "0018" TO  SPA-ERRCD
               END-IF
           END-EVALUATE
      *
           IF    ( XSUB01-O-BYOMEI-CNT >   1 )
               MOVE    "0023"          TO  SPA-ERRCD
               INITIALIZE                  SPA-NAI-BYOCD-G
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
            OR   ( SPA-ERRCD           =   "0016" )
               PERFORM 41011-BYOMEICD-SEC
           ELSE
               MOVE    SPA-GMN-SRYCD   TO  WRK-MAE-INPUT
               PERFORM 800-KANA-CHK-SEC
               IF    ( FLG-ZENKAKU     =   1 )
                   INITIALIZE          SPA-X01-USERBYOMEI-G
                   INITIALIZE          SPA-GMN-BYOSTATUS
                                       SPA-NAI-BYOCD-G
                   MOVE    SPACE       TO  SPA-ERRCD
                   MOVE    ZERO        TO  SPA-GMN-CUR
                   MOVE    SPA-GMN-SRYCD   TO  WRK-BYOMEI
                   PERFORM 410-KENSAKU-SEC
               ELSE
                   MOVE    2           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
      *
           .
       4101-BYOMEICD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェック病名検索処理
      *****************************************************************
       41011-BYOMEI-CHK005-SEC         SECTION.
      *
           PERFORM 900-CHK005-KEY2-SEL-SEC
      *
           MOVE    ZERO            TO  IDX
           PERFORM UNTIL      (IDX     =  100  )  OR
                              (FLG-CHK005 =   1   )
               ADD     1                   TO  IDX 
               MOVE    IDX                 TO  SPA-GMN-TBANGO
                                                          (IDX)
               MOVE    CHK005-YUKOSTYMD    TO  SPA-NAI-TSTYUKYMD
                                                          (IDX)
               MOVE    CHK005-YUKOSTYMD    TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG         TO  SPA-GMN-TSTYUKYMD
                                                          (IDX)
               MOVE    CHK005-YUKOEDYMD    TO  SPA-NAI-TEDYUKYMD
                                                          (IDX)
               MOVE    CHK005-YUKOEDYMD    TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG         TO  SPA-GMN-TEDYUKYMD
                                                          (IDX)
      *
               MOVE    CHK005-ID           TO  SPA-NAI-TID (IDX)
      *
               MOVE    IDX                 TO  SPA-GMN-MAX
      *
               PERFORM 900-CHK005-KEY2-FET-SEC
      *
           END-PERFORM
      *
      *    カーソルクロース
           MOVE    "tbl_chk005"         TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           PERFORM 41041-CHKKBNLIST-INIT-SEC
      *
           .
       41011-BYOMEI-CHK005-EXT.
           EXIT.
      *****************************************************************
      *    病名チェック処理
      *****************************************************************
       4100-BYOMEI-CHK-SEC             SECTION.
      *
           INITIALIZE                  SPA-GMN-BYOSTATUS
                                       SPA-GMN-SRYCD
                                       SPA-NAI-BYOCD-G
                                       SPA-GMN-NAME
                                       SPA-GMN-KOUMOKU1
                                       SPA-NAI-KOUMOKU1
                                       SPA-GMN-MAX
      *
           MOVE    ZERO            TO  SPA-GMN-BYOMEICD-CHK-FLG
           MOVE    1               TO  SPA-GMN-BYOMEI-CHK-FLG
      *
           MOVE    X01-NAME        TO  SPA-GMN-NAME
      *
           PERFORM 4101-BYOMEI-CHK-SEC
           IF    ( SPA-ERRCD       =   SPACE )
            OR   ( SPA-ERRCD       =   "0016" )
               MOVE    ZERO        TO  SPA-GMN-BYOMEI-CHK-FLG
               PERFORM 41011-BYOMEI-CHK005-SEC
           END-IF
      *
           .
       4100-BYOMEI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病名チェック処理
      *****************************************************************
       4101-BYOMEI-CHK-SEC             SECTION.
      *
           IF    ( SPA-GMN-NAME      =   SPACE )
      *
               GO  TO  4101-BYOMEI-CHK-EXT
      *
           END-IF
      *
           MOVE    SPA-GMN-NAME      TO  WRK-MAE-INPUT
           PERFORM 800-KANA-CHK-SEC
           MOVE    WRK-OUT-INPUT       TO  SPA-GMN-NAME
           IF    ( FLG-ERR         NOT =   ZERO )
               MOVE    "0019"          TO SPA-ERRCD
               MOVE    6               TO SPA-GMN-CUR
               GO  TO  4101-BYOMEI-CHK-EXT
           END-IF
      *
      *
           MOVE    ZERO                TO  CNT-STR
           INSPECT SPA-GMN-NAME          TALLYING
                       CNT-STR FOR ALL "■"
           IF    ( CNT-STR        >    ZERO )
               MOVE    "0020"          TO SPA-ERRCD
               MOVE    6               TO SPA-GMN-CUR
               GO  TO  4101-BYOMEI-CHK-EXT
           END-IF
      *
           INITIALIZE                      ORCSBYOMEIAREA
           MOVE    "1"                 TO  LNK-BYO-SYORI
           MOVE    SPA-GMN-NAME        TO  LNK-BYO-BYOMEI
           CALL    "ORCSBYOMEI"    USING   ORCSBYOMEIAREA
                                           SPA-AREA
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >       LNK-BYO-BYOMEI-MAX )
      *
               MOVE    LNK-BYO-TBYOMEICD (IDX0)
                                   TO  SPA-NAI-BYOCD (IDX0)
      *
           END-PERFORM
      *
           MOVE    LNK-BYO-BYOMEIHENFLG
                                   TO  SPA-GMN-BYOSTATUS
      *
           PERFORM 41011-BYOMEICD-SEC
      *
           IF    ( LNK-BYO-BYOMEICD-MAX-ERR    =   1 )
               MOVE    "0016"          TO  SPA-ERRCD
           END-IF
      *
           .
       4101-BYOMEI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病名コード欄編集処理
      *****************************************************************
       41011-BYOMEICD-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-GMN-SRYCD
           MOVE    1                   TO  IDX-PNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >       CONST-BYOCD-MAX )
                    OR   ( SPA-NAI-BYOCD (IDX1)
                                   =       SPACE OR "0000999" )
      *
               IF    ( IDX1        =       1 )
                   STRING    SPA-NAI-BYOCD (IDX1)  DELIMITED   BY  SPACE
                   INTO      SPA-GMN-SRYCD
                   WITH POINTER IDX-PNT
                   END-STRING
               ELSE
                   STRING    ","                   DELIMITED   BY  SIZE
                             SPA-NAI-BYOCD (IDX1)  DELIMITED   BY  SPACE
                   INTO      SPA-GMN-SRYCD
                   WITH POINTER IDX-PNT
                   END-STRING
               END-IF
      *
           END-PERFORM
      *
           .
      *
       41011-BYOMEICD-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理
      *****************************************************************
       411-PRINT-SEC               SECTION.
      *
      *    画面移行用のパラメタ
           MOVE    SPA-X01KYOUTU       TO  SPA-WORK
      *
           MOVE    "X91"               TO  SPA-SAKIPG
           MOVE    "X01"               TO  SPA-MOTOPG
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   SPA-SAKIPG           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       411-PRINT-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    検索処理
      *****************************************************************
       409-KENSAKU-SEC             SECTION.
      *
      *    共通ＳＰＡ編集
           INITIALIZE                  SPA-X98-AREA
           MOVE    "2"                 TO  SPA-X98-TYPE
      *
           INITIALIZE                  SPA-X98GMN-AREA
           IF    ( WRK-INPUTCD         NOT =   SPACE )
      *
               EVALUATE    SPA-GMN-CHKKBN
               WHEN    "1"
               WHEN    "3"
               WHEN    "4"
               WHEN    "6"
               WHEN    "7"
      *                薬剤
                       MOVE    "3"         TO  SPA-X98-GMN-SYORI
               WHEN    "2"
               WHEN    "5"
               WHEN    "A"
               WHEN    "B"
      *                診療行為
                       MOVE    "8"         TO  SPA-X98-GMN-SYORI
               END-EVALUATE
      *
               MOVE    WRK-INPUTCD         TO  SPA-X98-INPUTCD
               MOVE    "1"                 TO  SPA-X98-TYPE
               IF    ( FLG-KANJI           =   1 )
                   MOVE    "J"             TO  SPA-X98-CDSYUBETU
               ELSE
                   IF    ( WRK-INPUTCD(1:1)    NUMERIC )
                       EVALUATE    WRK-CD-MCNT
                       WHEN    3
                       WHEN    6
                           MOVE    "6"     TO  SPA-X98-CDSYUBETU
                       WHEN    4
                           MOVE    "4"     TO  SPA-X98-CDSYUBETU
                       WHEN    5
                           MOVE    "5"     TO  SPA-X98-CDSYUBETU
                       END-EVALUATE
                   ELSE
                       MOVE    "A"         TO  SPA-X98-CDSYUBETU
                   END-IF
               END-IF
      *
               MOVE    "2"                 TO  SPA-X98-GMN-KBN
      *
      *        画面編集
               PERFORM 41011-X98-SET-SEC
      *        対象データなしの時、診療行為で検索する
               IF     (SPA-X98-GMN-MAX     =   ZERO )  AND
                      (SPA-X98-CDSYUBETU   =   "J"  )
                   INITIALIZE                  SPA-X98GMN-AREA
                   MOVE    "8"                 TO  SPA-X98-GMN-SYORI
      *            画面編集
                   PERFORM 41011-X98-SET-SEC
               END-IF
           ELSE
      *        画面編集
               MOVE    "2"                 TO  SPA-X98-GMN-KBN
               PERFORM 41011-X98-SET-SEC
           END-IF
      *
           MOVE    "X01"               TO  SPA-MOTOPG
           MOVE    "X98"               TO  SPA-SAKIPG
           MOVE    "X01"               TO  SPA-X98-MOTOPG
      *
      *
           IF    ( SPA-X98-GMN-MAX     =   ZERO )
               MOVE    "INPUT"             TO  MCP-WIDGET
           ELSE
               MOVE    "SELNUM1"           TO  MCP-WIDGET
           END-IF
      *
           MOVE   "NEW"                TO  MCP-PUTTYPE
           MOVE   "X98    "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       409-KENSAKU-EXT.
           EXIT.
      *****************************************************************
      *    コード表示へ
      *****************************************************************
       410-X100-SENI-SEC               SECTION.
      *
           IF    ( SPA-GMN-CHKKBN      =   "8" OR "9" )
               PERFORM 410-X100-BYOMEI-SEC
           ELSE
               IF    ( SPA-NAI-SRYCD (1:1) =   "S" OR "P" )
                   PERFORM 410-X100-SETCD-SEC
               END-IF
           END-IF
      *
           IF    ( X100-COUNT           >   ZERO )
      *
               MOVE    "B01"                TO  MCP-WIDGET
      *
               MOVE    "X01"                TO  SPA-MOTOPG
               MOVE    "X100"               TO  SPA-SAKIPG
      *
               MOVE    "NEW"                TO  MCP-PUTTYPE
               MOVE    SPA-SAKIPG           TO  MCP-WINDOW
      *
               PERFORM 900-PUT-WINDOW
      *
               MOVE    1                    TO  FLG-END
      *
           END-IF
      *
      *
           .
       410-X100-SENI-EXT.
           EXIT.
      *****************************************************************
      *    病名コード表示編集処理
      *****************************************************************
       410-X100-BYOMEI-SEC             SECTION.
      *
           INITIALIZE                  X100
      *
           MOVE    ZERO                TO  IDX1
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >       CONST-MKCD-MAX )
                    OR   ( IDX1    >       CONST-CDLST-MAX )
                    OR   ( SPA-NAI-BYOCD (IDX1)    =   SPACE )
      *
               IF   ( SPA-NAI-BYOCD (IDX1) NOT =   SPACE )
      *
                   COMPUTE X100-COUNT
                       =   X100-COUNT  +   1
      *
                   MOVE    X100-COUNT  TO  WRK-ZZ9
                   MOVE    WRK-ZZ9     TO  X100-TBANGO    (X100-COUNT)
      *
                   MOVE    SPA-NAI-BYOCD (IDX1)
                                       TO  X100-TBYOMEICD (X100-COUNT)
      *
                   IF    ( SPA-NAI-BYOCD (IDX1)
                                           =   "0000999" )
                       IF    ( IDX1    =   1 )
                           MOVE    SPA-GMN-NAME
                                       TO  X100-TBYOMEI   (X100-COUNT)
                       END-IF
                   ELSE
      *
                       MOVE    SPA-NAI-BYOCD (IDX1)
                                       TO  WRK-CMB-CD
                       PERFORM 4200-YYKBYOCD-CHK-SEC
                       IF    ( WRK-CMB-ITM NOT =   SPACE )
                           MOVE    WRK-CMB-ITM
                                       TO  X100-TBYOMEI   (X100-COUNT)
                       ELSE
      *
                           INITIALIZE                  BYOMEI-REC
                           MOVE    SPA-NAI-BYOCD (IDX1)
                                           TO  BYO-BYOMEICD
      *****************    MOVE    SPA-HOSPNUM     TO  BYO-HOSPNUM
                           MOVE    BYOMEI-REC  TO  MCPDATA-REC
                           MOVE    "tbl_byomei"    TO  MCP-TABLE
                           MOVE    "key"           TO  MCP-PATHNAME
                           PERFORM 910-DBSELECT-SEC
                           IF    ( MCP-RC      =   ZERO )
                               MOVE    MCPDATA-REC TO  BYOMEI-REC
                           ELSE
                               INITIALIZE              BYOMEI-REC
                           END-IF
      *
                           MOVE    "tbl_byomei"     TO  MCP-TABLE
                           MOVE    "key"            TO  MCP-PATHNAME
                           PERFORM 910-DBCLOSECURSOR-SEC
      *
                           MOVE    BYO-BYOMEI
                                       TO  X100-TBYOMEI   (X100-COUNT)
                       END-IF
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
       410-X100-BYOMEI-EXT.
           EXIT.
      *****************************************************************
      *    入力コード表示編集処理
      *****************************************************************
       410-X100-SETCD-SEC              SECTION.
      *
           INITIALIZE                          X100
      *
           PERFORM 900-INPUTSET-KEY7-SEL-SEC
      *
           MOVE    ZERO                TO      IDXL
                                               X100-COUNT
      *
           PERFORM UNTIL ( X100-COUNT      >=  CONST-CDLST-MAX )
                    OR   ( IDXL            >=  CONST-SETCD-MAX )
                    OR   ( FLG-INPUTSET NOT =  ZERO )
      *
               IF    ( ISET-INPUTCD(1:1)   =   "." )
                   CONTINUE
               ELSE
      *
                   COMPUTE X100-COUNT  =   X100-COUNT  +   1
                   COMPUTE IDXL        =   IDXL        +   1
      *
                   MOVE    IDXL        TO  WRK-ZZ9
                   MOVE    WRK-ZZ9     TO  X100-TBANGO    (X100-COUNT)
                   MOVE    ISET-INPUTCD TO X100-TBYOMEICD (X100-COUNT)
                   MOVE    ISET-INPUTCD TO WRK-TNS-SRYCD
                   IF    ( ISET-INPUTCD(1:1)   =   "S" )
                       PERFORM 900-INPUTCD-KEY2-SEL-SEC
                       MOVE    ICD-DSPNAME TO  X100-TBYOMEI (X100-COUNT)
                   ELSE
                       PERFORM 900-TENSU-KEY24-SEL-SEC
                       MOVE    TNS-NAME    TO  X100-TBYOMEI (X100-COUNT)
      *
                       PERFORM 900-SRYCDCHG-KEY2-SEL-SEC
                       IF    ( FLG-SRYCDCHG    =   ZERO )
                           COMPUTE X100-COUNT  =   X100-COUNT  +   1
                           STRING  "("             DELIMITED BY SIZE
                                   CHG-RJNSRYCD    DELIMITED BY SIZE
                                   ")"             DELIMITED BY SIZE
                           INTO X100-TBYOMEICD (X100-COUNT)
                           END-STRING
                           MOVE    CHG-RJNSRYCD TO WRK-TNS-SRYCD
                           PERFORM 900-TENSU-KEY24-SEL-SEC
                           STRING  "（"            DELIMITED BY SIZE
                                   TNS-NAME        DELIMITED BY SPACE
                                   "）"            DELIMITED BY SIZE
                           INTO  X100-TBYOMEI(X100-COUNT)
                           END-STRING
                       END-IF
                   END-IF
               END-IF
      *
               PERFORM 900-INPUTSET-KEY7-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_inputset"  TO  MCP-TABLE
           MOVE    "key7"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       410-X100-SETCD-EXT.
           EXIT.
      *****************************************************************
      *    検索画面編集処理
      *****************************************************************
       41011-X98-SET-SEC                           SECTION.
      *
           MOVE    SPA-SYSYMD      TO  SPA-X01-KJNYMD
      *
           MOVE    SPA-X01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-X01         TO  SPA-X01SONOTA
           MOVE    SPA-X01FREE     TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGX98"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-X01KYOUTU 
           MOVE    SPA-FREE        TO  SPA-X01FREE
           MOVE    SPA-X01SONOTA   TO  SPA-X01 
      *
           .
       41011-X98-SET-EXT.
           EXIT.
      *****************************************************************
      *    検索処理
      *****************************************************************
       410-KENSAKU-SEC             SECTION.
      *
      *    共通ＳＰＡ編集
           INITIALIZE                  SPA-X99-AREA
      *
           INITIALIZE                  SPA-GMN-X99-AREA
           MOVE    WRK-BYOMEI          TO  SPA-X99-BYOMEI
      *
           PERFORM 41011-X99-SET-SEC
      *
           MOVE    "X01"               TO  SPA-MOTOPG
           MOVE    "X99"               TO  SPA-SAKIPG
           MOVE    "X01"               TO  SPA-X99-MOTOPG
      *
           IF    ( SPA-GMN-X99-SRCBYOLST-MAX   =   ZERO )
               MOVE    "SEL_BYOMEI"    TO  MCP-WIDGET
           ELSE
               MOVE    "SELNUM"        TO  MCP-WIDGET
           END-IF
      *
           MOVE   "NEW"                TO  MCP-PUTTYPE
           MOVE   "X99    "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       410-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    検索画面編集処理
      *****************************************************************
       41011-X99-SET-SEC               SECTION.
      *
           MOVE    SPA-X01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-X01         TO  SPA-X01SONOTA
           MOVE    SPA-X01FREE     TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGX99"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-X01KYOUTU 
           MOVE    SPA-FREE        TO  SPA-X01FREE
           MOVE    SPA-X01SONOTA   TO  SPA-X01 
      *
           .
       41011-X99-SET-EXT.
           EXIT.
      *****************************************************************
      *    項目クリア
      *****************************************************************
       493-CLEAR-SEC                   SECTION.
      *
           MOVE    SPA-X01-MAE-G   TO  WRK-X01-MAE-G
           PERFORM 310-SPASET-SEC
           MOVE    WRK-X01-MAE-G   TO  SPA-X01-MAE-G
           MOVE    1               TO  SPA-GMN-PAGE
      *
           .
       491-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定
      *****************************************************************
       410-KAKUTEI-SEC           SECTION.
      *
           MOVE    1                   TO  FLG-TOUROKU
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-KAKUTEI-SEC
           END-IF
      *
           .
       410-KAKUTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定
      *****************************************************************
       4101-KAKUTEI-SEC                SECTION.
      *
           IF    ( SPA-NAI-STYUKYMD    =   ZERO )
               IF    ( SPA-GMN-STYUKYMD(1:8)   =   ALL "0" )
                   CONTINUE
               ELSE
                   MOVE    2                   TO  SPA-GMN-CUR
                   MOVE    "0003"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
             IF    ( SPA-NAI-EDYUKYMD    =   ZERO   )
              OR   ( SPA-NAI-CHKKBN      =   SPACE  )
               MOVE    "0003"  TO  SPA-ERRCD
             END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
             IF    ( SPA-GMN-CHKKBN  =   "8" OR "9" )
               IF    ( SPA-NAI-BYOCD (1)   =   SPACE  )
                   MOVE    2       TO  SPA-GMN-CUR
                   MOVE    "0003"  TO  SPA-ERRCD
               END-IF
             ELSE
               IF    ( SPA-NAI-SRYCD   =   SPACE  )
                   MOVE    2       TO  SPA-GMN-CUR
                   MOVE    "0003"  TO  SPA-ERRCD
               END-IF
             END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
             IF    ( SPA-NAI-EDYUKYMD    <   SPA-NAI-STYUKYMD )
               MOVE    4               TO  SPA-GMN-CUR
               MOVE    "0011"          TO  SPA-ERRCD 
             END-IF
           END-IF
      *
           IF    ( SPA-NAI-SELNUM      >   ZERO )
            AND  ( SPA-NAI-TCHOFUKU (SPA-NAI-SELNUM)
                                       =   1 )
               CONTINUE
           ELSE
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-GMN-MAX
                        OR   ( SPA-ERRCD   NOT =   SPACE )
      *
                   IF    ( SPA-NAI-SELNUM      =   IDX )
                       CONTINUE
                   ELSE
      *
                       IF    ( SPA-NAI-EDYUKYMD    <  SPA-NAI-TSTYUKYMD
                                                              (IDX))
                       OR    ( SPA-NAI-STYUKYMD    >  SPA-NAI-TEDYUKYMD
                                                              (IDX))
                           CONTINUE
                       ELSE
                           MOVE    "0004"  TO  SPA-ERRCD
                           MOVE    4       TO  SPA-GMN-CUR
                       END-IF
      *
                   END-IF
      *
               END-PERFORM
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-CHKMST-SENI-SEC
           END-IF
      *
           .
       4101-KAKUTEI-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ設定画面遷移処理
      *****************************************************************
       4101-CHKMST-SENI-SEC            SECTION.
      *
      *
      *     画面移行用のパラメタ
           INITIALIZE                  SPA-X01KYOUTU
      *
           MOVE    SPA-GMN-CHKKBN-G    TO  SPA-X01-CHKKBN-G
      *
           MOVE    SPA-GMN-SELNUM      TO  SPA-X01-SELNUM
           MOVE    SPA-NAI-STYUKYMD    TO  SPA-X01-STYUKYMD
           MOVE    SPA-NAI-EDYUKYMD    TO  SPA-X01-EDYUKYMD 
           MOVE    SPA-GMN-STYUKYMD    TO  SPA-X01-STYUKYMDH
           MOVE    SPA-GMN-EDYUKYMD    TO  SPA-X01-EDYUKYMDH
           IF    ( SPA-NAI-SELNUM      >   ZERO )
               MOVE    SPA-NAI-TSTYUKYMD (SPA-NAI-SELNUM)
                                       TO  SPA-X01-SEL-STYUKYMD
               MOVE    SPA-NAI-TEDYUKYMD (SPA-NAI-SELNUM)
                                       TO  SPA-X01-SEL-EDYUKYMD
               IF    ( SPA-X01-CHKKBN  =   "8" OR "9" )
                   MOVE    SPA-NAI-ID  TO  SPA-X01-ID
               ELSE
                   MOVE    SPA-NAI-TCHOFUKU (SPA-NAI-SELNUM)
                                       TO  SPA-X01-ID
               END-IF
           END-IF
      *
           EVALUATE    TRUE
           WHEN  ( SPA-SYSYMD          >   SPA-X01-EDYUKYMD )
               MOVE    SPA-X01-EDYUKYMD
                                       TO  SPA-X01-KJNYMD
           WHEN  ( SPA-SYSYMD          <   SPA-X01-STYUKYMD )
               MOVE    SPA-X01-STYUKYMD
                                       TO  SPA-X01-KJNYMD
           WHEN    OTHER
               MOVE    SPA-SYSYMD      TO  SPA-X01-KJNYMD
           END-EVALUATE
      *
           IF    ( SPA-X01-CHKKBN      =   "8" OR  "9" )
               MOVE    CONST-BYO       TO  SPA-X01-SRYBYO
           ELSE
               MOVE    CONST-SRY       TO  SPA-X01-SRYBYO
           END-IF
      *
           MOVE    SPA-GMN-SRYCD       TO  SPA-X01-SRYCD
           MOVE    SPA-GMN-NAME        TO  SPA-X01-NAME
           MOVE    SPA-GMN-BYOSTATUS   TO  SPA-X01-BYOSTATUS
           MOVE    SPA-NAI-JYKNKBN     TO  SPA-X01-JYKNKBN
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   CONST-MKCD-MAX )
                    OR   ( SPA-NAI-BYOCD (IDX0)
                                   =   SPACE )
      *
               MOVE    SPA-NAI-BYOCD (IDX0)
                                       TO  SPA-X01-BYOCD  (IDX0)
               IF    ( IDX0        >   1 )
                   MOVE    "-"     TO  SPA-X01-BYOCDSEP (IDX0 - 1)
               END-IF
      *
           END-PERFORM
      *
           MOVE    SPA-NAI-MOTOPG      TO  SPA-X01-MOTOPG
           MOVE    SPA-NAI-BYOMEI      TO  SPA-X01-BYOMEI
           MOVE    SPA-NAI-HOSOKU      TO  SPA-X01-HOSOKU
      *
           MOVE    SPA-X01-CHKKBN      TO  SPA-X01-MAE-CHKKBN
           MOVE    SPA-X01-SRYCD       TO  SPA-X01-MAE-SRYCD
           MOVE    SPA-X01-NAME        TO  SPA-X01-MAE-NAME
      *
           MOVE    SPA-X01KYOUTU       TO  SPA-WORK
      *
           EVALUATE SPA-X01-CHKKBN
           WHEN    "1"
           WHEN    "2"
           WHEN    "6"
               MOVE    "X04"           TO  SPA-SAKIPG
           WHEN    "7"
           WHEN    "8"
           WHEN    "9"
               MOVE    "X05"           TO  SPA-SAKIPG
      *H29.9
      *GNGEE
           WHEN    "5"
           WHEN    "A"
           WHEN    "B"
               MOVE    ZERO            TO  SPA-X01-SELNUM
               MOVE    ZERO            TO  SPA-NAI-SELNUM
               MOVE    "X08"           TO  SPA-SAKIPG
      * 
           WHEN    OTHER
               MOVE    "X02"           TO  SPA-SAKIPG
           END-EVALUATE
      *
           MOVE    "X01"               TO  SPA-MOTOPG
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   SPA-SAKIPG           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       4101-CHKMST-SENI-EXT.
           EXIT.
      *****************************************************************
      *    前回ＣＤ処理
      *****************************************************************
       430-ZENKAICD-SEC                SECTION.
      *
           IF    ( SPA-X01-MAE-CHKKBN  NOT =   SPACE )
      *
               MOVE    SPACE           TO  SPA-NAI-CHKKBN
                                           SPA-GMN-SRYCD
                                           SPA-NAI-SRYCD
                                           SPA-GMN-NAME
               MOVE    SPA-X01-MAE-CHKKBN
                                       TO  SPA-GMN-CHKKBN
      *
               IF    ( SPA-GMN-CHKKBN  =   "8" OR "9" )
                   MOVE    SPA-X01-MAE-NAME
                                       TO  X01-NAME
                   PERFORM 4100-BYOMEI-CHK-SEC
                   MOVE    SPACE       TO  SPA-ERRCD
                   PERFORM 4102-KIHON-CHK-SEC
               ELSE
                   MOVE    SPA-X01-MAE-SRYCD
                                       TO  SPA-GMN-SRYCD
                   PERFORM 4102-KIHON-CHK-SEC
               END-IF
           END-IF
      *
           .
       430-ZENKAICD-EXT.
           EXIT.
      *****************************************************************
      *    予約病名コードチェック処理
      *****************************************************************
       4200-YYKBYOCD-CHK-SEC           SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   TBL-BYOMEICD-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( TBL-YYK-BYOMEICD (IDX-CMB)  =   WRK-CMB-CD )
                   MOVE   TBL-YYK-BYOMEI(IDX-CMB)
                                           TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
       4200-YYKBYOCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       5002-HIZUKE-CHK-SEC         SECTION.
      *
           EVALUATE  WRK-YMD(1:8)
           WHEN  ZERO
               MOVE  ZERO              TO  WRK-SYMD
               MOVE  WRK-YMD(1:8)      TO  WRK-HENYMDG
           WHEN  "99999999"
               MOVE  ALL "9"           TO  WRK-SYMD
               MOVE  WRK-YMD(1:8)      TO  WRK-HENYMDG
           WHEN    OTHER
      *        日付画面チェックサブ
               INITIALIZE                  ORCSGDAYAREA
               MOVE    WRK-YMD         TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF    ( SGDAY-RC        =   ZERO )
                   MOVE    SGDAY-OTDAY TO  WRK-HENYMDG
                   MOVE    SGDAY-SDAY  TO  WRK-SYMD
               ELSE
                   MOVE    "0002"      TO  SPA-ERRCD
               END-IF
           END-EVALUATE
           .
       5002-HIZUKE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    和暦西暦変換編集処理
      *****************************************************************
       5002-HIZUKE-HEN-SEC             SECTION.
      *
           IF    ( WRK-SYMD        =   ALL "9"  OR   ZERO )
               MOVE    WRK-SYMD (1:8)      TO  WRK-HENYMDG
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF
      *
           .
       5002-HIZUKE-HEN-EXT.
           EXIT.
      *
     *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF    ( SPA-ERRCD       NOT =   SPACE )
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "X01    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  X01
           INITIALIZE                      X01
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL     ( IDX     >       SPA-GMN-MAX )
      *
               MOVE    SPA-GMN-TBANGO    (IDX) TO  X01-TBANGO (IDX)
               MOVE    SPA-GMN-TSTYUKYMD (IDX) TO  X01-TSTYUKYMD(IDX)
               MOVE    SPA-GMN-TEDYUKYMD (IDX) TO  X01-TEDYUKYMD(IDX)
               MOVE    IDX                     TO  X01-COUNT
      *
               IF    ( IDX             =   SPA-NAI-SELNO )
                   MOVE    "T"         TO  X01-SELECT (IDX)
               ELSE
                   MOVE    "F"         TO  X01-SELECT (IDX)
               END-IF
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-MAX         TO  X01-COUNT
      *
           MOVE    SPA-GMN-SRYCD       TO  X01-SRYCD
           MOVE    SPA-GMN-NAME        TO  X01-NAME
           MOVE    SPA-GMN-SELNUM      TO  X01-SELNUM
           MOVE    SPA-GMN-STYUKYMD    TO  X01-STYUKYMD
           MOVE    SPA-GMN-EDYUKYMD    TO  X01-EDYUKYMD
           MOVE    SPA-GMN-CHKKBN-G    TO  X01-CHKKBN 
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL ( IDX >   SPA-GMN-CHKKBN-MAX )
      *
               MOVE    SPA-GMN-CHKKBNLIST(IDX) TO  X01-CHKKBNLIST (IDX)
               MOVE    IDX                     TO  X01-CHKKBNCOUNT
      *
           END-PERFORM
      *
           MOVE    WIDGET-NORMAL           TO  X01-B10-STATE
      *
           IF    ( SPA-GMN-CHKKBN      =   "8" OR  "9" )
               MOVE    "病名コード"        TO  X01-LBLSRYCD
               MOVE    "ＣＤ表示"          TO  X01-B10-LABEL
      *
               IF    ( SPA-NAI-BYOCD (1)       =   SPACE )
                   MOVE    WIDGET-INSENSITIVE
                                           TO  X01-B10-STATE
               END-IF
           ELSE
               MOVE    "診療行為コード"    TO  X01-LBLSRYCD
               IF    ( SPA-NAI-SRYCD (1:1) =   "S" OR "P" )
                   MOVE    "ＣＤ表示"      TO  X01-B10-LABEL
               ELSE
                   MOVE    WIDGET-INSENSITIVE
                                           TO  X01-B10-STATE
               END-IF
           END-IF
      *
           MOVE    "black"                 TO  X01-LBLBYOSTATUS-STYLE
           IF    ( SPA-GMN-CHKKBN      =   "8" OR  "9" )
               IF    ( SPA-GMN-BYOSTATUS   =   "1" )
                   MOVE    "＊病名編集＊"      TO  X01-LBLBYOSTATUS
               ELSE
                   MOVE    "病名"              TO  X01-LBLBYOSTATUS
               END-IF
           END-IF
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF    ( SPA-GMN-CUR     =   ZERO )
               PERFORM 50011-CUR-SET-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-CUR
           WHEN   1
               MOVE  "CHKKBN"      TO  MCP-WIDGET
           WHEN   2
               MOVE  "SRYCD"       TO  MCP-WIDGET
           WHEN   3
               MOVE  "SELNUM"      TO  MCP-WIDGET
           WHEN   4
               MOVE  "STYUKYMD"    TO  MCP-WIDGET
           WHEN   5
               MOVE  "EDYUKYMD"    TO  MCP-WIDGET
           WHEN   6
               MOVE  "NAME"        TO  MCP-WIDGET
           WHEN   12
               MOVE  "B12"         TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       50011-CUR-SET-SEC             SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
           WHEN    "CHKKBN"
               MOVE    2           TO  SPA-GMN-CUR
           WHEN    "SRYCD"
               IF    ( SPA-GMN-CHKKBN  =   "8" OR  "9" )
                   MOVE   6        TO  SPA-GMN-CUR
               ELSE
                   MOVE   3        TO  SPA-GMN-CUR
               END-IF
           WHEN    "SELNUM"
                MOVE   4           TO  SPA-GMN-CUR
           WHEN    "STYUKYMD"
                MOVE   5           TO  SPA-GMN-CUR
           WHEN    "EDYUKYMD"
                MOVE   12          TO  SPA-GMN-CUR
           WHEN    "NAME"
               MOVE    3           TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
       50011-CUR-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0001"
               MOVE    "選択番号が有りません"     TO  SPA-ERRMSG
           WHEN    "0002"
               MOVE    "日付入力エラー"           TO  SPA-ERRMSG
           WHEN    "0003"
               MOVE    "エラーの項目があります"   TO  SPA-ERRMSG
           WHEN    "0004"
               MOVE    "有効期間が重複します"     TO  SPA-ERRMSG
           WHEN    "0005"
               MOVE    "点数マスタがありません"   TO  SPA-ERRMSG
           WHEN    "0006"
               MOVE    "チェック区分がありません" TO  SPA-ERRMSG
           WHEN    "0007"
               MOVE    "診療コード入力エラー"     TO  SPA-ERRMSG
           WHEN    "0008"
               MOVE    "チェック区分に対して入力できないコードです"
                                                  TO  SPA-ERRMSG
           WHEN    "0009"
               MOVE    "チェック区分を入力してから印刷して下さい"
                                                  TO  SPA-ERRMSG
           WHEN    "0010"
               MOVE    "薬剤と禁忌のチェックコードは使用できません"
                                                  TO  SPA-ERRMSG
           WHEN    "0011"
               MOVE    "有効期間を正しく入力してください"
                                                  TO  SPA-ERRMSG
           WHEN    "0015"
               STRING  "未コード化傷病名は他のコードといっしょに"
                                                   DELIMITED   BY  SIZE
                       "入力できません"            DELIMITED   BY  SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    "0016"
               STRING  "病名コードが２２個以上になりますので、編"
                                                   DELIMITED   BY  SIZE
                       "集病名とします"            DELIMITED   BY  SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    "0017"
               MOVE    "病名コードがありません"    TO  SPA-ERRMSG
           WHEN    "0018"
               MOVE    "自院病名コードがありません"
                                                   TO  SPA-ERRMSG
           WHEN    "0019"
               MOVE    "病名入力エラー"            TO  SPA-ERRMSG
           WHEN    "0020"
               MOVE    "不明の病名コードが入力されています"
                                       TO  SPA-ERRMSG
           WHEN    "0021"
               STRING  "約束処方セットは"      DELIMITED   BY  SIZE
                       "薬剤と病名のチェック"  DELIMITED   BY  SIZE
                       "のみ指定可能です"      DELIMITED   BY  SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    "0022"
               STRING  "診療セットは"          DELIMITED   BY  SIZE
                       "診療行為と病名のチェック"  DELIMITED   BY  SIZE
                       "のみ指定可能です"      DELIMITED   BY  SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    "0023"
               MOVE    "複数病名の指定はできません"
                                                   TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  XERR
           MOVE    SPA-ERRCD            TO  XERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  XERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "X01"                TO  SPA-MOTOPG
           MOVE    "XERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "XERR"               TO  MCP-WINDOW
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
      *    カナチェック処理
      *****************************************************************
       800-KANA-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-ZENKAKU
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU     =   1     )
               MOVE    1                   TO  FLG-ERR
               MOVE    WRK-MAE-INPUT       TO  WRK-OUT-INPUT
               IF    ( KANACHK-RC2     =   7 )
                   MOVE    KANACHK-OUT-INPUT
                                           TO  WRK-OUT-INPUT
               END-IF
           ELSE
               MOVE    ZERO                TO  FLG-ERR
               MOVE    KANACHK-OUT-INPUT   TO  WRK-OUT-INPUT
           END-IF
      *
           IF    ( KANACHK-SYUBETU     =   2   )
               MOVE    1               TO  FLG-ZENKAKU
           END-IF
      *
           .
       800-KANA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    カレンダー処理
      *****************************************************************
       800-CALENDAR-SEC               SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-SYMD
      *
           .
       800-CALENDAR-EXT.
           EXIT.
      *****************************************************************
      *    点数マスター検索（KEY4）
      *****************************************************************
       900-TENSU-KEY4-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
      *
           INITIALIZE                      TNS-TENSU-REC
           MOVE    SPA-GMN-SRYCD       TO  TNS-SRYCD
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
           ELSE
               MOVE    1               TO  FLG-TENSU
               INITIALIZE                  TNS-TENSU-REC
           END-IF
      *
           .
       900-TENSU-KEY4-SEL-EXT.
           EXIT.
      *****************************************************************
      *    点数マスター読込（KEY4）
      *****************************************************************
       900-TENSU-KEY4-FET-SEC          SECTION.
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
           ELSE
               MOVE    1               TO  FLG-TENSU
               INITIALIZE                  TNS-TENSU-REC
           END-IF
      *
           .
       900-TENSU-KEY4-FET-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ検索処理
      *****************************************************************
       900-TENSU-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
      *
           INITIALIZE                  TNS-TENSU-REC
           MOVE    WRK-TNS-SRYCD       TO  TNS-SRYCD
           MOVE    WRK-TNS-YUKOSTYMD   TO  TNS-YUKOSTYMD
                                           TNS-YUKOEDYMD
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
           ELSE
               MOVE    1               TO  FLG-TENSU
               INITIALIZE                  TNS-TENSU-REC
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-TENSU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ検索処理
      *****************************************************************
       900-TENSU-KEY24-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
      *
           INITIALIZE                  TNS-TENSU-REC
           MOVE    WRK-TNS-SRYCD       TO  TNS-SRYCD
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key24"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
               IF     (TNS-SRYCD(1:1)      =   "1") AND
                      (TNS-ROUTEKKBN       =   2  )
                   MOVE    SPACE       TO  WRK-TNS-NAME
                   STRING  "［老］"    DELIMITED   BY  SIZE
                           TNS-NAME    DELIMITED   BY  SIZE
                   INTO    WRK-TNS-NAME
                   END-STRING
                   MOVE    WRK-TNS-NAME TO TNS-NAME
               END-IF
           ELSE
               MOVE    1               TO  FLG-TENSU
               INITIALIZE                  TNS-TENSU-REC
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key24"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-TENSU-KEY24-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    チェックマスタ検索処理
      *****************************************************************
       900-CHK-KEY12-SEL-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-CHK
      *
           INITIALIZE                  CHK-REC
           MOVE    SPA-GMN-CHKKBN      TO  CHK-CHKKBN
           MOVE    SPA-GMN-SRYCD       TO  CHK-SRYCD
           MOVE    SPA-HOSPNUM         TO  CHK-HOSPNUM
           MOVE    CHK-REC             TO  MCPDATA-REC
           MOVE    "tbl_chk"           TO  MCP-TABLE
           MOVE    "key12"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHK-REC
               MOVE    ZERO            TO  FLG-CHK
           ELSE
               INITIALIZE                  CHK-REC
               MOVE    1               TO  FLG-CHK
           END-IF
      *
           .
       900-CHK-KEY12-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタFETCH処理(KEY12)
      *****************************************************************
       900-CHK-KEY12-FET-SEC           SECTION.
      *
           MOVE    "tbl_chk"           TO  MCP-TABLE
           MOVE    "key12"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHK-REC
               MOVE    ZERO            TO  FLG-CHK
           ELSE
               INITIALIZE                  CHK-REC
               MOVE    1               TO  FLG-CHK
           END-IF
      *
           .
       900-CHK-KEY12-FET-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ検索処理
      *****************************************************************
       900-CHK-KEY23-SEL-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-CHK
      *
           INITIALIZE                  CHK-REC
           MOVE    SPA-GMN-CHKKBN      TO  CHK-CHKKBN
           MOVE    SPA-GMN-SRYCD       TO  CHK-SRYCD
           MOVE    SPA-HOSPNUM         TO  CHK-HOSPNUM
           MOVE    CHK-REC             TO  MCPDATA-REC
           MOVE    "tbl_chk"           TO  MCP-TABLE
           MOVE    "key23"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHK-REC
               MOVE    ZERO            TO  FLG-CHK
           ELSE
               INITIALIZE                  CHK-REC
               MOVE    1               TO  FLG-CHK
           END-IF
      *
           .
       900-CHK-KEY23-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタFETCH処理(KEY12)
      *****************************************************************
       900-CHK-KEY23-FET-SEC           SECTION.
      *
           MOVE    "tbl_chk"           TO  MCP-TABLE
           MOVE    "key23"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHK-REC
               MOVE    ZERO            TO  FLG-CHK
           ELSE
               INITIALIZE                  CHK-REC
               MOVE    1               TO  FLG-CHK
           END-IF
      *
           .
       900-CHK-KEY23-FET-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２検索処理
      *****************************************************************
       900-CHKSND-KEY3-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-CHKSND
      *
           INITIALIZE                  CHKSND-REC
           MOVE    SPA-GMN-CHKKBN      TO  CHKSND-CHKKBN
           MOVE    SPA-GMN-SRYCD       TO  CHKSND-SRYCD
           MOVE    SPA-HOSPNUM         TO  CHKSND-HOSPNUM
           MOVE    "99999999"          TO  CHKSND-YUKOSTYMD
           MOVE    "00000000"          TO  CHKSND-YUKOEDYMD
           MOVE    ZERO                TO  CHKSND-CDKBN
           MOVE    CHKSND-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHKSND-REC
               MOVE    ZERO            TO  FLG-CHKSND
           ELSE
               INITIALIZE                  CHKSND-REC
               MOVE    1               TO  FLG-CHKSND
           END-IF
      *
           .
       900-CHKSND-KEY3-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２FETCH処理(KEY12)
      *****************************************************************
       900-CHKSND-KEY3-FET-SEC             SECTION.
      *
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHKSND-REC
               MOVE    ZERO            TO  FLG-CHKSND
           ELSE
               INITIALIZE                  CHKSND-REC
               MOVE    1               TO  FLG-CHKSND
           END-IF
      *
           .
       900-CHKSND-KEY3-FET-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ３検索処理
      *****************************************************************
       900-CHKTRD-KEY12-SEL-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-CHKTRD
      *
           INITIALIZE                  CHKTRD-REC
           MOVE    SPA-GMN-CHKKBN      TO  CHKTRD-CHKKBN
           MOVE    SPA-GMN-SRYCD       TO  CHKTRD-SRYCD
           MOVE    SPA-HOSPNUM         TO  CHKTRD-HOSPNUM
           MOVE    CHKTRD-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_chktrd"        TO  MCP-TABLE
           MOVE    "key12"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHKTRD-REC
               MOVE    ZERO            TO  FLG-CHKTRD
           ELSE
               INITIALIZE                  CHKTRD-REC
               MOVE    1               TO  FLG-CHKTRD
           END-IF
      *
           .
       900-CHKTRD-KEY12-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ３FETCH処理(KEY12)
      *****************************************************************
       900-CHKTRD-KEY12-FET-SEC        SECTION.
      *
           MOVE    "tbl_chktrd"        TO  MCP-TABLE
           MOVE    "key12"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHKTRD-REC
               MOVE    ZERO            TO  FLG-CHKTRD
           ELSE
               INITIALIZE                  CHKTRD-REC
               MOVE    1               TO  FLG-CHKTRD
           END-IF
      *
           .
       900-CHKTRD-KEY12-FET-EXT.
           EXIT.
      * 
      *****************************************************************
      *    チェックマスタ５検索処理
      *****************************************************************
       900-CHK005-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-CHK005
      *
           INITIALIZE                  CHK005-REC
           MOVE    SPA-GMN-CHKKBN      TO  CHK005-CHKKBN
           MOVE    SPA-NAI-ID          TO  CHK005-ID
           MOVE    1                   TO  CHK005-RENNUM
           MOVE    SPA-HOSPNUM         TO  CHK005-HOSPNUM
           MOVE    CHK005-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_chk005"        TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHK005-REC
               MOVE    ZERO            TO  FLG-CHK005
           ELSE
               INITIALIZE                  CHK005-REC
               MOVE    1               TO  FLG-CHK005
           END-IF
      *
      *    カーソルクロース
           MOVE    "tbl_chk005"         TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-CHK005-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ５検索処理
      *****************************************************************
       900-CHK005-KEY2-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-CHK005
      *
           INITIALIZE                  CHK005-REC
           MOVE    SPA-GMN-CHKKBN      TO  CHK005-CHKKBN
           MOVE    SPA-GMN-NAME        TO  CHK005-BYOMEI
           MOVE    SPA-HOSPNUM         TO  CHK005-HOSPNUM
           MOVE    CHK005-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_chk005"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHK005-REC
               MOVE    ZERO            TO  FLG-CHK005
           ELSE
               INITIALIZE                  CHK005-REC
               MOVE    1               TO  FLG-CHK005
           END-IF
      *
           .
       900-CHK005-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ３FETCH処理(KEY12)
      *****************************************************************
       900-CHK005-KEY2-FET-SEC         SECTION.
      *
           MOVE    "tbl_chk005"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHK005-REC
               MOVE    ZERO            TO  FLG-CHK005
           ELSE
               INITIALIZE                  CHK005-REC
               MOVE    1               TO  FLG-CHK005
           END-IF
      *
           .
       900-CHK005-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    自院病名検索処理(KEY11)
      *****************************************************************
       900-USERBYOMEI-KEY11-SEL-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-USERBYOMEI
      *
           INITIALIZE                      USERBYOMEI-REC
           MOVE    SPA-HOSPNUM         TO  USBYO-HOSPNUM
      *
           STRING  WRK-INPUTCD             DELIMITED   BY  SPACE
                   "%"                     DELIMITED   BY  SIZE
           INTO    USBYO-BYOMEIINPUTCD
           END-STRING
      *
           MOVE    USERBYOMEI-REC      TO  MCPDATA-REC
           MOVE    "tbl_userbyomei"    TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC    =     ZERO )
               MOVE    MCPDATA-REC     TO  USERBYOMEI-REC
           ELSE
               INITIALIZE                  USERBYOMEI-REC
               MOVE    1               TO  FLG-USERBYOMEI
           END-IF
      *
           MOVE    "tbl_userbyomei"    TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-USERBYOMEI-KEY11-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入力コード検索処理
      *****************************************************************
       900-INPUTCD-KEY2-SEL-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-INPUTCD
      *
           INITIALIZE                  INPUTCD-REC
           MOVE    SPACE           TO  INPUTCD-REC
           MOVE    SPA-HOSPNUM     TO  ICD-HOSPNUM
           MOVE    "6"             TO  ICD-CDSYU
           MOVE    WRK-TNS-SRYCD   TO  ICD-INPUTCD
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  INPUTCD-REC
           ELSE
               INITIALIZE                  INPUTCD-REC
               MOVE    1               TO  FLG-INPUTCD
           END-IF
      *
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-INPUTCD-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入力セット検索処理
      *****************************************************************
       900-INPUTSET-KEY7-SEL-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-INPUTSET
      *
           INITIALIZE                  INPUTSET-REC
           MOVE    SPA-HOSPNUM     TO  ISET-HOSPNUM
           MOVE    SPA-NAI-SRYCD   TO  ISET-SETCD
           IF    ( SPA-NAI-STYUKYMD    =   SPACE OR ZERO )
            OR   ( SPA-NAI-STYUKYMD   <=   SPA-SYSYMD )
               MOVE    SPA-SYSYMD  TO  ISET-YUKOSTYMD
                                       ISET-YUKOEDYMD
           ELSE
               MOVE    SPA-NAI-STYUKYMD
                                   TO  ISET-YUKOSTYMD
                                       ISET-YUKOEDYMD
           END-IF
           MOVE    INPUTSET-REC    TO  MCPDATA-REC
           MOVE    "tbl_inputset"  TO  MCP-TABLE
           MOVE    "key7"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC TO  INPUTSET-REC
           ELSE
               INITIALIZE              INPUTSET-REC
               MOVE    1           TO  FLG-INPUTSET
           END-IF
      *
           .
       900-INPUTSET-KEY7-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入力セットFETCH処理
      *****************************************************************
       900-INPUTSET-KEY7-FET-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-INPUTSET
      *
           MOVE    "tbl_inputset"  TO  MCP-TABLE
           MOVE    "key7"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC TO  INPUTSET-REC
           ELSE
               INITIALIZE              INPUTSET-REC
               MOVE    1           TO  FLG-INPUTSET
           END-IF
      *
           .
       900-INPUTSET-KEY7-FET-EXT.
           EXIT.
      *****************************************************************
      *    入力コード検索処理
      *****************************************************************
       900-SRYCDCHG-KEY2-SEL-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-SRYCDCHG
      *
           INITIALIZE                  SRYCDCHG-REC
           MOVE    SPACE           TO  SRYCDCHG-REC
           MOVE    SPA-HOSPNUM     TO  CHG-HOSPNUM
           MOVE    ISET-INPUTCD    TO  CHG-IPNSRYCD
      *
           MOVE    SRYCDCHG-REC    TO  MCPDATA-REC
           MOVE    "tbl_srycdchg"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SRYCDCHG-REC
           ELSE
               INITIALIZE              SRYCDCHG-REC
               MOVE    1           TO  FLG-SRYCDCHG
           END-IF
      *
           MOVE    "tbl_srycdchg"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SRYCDCHG-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHは行わない)
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
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

