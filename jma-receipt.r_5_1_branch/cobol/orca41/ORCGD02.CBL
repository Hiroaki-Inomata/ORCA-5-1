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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCGD02.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : データチェック
      *  コンポーネント名  : 患者選択
      *  管理者            : 
      *  作成日付    作業者        記述
      *  07.02.22    太田          新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.00    NACL-太田    07/06/01  グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    データチェック共通パラメタ
           COPY    "D01COMMON-SPA".
      *
      *    画面用ＳＰＡ
       01  SPA-D02.
           03  SPA-GMN-CUR             PIC 9(03).
           03  SPA-GMN-CHKLST-PAGE     PIC 9(05).
           03  SPA-GMN-AREA.
               05  SPA-GMN-LBLSRYYM    PIC X(30).
               05  SPA-GMN-LBLNYUGAI   PIC X(10).
               05  SPA-GMN-TAISYO-G.
                   07  SPA-GMN-TAISYO      PIC X(01).
                   07  SPA-GMN-TAISYOFL    PIC X(01).
                   07  SPA-GMN-TAISYOMEI   PIC X(30).
               05  SPA-GMN-INPUT-AREA.
                   07  SPA-GMN-SELNUM      PIC 9(5).
                   07  SPA-GMN-PTNUM       PIC X(20).
                   07  SPA-GMN-NAME        PIC X(100).
           03  SPA-GMN-CHKLST-AREA.
               05  SPA-GMN-CHKLST-MAX  PIC 9(5).
               05  SPA-GMN-CHKLST-SEL  PIC 9(5).
               05  SPA-GMN-CHKLST-NEXT PIC 9(01).
               05  SPA-GMN-CHKLST-OCC  OCCURS  200.
                   07  SPA-GMN-TNUM    PIC 9(5).
                   07  SPA-GMN-TTESTPT PIC X(1).
                   07  SPA-GMN-TPTNUM  PIC X(20).
                   07  SPA-GMN-TNAME   PIC X(100).
           03  SPA-GMN-CMB-AREA.
               05  SPA-GMN-TAISYOLST-G.
                   07  SPA-GMN-TAISYO-MAX  PIC 9(03).
                   07  SPA-GMN-TAISYOLST   OCCURS 5.
                       09  SPA-GMN-TAISYOL PIC X(01).
                       09  SPA-GMN-TAISYOFL PIC X(01).
                       09  SPA-GMN-TAISYOMEIL
                                           PIC X(30).
           03  SPA-NAI-AREA.
               05  SPA-NAI-IKTKBT          PIC  X(01).
               05  SPA-NAI-SRYYM           PIC  X(06).
               05  SPA-NAI-NYUGAIKBN       PIC  X(01).
               05  SPA-NAI-CHKKBN-G.
                   07  SPA-NAI-CHKKBN      PIC  X(01)  OCCURS 50.
               05  SPA-NAI-SYAHOPROCKBN    PIC  X(01).
               05  SPA-NAI-KOKUHOPROCKBN   PIC  X(01).
               05  SPA-NAI-KOUKIPROCKBN    PIC  X(01).
               05  SPA-NAI-RSIPROCKBN      PIC  X(01).
               05  SPA-NAI-JBIPROCKBN      PIC  X(01).
               05  SPA-NAI-DAISANPROCKBN   PIC  X(01).
               05  SPA-NAI-KGIPROCKBN      PIC  X(01).
               05  SPA-NAI-JIHIPROCKBN     PIC  X(01).
               05  SPA-NAI-CHIKENPROCKBN   PIC  X(01).
               05  SPA-NAI-INGAIPROCKBN    PIC  X(01).
               05  SPA-NAI-PRTSRTKBN       PIC  X(01).
               05  SPA-NAI-SRYYMDKBN       PIC  X(01).
               05  SPA-NAI-STSRYYMD        PIC  9(02).
               05  SPA-NAI-EDSRYYMD        PIC  9(02).
               05  SPA-NAI-INPUT-AREA.
                   07  SPA-NAI-PTID        PIC  9(10).
           03  SPA-NAI-CHKLST-AREA.
               05  SPA-NAI-CHKLST-OCC      OCCURS  200.
                   07  SPA-NAI-TPTID       PIC 9(10).
                   07  SPA-NAI-TKBNCD      PIC X(08).
                   07  SPA-NAI-TSTYUKYMD   PIC X(08).
                   07  SPA-NAI-TEDYUKYMD   PIC X(08).
                   07  SPA-NAI-TCREYMD     PIC X(08).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                     PIC 9(01).
           03  FLG-ERR                     PIC 9(01).
           03  FLG-CHK004                  PIC 9(01).
           03  FLG-PTNUM                   PIC 9(01).
           03  FLG-PTINF                   PIC 9(01).
           03  FLG-HIZUKE-ERR              PIC 9(01).
           03  FLG-CHKLST-ROW              PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-2001                    PIC 9(05).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                        PIC 9(05).
           03  IDX-LST                     PIC 9(05).
           03  IDX-CMB                     PIC 9(05).
      *
      *    作業領域
       01  WRK-AREA.
           03  WRK-CMB-CD                  PIC X(10).
           03  WRK-CMB-ITM-G.
               07  WRK-CMB-ITM             PIC X(200).
           03  WRK-MCP-WIDGET              PIC X(64).
           03  WRK-YMD                     PIC X(10).
           03  WRK-SYMD                    PIC X(08).
           03  WRK-EDTYMD1                 PIC X(10).
           03  WRK-EDTYMD3                 PIC X(22).
           03  WRK-HENYMDG                 PIC X(09).
           03  WRK-DIDMSG                  PIC X(200).
           03  WRK-PTID                    PIC 9(10).
           03  WRK-PTNUM                   PIC X(20).
           03  WRK-DELKBNCD                PIC X(08).
           03  WRK-KBNCD-G.
               05  WRK-KBNCD-KBN           PIC X(01).
               05  WRK-KBNCD-SEQ           PIC 9(07).
           03  WRK-ZZZZ9                   PIC ZZZZ9.
           03  WRK-CHKLST-SEL              PIC 9(05).
           03  WRK-MOTOPG                  PIC X(05).
           03  WRK-SELKBNCD                PIC X(08).
           03  WRK-NAME                    PIC X(100).
      *
       01  WRK-ROW-AREA.
           03  WRK-CHKLST-ROW              PIC S9(9).
      *
       01  CONST-AREA.
           03  CONST-CHKLST-MAX            PIC 9(05)   VALUE   200.
      *
      *    データチェック一括処理パラメータ
           COPY    "CPORCBSD01PARA.INC".
      *
      *    データチェック確認項目
           COPY    "CPDTCHKKOMOKU.INC".
      *
           COPY "COMMON-SPA"       REPLACING //SPA-//
                                   BY        //WKSPA-//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    チェックマスタ０４
           COPY    "CPCHK004.INC".
           COPY    "CPCK2000.INC".
           COPY    "CPCK2001.INC".
      *
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTNUM.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    保険分類取得
           COPY    "CPORCSS001.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
      *
        LINKAGE                     SECTION.
      *
      *     テーブルアクセス
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA41SCRAREA.INC".
      *
       PROCEDURE                       DIVISION    USING
                                                   MCPAREA
                                                   SPAAREA
                                                   LINKAREA
                                                   SCRAREA.
      *****************************************************************
      *    主    処    理
      *****************************************************************
      *
       000-PROC-SEC                    SECTION.
      *
           INITIALIZE                      FLG-AREA
                                           IDX-AREA
                                           WRK-AREA
                                           CNT-AREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA.
           MOVE    SPA-WORK            TO  SPA-D01KYOUTU
           MOVE    SPA-FREE            TO  SPA-D02
      *
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    ZERO                TO  FLG-END
      *
           PERFORM 800-MCNDATE-SEC
      *
           EVALUATE    MCP-STATUS  ALSO    MCP-EVENT
           WHEN    "LINK"          ALSO    ANY
               PERFORM 100-INIT-SEC
      *    画面遷移
           WHEN    "PUTG"          ALSO    "CLICKED"
               PERFORM 200-GMNSENI
      *    入力
           WHEN    OTHER
               PERFORM 200-ENTRY
           END-EVALUATE
      *
      *    画面遷移がない時、画面表示へ
           IF    ( FLG-END             =   ZERO )
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-D01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-D02             TO  SPA-FREE
           .
      *
       000-PROC-SEC-EXT.
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
      *    エラー画面より
           IF    ( SPA-MOTOPG          =   "DERR" )
               MOVE    SPACE           TO  SPA-MOTOPG
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
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "D02    "           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
      *
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
               MOVE    LNK-KYOUTU      TO  SPA-KYOUTU
      *
               EVALUATE    SPA-MOTOPG
               WHEN    "P97"
                   MOVE    LNK-COMMON  TO  SPA-KYOUTU
                   MOVE    LNK-SCRSPA  TO  SPA-D02
                   MOVE    SPA-WORK    TO  SPA-D01KYOUTU
                   PERFORM 330-P97-SET-SEC
                   GO  TO  300-SCREEN-EXT
               END-EVALUATE
      *
           END-IF
      *
           EVALUATE    SPA-MOTOPG
      *    確認画面１
           WHEN    "DID1"
               PERFORM 330-DID1-SET-SEC
               GO  TO  300-SCREEN-EXT
           END-EVALUATE
      *
           PERFORM 310-SPASET-SEC.
      *
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC                  SECTION.
      *
           PERFORM 3101-SPASET-SEC
      *
           PERFORM 3101-EDIT-CKHPTDATA-SEC
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 3101-EDIT-CHKLST-SEC
           END-IF
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       3101-SPASET-SEC                 SECTION.
      *
           INITIALIZE                  SPA-D02
      *
      *    対象コンボ編集処理
           PERFORM 3101-TAISYOLST-HEN-SEC
      *
           MOVE    SPA-D01-IKTKBT          TO  SPA-NAI-IKTKBT
           MOVE    SPA-D01-SRYYM           TO  SPA-NAI-SRYYM
           MOVE    SPA-D01-NYUGAIKBN       TO  SPA-NAI-NYUGAIKBN
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   DTCHKKOMOKU-MAX )
               MOVE    SPA-D01-CHKKBN  (IDX0)
                                           TO  SPA-NAI-CHKKBN  (IDX0)
           END-PERFORM
      *
           MOVE    SPA-D01-SYAHOPROCKBN    TO  SPA-NAI-SYAHOPROCKBN
           MOVE    SPA-D01-KOKUHOPROCKBN   TO  SPA-NAI-KOKUHOPROCKBN
           MOVE    SPA-D01-KOUKIPROCKBN    TO  SPA-NAI-KOUKIPROCKBN
           MOVE    SPA-D01-RSIPROCKBN      TO  SPA-NAI-RSIPROCKBN
           MOVE    SPA-D01-JBIPROCKBN      TO  SPA-NAI-JBIPROCKBN
           MOVE    SPA-D01-DAISANPROCKBN   TO  SPA-NAI-DAISANPROCKBN
           MOVE    SPA-D01-KGIPROCKBN      TO  SPA-NAI-KGIPROCKBN
           MOVE    SPA-D01-JIHIPROCKBN     TO  SPA-NAI-JIHIPROCKBN
           MOVE    SPA-D01-CHIKENPROCKBN   TO  SPA-NAI-CHIKENPROCKBN
           MOVE    SPA-D01-INGAIPROCKBN    TO  SPA-NAI-INGAIPROCKBN
           MOVE    SPA-D01-PRTSRTKBN       TO  SPA-NAI-PRTSRTKBN
           MOVE    SPA-D01-SRYYMDKBN       TO  SPA-NAI-SRYYMDKBN
           MOVE    SPA-D01-STSRYYMD        TO  SPA-NAI-STSRYYMD
           MOVE    SPA-D01-EDSRYYMD        TO  SPA-NAI-EDSRYYMD
      *
           PERFORM 3101-EDIT-LBLCHKLST-SEC
      *
           MOVE    1                       TO  SPA-GMN-CHKLST-PAGE
      *
           .
       3101-SPASET-EXT.
           EXIT.
      *****************************************************************
      *    対象コンボ編集処理
      *****************************************************************
       3101-TAISYOLST-HEN-SEC          SECTION.
      *
           INITIALIZE                      SPA-GMN-TAISYOLST-G
      *
           MOVE    2                   TO  SPA-GMN-TAISYO-MAX
      *
           MOVE    "1"                 TO  SPA-GMN-TAISYOL    (1)
           MOVE    "前回個別指示患者"  TO  SPA-GMN-TAISYOMEIL (1)
      *
           MOVE    "2"                 TO  SPA-GMN-TAISYOL    (2)
           MOVE    "前回チェックエラー患者"
                                       TO  SPA-GMN-TAISYOMEIL (2)
      *
           .
       3101-TAISYOLST-HEN-EXT.
           EXIT.
      *****************************************************************
      *    個別患者リストラベル編集処理
      *****************************************************************
       3101-EDIT-LBLCHKLST-SEC         SECTION.
      *
           MOVE    SPACE               TO  SPA-GMN-LBLSRYYM
                                           SPA-GMN-LBLNYUGAI
      *
           MOVE    SPA-NAI-SRYYM       TO  WRK-YMD
           MOVE    "01"                TO  WRK-YMD(7:2)
           PERFORM 800-SEIWA-HEN-SEC
           STRING  WRK-EDTYMD3 (1:16)  DELIMITED   BY  SIZE
                   "診療分"            DELIMITED   BY  SIZE
           INTO    SPA-GMN-LBLSRYYM
           END-STRING
      *
           IF    ( SPA-NAI-NYUGAIKBN   =   "1" )
               MOVE    "入院"          TO  SPA-GMN-LBLNYUGAI
           ELSE
               MOVE    "外来"          TO  SPA-GMN-LBLNYUGAI
           END-IF
      *
           .
       3101-EDIT-LBLCHKLST-EXT.
           EXIT.
      *****************************************************************
      *    個別患者情報編集処理
      *****************************************************************
       3101-EDIT-CKHPTDATA-SEC          SECTION.
      *
           MOVE    "3%"            TO   WRK-DELKBNCD
           PERFORM 410011-DEL-CK2001-SEC
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 31011-EDIT-CKHPTDATA-SEC
           END-IF
      *
           .
       3101-EDIT--CKHPTDATA-EXT.
           EXIT.
      *****************************************************************
      *    個別患者情報編集処理
      *****************************************************************
       31011-EDIT-CKHPTDATA-SEC         SECTION.
      *
           INITIALIZE                  CHK004-REC
           MOVE    SPA-HOSPNUM     TO  CHK004-HOSPNUM
           MOVE    "2001"          TO  CHK004-KANRICD
           MOVE    "2%"            TO  CHK004-KBNCD
           MOVE    SPA-SYSYMD      TO  CHK004-STYUKYMD
                                       CHK004-EDYUKYMD
           PERFORM 900-CHK004-KEY7-SEL-SEC
           MOVE    CHK004-REC      TO  CK4-2001-REC
      *
           PERFORM UNTIL ( FLG-CHK004  NOT =   ZERO )
                     OR  ( SPA-ERRCD   NOT =   SPACE )
      *
               MOVE    "3"          TO CK4-2001-KBNCD (1:1)
               MOVE    SMCNDATE-YMD TO CK4-2001-CREYMD
                                       CK4-2001-UPYMD
               MOVE    SMCNDATE-HMS TO CK4-2001-UPHMS
      *
               MOVE    CK4-2001-REC TO MCPDATA-REC
               MOVE    "tbl_chk004" TO MCP-TABLE
               MOVE    "key"       TO  MCP-PATHNAME
               PERFORM 910-DBINSERT-SEC
               IF    ( MCP-RC  NOT =   ZERO )
                   MOVE    "4001"  TO  SPA-ERRCD
               END-IF
      *
               PERFORM 900-CHK004-KEY7-FET-SEC
               MOVE    CHK004-REC  TO  CK4-2001-REC
      *
           END-PERFORM
      *
           MOVE    "tbl_chk004"    TO  MCP-TABLE
           MOVE    "key7"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       31011-EDIT-CKHPTDATA-EXT.
           EXIT.
      *****************************************************************
      *    個別患者リスト編集処理
      *****************************************************************
       3101-EDIT-CHKLST-SEC            SECTION.
      *
           MOVE    1               TO  FLG-CHKLST-ROW
      *
           INITIALIZE                  SPA-GMN-CHKLST-AREA
                                       SPA-NAI-CHKLST-AREA
      *
           INITIALIZE                  CHK004-REC
           MOVE    SPA-HOSPNUM     TO  CHK004-HOSPNUM
           MOVE    "2001"          TO  CHK004-KANRICD
           MOVE    "3%"            TO  CHK004-KBNCD
           MOVE    SPA-SYSYMD      TO  CHK004-STYUKYMD
                                       CHK004-EDYUKYMD
           COMPUTE CHK004-SELLIMIT
               =   CONST-CHKLST-MAX    +   1
           COMPUTE CHK004-SELOFFSET
               = ( SPA-GMN-CHKLST-PAGE -   1 )
               *   CONST-CHKLST-MAX
      *
           PERFORM 900-CHK004-KEY4-SEL-SEC
           MOVE    CHK004-REC      TO  CK4-2001-REC
      *
           PERFORM UNTIL ( FLG-CHK004  NOT =   ZERO )
                    OR   ( SPA-GMN-CHKLST-MAX
                                   >=  CONST-CHKLST-MAX )
      *
               COMPUTE SPA-GMN-CHKLST-MAX
                   =   SPA-GMN-CHKLST-MAX
                   +   1
      *
               MOVE    SPA-GMN-CHKLST-MAX  TO  IDX-LST
      *
               MOVE    CK4-2001-PTID   TO  WRK-PTID
               PERFORM 900-PTINF-KEY-SEL-SEC
               PERFORM 900-PTNUM-KEY-SEL-SEC
      *
               COMPUTE SPA-GMN-TNUM(IDX-LST)
                   = ( SPA-GMN-CHKLST-PAGE -   1 )
                   *   CONST-CHKLST-MAX
                   +   IDX-LST
      *
               MOVE    PTINF-TSTPTNUMKBN
                                       TO  SPA-GMN-TTESTPT(IDX-LST)
      *
               MOVE    CK4-2001-PTID   TO  SPA-NAI-TPTID  (IDX-LST)
      *
               MOVE    CK4-2001-KBNCD  TO  SPA-NAI-TKBNCD (IDX-LST)
      *
               MOVE    CK4-2001-STYUKYMD TO SPA-NAI-TSTYUKYMD(IDX-LST)
      *
               MOVE    CK4-2001-EDYUKYMD TO SPA-NAI-TEDYUKYMD(IDX-LST)
      *
               MOVE    CK4-2001-CREYMD TO SPA-NAI-TCREYMD (IDX-LST)
      *
               MOVE    PTNUM-PTNUM     TO  SPA-GMN-TPTNUM (IDX-LST)
      *
               MOVE    PTINF-NAME      TO  SPA-GMN-TNAME  (IDX-LST)
      *
               PERFORM 900-CHK004-KEY4-FET-SEC
               MOVE    CHK004-REC      TO  CK4-2001-REC
      *
           END-PERFORM
      *
           MOVE    "tbl_chk004"    TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( FLG-CHK004      =   ZERO )
               MOVE    1           TO  SPA-GMN-CHKLST-NEXT
           END-IF
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           .
       3101-EDIT-CHKLST-EXT.
           EXIT.
      *****************************************************************
      *    氏名検索より戻り時の処理
      *****************************************************************
       330-P97-SET-SEC             SECTION.
      *
           MOVE    LNK-KYOUTU      TO  WKSPA-KYOUTU
      *
           IF    ( WKSPA-PTID    =   ZERO )
             OR  ( WKSPA-PTID    =   SPA-NAI-PTID )
               MOVE    2           TO  SPA-GMN-CUR
           ELSE
               MOVE    WKSPA-PTNUM TO  D02-PTNUM
               PERFORM 4100-PTNUM-CHK-SEC
               MOVE    SPACE       TO  SPA-ERRCD
               MOVE    2           TO  SPA-GMN-CUR
           END-IF
      *
           .
       330-P97-SET-EXT.
           EXIT.
      *****************************************************************
      *    確認画面（ＤＩＤ１）ＯＫ処理
      *****************************************************************
       330-DID1-SET-SEC            SECTION.
      *
      *    IF    ( SPA-DID1-FLG    =   "OK" )
      *        EVALUATE    SPA-DIDCD
      *        WHEN    "6666"
      *            MOVE    "JOIN"      TO  MCP-PUTTYPE
      *            PERFORM 210-BACK-SYORI-SEC
      *        WHEN    OTHER
      *        END-EVALUATE
      *    END-IF
      *
           MOVE    SPACE               TO  SPA-DID1-FLG
           MOVE    SPACE               TO  SPA-DIDCD
      *
           .
       330-DID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                     SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
           WHEN    "CLICKED"       ALSO    "B01"
      *        戻る
               MOVE    SPACE           TO  SPA-DIDCD
               MOVE   "CHANGE"         TO  MCP-PUTTYPE
               PERFORM 210-BACK
           WHEN    "CLICKED"       ALSO    "B02"
               PERFORM 220-CLEAR-SEC
           WHEN    "CLICKED"       ALSO    "B04"
               PERFORM 240-DEL-SELPT-SEC
           WHEN    "CLICKED"       ALSO    "B05"
               MOVE    3           TO  SPA-GMN-CUR
               PERFORM 260-PREVPAGE-SEC
           WHEN    "CLICKED"       ALSO    "B06"
               PERFORM 260-PREVPAGE-SEC
           WHEN    "CLICKED"       ALSO    "B07"
               PERFORM 270-NEXTPAGE-SEC
           WHEN    "CLICKED"       ALSO    "B09"
               MOVE    SPACE       TO      WRK-NAME
               PERFORM 490-KENSAKU-SEC
           WHEN    "CLICKED"       ALSO    "B12"
               PERFORM 412-KAKUTEI-SEC
           END-EVALUATE.
      *
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
           WHEN    "ACTIVATE"      ALSO    "TAISYO"
               PERFORM 4100-TAISYO-CHK-SEC
           WHEN    "ACTIVATE"      ALSO    "SELNUM"
               PERFORM 4100-SELNUM-CHK-SEC
           WHEN    "ACTIVATE"      ALSO    "PTNUM"
               PERFORM 4100-PTNUM-CHK-SEC
           WHEN    ANY             ALSO    "CHKLST"
               PERFORM 4100-CHKLST-CHK-SEC
           WHEN    OTHER
      *        入力個所のセット
               PERFORM 400-GMN-INPUT-SEC
      *        入力チェック処理
               PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
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
           IF    ( SPA-ERRCD   NOT =   SPACE )
             OR  ( FLG-END     NOT =   ZERO  )
               GO  TO  410-INPUT-CHK-EXT
           END-IF
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
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-TAISYO-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-SELNUM-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-PTNUM-CHK-SEC
           END-IF
      *
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    対象チェック処理
      *****************************************************************
       4100-TAISYO-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           MOVE    D02-TAISYO          TO  SPA-GMN-TAISYO-G
      *
           PERFORM 4101-TAISYO-CHK-SEC
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-GMN-TAISYO  =   SPACE )
                   CONTINUE
               ELSE
                   INITIALIZE              SPA-GMN-INPUT-AREA
                                           SPA-NAI-INPUT-AREA
                   EVALUATE  SPA-GMN-TAISYO
                   WHEN    "1"
                       MOVE    "1"         TO  WRK-SELKBNCD
                   WHEN    "2"
                       MOVE    "4"         TO  WRK-SELKBNCD
                   END-EVALUATE
      *
                   INITIALIZE                  CHK004-REC
                   MOVE    SPA-HOSPNUM     TO  CHK004-HOSPNUM
                   MOVE    "2000"          TO  CHK004-KANRICD
                   MOVE    WRK-SELKBNCD    TO  CHK004-KBNCD
                   MOVE    SPA-SYSYMD      TO  CHK004-STYUKYMD
                                               CHK004-EDYUKYMD
                   PERFORM 900-CHK004-KEY-SEL-SEC
                   IF    ( FLG-CHK004      =   ZERO )
                       PERFORM 41001-EDIT-PTDATA-SEC
                   ELSE
                       MOVE    "0006"      TO  SPA-ERRCD
                       MOVE    3           TO  SPA-GMN-CUR
                   END-IF
               END-IF
      *
           END-IF
      *
           .
      *
       4100-TAISYO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号チェック処理
      *****************************************************************
       4100-SELNUM-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           MOVE    ZERO                TO  SPA-GMN-CHKLST-SEL
           INITIALIZE                      SPA-GMN-INPUT-AREA
                                           SPA-NAI-INPUT-AREA
      *
           MOVE    D02-SELNUM          TO  SPA-GMN-SELNUM
      *
           PERFORM 4101-SELNUM-CHK-SEC
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( WRK-CHKLST-SEL  >   ZERO )
                   MOVE    WRK-CHKLST-SEL  TO  SPA-GMN-CHKLST-SEL
                   PERFORM 41011-EDIT-INPUT-AREA-SEC
               END-IF
           END-IF
      *
           .
      *
       4100-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者番号チェック処理
      *****************************************************************
       4100-PTNUM-CHK-SEC              SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           MOVE    SPACE               TO  SPA-GMN-NAME
           MOVE    ZERO                TO  SPA-NAI-PTID
      *
           MOVE    D02-PTNUM           TO  SPA-GMN-PTNUM
      *
           PERFORM 4101-PTNUM-CHK-SEC
           IF    ( SPA-ERRCD           =   SPACE )
            AND  ( SPA-GMN-PTNUM  NOT  =   SPACE )
            AND  ( FLG-END             =   ZERO  )
               IF    ( SPA-GMN-CHKLST-SEL  =   ZERO )
      *
                   INITIALIZE              WRK-KBNCD-G
      *
                   INITIALIZE              CHK004-REC
                   MOVE    SPA-HOSPNUM TO  CHK004-HOSPNUM
                   MOVE    "2001"      TO  CHK004-KANRICD
                   MOVE    "3%"        TO  CHK004-KBNCD
                   MOVE    SPA-SYSYMD  TO  CHK004-STYUKYMD
                                           CHK004-EDYUKYMD
                   PERFORM 900-CHK004-KEY6-SEL-SEC
                   IF    ( FLG-CHK004  =   ZERO )
                       MOVE    CHK004-KBNCD
                                       TO  WRK-KBNCD-G
                   END-IF
      *
                   MOVE    "3"         TO  WRK-KBNCD-KBN
      *
                   COMPUTE WRK-KBNCD-SEQ
                       =   WRK-KBNCD-SEQ   +   1
      *
                   INITIALIZE              CHK004-REC
                   MOVE    SPA-HOSPNUM TO  CHK004-HOSPNUM
                   MOVE    "2001"      TO  CHK004-KANRICD
                   MOVE    WRK-KBNCD-G TO  CHK004-KBNCD
                   MOVE    "00000000"  TO  CHK004-STYUKYMD
                   MOVE    "99999999"  TO  CHK004-EDYUKYMD
                   MOVE    SPA-NAI-PTID
                                       TO  CHK004-PTID
                   MOVE    SMCNDATE-YMD TO CHK004-CREYMD
                                           CHK004-UPYMD
                   MOVE    SMCNDATE-HMS TO CHK004-UPHMS
                   MOVE    CHK004-REC  TO  MCPDATA-REC
                   MOVE    "tbl_chk004" TO MCP-TABLE
                   MOVE    "key"       TO  MCP-PATHNAME
                   PERFORM 910-DBINSERT-SEC
                   IF    ( MCP-RC      NOT =   ZERO )
                       MOVE    "4002"  TO  SPA-ERRCD
                   END-IF
      *
                   IF    ( SPA-ERRCD           =   SPACE )
                       INITIALIZE          SPA-GMN-INPUT-AREA
                                           SPA-NAI-INPUT-AREA
                       PERFORM 3101-EDIT-CHKLST-SEC
                       MOVE    2       TO  SPA-GMN-CUR
                   END-IF
      *
               ELSE
                   MOVE    SPA-GMN-CHKLST-SEL
                                       TO  IDX-LST
      *
                   INITIALIZE              CHK004-REC
                   MOVE    SPA-HOSPNUM TO  CHK004-HOSPNUM
                   MOVE    "2001"      TO  CHK004-KANRICD
                   MOVE    SPA-NAI-TKBNCD (IDX-LST)
                                       TO  CHK004-KBNCD
                   MOVE    SPA-NAI-TSTYUKYMD (IDX-LST)
                                       TO  CHK004-STYUKYMD
                   MOVE    SPA-NAI-TEDYUKYMD (IDX-LST)
                                       TO  CHK004-EDYUKYMD
                   MOVE    SPA-NAI-PTID
                                       TO  CHK004-PTID
                   MOVE    SPA-NAI-TCREYMD (IDX-LST)
                                       TO  CHK004-CREYMD
                   MOVE    SMCNDATE-YMD TO CHK004-UPYMD
                   MOVE    SMCNDATE-HMS TO CHK004-UPHMS
                   MOVE    CHK004-REC  TO  MCPDATA-REC
                   MOVE    "tbl_chk004" TO MCP-TABLE
                   MOVE    "key"       TO  MCP-PATHNAME
                   PERFORM 910-DBUPDATE-SEC
                   IF    ( MCP-RC      NOT =   ZERO )
                       MOVE    "4003"  TO  SPA-ERRCD
                   END-IF
      *
                   IF    ( SPA-ERRCD           =   SPACE )
                       INITIALIZE          SPA-GMN-INPUT-AREA
                                           SPA-NAI-INPUT-AREA
                       PERFORM 3101-EDIT-CHKLST-SEC
                   END-IF
      *
               END-IF
           END-IF
      *
           .
      *
       4100-PTNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    個別患者リストチェック処理
      *****************************************************************
       4100-CHKLST-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-CHKLST-SEL
                                           SPA-GMN-SELNUM
           INITIALIZE                      SPA-GMN-INPUT-AREA
                                           SPA-NAI-INPUT-AREA
      *
           PERFORM VARYING IDX-LST FROM    1   BY  1
                   UNTIL ( IDX-LST >   SPA-GMN-CHKLST-MAX )
                    OR   ( SPA-GMN-CHKLST-SEL  >   ZERO )
      *
               IF    ( D02-CHKLST-SELECT (IDX-LST) =   "T")
                   MOVE    IDX-LST     TO  SPA-GMN-CHKLST-SEL
                   MOVE    SPA-GMN-TNUM (IDX-LST)
                                       TO  SPA-GMN-SELNUM
               END-IF
      *
           END-PERFORM
      *
           IF    ( SPA-GMN-CHKLST-SEL  >   ZERO )
               PERFORM 41011-EDIT-INPUT-AREA-SEC
           END-IF
      *
           .
      *
       4100-CHKLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    対象チェック処理
      *****************************************************************
       4101-TAISYO-CHK-SEC             SECTION.
      *
           MOVE    SPA-GMN-TAISYO  TO  WRK-CMB-CD
           PERFORM 4200-TAISYOLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-TAISYO-G
      *
           .
      *
       4101-TAISYO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号チェック処理
      *****************************************************************
       4101-SELNUM-CHK-SEC             SECTION.
      *
           IF    ( SPA-GMN-SELNUM  >   ZERO )
               MOVE    ZERO            TO  WRK-CHKLST-SEL
               PERFORM VARYING IDX-LST     FROM    1   BY  1
                       UNTIL ( IDX-LST     >   SPA-GMN-CHKLST-MAX )
                        OR   ( WRK-CHKLST-SEL  >   ZERO )
      *
                   IF    ( SPA-GMN-SELNUM  =   SPA-GMN-TNUM (IDX-LST))
                       MOVE    IDX-LST
                                       TO  WRK-CHKLST-SEL
                   END-IF
      *
               END-PERFORM
      *
               IF    ( WRK-CHKLST-SEL  =   ZERO )
                   MOVE    "0001"      TO  SPA-ERRCD
                   MOVE    1           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
      *
       4101-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者番号チェック処理
      *****************************************************************
       4101-PTNUM-CHK-SEC              SECTION.
      *
           IF    ( SPA-GMN-PTNUM       =   SPACE )
            AND  ( SPA-GMN-CHKLST-SEL  =   ZERO )
               GO  TO  4101-PTNUM-CHK-EXT
           END-IF
      *
           INITIALIZE                  ORCSPTNUMAREA
           MOVE    SPA-GMN-PTNUM   TO  SPTNUM-PTNUM
           CALL    "ORCSPTNUM"     USING   ORCSPTNUMAREA
                                           SPA-AREA
           MOVE    SPTNUM-PTNUM    TO  SPA-GMN-PTNUM
                                       WRK-PTNUM
           IF    ( SPTNUM-RC       NOT =   ZERO )
               IF    ( SPTNUM-RC       =   20 )
                   MOVE    SPA-GMN-PTNUM   TO  WRK-NAME
                   PERFORM 490-KENSAKU-SEC
                   GO  TO  4101-PTNUM-CHK-EXT
               ELSE
                   MOVE    "0002"      TO  SPA-ERRCD
                   MOVE    2           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    SPTNUM-PTID     TO  SPA-NAI-PTID
      *
               MOVE    SPA-NAI-PTID    TO  WRK-PTID
               PERFORM 900-PTINF-KEY-SEL-SEC
               MOVE    PTINF-NAME      TO  SPA-GMN-NAME
      *
               INITIALIZE                  SS001-AREA
               MOVE    SPA-HOSPNUM      TO  SS001-HOSPNUM
               MOVE    SPA-NAI-PTID    TO  SS001-PTID
               MOVE    SPA-NAI-SRYYM   TO  SS001-SRYYM
               CALL    "ORCSS001"          USING   SS001-AREA
                                                   SPA-AREA
               IF    ( SPA-NAI-NYUGAIKBN   =   "1" )
                   IF    ( SS001-NIHO-FLG  =   1 )
                    OR   ( SS001-NRSI-FLG  =   1 )
                    OR   ( SS001-NJBI-FLG  =   1 )
                    OR   ( SS001-NKGI-FLG  =   1 )
                       CONTINUE
                   ELSE
                       MOVE    "0003"  TO  SPA-ERRCD
                       MOVE    2       TO  SPA-GMN-CUR
                   END-IF
               ELSE
                   IF    ( SS001-GIHO-FLG  =   1 )
                    OR   ( SS001-GRSI-FLG  =   1 )
                    OR   ( SS001-GJBI-FLG  =   1 )
                    OR   ( SS001-GKGI-FLG  =   1 )
                       CONTINUE
                   ELSE
                       MOVE    "0003"  TO  SPA-ERRCD
                       MOVE    2       TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               INITIALIZE              CHK004-REC
               MOVE    SPA-HOSPNUM TO  CHK004-HOSPNUM
               MOVE    "2001"      TO  CHK004-KANRICD
               MOVE    "3%"        TO  CHK004-KBNCD
               IF    ( SPA-GMN-CHKLST-SEL  =   ZERO )
                   MOVE    "30000000"
                                   TO  CHK004-UPKBNCD
               ELSE
                   MOVE    SPA-NAI-TKBNCD(SPA-GMN-CHKLST-SEL)
                                   TO  CHK004-UPKBNCD
               END-IF
               MOVE    SPA-SYSYMD  TO  CHK004-STYUKYMD
                                       CHK004-EDYUKYMD
               MOVE    SPA-NAI-PTID TO CHK004-PTID
               PERFORM 900-CHK004-KEY5-SEL-SEC
               IF    ( FLG-CHK004  =   ZERO )
                       MOVE    "0004"  TO  SPA-ERRCD
                       MOVE    2       TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
      *
       4101-PTNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入力領域編集処理
      *****************************************************************
       41011-EDIT-INPUT-AREA-SEC       SECTION.
      *
           INITIALIZE                      SPA-GMN-INPUT-AREA
                                           SPA-NAI-INPUT-AREA
      *
           MOVE    SPA-GMN-CHKLST-SEL  TO  IDX-LST
      *
           MOVE    SPA-GMN-TNUM (IDX-LST)
                                       TO  SPA-GMN-SELNUM
      *
           MOVE    SPA-GMN-TPTNUM (IDX-LST)
                                       TO  SPA-GMN-PTNUM
      *
           MOVE    SPA-GMN-TNAME  (IDX-LST)
                                       TO  SPA-GMN-NAME
      *
           MOVE    SPA-NAI-TPTID  (IDX-LST)
                                       TO  SPA-NAI-PTID
      *
           MOVE    2                   TO  SPA-GMN-CUR
      *
           .
      *
       41011-EDIT-INPUT-AREA-EXT.
           EXIT.
      *****************************************************************
      *    他情報より個別患者リスト編集処理
      *****************************************************************
       41001-EDIT-PTDATA-SEC            SECTION.
      *
           MOVE    "3%"            TO   WRK-DELKBNCD
           PERFORM 410011-DEL-CK2001-SEC
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 410011-EDIT-PTDATA-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    1           TO  SPA-GMN-CHKLST-PAGE
               PERFORM 3101-EDIT-CHKLST-SEC
           END-IF
      *
           .
      *
       41001-EDIT-ERRPT-EXT.
           EXIT.
      *****************************************************************
      *    個別処理患者情報削除処理
      *****************************************************************
       410011-DEL-CK2001-SEC            SECTION.
      *
           INITIALIZE                  CHK004-REC
           MOVE    SPA-HOSPNUM     TO  CHK004-HOSPNUM
           MOVE    "2001"          TO  CHK004-KANRICD
           MOVE    WRK-DELKBNCD    TO  CHK004-KBNCD
           MOVE    SPA-SYSYMD      TO  CHK004-STYUKYMD
                                       CHK004-EDYUKYMD
           MOVE    CHK004-REC      TO  MCPDATA-REC
           MOVE    "tbl_chk004"    TO  MCP-TABLE
           MOVE    "del1"          TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               MOVE    "4004"      TO  SPA-ERRCD
           END-IF
      *
           .
      *
       410011-DEL-CK2001-EXT.
           EXIT.
      *****************************************************************
      *    他情報より個別患者リスト編集処理
      *****************************************************************
       410011-EDIT-PTDATA-SEC          SECTION.
      *
           INITIALIZE                  CHK004-REC
           MOVE    SPA-HOSPNUM     TO  CHK004-HOSPNUM
           MOVE    "2000"          TO  CHK004-KANRICD
           MOVE    WRK-SELKBNCD    TO  CHK004-KBNCD
           MOVE    SPA-SYSYMD      TO  CHK004-STYUKYMD
                                       CHK004-EDYUKYMD
           PERFORM 900-CHK004-KEY-SEL-SEC
           MOVE    CHK004-REC      TO  CK4-2000-REC
           MOVE    CK4-2000-TBL    TO  ORCBSD01PARA
           MOVE    ORCBSD01-SRYYM  TO  SPA-NAI-SRYYM
           MOVE    ORCBSD01-NYUGAIKBN
                                   TO  SPA-NAI-NYUGAIKBN
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   DTCHKKOMOKU-MAX )
               MOVE    ORCBSD01-CHKKBN (IDX0)
                                   TO  SPA-NAI-CHKKBN (IDX0)
           END-PERFORM
      *
           MOVE    ORCBSD01-SYAHOPROCKBN
                                   TO  SPA-NAI-SYAHOPROCKBN
           MOVE    ORCBSD01-KOKUHOPROCKBN
                                   TO  SPA-NAI-KOKUHOPROCKBN
           MOVE    ORCBSD01-KOUKIPROCKBN
                                   TO  SPA-NAI-KOUKIPROCKBN
           MOVE    ORCBSD01-RSIPROCKBN
                                   TO  SPA-NAI-RSIPROCKBN
           MOVE    ORCBSD01-JBIPROCKBN
                                   TO  SPA-NAI-JBIPROCKBN
           MOVE    ORCBSD01-DAISANPROCKBN
                                   TO  SPA-NAI-DAISANPROCKBN
           MOVE    ORCBSD01-KGIPROCKBN
                                   TO  SPA-NAI-KGIPROCKBN
           MOVE    ORCBSD01-JIHIPROCKBN
                                   TO  SPA-NAI-JIHIPROCKBN
           MOVE    ORCBSD01-CHIKENPROCKBN
                                   TO  SPA-NAI-CHIKENPROCKBN
           MOVE    ORCBSD01-INGAIPROCKBN
                                   TO  SPA-NAI-INGAIPROCKBN
           MOVE    ORCBSD01-PRTSRTKBN
                                   TO  SPA-NAI-PRTSRTKBN
           MOVE    ORCBSD01-SRYYMDKBN
                                   TO  SPA-NAI-SRYYMDKBN
           MOVE    ORCBSD01-STSRYYMD
                                   TO  SPA-NAI-STSRYYMD
           MOVE    ORCBSD01-EDSRYYMD
                                   TO  SPA-NAI-EDSRYYMD
      *
           PERFORM 3101-EDIT-LBLCHKLST-SEC
      *
           INITIALIZE                  CHK004-REC
           MOVE    SPA-HOSPNUM     TO  CHK004-HOSPNUM
           MOVE    "2001"          TO  CHK004-KANRICD
           MOVE    WRK-SELKBNCD    TO  CHK004-KBNCD
           MOVE    "%"             TO  CHK004-KBNCD (2:1)
           MOVE    SPA-SYSYMD      TO  CHK004-STYUKYMD
                                       CHK004-EDYUKYMD
           PERFORM 900-CHK004-KEY7-SEL-SEC
           MOVE    CHK004-REC      TO  CK4-2001-REC
      *
           PERFORM UNTIL ( FLG-CHK004  NOT =   ZERO )
                     OR  ( SPA-ERRCD   NOT =   SPACE )
      *
               MOVE    "3"          TO CK4-2001-KBNCD (1:1)
               MOVE    SMCNDATE-YMD TO CK4-2001-CREYMD
                                       CK4-2001-UPYMD
               MOVE    SMCNDATE-HMS TO CK4-2001-UPHMS
      *
               MOVE    CK4-2001-REC TO MCPDATA-REC
               MOVE    "tbl_chk004" TO MCP-TABLE
               MOVE    "key"       TO  MCP-PATHNAME
               PERFORM 910-DBINSERT-SEC
               IF    ( MCP-RC  NOT =   ZERO )
                   MOVE    "4005"  TO  SPA-ERRCD
               END-IF
      *
               PERFORM 900-CHK004-KEY7-FET-SEC
               MOVE    CHK004-REC  TO  CK4-2001-REC
      *
           END-PERFORM
      *
           MOVE    "tbl_chk004"    TO  MCP-TABLE
           MOVE    "key7"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       410011-EDIT-PTDATA-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "D01"               TO  SPA-SAKIPG
           MOVE    "D02"               TO  SPA-MOTOPG
      *
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
      *
       210-BACK-EXT.
           EXIT.
      *****************************************************************
      *    クリア処理
      *****************************************************************
       220-CLEAR-SEC                   SECTION.
      *
           IF    ( SPA-GMN-SELNUM  NOT =   ZERO )
            OR   ( SPA-GMN-PTNUM   NOT =   SPACE )
               INITIALIZE              SPA-GMN-INPUT-AREA
                                       SPA-NAI-INPUT-AREA
                                       SPA-GMN-CHKLST-SEL
               MOVE    2               TO   SPA-GMN-CUR
           ELSE
               PERFORM 3101-SPASET-SEC
      *
               MOVE    "3%"            TO   WRK-DELKBNCD
               PERFORM 410011-DEL-CK2001-SEC
               IF    ( SPA-ERRCD       =   SPACE )
                   PERFORM 3101-EDIT-CHKLST-SEC
               END-IF
           END-IF
      *
           .
      *
       220-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    削除処理
      *****************************************************************
       240-DEL-SELPT-SEC               SECTION.
      *
           IF    ( SPA-GMN-CHKLST-SEL  =   ZERO )
               MOVE    "0005"      TO  SPA-ERRCD
           ELSE
      *
               MOVE    SPA-GMN-CHKLST-SEL
                                   TO  IDX-LST
      *
               INITIALIZE          CHK004-REC
               MOVE    SPA-HOSPNUM TO  CHK004-HOSPNUM
               MOVE    "2001"      TO  CHK004-KANRICD
               MOVE    SPA-NAI-TKBNCD (IDX-LST)
                                   TO  CHK004-KBNCD
               MOVE    SPA-NAI-TSTYUKYMD (IDX-LST)
                                   TO  CHK004-STYUKYMD
               MOVE    SPA-NAI-TEDYUKYMD (IDX-LST)
                                   TO  CHK004-EDYUKYMD
               MOVE    CHK004-REC  TO  MCPDATA-REC
               MOVE    "tbl_chk004" TO MCP-TABLE
               MOVE    "key"       TO  MCP-PATHNAME
               PERFORM 910-DBDELETE-SEC
               IF    ( MCP-RC      =   ZERO )
                   INITIALIZE          SPA-GMN-INPUT-AREA
                                       SPA-NAI-INPUT-AREA
                   PERFORM 3101-EDIT-CHKLST-SEC
               ELSE
                   MOVE    "4006"  TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
      *
       240-DEL-SELPT-EXT.
           EXIT.
      *****************************************************************
      *    前頁処理
      *****************************************************************
       260-PREVPAGE-SEC                SECTION.
      *
           IF    ( SPA-GMN-CHKLST-PAGE >   1 )
      *
               INITIALIZE              SPA-GMN-INPUT-AREA
                                       SPA-NAI-INPUT-AREA
      *
               COMPUTE SPA-GMN-CHKLST-PAGE
                   =   SPA-GMN-CHKLST-PAGE     -   1
      *
               PERFORM 3101-EDIT-CHKLST-SEC
           END-IF
      *
           .
       260-PREVPAGE-EXT.
           EXIT.
      *****************************************************************
      *    次頁処理
      *****************************************************************
       270-NEXTPAGE-SEC                SECTION.
      *
           IF    ( SPA-GMN-CHKLST-NEXT  =   1 )
      *
               INITIALIZE              SPA-GMN-INPUT-AREA
                                       SPA-NAI-INPUT-AREA
      *
               COMPUTE SPA-GMN-CHKLST-PAGE
                   =   SPA-GMN-CHKLST-PAGE     +   1
      *
               PERFORM 3101-EDIT-CHKLST-SEC
           END-IF
      *
           .
       270-NEXTPAGE-EXT.
           EXIT.
      *****************************************************************
      *    氏名検索へ
      *****************************************************************
       490-KENSAKU-SEC             SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-D02             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "D02"               TO  SPA-MOTOPG
           MOVE    "P97"               TO  SPA-SAKIPG
      *
           MOVE    WRK-NAME            TO  SPA-NAME
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "P97"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       490-KENSAKU-EXT.
           EXIT.
      *****************************************************************
      *    確定
      *****************************************************************
       412-KAKUTEI-SEC                 SECTION.
      *
      *    入力チェック
           PERFORM 410-INPUT-CHK-SEC
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4900-KANRENCHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM  4121-KAKUTEI-SEC
           END-IF
      *
           .
      *
       412-KAKUTEI-EXT.
           EXIT.
      *****************************************************************
      *    関連チェック処理
      *****************************************************************
       4900-KANRENCHK-SEC              SECTION.
      *
           IF    ( SPA-GMN-SELNUM  NOT =   ZERO )
            OR   ( SPA-GMN-PTNUM   NOT =   SPACE )
               MOVE    "1001"      TO  SPA-ERRCD
               MOVE    2           TO  SPA-GMN-CUR
           END-IF
      *
           .
       4900-KANRENCHK-EXT.
           EXIT.
      *****************************************************************
      *    確定処理
      *****************************************************************
       4121-KAKUTEI-SEC                SECTION.
      *
           INITIALIZE              SPA-D01-INF-AREA
      *
           INITIALIZE                  CHK004-REC
           MOVE    SPA-HOSPNUM     TO  CHK004-HOSPNUM
           MOVE    "2001"          TO  CHK004-KANRICD
           MOVE    "2%"            TO  CHK004-KBNCD
           MOVE    SPA-SYSYMD      TO  CHK004-STYUKYMD
                                       CHK004-EDYUKYMD
           MOVE    CHK004-REC      TO  MCPDATA-REC
           MOVE    "tbl_chk004"    TO  MCP-TABLE
           MOVE    "del1"          TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               MOVE    "4007"      TO  SPA-ERRCD
               GO  TO  4121-KAKUTEI-EXT
           END-IF
      *
           MOVE    SPA-NAI-SRYYM   TO  SPA-D01-SRYYM
           MOVE    SPA-NAI-NYUGAIKBN
                                   TO  SPA-D01-NYUGAIKBN
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   DTCHKKOMOKU-MAX )
               MOVE    SPA-NAI-CHKKBN (IDX0)
                                   TO  SPA-D01-CHKKBN (IDX0)
           END-PERFORM
           MOVE    SPA-NAI-SYAHOPROCKBN    TO  SPA-D01-SYAHOPROCKBN
           MOVE    SPA-NAI-KOKUHOPROCKBN   TO  SPA-D01-KOKUHOPROCKBN
           MOVE    SPA-NAI-KOUKIPROCKBN    TO  SPA-D01-KOUKIPROCKBN
           MOVE    SPA-NAI-RSIPROCKBN      TO  SPA-D01-RSIPROCKBN
           MOVE    SPA-NAI-JBIPROCKBN      TO  SPA-D01-JBIPROCKBN
           MOVE    SPA-NAI-DAISANPROCKBN   TO  SPA-D01-DAISANPROCKBN
           MOVE    SPA-NAI-KGIPROCKBN      TO  SPA-D01-KGIPROCKBN
           MOVE    SPA-NAI-JIHIPROCKBN     TO  SPA-D01-JIHIPROCKBN
           MOVE    SPA-NAI-CHIKENPROCKBN   TO  SPA-D01-CHIKENPROCKBN
           MOVE    SPA-NAI-INGAIPROCKBN    TO  SPA-D01-INGAIPROCKBN
           MOVE    SPA-NAI-PRTSRTKBN       TO  SPA-D01-PRTSRTKBN
           MOVE    SPA-NAI-SRYYMDKBN       TO  SPA-D01-SRYYMDKBN
           MOVE    SPA-NAI-STSRYYMD        TO  SPA-D01-STSRYYMD
           MOVE    SPA-NAI-EDSRYYMD        TO  SPA-D01-EDSRYYMD
      *
           MOVE    ZERO            TO  CNT-2001
      *
           INITIALIZE                  CHK004-REC
           MOVE    SPA-HOSPNUM     TO  CHK004-HOSPNUM
           MOVE    "2001"          TO  CHK004-KANRICD
           MOVE    "3%"            TO  CHK004-KBNCD
           MOVE    SPA-SYSYMD      TO  CHK004-STYUKYMD
                                       CHK004-EDYUKYMD
           PERFORM 900-CHK004-KEY7-SEL-SEC
           MOVE    CHK004-REC      TO  CK4-2001-REC
      *
           PERFORM UNTIL ( FLG-CHK004  NOT =   ZERO )
                     OR  ( SPA-ERRCD   NOT =   SPACE )
      *
               COMPUTE CNT-2001    =   CNT-2001    +   1
      *
               MOVE    "2"          TO CK4-2001-KBNCD (1:1)
               MOVE    SMCNDATE-YMD TO CK4-2001-CREYMD
                                       CK4-2001-UPYMD
               MOVE    SMCNDATE-HMS TO CK4-2001-UPHMS
      *
               MOVE    CK4-2001-REC TO MCPDATA-REC
               MOVE    "tbl_chk004" TO MCP-TABLE
               MOVE    "key"        TO MCP-PATHNAME
               PERFORM 910-DBINSERT-SEC
               IF    ( MCP-RC  NOT =   ZERO )
                   MOVE    "4001"  TO  SPA-ERRCD
               END-IF
      *
               PERFORM 900-CHK004-KEY7-FET-SEC
               MOVE    CHK004-REC  TO  CK4-2001-REC
      *
           END-PERFORM
      *
           MOVE    "tbl_chk004"    TO  MCP-TABLE
           MOVE    "key7"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( CNT-2001        >   ZERO )
               MOVE    "2"         TO  SPA-D01-IKTKBT
               MOVE    1           TO  SPA-D01-KBTUSER
           ELSE
               MOVE    "1"         TO  SPA-D01-IKTKBT
               MOVE    ZERO        TO  SPA-D01-KBTUSER
           END-IF
      *
           MOVE   "OK"             TO  SPA-DID1-FLG
           MOVE   "CHANGE"         TO  MCP-PUTTYPE
           PERFORM 210-BACK
      *
           DISPLAY "D02 SPA-D01-IKTKBT  =" SPA-D01-IKTKBT  
      *
           .
       4121-KAKUTEI-EXT.
           EXIT.
      *****************************************************************
      *    対象コンボチェック処理
      *****************************************************************
       4200-TAISYOLST-CHK-SEC          SECTION.
      *
           MOVE    SPACE               TO  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
               UNTIL ( IDX-CMB     >   SPA-GMN-TAISYO-MAX )
                OR   ( WRK-CMB-CD  =   SPA-GMN-TAISYOL (IDX-CMB))
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX-CMB     <=  SPA-GMN-TAISYO-MAX )
               MOVE    SPA-GMN-TAISYOLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-TAISYOLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN                  SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF    ( SPA-ERRCD       NOT =   SPACE )
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF    ( SPA-DIDCD       NOT =   SPACE )
               PERFORM 520-DID1SET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "D02    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *****************************************************************
      *    画面内容編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
           MOVE   D02-CHKLST-ROW       TO  WRK-CHKLST-ROW
      *
           INITIALIZE                      D02
      *
           IF    ( FLG-CHKLST-ROW      =   1 )
               MOVE    ZERO            TO  D02-CHKLST-ROW
                                           D02-CHKLST-ROWATTR
           ELSE
               MOVE    WRK-CHKLST-ROW  TO  D02-CHKLST-ROW
               MOVE    ZERO            TO  D02-CHKLST-ROWATTR
           END-IF
      *
           MOVE    "blue"              TO  D02-LBLSRYYM-STYLE
                                           D02-LBLNYUGAI-STYLE
           MOVE    SPA-GMN-LBLSRYYM    TO  D02-LBLSRYYM
           MOVE    SPA-GMN-LBLNYUGAI   TO  D02-LBLNYUGAI
           MOVE    SPA-GMN-SELNUM      TO  D02-SELNUM
           MOVE    SPA-GMN-PTNUM       TO  D02-PTNUM
           MOVE    SPA-GMN-NAME        TO  D02-NAME
           MOVE    SPA-GMN-CHKLST-MAX  TO  D02-CHKLST-COUNT
      *
           MOVE    SPA-GMN-TAISYO-G    TO  D02-TAISYO
           MOVE    SPA-GMN-TAISYO-MAX  TO  D02-TAISYO-COUNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SPA-GMN-TAISYO-MAX )
      *
               MOVE    SPA-GMN-TAISYOLST (IDX0)
                                       TO  D02-TAISYO-ITEM (IDX0)
      *
           END-PERFORM
      *
           PERFORM VARYING IDX-LST FROM    1   BY  1
                   UNTIL ( IDX-LST >   CONST-CHKLST-MAX  )
      *
               MOVE    SPA-GMN-TNUM (IDX-LST)
                                       TO  WRK-ZZZZ9
               MOVE    WRK-ZZZZ9       TO  D02-CHKLST-TNUM (IDX-LST)
      *
               IF    ( SPA-GMN-TTESTPT(IDX-LST) =   "1" )
                   MOVE    "テ"        TO  D02-CHKLST-TTESTPT(IDX-LST)
               END-IF
      *
               MOVE    SPA-GMN-TPTNUM (IDX-LST)
                                       TO  D02-CHKLST-TPTNUM(IDX-LST)
               MOVE    SPA-GMN-TNAME (IDX-LST)
                                       TO  D02-CHKLST-TNAME (IDX-LST)
      *
               IF    ( SPA-GMN-CHKLST-SEL
                                       =    IDX-LST )
                   MOVE    "T"         TO   D02-CHKLST-SELECT (IDX-LST)
               ELSE
                   MOVE    "F"         TO   D02-CHKLST-SELECT (IDX-LST)
               END-IF
      *
           END-PERFORM
      *
           PERFORM 500-GMNHEN-STATE-EDIT-SEC
      *
      *    カーソル位置
           PERFORM 5001-MAPCUR-SEC
           .
      *
       500-GMNHEN-EXT.
           EXIT.
      *****************************************************************
      *    項目状態編集処理
      *****************************************************************
       500-GMNHEN-STATE-EDIT-SEC       SECTION.
      *
           MOVE    WIDGET-NORMAL       TO  D02-B06-STATE
                                           D02-B07-STATE
      *
           IF    ( SPA-GMN-CHKLST-PAGE <=  1 )
               MOVE    WIDGET-INSENSITIVE
                                       TO  D02-B06-STATE
           END-IF
      *
           IF    ( SPA-GMN-CHKLST-NEXT  =  ZERO )
               MOVE    WIDGET-INSENSITIVE
                                       TO  D02-B07-STATE
           END-IF
      *
           .
       500-GMNHEN-STATE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    カーソル位置処理
      *****************************************************************
       5001-MAPCUR-SEC                 SECTION.
      *
      *
           IF    ( SPA-ERRCD           =   SPACE)
             AND ( SPA-GMN-CUR         =   ZERO )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           PERFORM 5011-INPUT-CUR-SKIP-SEC
      *
           EVALUATE    SPA-GMN-CUR
           WHEN    001
               MOVE    "SELNUM"        TO  MCP-WIDGET
           WHEN    002
               MOVE    "PTNUM"         TO  MCP-WIDGET
           WHEN    003
               MOVE    "TAISYO"        TO  MCP-WIDGET
           WHEN    212
               MOVE    "B12"           TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       5011-INPUT-CUR-SEC              SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
           WHEN    "SELNUM"
               MOVE    002             TO  SPA-GMN-CUR
           WHEN    "PTNUM"
               MOVE    001             TO  SPA-GMN-CUR
           WHEN    "TAISYO"
               MOVE    001             TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    カーソルスキップ処理
      *****************************************************************
       5011-INPUT-CUR-SKIP-SEC         SECTION.
      *
           IF    ( SPA-GMN-CUR         =   1 )
               IF    ( SPA-GMN-CHKLST-MAX  =   ZERO )
                   MOVE    2           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
      *
       5011-INPUT-CUR-SKIP-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC                  SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0001"
               MOVE    "選択番号入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0002"
               MOVE    "該当の患者が存在しません"
                                       TO  SPA-ERRMSG
           WHEN    "0003"
               MOVE    "診療年月の診療情報が存在しません"
                                       TO  SPA-ERRMSG
           WHEN    "0004"
               MOVE    "該当患者は既に登録済みです"
                                       TO  SPA-ERRMSG
           WHEN    "0005"
               MOVE    "対象のデータが選択してありません"
                                       TO  SPA-ERRMSG
           WHEN    "0006"
               MOVE    "処理情報が存在しません"
                                       TO  SPA-ERRMSG
           WHEN    "1001"
               MOVE    "入力の途中です"
                                       TO  SPA-ERRMSG
           WHEN    "4001"  THRU    "4999"
               MOVE    "更新処理に失敗しました"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  DERR
           MOVE    SPA-ERRCD           TO  DERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  DERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "D02"               TO  SPA-MOTOPG
           MOVE    "DERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "DERR"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ガイドメッセージ表示
      *****************************************************************
       520-DID1SET-SEC                 SECTION.
      *
           MOVE    SPACE               TO  WRK-DIDMSG
      *
      *    EVALUATE    SPA-DIDCD
      *    WHEN    "6666"
      *        MOVE    "処理中です"    TO  WRK-DIDMSG
      *    END-EVALUATE
      *
           MOVE    SPACE               TO  DID1
           INITIALIZE                      DID1
           MOVE    SPA-DIDCD           TO  DID1-ID1CODE
           MOVE    WRK-DIDMSG          TO  DID1-ID1MSG
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "D02"               TO  SPA-MOTOPG
           MOVE    "DID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "DID1"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-DID1SET-EXT.
           EXIT.
      *****************************************************************
      *    年月チェック・編集処理
      *****************************************************************
       800-YMD-CHK-SEC                 SECTION.
      *
           MOVE    ZERO                TO  FLG-HIZUKE-ERR
      *
           MOVE    ZERO                TO  WRK-SYMD
           MOVE    SPACE               TO  WRK-HENYMDG
      *
      *    日付画面チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    WRK-YMD             TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
           IF  ( SGDAY-RC              =   ZERO )
               MOVE    SGDAY-OTDAY     TO  WRK-HENYMDG
               MOVE    SGDAY-SDAY      TO  WRK-SYMD
           ELSE
               MOVE    1               TO  FLG-HIZUKE-ERR
           END-IF
      *
           .
       800-YMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       800-SEIWA-HEN-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-EDTYMD1
                                           WRK-EDTYMD3
      *
           IF    ( WRK-YMD             =   SPACE )
               GO  TO  800-SEIWA-HEN-EXT
           END-IF
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-YMD             TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                           LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD1    TO  WRK-EDTYMD1
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-EDTYMD3
           INSPECT WRK-EDTYMD3     REPLACING  ALL "  "  BY  "　"
      *
           .
       800-SEIWA-HEN-EXT.
           EXIT.
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
      *    患者情報検索(KEY)
      *****************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-PTINF
      *
           INITIALIZE                  PTINF-REC
           MOVE    SPA-HOSPNUM      TO  PTINF-HOSPNUM
           MOVE    WRK-PTID        TO  PTINF-PTID
           MOVE    PTINF-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTINF-REC
           ELSE
               MOVE    1           TO  FLG-PTINF
               INITIALIZE              PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者番号情報検索処理(KEY)
      *****************************************************************
       900-PTNUM-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNUM
      *
           INITIALIZE                  PTNUM-REC
           MOVE    SPA-HOSPNUM          TO  PTNUM-HOSPNUM
           MOVE    WRK-PTID            TO  PTNUM-PTID
           MOVE    PTNUM-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNUM-REC
           ELSE
               INITIALIZE                  PTNUM-REC
               MOVE    1               TO  FLG-PTNUM
           END-IF
      *
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNUM-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ４検索処理
      *****************************************************************
       900-CHK004-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-CHK004
      *
           MOVE    SPA-HOSPNUM     TO  CHK004-HOSPNUM
           MOVE    CHK004-REC      TO  MCPDATA-REC
           MOVE    "tbl_chk004"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  CHK004-REC
           ELSE
               INITIALIZE              CHK004-REC
               MOVE    1           TO  FLG-CHK004
           END-IF
      *
           MOVE    "tbl_chk004"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-CHK004-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ４検索処理
      *****************************************************************
       900-CHK004-KEY7-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-CHK004
      *
           MOVE    SPA-HOSPNUM     TO  CHK004-HOSPNUM
           MOVE    CHK004-REC      TO  MCPDATA-REC
           MOVE    "tbl_chk004"    TO  MCP-TABLE
           MOVE    "key7"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  CHK004-REC
           ELSE
               INITIALIZE              CHK004-REC
               MOVE    1           TO  FLG-CHK004
           END-IF
      *
           .
       900-CHK004-KEY7-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ４FETCH処理
      *****************************************************************
       900-CHK004-KEY7-FET-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-CHK004
      *
           MOVE    "tbl_chk004"    TO  MCP-TABLE
           MOVE    "key7"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  CHK004-REC
           ELSE
               INITIALIZE              CHK004-REC
               MOVE    1           TO  FLG-CHK004
           END-IF
      *
           .
       900-CHK004-KEY7-FET-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ４検索処理
      *****************************************************************
       900-CHK004-KEY4-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-CHK004
      *
           MOVE    SPA-HOSPNUM     TO  CHK004-HOSPNUM
           MOVE    CHK004-REC      TO  MCPDATA-REC
           MOVE    "tbl_chk004"    TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  CHK004-REC
           ELSE
               INITIALIZE              CHK004-REC
               MOVE    1           TO  FLG-CHK004
           END-IF
      *
           .
       900-CHK004-KEY4-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ４FETCH処理
      *****************************************************************
       900-CHK004-KEY4-FET-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-CHK004
      *
           MOVE    "tbl_chk004"    TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  CHK004-REC
           ELSE
               INITIALIZE              CHK004-REC
               MOVE    1           TO  FLG-CHK004
           END-IF
      *
           .
       900-CHK004-KEY4-FET-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ４検索処理
      *****************************************************************
       900-CHK004-KEY5-SEL-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-CHK004
      *
           MOVE    SPA-HOSPNUM     TO  CHK004-HOSPNUM
           MOVE    CHK004-REC      TO  MCPDATA-REC
           MOVE    "tbl_chk004"    TO  MCP-TABLE
           MOVE    "key5"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  CHK004-REC
           ELSE
               INITIALIZE              CHK004-REC
               MOVE    1           TO  FLG-CHK004
           END-IF
      *
           MOVE    "tbl_chk004"    TO  MCP-TABLE
           MOVE    "key5"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-CHK004-KEY5-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ４検索処理
      *****************************************************************
       900-CHK004-KEY6-SEL-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-CHK004
      *
           MOVE    SPA-HOSPNUM     TO  CHK004-HOSPNUM
           MOVE    CHK004-REC      TO  MCPDATA-REC
           MOVE    "tbl_chk004"    TO  MCP-TABLE
           MOVE    "key6"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  CHK004-REC
           ELSE
               INITIALIZE              CHK004-REC
               MOVE    1           TO  FLG-CHK004
           END-IF
      *
           MOVE    "tbl_chk004"    TO  MCP-TABLE
           MOVE    "key6"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-CHK004-KEY6-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DB-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
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
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＤＥＬＥＴＥ処理
      *****************************************************************
       910-DBDELETE-SEC                SECTION.
      *
           MOVE    "DBDELETE"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＩＮＳＥＲＴ処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＵＰＤＡＴＥ処理
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"          TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
