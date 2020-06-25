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
       PROGRAM-ID.             ORCGX03.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : チェックマスタ情報設定
      *  コンポーネント名  : チェック項目複写
      *  管理者            : 
      *  04/02/27    NACL-太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.00    NACL-多々納  07/05/08  グループ診療対応
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
       WORKING-STORAGE             SECTION.
      *
      *    画面活性非活性
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    チェックマスタ設定用共通パラメタ
           COPY    "X01COMMON-SPA".
      *
      *    第一画面ＳＰＡエリア
           COPY    "X01SCR-SPA".
      *
      *
      *    Ｘ０３画面用
       01  SPA-X03-AREA.
           03  SPA-X03-LCHKLST-MAX                 PIC 9(05).
           03  SPA-GMN-AREA.
               05  SPA-GMN-DEFAULT-UTAGAI          PIC X(01).
               05  SPA-GMN-NEXT                    PIC 9(01).
               05  SPA-GMN-PAGE                    PIC 9(05).
               05  SPA-GMN-CUR                     PIC 9(03).
               05  SPA-GMN-LCHKKBNMEI              PIC X(70).
               05  SPA-GMN-LYUKOSTYMD              PIC X(09).
               05  SPA-GMN-LYUKOEDYMD              PIC X(09).
               05  SPA-GMN-LNAME                   PIC X(70).
               05  SPA-GMN-LCHKLST.
                   07  SPA-GMN-LCHKLST-MAX         PIC 9(03).
                   07  SPA-GMN-LCHKLST-OCC         OCCURS  601.
                       09  SPA-GMN-TLCHK-NUM       PIC 9(03).
                       09  SPA-GMN-TLCHK-NAME      PIC X(100).
                       09  SPA-GMN-TLCHK-JYKN      PIC X(01).
                       09  SPA-GMN-TLCHK-GRPID     PIC X(01).
                       09  SPA-GMN-TLCHK-HOSOKU    PIC X(40).
                       09  SPA-GMN-TLCHK-HOSOKU-JYKN
                                                       PIC X(01).
                   07  SPA-GMN-LCHKLST-FLG         PIC 9(01)
                                                       OCCURS  600.
               05  SPA-GMN-RCHKKBNMEI              PIC X(70).
               05  SPA-GMN-RYUKOSTYMD              PIC X(09).
               05  SPA-GMN-RYUKOEDYMD              PIC X(09).
               05  SPA-GMN-RNAME                   PIC X(70).
               05  SPA-GMN-RCHKLST.
                   07  SPA-GMN-RCHKLST-MAX         PIC 9(03).
                   07  SPA-GMN-RCHKLST-OCC         OCCURS  400.
                       09  SPA-GMN-TRCHK-NUM       PIC 9(03).
                       09  SPA-GMN-TRCHK-NAME      PIC X(100).
                       09  SPA-GMN-TRCHK-JYKN      PIC X(01).
                       09  SPA-GMN-TRCHK-GRPID     PIC X(01).
                       09  SPA-GMN-TRCHK-HOSOKU    PIC X(40).
                       09  SPA-GMN-TRCHK-HOSOKU-JYKN
                                                       PIC X(01).
                   07  SPA-GMN-RCHKLST-FLG-G.
                       09  SPA-GMN-RCHKLST-FLG     PIC 9(01)
                                                       OCCURS  400.
               05  SPA-GMN-CHKKBN-G.
                   07  SPA-GMN-CHKKBN              PIC X(01).
                   07  SPA-GMN-CHKKBNFL            PIC X(01).
                   07  SPA-GMN-CHKKBNMEI           PIC X(70).
               05  SPA-GMN-CHKKBNLST-G.
                   07  SPA-GMN-CHKKBN-MAX          PIC 9(03).
                   07  SPA-GMN-CHKKBNLST           OCCURS  05.
                       09  SPA-GMN-CHKKBNL         PIC X(01).
                       09  SPA-GMN-CHKKBNFLL       PIC X(01).
                       09  SPA-GMN-CHKKBNMEIL      PIC X(70).
               05  SPA-GMN-SELNAME                 PIC X(70).
               05  SPA-GMN-SELCHKLST.
                   07  SPA-GMN-SELCHKLST-MAX       PIC 9(03).
                   07  SPA-GMN-SELCHKLST-OCC       OCCURS  100.
                       09  SPA-GMN-TSEL-CHKNUM     PIC 9(03).
                       09  SPA-GMN-TSEL-NAME       PIC X(70).
                       09  SPA-GMN-TSEL-YUKOSTYMD  PIC X(09).
                       09  SPA-GMN-TSEL-YUKOEDYMD  PIC X(09).
                   07  SPA-GMN-SELCHKLST-SEL       PIC 9(05).
               05  SPA-GMN-KJNYMD                  PIC X(10).
           03  SPA-NAI-AREA.
               05  SPA-NAI-LCHKLST.
                   07  SPA-NAI-LCHKLST-OCC         OCCURS  601.
                       09  SPA-NAI-TLCHK-SRYCD     PIC X(09).
                       09  SPA-NAI-TLCHK-BYOCD     PIC X(07)
                                                       OCCURS  21.
                       09  SPA-NAI-TLCHK-BYOSTATUS PIC X(01).
                       09  SPA-NAI-TLCHK-UTAGAI    PIC X(01).
                       09  SPA-NAI-TLCHK-SYORIKBN  PIC X(01).
                       09  SPA-NAI-TLCHK-NGBNDKBN  PIC X(01).
                       09  SPA-NAI-TLCHK-CHKERR    PIC 9(01).
                       09  SPA-NAI-TLCHK-DATAKBN   PIC X(01).
               05  SPA-NAI-RYUKOSTYMD              PIC X(08).
               05  SPA-NAI-RYUKOEDYMD              PIC X(08).
               05  SPA-NAI-RCHKLST.
                   07  SPA-NAI-RCHKLST-OCC         OCCURS  400.
                       09  SPA-NAI-TRCHK-SRYCD     PIC X(09).
                       09  SPA-NAI-TRCHK-DELFLG    PIC 9(01).
                       09  SPA-NAI-TRCHK-BYOCD     PIC X(07)
                                                       OCCURS  21.
                       09  SPA-NAI-TRCHK-BYOSTATUS PIC X(01).
                       09  SPA-NAI-TRCHK-UTAGAI    PIC X(01).
                       09  SPA-NAI-TRCHK-SYORIKBN  PIC X(01).
                       09  SPA-NAI-TRCHK-NGBNDKBN  PIC X(01).
                       09  SPA-NAI-TRCHK-CHKERR    PIC 9(01).
                       09  SPA-NAI-TRCHK-DATAKBN   PIC X(01).
               05  SPA-NAI-SELCHKLST.
                   07  SPA-NAI-SELCHKLST-KBN       PIC X(01).
                   07  SPA-NAI-SELCHKLST-OCC       OCCURS  100.
                       09  SPA-NAI-TSEL-SRYCD      PIC X(09).
                       09  SPA-NAI-TSEL-ID         PIC 9(09).
                       09  SPA-NAI-TSEL-YUKOSTYMD  PIC X(08).
                       09  SPA-NAI-TSEL-YUKOEDYMD  PIC X(08).
               05  SPA-NAI-KJNYMD                  PIC X(08).
      *
       01  CNT-AREA.
           03  CNT-SELCHKLST               PIC 9(05).
      *
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-END                     PIC 9(01).
           03  FLG-CHANGE                  PIC 9(01).
           03  FLG-HIT                     PIC 9(01).
           03  FLG-CHK                     PIC 9(01).
           03  FLG-CHKSND                  PIC 9(01).
           03  FLG-CHKTRD                  PIC 9(01).
           03  FLG-CHK005                  PIC 9(01).
           03  FLG-TENSU                   PIC 9(01).
           03  FLG-INPUTCD                 PIC 9(01).
           03  FLG-HIZUKE-ERR              PIC 9(01).
           03  FLG-ENTRYCHK                PIC 9(01).
           03  FLG-SRYCD-DEL               PIC 9(01).
           03  FLG-OK                      PIC 9(01).
           03  FLG-LCHKLST-ROW             PIC 9(01).
           03  FLG-RCHKLST-ROW             PIC 9(01).
           03  FLG-SELCHKLST-ROW           PIC 9(01).
           03  FLG-TEKIOUBYOMEI            PIC 9(01).
           03  FLG-KANJI                   PIC 9(01).
           03  FLG-RCHKLST-SELECT          PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                        PIC 9(05).
           03  IDX1                        PIC 9(05).
           03  IDX2                        PIC 9(05).
           03  IDX3                        PIC 9(05).
           03  IDX4                        PIC 9(05).
           03  IDX5                        PIC 9(05).
           03  IDX6                        PIC 9(05).
           03  IDX7                        PIC 9(05).
           03  IDXA                        PIC 9(05).
           03  IDXE                        PIC 9(05).
           03  IDXF                        PIC 9(05).
           03  IDXS                        PIC 9(05).
           03  IDX-CMB                     PIC 9(05).
           03  IDX-LST                     PIC 9(05).
           03  IDX-ST                      PIC 9(05).
      *
       01  WRK-AREA.
           03  WRK-TABLE                   PIC X(64).
           03  WRK-PATHNAME                PIC X(64).
           03  WRK-MCP-WIDGET              PIC X(64).
           03  WRK-YMD                     PIC X(10).
           03  WRK-SYMD                    PIC X(08).
           03  WRK-HENYMDG                 PIC X(10).
           03  WRK-CMB-CD                  PIC X(10).
           03  WRK-CMB-ITM-G.
               07  WRK-CMB-ITM             PIC X(200).
           03  WRK-ZZZZ                    PIC ZZZZ.
           03  WRK-CDSYUBETU               PIC X(01).
           03  WRK-SRYCD                   PIC X(20).
           03  WRK-SKIP-MAX                PIC 9(05).
           03  WRK-KJNYMD-MOT              PIC X(08).
           03  WRK-KANACHK-MAE-INPUT       PIC X(400).
           03  WRK-KANACHK-OUT-INPUT       PIC X(400).
           03  WRK-RCHKLST-SRYCD           PIC X(09).
           03  WRK-RCHKLST-ID              PIC 9(09).
           03  WRK-TNS-NAME                PIC X(70).
           03  WRK-CHK-CD                  PIC X(09).
           03  WRK-NAME                    PIC X(70).
           03  WRK-KEY-CD-G.
               05  WRK-KEY-CD-OCC          OCCURS  21.
                   07  WRK-KEY-CD          PIC X(07).
                   07  WRK-KEY-CDSEP       PIC X(01).
           03  WRK-MOJI                    PIC 9(03).
           03  WRK-PRT-NUM-G.
               05  WRK-PRT-NUM             PIC ZZZ9.
               05  WRK-PRT-GRPID           PIC X(02).
           03  WRK-BYOMEI-JYKN             PIC X(10).
           03  WRK-HOSOKU-JYKN             PIC X(10).
      *
       01  WRK-LCHKLST-AREA.
           03  WRK-LCHKLST-MAX             PIC 9(05).
           03  WRK-LCHKLST-OCC             OCCURS   600.
               05  WRK-LCHKLST-NAME        PIC X(100).
               05  WRK-LCHKLST-SRYCD       PIC X(09).
               05  WRK-LCHKLST-JYKN        PIC X(01).
               05  WRK-LCHKLST-GRPID       PIC X(01).
               05  WRK-LCHKLST-BYOCD       PIC X(07)
                                           OCCURS   21.
               05  WRK-LCHKLST-BYOSTATUS   PIC X(01).
               05  WRK-LCHKLST-UTAGAI      PIC X(01).
               05  WRK-LCHKLST-SYORIKBN    PIC X(01).
               05  WRK-LCHKLST-NGBNDKBN    PIC X(01).
               05  WRK-LCHKLST-CHKERR      PIC 9(01).
               05  WRK-LCHKLST-DATAKBN     PIC X(01).
               05  WRK-LCHKLST-HOSOKU      PIC X(40).
               05  WRK-LCHKLST-HOSOKU-JYKN PIC X(01).
      *
       01  WRK-ROW-AREA.
           03  WRK-LCHKLST-ROW             PIC S9(9)   BINARY.
           03  WRK-RCHKLST-ROW             PIC S9(9)   BINARY.
           03  WRK-SELCHKLST-ROW           PIC S9(9)   BINARY.
      *
       01  CONST-AREA.
           03  CONST-MKCD-MAX              PIC 9(05)   VALUE   21.
           03  CONST-SELCHKLST-MAX         PIC 9(05)   VALUE   50.
           03  CONST-LCHKLST-MAX           PIC 9(05)   VALUE   600.
           03  CONST-RCHKLST-MAX           PIC 9(05)   VALUE   400.
           03  CONST-JYKN-MAX              PIC 9(05)   VALUE   4.
           03  CONST-JYKN-VAL.
               05                          PIC X(01)   VALUE   "1".
               05                          PIC X(04)   VALUE   "部分".
               05                          PIC X(01)   VALUE   "2".
               05                          PIC X(04)   VALUE   "完全".
               05                          PIC X(01)   VALUE   "3".
               05                          PIC X(04)   VALUE   "前方".
               05                          PIC X(01)   VALUE   "4".
               05                          PIC X(04)   VALUE   "後方".
           03  CONST-JYKN-R                REDEFINES   CONST-JYKN-VAL.
               05  CONST-JYKNLST           OCCURS  4.
                   07  CONST-JYKN-CD       PIC X(01).
                   07  CONST-JYKN-MEI      PIC X(04).
           03  CONST-DMKBN-MAX             PIC 9(05)   VALUE   2.
           03  CONST-DMKBN-VAL.
               05                          PIC X(01)   VALUE   "0".
               05                          PIC X(04)   VALUE   "　日".
               05                          PIC X(01)   VALUE   "1".
               05                          PIC X(04)   VALUE   "　月".
           03  CONST-DMKBN-R               REDEFINES   CONST-DMKBN-VAL.
               05  CONST-DMKBNLST          OCCURS  2.
                   07  CONST-DMKBN-CD      PIC X(01).
                   07  CONST-DMKBN-MEI     PIC X(04).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    自院マスター
       01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
      *
      *    チェック
       01  CHK-REC.
           COPY    "CPCHK.INC".
      *
      *    チェック２
       01  CHKSND-REC.
           COPY    "CPCHKSND.INC".
      *
      *    チェック３
       01  CHKTRD-REC.
           COPY    "CPCHKTRD.INC".
      *
      *    チェック５
       01  CHK005-REC.
           COPY    "CPCHK005.INC".
      *
      *    適応病名マスター
       01  TEKIOUBYOMEI-REC.
           COPY    "CPTEKIOUBYOMEI.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    パラメタ保存
           COPY    "CPORCSPARA.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *****************************************************************
      *    連絡　領域
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
           INITIALIZE                  CNT-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-WORK        TO  SPA-X01KYOUTU
           MOVE    SPA-FREE        TO  SPA-X01FREE
      *
           IF    ( SPA-X03-GMN-UUID    NOT =   SPACE )
               INITIALIZE              SPARA-AREA
               MOVE    SPA-X03-GMN-UUID
                                       TO  SPARA-ID
               MOVE    "LOAD_AND_DELETE"
                                       TO  SPARA-REQUEST
               CALL    "ORCSPARA"          USING
                                           SPARA-AREA
                                           SPA-AREA
               MOVE    SPARA-DATA      TO  SPA-X03-AREA
               IF    ( SPARA-RC        NOT =   ZERO )
                   DISPLAY "LOAD SPARA-RC=" SPARA-RC
               END-IF
           END-IF
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS  ALSO    MCP-EVENT
           WHEN    "LINK"          ALSO    ANY
               PERFORM 100-INIT-SEC
      *    画面遷移
           WHEN     "PUTG"         ALSO    "CLICKED"
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
           IF    ( FLG-CHANGE      =   ZERO )
               INITIALIZE                  SPARA-AREA
               MOVE    "SAVE"          TO  SPARA-REQUEST
               MOVE    SPA-X03-AREA    TO  SPARA-DATA
               CALL    "ORCSPARA"          USING
                                           SPARA-AREA
                                           SPA-AREA
               MOVE    SPARA-ID        TO  SPA-X03-GMN-UUID
               IF    ( SPARA-RC        NOT =   ZERO )
                   DISPLAY "SAVE SPARA-RC=" SPARA-RC
               END-IF
           ELSE
               MOVE    SPACE       TO  SPA-X03-GMN-UUID
           END-IF
      *
           MOVE    SPA-X01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-X01FREE     TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    エラー画面より
           IF    ( SPA-MOTOPG          =   "XERR" )
               MOVE    SPACE       TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF    ( FLG-END     =   1 )
                   GO  TO  100-INIT-EXT
               END-IF
      *
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
      ***  MOVE    "NEW"           TO  MCP-PUTTYPE.
           MOVE    SPACE           TO  MCP-PUTTYPE.
           MOVE    "X03"           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1               TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    他のＬＤより
           IF    ( LINKAREA        NOT =   SPACE )
               MOVE    LNK-KYOUTU      TO  SPA-KYOUTU
      *
               MOVE    SPA-MOTOPG      TO  SPA-MOTOPG2
      *
               MOVE    SPACE           TO  SPA-XID1-FLG
               MOVE    SPACE           TO  SPA-XIDCD
           END-IF
      *
      *    EVALUATE    SPA-MOTOPG
      *    WHEN    "xxx"
      *        GO  TO      300-SCREEN-EXT
      *    END-EVALUATE
      *
           PERFORM 310-SPASET-SEC
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC                  SECTION.
      *
           INITIALIZE                  SPA-X03-AREA
      *
           IF    ( SPA-X01-CHKKBN  =   "5" OR "A" OR "B" )
               MOVE    CONST-LCHKLST-MAX
                                   TO  SPA-X03-LCHKLST-MAX
           ELSE
               MOVE    CONST-RCHKLST-MAX
                                   TO  SPA-X03-LCHKLST-MAX
           END-IF
      *
           MOVE    SPA-X03-GMN-LCHKKBNMEI
                                   TO  SPA-GMN-LCHKKBNMEI
           MOVE    SPA-X03-GMN-LYUKOSTYMD
                                   TO  SPA-GMN-LYUKOSTYMD
           MOVE    SPA-X03-GMN-LYUKOEDYMD
                                   TO  SPA-GMN-LYUKOEDYMD
           MOVE    SPA-X03-GMN-LNAME
                                   TO  SPA-GMN-LNAME
           MOVE    SPA-X03-GMN-LCHKLST-MAX
                                   TO  SPA-GMN-LCHKLST-MAX
           PERFORM VARYING IDXE    FROM    1   BY  1
                   UNTIL ( IDXE    >   SPA-GMN-LCHKLST-MAX )
      *
               MOVE    SPA-X03-GMN-TLCHK-NUM (IDXE)
                                   TO  SPA-GMN-TLCHK-NUM (IDXE)
               MOVE    SPA-X03-GMN-TLCHK-NAME (IDXE)
                                   TO  SPA-GMN-TLCHK-NAME (IDXE)
               MOVE    SPA-X03-GMN-TLCHK-JYKN (IDXE)
                                   TO  SPA-GMN-TLCHK-JYKN (IDXE)
               MOVE    SPA-X03-GMN-TLCHK-GRPID (IDXE)
                                   TO  SPA-GMN-TLCHK-GRPID (IDXE)
               MOVE    SPA-X03-GMN-TLCHK-HOSOKU (IDXE)
                                   TO  SPA-GMN-TLCHK-HOSOKU (IDXE)
               MOVE    SPA-X03-GMN-TLCHK-HOSOKU-JYKN (IDXE)
                               TO  SPA-GMN-TLCHK-HOSOKU-JYKN (IDXE)
               MOVE    SPA-X03-NAI-TLCHK-SRYCD (IDXE)
                                   TO  SPA-NAI-TLCHK-SRYCD (IDXE)
      *
               PERFORM VARYING IDXF    FROM    1   BY  1
                       UNTIL ( IDXF    >   CONST-MKCD-MAX )
                   MOVE    SPA-X03-NAI-TLCHK-BYOCD (IDXE IDXF)
                               TO  SPA-NAI-TLCHK-BYOCD (IDXE IDXF)
               END-PERFORM
      *
               MOVE    SPA-X03-NAI-TLCHK-BYOSTATUS (IDXE)
                                   TO  SPA-NAI-TLCHK-BYOSTATUS(IDXE)
               MOVE    SPA-X03-NAI-TLCHK-UTAGAI (IDXE)
                                   TO  SPA-NAI-TLCHK-UTAGAI (IDXE)
               MOVE    SPA-X03-NAI-TLCHK-SYORIKBN (IDXE)
                                   TO  SPA-NAI-TLCHK-SYORIKBN (IDXE)
               MOVE    SPA-X03-NAI-TLCHK-NGBNDKBN (IDXE)
                                   TO  SPA-NAI-TLCHK-NGBNDKBN (IDXE)
               MOVE    SPA-X03-NAI-TLCHK-CHKERR (IDXE)
                                   TO  SPA-NAI-TLCHK-CHKERR (IDXE)
               MOVE    SPA-X03-NAI-TLCHK-DATAKBN (IDXE)
                                   TO  SPA-NAI-TLCHK-DATAKBN (IDXE)
      *
           END-PERFORM
      *
           MOVE    1                   TO  FLG-LCHKLST-ROW
      *
      *    ※画面のINITIALIZE,左側のコラムリストの設定は呼出元で行う
      *
           EVALUATE    SPA-X01-CHKKBN
           WHEN    "1"
               MOVE    "0"             TO  SPA-GMN-DEFAULT-UTAGAI
           WHEN    "2"
               MOVE    "1"             TO  SPA-GMN-DEFAULT-UTAGAI
           WHEN    "6"
               MOVE    "1"             TO  SPA-GMN-DEFAULT-UTAGAI
           WHEN    OTHER
               MOVE    "2"             TO  SPA-GMN-DEFAULT-UTAGAI
           END-EVALUATE
      *
           MOVE    SPA-X01-KJNYMD      TO  SPA-NAI-KJNYMD
           MOVE    SPA-NAI-KJNYMD  TO  WRK-YMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-KJNYMD
      *
      *    コンボ項目設定処理
           PERFORM 3101-COMBO-SET-SEC
      *
      *    選択用チェック区分コンボ初期値設定
           MOVE    SPA-X01-CHKKBN      TO  WRK-CMB-CD
           PERFORM 4200-CHKKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-CHKKBN-G
      *
           MOVE    SPACE               TO  SPA-GMN-SELNAME
      *
           MOVE    1                   TO  SPA-GMN-PAGE
      *
      *    チェック項目リスト編集処理
           PERFORM 3101-SELCHKLST-HEN-SEC
      *
           MOVE    2                   TO  SPA-GMN-CUR
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    コンボ項目設定処理
      *****************************************************************
       3101-COMBO-SET-SEC              SECTION.
      *
      *    選択用チェック区分コンボ設定処理
           PERFORM 3101-CHKKBNLST-SET-SEC
      *
           . 
       3101-CMB-DAT-SET-EXT.
           EXIT.
      *****************************************************************
      *    選択用チェック区分コンボ設定処理
      *****************************************************************
       3101-CHKKBNLST-SET-SEC          SECTION.
      *
           INITIALIZE                      SPA-GMN-CHKKBNLST-G
      *
           EVALUATE    SPA-X01-CHKKBN
           WHEN    "1"
           WHEN    "2"
      *
      *?       MOVE    3                   TO  SPA-GMN-CHKKBN-MAX
      *
      *?       MOVE    "1"                 TO  SPA-GMN-CHKKBNL   (1)
      *?       MOVE    "薬剤と病名"        TO  SPA-GMN-CHKKBNMEIL(1)
      *?       MOVE    "2"                 TO  SPA-GMN-CHKKBNL   (2)
      *?       MOVE    "診療行為と病名"    TO  SPA-GMN-CHKKBNMEIL(2)
      *?       MOVE    "M"                 TO  SPA-GMN-CHKKBNL   (3)
      *?       MOVE    "適応病名"          TO  SPA-GMN-CHKKBNMEIL(3)
      *
               MOVE    2                   TO  SPA-GMN-CHKKBN-MAX
      *
               MOVE    "1"                 TO  SPA-GMN-CHKKBNL   (1)
               MOVE    "薬剤と病名"        TO  SPA-GMN-CHKKBNMEIL(1)
               MOVE    "2"                 TO  SPA-GMN-CHKKBNL   (2)
               MOVE    "診療行為と病名"    TO  SPA-GMN-CHKKBNMEIL(2)
           WHEN    "4"
      *
               MOVE    1                   TO  SPA-GMN-CHKKBN-MAX
      *
               MOVE    "4"                 TO  SPA-GMN-CHKKBNL   (1)
               MOVE    "薬剤と併用禁忌"    TO  SPA-GMN-CHKKBNMEIL(1)
           WHEN    "5"
      *
               MOVE    1                   TO  SPA-GMN-CHKKBN-MAX
      *
               MOVE    "5"                 TO  SPA-GMN-CHKKBNL   (1)
               MOVE    "診療行為の併用算定（同月内）"
                                           TO  SPA-GMN-CHKKBNMEIL(1)
           WHEN    "6"
      *
               MOVE    1                   TO  SPA-GMN-CHKKBN-MAX
      *
               MOVE    "6"                 TO  SPA-GMN-CHKKBNL   (1)
               MOVE    "投与禁忌薬剤と病名"
                                           TO  SPA-GMN-CHKKBNMEIL(1)
           WHEN    "7"
      *
               MOVE    1                   TO  SPA-GMN-CHKKBN-MAX
      *
               MOVE    "7"                 TO  SPA-GMN-CHKKBNL   (1)
               MOVE    "診療行為どうしの算定漏れ"
                                           TO  SPA-GMN-CHKKBNMEIL(1)
           WHEN    "8"
      *
               MOVE    1                   TO  SPA-GMN-CHKKBN-MAX
      *
               MOVE    "8"                 TO  SPA-GMN-CHKKBNL   (1)
               MOVE    "病名と薬剤"        TO  SPA-GMN-CHKKBNMEIL(1)
           WHEN    "9"
      *
               MOVE    1                   TO  SPA-GMN-CHKKBN-MAX
      *
               MOVE    "9"                 TO  SPA-GMN-CHKKBNL   (1)
               MOVE    "病名と診療行為"    TO  SPA-GMN-CHKKBNMEIL(1)
           WHEN    "A"
      *
               MOVE    1                   TO  SPA-GMN-CHKKBN-MAX
      *
               MOVE    "A"                 TO  SPA-GMN-CHKKBNL   (1)
               MOVE    "診療行為の併用算定（同日内）"
                                           TO  SPA-GMN-CHKKBNMEIL(1)
           WHEN    "B"
      *
               MOVE    1                   TO  SPA-GMN-CHKKBN-MAX
      *
               MOVE    "B"                 TO  SPA-GMN-CHKKBNL   (1)
               MOVE    "診療行為の併用算定（同会計内）"
                                           TO  SPA-GMN-CHKKBNMEIL(1)
           END-EVALUATE
      *
           . 
       3101-CHKKBNLST-SET-EXT.
           EXIT.
      *****************************************************************
      *    チェック項目リスト編集処理
      *****************************************************************
       3101-SELCHKLST-HEN-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-KANJI
           MOVE    ZERO                TO  WRK-MOJI
           MOVE    SPACE               TO  WRK-CDSYUBETU
           MOVE    SPACE               TO  WRK-SRYCD
      *
           IF  ( SPA-GMN-SELNAME (1:1) =   "P"  OR  "S" )
               MOVE    "0012"      TO  SPA-ERRCD
	       MOVE    2           TO  SPA-GMN-CUR
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 41011-INPUTCD-SELECT-EDIT-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               EVALUATE    SPA-GMN-CHKKBN
               WHEN    "1"
               WHEN    "2"
               WHEN    "6"
                   PERFORM 3101-SELCHKLST-CHKSND-SEC
               WHEN    "M"
                   IF    ( SPA-GMN-SELNAME =   SPACE )
                       INITIALIZE              SPA-GMN-SELCHKLST
                                               SPA-NAI-SELCHKLST
                                               SPA-GMN-NEXT
                   ELSE
                       PERFORM 3101-SELCHKLST-TEKBYO-SEC
                   END-IF
               WHEN    "7"
                   PERFORM 3101-SELCHKLST-CHKTRD-SEC
               WHEN    "8"
               WHEN    "9"
                   PERFORM 3101-SELCHKLST-CHK005-SEC
               WHEN    OTHER
                   PERFORM 3101-SELCHKLST-CHKMST-SEC
               END-EVALUATE
           END-IF
      *
           . 
       3101-SELCHKLST-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入力コードマスタ−検索条件編集処理
      *****************************************************************
       41011-INPUTCD-SELECT-EDIT-SEC   SECTION.
      *
           MOVE    SPA-GMN-SELNAME TO  WRK-SRYCD
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL  (IDX0    >   20  )
                    OR    (WRK-SRYCD (IDX0:)   =   SPACE )
      *
               COMPUTE WRK-MOJI    =   WRK-MOJI    +   1
      *
           END-PERFORM
      *
           IF  ( WRK-MOJI          >   ZERO )
      *
      *        全角半角混在チェック
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"             TO  KANACHK-SYORI
               MOVE    WRK-SRYCD       TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
               IF      ( KANACHK-RC    NOT =   ZERO )
                   MOVE    "0003"      TO  SPA-ERRCD
                   MOVE    2           TO  SPA-GMN-CUR
               END-IF
      *
               IF    ( SPA-ERRCD       =   SPACE )
                   EVALUATE    KANACHK-SYUBETU
      *            半角入力
                   WHEN    1
                       MOVE    ZERO        TO  FLG-KANJI
      *
                       IF  ( WRK-SRYCD (1:WRK-MOJI) IS  NUMERIC )
                           EVALUATE    WRK-MOJI
                           WHEN    4
                               MOVE    "4" TO  WRK-CDSYUBETU
                           WHEN    5
                               MOVE    "5" TO  WRK-CDSYUBETU
                           WHEN    6
                           WHEN    3
                               MOVE   "6"  TO  WRK-CDSYUBETU
                           WHEN    5
                               MOVE    "5" TO  WRK-CDSYUBETU
                           END-EVALUATE
                       ELSE
                           MOVE    "A"     TO  WRK-CDSYUBETU
                       END-IF
      *            全角入力
                   WHEN    2
                       MOVE    1           TO  FLG-KANJI
                       MOVE    "J"         TO  WRK-CDSYUBETU
                   WHEN    OTHER
                       MOVE    "0003"      TO  SPA-ERRCD
                       MOVE    2           TO  SPA-GMN-CUR
                   END-EVALUATE
               END-IF
           END-IF
      *
           .
      *
       41011-INPUTCD-SELECT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    チェック項目リスト編集処理（チェックマスタより）
      *****************************************************************
       3101-SELCHKLST-CHKMST-SEC       SECTION.
      *
           MOVE    1                   TO  FLG-SELCHKLST-ROW
      *
           INITIALIZE                  SPA-GMN-SELCHKLST
                                       SPA-NAI-SELCHKLST
      *
           MOVE    ZERO                TO  CNT-SELCHKLST
      *
           IF    ( WRK-MOJI            >   ZERO )
               MOVE    "tbl_chk"       TO  WRK-TABLE
               MOVE    "key15"         TO  WRK-PATHNAME
               PERFORM 900-CHK-KEY15-SEL-SEC
               IF    ( FLG-CHK         =   ZERO )
                   PERFORM 900-CHK-FET-SEC
                   IF    ( FLG-CHK     =   ZERO )
                       COMPUTE CNT-SELCHKLST
                             = CNT-SELCHKLST   +   1
                   END-IF
               END-IF
               PERFORM 31011-SELCHKLST-CHKMST-SEC
      *
               IF    ( CNT-SELCHKLST   =   ZERO )
                   MOVE    "tbl_chk"   TO  WRK-TABLE
                   MOVE    "key16"     TO  WRK-PATHNAME
                   PERFORM 900-CHK-KEY16-SEL-SEC
                   IF    ( FLG-CHK         =   ZERO )
                       PERFORM 900-CHK-FET-SEC
                       IF    ( FLG-CHK     =   ZERO )
                           COMPUTE CNT-SELCHKLST = CNT-SELCHKLST   +   1
                       END-IF
                   END-IF
                   PERFORM 31011-SELCHKLST-CHKMST-SEC
               END-IF
      *
               IF    ( CNT-SELCHKLST   =   ZERO )
                AND  ( WRK-MOJI    =   9 )
                AND  ( FLG-KANJI   =   ZERO )
                   MOVE    "tbl_chk"   TO  WRK-TABLE
                   MOVE    "key17"     TO  WRK-PATHNAME
                   PERFORM 900-CHK-KEY17-SEL-SEC
                   IF    ( FLG-CHK         =   ZERO )
                       PERFORM 900-CHK-FET-SEC
                       IF    ( FLG-CHK     =   ZERO )
                           COMPUTE CNT-SELCHKLST = CNT-SELCHKLST   +   1
                       END-IF
                   END-IF
                   PERFORM 31011-SELCHKLST-CHKMST-SEC
               END-IF
           END-IF
      *
           IF    ( CNT-SELCHKLST   =   ZERO )
               MOVE    "tbl_chk"       TO  WRK-TABLE
               MOVE    "key11"         TO  WRK-PATHNAME
               PERFORM 900-CHK-KEY11-SEL-SEC
               IF    ( FLG-CHK         =   ZERO )
                   PERFORM 900-CHK-FET-SEC
                   IF    ( FLG-CHK     =   ZERO )
                       COMPUTE CNT-SELCHKLST = CNT-SELCHKLST   +   1
                   END-IF
                   PERFORM 31011-SELCHKLST-CHKMST-SEC
               END-IF
           END-IF
      *
           . 
       3101-SELCHKLST-CHKMST-EXT.
           EXIT.
      *****************************************************************
      *    チェック項目リスト編集処理（チェックマスタより）
      *****************************************************************
       31011-SELCHKLST-CHKMST-SEC      SECTION.
      *
      *    読込スキップ処理
           PERFORM 3101-SELCHKLST-SKIP-SEC
      *
           PERFORM UNTIL ( FLG-CHK     NOT =   ZERO )
                    OR   ( SPA-GMN-SELCHKLST-MAX
                                          >= CONST-SELCHKLST-MAX )
      *
               COMPUTE SPA-GMN-SELCHKLST-MAX
                   =   SPA-GMN-SELCHKLST-MAX   +   1
      *
               MOVE    SPA-GMN-SELCHKLST-MAX
                                       TO  IDX-LST
      *
               MOVE    CNT-SELCHKLST   TO  SPA-GMN-TSEL-CHKNUM
                                                               (IDX-LST)
      *
               MOVE    CHK-SRYCD       TO  WRK-SRYCD
               MOVE    SPA-NAI-KJNYMD
                                       TO  WRK-YMD
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF     (TNS-SRYCD(1:1)      =   "1") AND
                      (TNS-ROUTEKKBN       =   2  )
                   MOVE    SPACE           TO  SPA-GMN-TSEL-NAME
                                                               (IDX-LST)
                   STRING  "［老］"        DELIMITED   BY  SIZE
                           TNS-NAME        DELIMITED   BY  SIZE
                   INTO    SPA-GMN-TSEL-NAME (IDX-LST)
                   END-STRING
               ELSE
                   MOVE    TNS-NAME        TO  SPA-GMN-TSEL-NAME
                                                               (IDX-LST)
               END-IF
      *
               MOVE    CHK-YUKOSTYMD   TO  SPA-NAI-TSEL-YUKOSTYMD
                                                               (IDX-LST)
               IF    ( CHK-YUKOSTYMD   =   "00000000" )
                   MOVE    CHK-YUKOSTYMD
                                       TO  SPA-GMN-TSEL-YUKOSTYMD
                                                           (IDX-LST)
               ELSE
                   MOVE    CHK-YUKOSTYMD
                                       TO  WRK-YMD
                   PERFORM 800-SEIWA-HEN-SEC
                   MOVE    WRK-HENYMDG TO  SPA-GMN-TSEL-YUKOSTYMD
                                                           (IDX-LST)
               END-IF
      *
               MOVE    CHK-YUKOEDYMD   TO  SPA-NAI-TSEL-YUKOEDYMD
                                                               (IDX-LST)
               IF    ( CHK-YUKOEDYMD   =   "99999999" )
                   MOVE    CHK-YUKOEDYMD
                                       TO  SPA-GMN-TSEL-YUKOEDYMD
                                                           (IDX-LST)
               ELSE
                   MOVE    CHK-YUKOEDYMD
                                       TO  WRK-YMD
                   PERFORM 800-SEIWA-HEN-SEC
                   MOVE    WRK-HENYMDG TO  SPA-GMN-TSEL-YUKOEDYMD
                                                           (IDX-LST)
               END-IF
      *
               MOVE    CHK-SRYCD       TO  SPA-NAI-TSEL-SRYCD
                                                               (IDX-LST)
      *
      *        チェックマスタ読込処理
               PERFORM 900-CHK-FET-SEC
               IF    ( FLG-CHK     =   ZERO )
                   COMPUTE CNT-SELCHKLST = CNT-SELCHKLST   +   1
               END-IF
      *
           END-PERFORM
      *
           MOVE    WRK-TABLE           TO  MCP-TABLE
           MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( FLG-CHK             =   ZERO )
               MOVE    1               TO  SPA-GMN-NEXT
           ELSE
               MOVE    ZERO            TO  SPA-GMN-NEXT
           END-IF
      *
           . 
       31011-SELCHKLST-CHKMST-EXT.
           EXIT.
      *****************************************************************
      *    チェック項目リスト編集処理（チェックマスタ２より）
      *****************************************************************
       3101-SELCHKLST-CHKSND-SEC       SECTION.
      *
           MOVE    1                   TO  FLG-SELCHKLST-ROW
      *
           INITIALIZE                  SPA-GMN-SELCHKLST
                                       SPA-NAI-SELCHKLST
      *
           MOVE    ZERO                TO  CNT-SELCHKLST
      *
           IF    ( WRK-MOJI            >   ZERO )
               MOVE    "tbl_chksnd"    TO  WRK-TABLE
               MOVE    "key16"         TO  WRK-PATHNAME
               PERFORM 900-CHKSND-KEY16-SEL-SEC
               IF    ( FLG-CHKSND         =   ZERO )
                   PERFORM 900-CHKSND-FET-SEC
                   IF    ( FLG-CHKSND  =   ZERO )
                       COMPUTE CNT-SELCHKLST
                             = CNT-SELCHKLST   +   1
                   END-IF
               END-IF
               PERFORM 31011-SELCHKLST-CHKSND-SEC
      *
               IF    ( CNT-SELCHKLST   =   ZERO )
                   MOVE    "tbl_chksnd"    TO  WRK-TABLE
                   MOVE    "key17"         TO  WRK-PATHNAME
                   PERFORM 900-CHKSND-KEY17-SEL-SEC
                   IF    ( FLG-CHKSND      =   ZERO )
                       PERFORM 900-CHKSND-FET-SEC
                       IF    ( FLG-CHKSND     =   ZERO )
                           COMPUTE CNT-SELCHKLST = CNT-SELCHKLST   +   1
                       END-IF
                   END-IF
                   PERFORM 31011-SELCHKLST-CHKSND-SEC
               END-IF
      *
               IF    ( CNT-SELCHKLST   =   ZERO )
                AND  ( WRK-MOJI    =   9 )
                AND  ( FLG-KANJI   =   ZERO )
                   MOVE    "tbl_chksnd"    TO  WRK-TABLE
                   MOVE    "key18"         TO  WRK-PATHNAME
                   PERFORM 900-CHKSND-KEY18-SEL-SEC
                   IF    ( FLG-CHKSND         =   ZERO )
                       PERFORM 900-CHKSND-FET-SEC
                       IF    ( FLG-CHKSND     =   ZERO )
                           COMPUTE CNT-SELCHKLST = CNT-SELCHKLST   +   1
                       END-IF
                   END-IF
                   PERFORM 31011-SELCHKLST-CHKSND-SEC
               END-IF
           END-IF
      *
           IF    ( CNT-SELCHKLST   =   ZERO )
               MOVE    "tbl_chksnd"       TO  WRK-TABLE
               MOVE    "key11"         TO  WRK-PATHNAME
               PERFORM 900-CHKSND-KEY11-SEL-SEC
               IF    ( FLG-CHKSND         =   ZERO )
                   PERFORM 900-CHKSND-FET-SEC
                   IF    ( FLG-CHKSND     =   ZERO )
                       COMPUTE CNT-SELCHKLST = CNT-SELCHKLST   +   1
                   END-IF
                   PERFORM 31011-SELCHKLST-CHKSND-SEC
               END-IF
           END-IF
      *
           . 
       3101-SELCHKLST-CHKSND-EXT.
           EXIT.
      *****************************************************************
      *    チェック項目リスト編集処理（チェックマスタ２より）
      *****************************************************************
       31011-SELCHKLST-CHKSND-SEC      SECTION.
      *
      *    読込スキップ処理
           PERFORM 3101-SELCHKLST-SKIP-SEC
      *
           PERFORM UNTIL ( FLG-CHKSND      NOT =   ZERO )
                    OR   ( SPA-GMN-SELCHKLST-MAX
                                          >= CONST-SELCHKLST-MAX )
      *
               COMPUTE SPA-GMN-SELCHKLST-MAX
                   =   SPA-GMN-SELCHKLST-MAX   +   1
      *
               MOVE    SPA-GMN-SELCHKLST-MAX
                                       TO  IDX-LST
      *
               MOVE    CNT-SELCHKLST   TO  SPA-GMN-TSEL-CHKNUM
                                                               (IDX-LST)
      *
               MOVE    CHKSND-SRYCD    TO  WRK-SRYCD
               MOVE    SPA-NAI-KJNYMD
                                       TO  WRK-YMD
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF     (TNS-SRYCD(1:1)      =   "1") AND
                      (TNS-ROUTEKKBN       =   2  )
                   MOVE    SPACE       TO  SPA-GMN-TSEL-NAME
                                                               (IDX-LST)
                   STRING  "［老］"        DELIMITED   BY  SIZE
                           TNS-NAME        DELIMITED   BY  SIZE
                   INTO    SPA-GMN-TSEL-NAME (IDX-LST)
                   END-STRING
               ELSE
                   MOVE    TNS-NAME    TO  SPA-GMN-TSEL-NAME
                                                               (IDX-LST)
               END-IF
      *
               MOVE    CHKSND-YUKOSTYMD
                                       TO  SPA-NAI-TSEL-YUKOSTYMD
                                                               (IDX-LST)
               IF    ( CHKSND-YUKOSTYMD   =   "00000000" )
                   MOVE    CHKSND-YUKOSTYMD
                                       TO  SPA-GMN-TSEL-YUKOSTYMD
                                                           (IDX-LST)
               ELSE
                   MOVE    CHKSND-YUKOSTYMD
                                       TO  WRK-YMD
                   PERFORM 800-SEIWA-HEN-SEC
                   MOVE    WRK-HENYMDG TO  SPA-GMN-TSEL-YUKOSTYMD
                                                           (IDX-LST)
               END-IF
      *
               MOVE    CHKSND-YUKOEDYMD
                                       TO  SPA-NAI-TSEL-YUKOEDYMD
                                                               (IDX-LST)
               IF    ( CHKSND-YUKOEDYMD   =   "99999999" )
                   MOVE    CHKSND-YUKOEDYMD
                                       TO  SPA-GMN-TSEL-YUKOEDYMD
                                                           (IDX-LST)
               ELSE
                   MOVE    CHKSND-YUKOEDYMD
                                       TO  WRK-YMD
                   PERFORM 800-SEIWA-HEN-SEC
                   MOVE    WRK-HENYMDG TO  SPA-GMN-TSEL-YUKOEDYMD
                                                           (IDX-LST)
               END-IF
      *
               MOVE    CHKSND-SRYCD    TO  SPA-NAI-TSEL-SRYCD
                                                           (IDX-LST)
      *
      *        チェックマスタ読込処理
               PERFORM 900-CHKSND-FET-SEC
               IF    ( FLG-CHKSND     =    ZERO )
                   COMPUTE CNT-SELCHKLST   = CNT-SELCHKLST   +   1
               END-IF
      *
           END-PERFORM
      *
           MOVE    WRK-TABLE           TO  MCP-TABLE
           MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( FLG-CHKSND              =   ZERO )
               MOVE    1               TO  SPA-GMN-NEXT
           ELSE
               MOVE    ZERO            TO  SPA-GMN-NEXT
           END-IF
      *
           . 
       31011-SELCHKLST-CHKSND-EXT.
           EXIT.
      *****************************************************************
      *    チェック項目リスト編集処理（チェックマスタ３より）
      *****************************************************************
       3101-SELCHKLST-CHKTRD-SEC       SECTION.
      *
           MOVE    1                   TO  FLG-SELCHKLST-ROW
      *
           INITIALIZE                  SPA-GMN-SELCHKLST
                                       SPA-NAI-SELCHKLST
      *
           MOVE    ZERO                TO  CNT-SELCHKLST
      *
           IF    ( WRK-MOJI            >   ZERO )
               MOVE    "tbl_chktrd"       TO  WRK-TABLE
               MOVE    "key15"         TO  WRK-PATHNAME
               PERFORM 900-CHKTRD-KEY15-SEL-SEC
               IF    ( FLG-CHKTRD         =   ZERO )
                   PERFORM 900-CHKTRD-FET-SEC
                   IF    ( FLG-CHKTRD     =   ZERO )
                       COMPUTE CNT-SELCHKLST
                             = CNT-SELCHKLST   +   1
                   END-IF
               END-IF
               PERFORM 31011-SELCHKLST-CHKTRD-SEC
      *
               IF    ( CNT-SELCHKLST   =   ZERO )
                   MOVE    "tbl_chktrd"   TO  WRK-TABLE
                   MOVE    "key16"     TO  WRK-PATHNAME
                   PERFORM 900-CHKTRD-KEY16-SEL-SEC
                   IF    ( FLG-CHKTRD         =   ZERO )
                       PERFORM 900-CHKTRD-FET-SEC
                       IF    ( FLG-CHKTRD     =   ZERO )
                           COMPUTE CNT-SELCHKLST = CNT-SELCHKLST   +   1
                       END-IF
                   END-IF
                   PERFORM 31011-SELCHKLST-CHKTRD-SEC
               END-IF
      *
               IF    ( CNT-SELCHKLST   =   ZERO )
                AND  ( WRK-MOJI    =   9 )
                AND  ( FLG-KANJI   =   ZERO )
                   MOVE    "tbl_chktrd"   TO  WRK-TABLE
                   MOVE    "key17"     TO  WRK-PATHNAME
                   PERFORM 900-CHKTRD-KEY17-SEL-SEC
                   IF    ( FLG-CHKTRD         =   ZERO )
                       PERFORM 900-CHKTRD-FET-SEC
                       IF    ( FLG-CHKTRD     =   ZERO )
                           COMPUTE CNT-SELCHKLST = CNT-SELCHKLST   +   1
                       END-IF
                   END-IF
                   PERFORM 31011-SELCHKLST-CHKTRD-SEC
               END-IF
           END-IF
      *
           IF    ( CNT-SELCHKLST   =   ZERO )
               MOVE    "tbl_chktrd"       TO  WRK-TABLE
               MOVE    "key11"         TO  WRK-PATHNAME
               PERFORM 900-CHKTRD-KEY11-SEL-SEC
               IF    ( FLG-CHKTRD         =   ZERO )
                   PERFORM 900-CHKTRD-FET-SEC
                   IF    ( FLG-CHKTRD     =   ZERO )
                       COMPUTE CNT-SELCHKLST = CNT-SELCHKLST   +   1
                   END-IF
                   PERFORM 31011-SELCHKLST-CHKTRD-SEC
               END-IF
           END-IF
      *
           . 
       3101-SELCHKLST-CHKTRD-EXT.
           EXIT.
      *****************************************************************
      *    チェック項目リスト編集処理（チェックマスタ３より）
      *****************************************************************
       31011-SELCHKLST-CHKTRD-SEC       SECTION.
      *
      *    読込スキップ処理
           PERFORM 3101-SELCHKLST-SKIP-SEC
      *
           PERFORM UNTIL ( FLG-CHKTRD      NOT =   ZERO )
                    OR   ( SPA-GMN-SELCHKLST-MAX
                                          >= CONST-SELCHKLST-MAX )
      *
               COMPUTE SPA-GMN-SELCHKLST-MAX
                   =   SPA-GMN-SELCHKLST-MAX   +   1
      *
               MOVE    SPA-GMN-SELCHKLST-MAX
                                       TO  IDX-LST
      *
               MOVE    CNT-SELCHKLST   TO  SPA-GMN-TSEL-CHKNUM
                                                               (IDX-LST)
      *
               MOVE    CHKTRD-SRYCD    TO  WRK-SRYCD
               MOVE    SPA-NAI-KJNYMD
                                       TO  WRK-YMD
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF     (TNS-SRYCD(1:1)      =   "1") AND
                      (TNS-ROUTEKKBN       =   2  )
                   MOVE    SPACE       TO  SPA-GMN-TSEL-NAME
                                                               (IDX-LST)
                   STRING  "［老］"        DELIMITED   BY  SIZE
                           TNS-NAME        DELIMITED   BY  SIZE
                   INTO    SPA-GMN-TSEL-NAME (IDX-LST)
                   END-STRING
               ELSE
                   MOVE    TNS-NAME    TO  SPA-GMN-TSEL-NAME
                                                               (IDX-LST)
               END-IF
      *
               MOVE    CHKTRD-YUKOSTYMD
                                       TO  SPA-NAI-TSEL-YUKOSTYMD
                                                               (IDX-LST)
               IF    ( CHKTRD-YUKOSTYMD   =   "00000000" )
                   MOVE    CHKTRD-YUKOSTYMD
                                       TO  SPA-GMN-TSEL-YUKOSTYMD
                                                           (IDX-LST)
               ELSE
                   MOVE    CHKTRD-YUKOSTYMD
                                       TO  WRK-YMD
                   PERFORM 800-SEIWA-HEN-SEC
                   MOVE    WRK-HENYMDG TO  SPA-GMN-TSEL-YUKOSTYMD
                                                           (IDX-LST)
               END-IF
      *
               MOVE    CHKTRD-YUKOEDYMD
                                       TO  SPA-NAI-TSEL-YUKOEDYMD
                                                               (IDX-LST)
               IF    ( CHKTRD-YUKOEDYMD   =   "99999999" )
                   MOVE    CHKTRD-YUKOEDYMD
                                       TO  SPA-GMN-TSEL-YUKOEDYMD
                                                           (IDX-LST)
               ELSE
                   MOVE    CHKTRD-YUKOEDYMD
                                       TO  WRK-YMD
                   PERFORM 800-SEIWA-HEN-SEC
                   MOVE    WRK-HENYMDG TO  SPA-GMN-TSEL-YUKOEDYMD
                                                           (IDX-LST)
               END-IF
      *
               MOVE    CHKTRD-SRYCD    TO  SPA-NAI-TSEL-SRYCD
                                                           (IDX-LST)
      *
      *        チェックマスタ読込処理
               PERFORM 900-CHKTRD-FET-SEC
               IF    ( FLG-CHKTRD     =    ZERO )
                   COMPUTE CNT-SELCHKLST   = CNT-SELCHKLST   +   1
               END-IF
      *
           END-PERFORM
      *
           MOVE    WRK-TABLE              TO  MCP-TABLE
           MOVE    WRK-PATHNAME           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( FLG-CHKTRD              =   ZERO )
               MOVE    1               TO  SPA-GMN-NEXT
           ELSE
               MOVE    ZERO            TO  SPA-GMN-NEXT
           END-IF
      *
           . 
       31011-SELCHKLST-CHKTRD-EXT.
           EXIT.
      *****************************************************************
      *    チェック項目リスト編集処理（チェックマスタ５より）
      *****************************************************************
       3101-SELCHKLST-CHK005-SEC       SECTION.
      *
           MOVE    1                   TO  FLG-SELCHKLST-ROW
      *
           INITIALIZE                  SPA-GMN-SELCHKLST
                                       SPA-NAI-SELCHKLST
      *
           MOVE    ZERO                TO  CNT-SELCHKLST
      *
      *    チェックマスタ検索
           PERFORM 900-CHK005-KEY7-SEL-SEC
           IF    ( FLG-CHK005         =   ZERO )
               PERFORM 900-CHK005-KEY7-FET-SEC
               IF    ( FLG-CHK005     =   ZERO )
                   COMPUTE CNT-SELCHKLST  = CNT-SELCHKLST   +   1
               END-IF
           END-IF
      *
      *    読込スキップ処理
           PERFORM 3101-SELCHKLST-SKIP-SEC
      *
           PERFORM UNTIL ( FLG-CHK005      NOT =   ZERO )
                    OR   ( SPA-GMN-SELCHKLST-MAX
                                          >= CONST-SELCHKLST-MAX )
      *
               COMPUTE SPA-GMN-SELCHKLST-MAX
                   =   SPA-GMN-SELCHKLST-MAX   +   1
      *
               MOVE    SPA-GMN-SELCHKLST-MAX
                                       TO  IDX-LST
      *
               MOVE    CNT-SELCHKLST   TO  SPA-GMN-TSEL-CHKNUM
                                                               (IDX-LST)
      *
               MOVE    CHK005-BYOMEI   TO  SPA-GMN-TSEL-NAME
                                                               (IDX-LST)
      *
               MOVE    CHK005-YUKOSTYMD
                                       TO  SPA-NAI-TSEL-YUKOSTYMD
                                                               (IDX-LST)
               IF    ( CHK005-YUKOSTYMD   =   "00000000" )
                   MOVE    CHK005-YUKOSTYMD
                                       TO  SPA-GMN-TSEL-YUKOSTYMD
                                                           (IDX-LST)
               ELSE
                   MOVE    CHK005-YUKOSTYMD
                                       TO  WRK-YMD
                   PERFORM 800-SEIWA-HEN-SEC
                   MOVE    WRK-HENYMDG TO  SPA-GMN-TSEL-YUKOSTYMD
                                                           (IDX-LST)
               END-IF
      *
               MOVE    CHK005-YUKOEDYMD
                                       TO  SPA-NAI-TSEL-YUKOEDYMD
                                                               (IDX-LST)
               IF    ( CHK005-YUKOEDYMD   =   "99999999" )
                   MOVE    CHK005-YUKOEDYMD
                                       TO  SPA-GMN-TSEL-YUKOEDYMD
                                                           (IDX-LST)
               ELSE
                   MOVE    CHK005-YUKOEDYMD
                                       TO  WRK-YMD
                   PERFORM 800-SEIWA-HEN-SEC
                   MOVE    WRK-HENYMDG TO  SPA-GMN-TSEL-YUKOEDYMD
                                                           (IDX-LST)
               END-IF
      *
               MOVE    CHK005-ID       TO  SPA-NAI-TSEL-ID
                                                           (IDX-LST)
      *
      *        チェックマスタ読込処理
               PERFORM 900-CHK005-KEY7-FET-SEC
               IF    ( FLG-CHK005     =    ZERO )
                   COMPUTE CNT-SELCHKLST   = CNT-SELCHKLST   +   1
               END-IF
      *
           END-PERFORM
      *
           MOVE    "tbl_chk005"           TO  MCP-TABLE
           MOVE    "key7"                 TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( FLG-CHK005              =   ZERO )
               MOVE    1               TO  SPA-GMN-NEXT
           ELSE
               MOVE    ZERO            TO  SPA-GMN-NEXT
           END-IF
      *
           . 
       3101-SELCHKLST-CHK005-EXT.
           EXIT.
      *****************************************************************
      *    チェック項目リスト編集処理（適応病名マスタより）
      *****************************************************************
       3101-SELCHKLST-TEKBYO-SEC       SECTION.
      *
           MOVE    1                   TO  FLG-SELCHKLST-ROW
      *
           INITIALIZE                  SPA-GMN-SELCHKLST
                                       SPA-NAI-SELCHKLST
      *
           MOVE    ZERO                TO  CNT-SELCHKLST
      *
      *    適応病名マスタ検索
           PERFORM 900-TEKIOUBYOMEI-KEY4-SEL-SEC
           IF    ( FLG-TEKIOUBYOMEI    =   ZERO )
               PERFORM 900-TEKIOUBYOMEI-KEY4-FET-SEC
               IF    ( FLG-TEKIOUBYOMEI   =   ZERO )
                   COMPUTE CNT-SELCHKLST  = CNT-SELCHKLST   +   1
               END-IF
           END-IF
      *
      *    読込スキップ処理
           PERFORM 3101-SELCHKLST-SKIP-SEC
      *
           PERFORM UNTIL ( FLG-TEKIOUBYOMEI    NOT =   ZERO )
                    OR   ( SPA-GMN-SELCHKLST-MAX
                                          >= CONST-SELCHKLST-MAX )
      *
               COMPUTE SPA-GMN-SELCHKLST-MAX
                   =   SPA-GMN-SELCHKLST-MAX   +   1
      *
               MOVE    SPA-GMN-SELCHKLST-MAX
                                       TO  IDX-LST
      *
               MOVE    CNT-SELCHKLST   TO  SPA-GMN-TSEL-CHKNUM
                                                               (IDX-LST)
      *
               MOVE    TEKBYO-SRYCD    TO  WRK-SRYCD
               MOVE    SPA-NAI-KJNYMD
                                       TO  WRK-YMD
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF     (TNS-SRYCD(1:1)      =   "1") AND
                      (TNS-ROUTEKKBN       =   2  )
                   MOVE    SPACE       TO  SPA-GMN-TSEL-NAME
                                                               (IDX-LST)
                   STRING  "［老］"        DELIMITED   BY  SIZE
                           TNS-NAME        DELIMITED   BY  SIZE
                   INTO    SPA-GMN-TSEL-NAME (IDX-LST)
                   END-STRING
               ELSE
                   MOVE    TNS-NAME    TO  SPA-GMN-TSEL-NAME
                                                               (IDX-LST)
               END-IF
      *
               MOVE    "00000000"      TO  SPA-NAI-TSEL-YUKOSTYMD
                                                               (IDX-LST)
                                           SPA-GMN-TSEL-YUKOSTYMD
                                                           (IDX-LST)
               MOVE    "99999999"      TO  SPA-NAI-TSEL-YUKOEDYMD
                                                               (IDX-LST)
                                           SPA-GMN-TSEL-YUKOEDYMD
                                                           (IDX-LST)
      *
               MOVE    TEKBYO-SRYCD    TO  SPA-NAI-TSEL-SRYCD
                                                           (IDX-LST)
      *
      *        適応病名マスタ読込処理
               PERFORM 900-TEKIOUBYOMEI-KEY4-FET-SEC
               IF    ( FLG-TEKIOUBYOMEI    =    ZERO )
                   COMPUTE CNT-SELCHKLST   = CNT-SELCHKLST   +   1
               END-IF
      *
           END-PERFORM
      *
           MOVE    "tbl_tekioubyomei"      TO  MCP-TABLE
           MOVE    "key4"                  TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( FLG-TEKIOUBYOMEI    =   ZERO )
               MOVE    1               TO  SPA-GMN-NEXT
           ELSE
               MOVE    ZERO            TO  SPA-GMN-NEXT
           END-IF
      *
           . 
       3101-SELCHKLST-TEKBYO-EXT.
           EXIT.
      *****************************************************************
      *    読込スキップ処理
      *****************************************************************
       3101-SELCHKLST-SKIP-SEC         SECTION.
      *
           COMPUTE WRK-SKIP-MAX    = ( SPA-GMN-PAGE    -   1 )
                                   *   CONST-SELCHKLST-MAX
      *
           EVALUATE    SPA-GMN-CHKKBN
           WHEN    "1"
           WHEN    "2"
           WHEN    "6"
               PERFORM UNTIL ( FLG-CHKSND  NOT =   ZERO )
                        OR   ( CNT-SELCHKLST   >   WRK-SKIP-MAX )
      *
                   PERFORM 900-CHKSND-FET-SEC
                   IF    ( FLG-CHKSND      =   ZERO )
                       COMPUTE CNT-SELCHKLST = CNT-SELCHKLST   +   1
                   END-IF
      *
               END-PERFORM
           WHEN    "M"
               PERFORM UNTIL ( FLG-TEKIOUBYOMEI    NOT =   ZERO )
                        OR   ( CNT-SELCHKLST   >   WRK-SKIP-MAX )
      *
                   PERFORM 900-TEKIOUBYOMEI-KEY4-FET-SEC
                   IF    ( FLG-TEKIOUBYOMEI    =   ZERO )
                       COMPUTE CNT-SELCHKLST = CNT-SELCHKLST   +   1
                   END-IF
      *
               END-PERFORM
           WHEN    "7"
               PERFORM UNTIL ( FLG-CHKTRD  NOT =   ZERO )
                        OR   ( CNT-SELCHKLST   >   WRK-SKIP-MAX )
      *
                   PERFORM 900-CHKTRD-FET-SEC
                   IF    ( FLG-CHKTRD      =   ZERO )
                       COMPUTE CNT-SELCHKLST = CNT-SELCHKLST   +   1
                   END-IF
      *
               END-PERFORM
           WHEN    "8"
           WHEN    "9"
               PERFORM UNTIL ( FLG-CHK005  NOT =   ZERO )
                        OR   ( CNT-SELCHKLST   >   WRK-SKIP-MAX )
      *
                   PERFORM 900-CHK005-KEY7-FET-SEC
                   IF    ( FLG-CHK005      =   ZERO )
                       COMPUTE CNT-SELCHKLST = CNT-SELCHKLST   +   1
                   END-IF
      *
               END-PERFORM
           WHEN    OTHER
               PERFORM UNTIL ( FLG-CHK     NOT =   ZERO )
                        OR   ( CNT-SELCHKLST   >   WRK-SKIP-MAX )
      *
                   PERFORM 900-CHK-FET-SEC
                   IF    ( FLG-CHK     =   ZERO )
                       COMPUTE CNT-SELCHKLST = CNT-SELCHKLST   +   1
                   END-IF
      *
               END-PERFORM
           END-EVALUATE
      *
           . 
       3101-SELCHKLST-SKIP-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                     SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *    戻る
           WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *    追加
           WHEN    "CLICKED"       ALSO    "BTN_INS"
               PERFORM 4101-LCHKLST-INS-SEC
      *    全て追加
           WHEN    "CLICKED"       ALSO    "BTN_ALLINS"
               PERFORM 4101-LCHKLST-ALLINS-SEC
      *    削除
           WHEN    "CLICKED"       ALSO    "BTN_DEL"
               PERFORM 4101-LCHKLST-DEL-SEC
      *    選択解除
           WHEN    "CLICKED"       ALSO    "B09S"
               PERFORM 490-CANCEL-SEL-SEC
      *    前頁
           WHEN    "CLICKED"       ALSO    "B09"
           WHEN    "CLICKED"       ALSO    "B09_2"
               PERFORM 490-PAGE-PREV-SEC
      *    次頁
           WHEN    "CLICKED"       ALSO    "B10"
           WHEN    "CLICKED"       ALSO    "B10_2"
               PERFORM 490-PAGE-NEXT-SEC
      *    確定
           WHEN    "CLICKED"       ALSO    "B12"
               PERFORM 490-KAKUTEI-SEC
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
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *    基準日
           WHEN    "ACTIVATE"      ALSO    "KJNYMD"
               PERFORM 4100-KJNYMD-CHK-SEC
           WHEN        ANY         ALSO    "LCHKLST"
               PERFORM 4101-LCHKLST-SEL-SEC
           WHEN        ANY         ALSO    "RCHKLST"
               PERFORM 4101-RCHKLST-SEL-SEC
           WHEN        ANY         ALSO    "SELCHKLST"
               PERFORM 4101-SELCHKLST-SEL-SEC
           WHEN    OTHER
      *
               MOVE    ZERO            TO  SPA-GMN-CUR
      *
      *        入力チェック処理
               PERFORM 410-INPUT-CHK-SEC
      *
           END-EVALUATE
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC       SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  410-INPUT-CHK-EXT
           END-IF
      *
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面をＳＰＡセット処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC            SECTION.
      *
           MOVE    X03-CHKKBN          TO  SPA-GMN-CHKKBN-G
           MOVE    X03-SELNAME         TO  SPA-GMN-SELNAME
      *
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *****************************************************************
      *    基準日チェック処理
      *****************************************************************
       4100-KJNYMD-CHK-SEC             SECTION.
      *
           MOVE    X03-KJNYMD          TO  SPA-GMN-KJNYMD
      *
           PERFORM 4101-KJNYMD-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4100-KJNYMD-CHK-EXT
           END-IF
      *
           MOVE    1                   TO  SPA-GMN-PAGE
           PERFORM 3101-SELCHKLST-HEN-SEC
      *
           .
       4100-KJNYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC              SECTION.
      *
      *    基準日チェック
           PERFORM 4101-KJNYMD-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    チェック区分チェック
           PERFORM 4101-CHKKBN-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    チェック名称
           PERFORM 4101-SELNAME-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    基準日チェック処理
      *****************************************************************
       4101-KJNYMD-CHK-SEC             SECTION.
      *
           MOVE    SPA-NAI-KJNYMD  TO  WRK-KJNYMD-MOT
      *
           MOVE    SPACE               TO  SPA-NAI-KJNYMD
      *
           IF    ( SPA-GMN-KJNYMD (1:3) = SPACE )
               MOVE    SPA-X01-KJNYMD  TO  SPA-GMN-KJNYMD
           END-IF
      *
           IF    ( SPA-GMN-KJNYMD (1:3) = "000" )
               MOVE    "00000000"      TO   SPA-GMN-KJNYMD
           END-IF
      *
           IF    ( SPA-GMN-KJNYMD (1:3) = "999" )
               MOVE    "99999999"      TO   SPA-GMN-KJNYMD
           END-IF
      *
           IF    ( SPA-GMN-KJNYMD  =   "00000000"
                                       OR  "99999999" )
               MOVE    SPA-GMN-KJNYMD  TO  SPA-NAI-KJNYMD
               GO  TO  4101-KJNYMD-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-KJNYMD  TO  WRK-YMD
           PERFORM 800-SEIWA-HEN-SEC
           IF    ( FLG-HIZUKE-ERR  NOT =   ZERO )
               MOVE    WRK-KJNYMD-MOT  TO  SPA-NAI-KJNYMD
               IF    ( SPA-NAI-KJNYMD  =   "00000000"
                                           OR  "99999999" )
                   MOVE    SPA-NAI-KJNYMD
                                       TO  SPA-GMN-KJNYMD
               ELSE
                   MOVE    SPA-NAI-KJNYMD  TO  WRK-YMD
                   PERFORM 800-SEIWA-HEN-SEC
                   MOVE    WRK-HENYMDG TO  SPA-GMN-KJNYMD
               END-IF
      *
               MOVE    "0001"      TO  SPA-ERRCD
               MOVE    3           TO  SPA-GMN-CUR
               GO  TO  4101-KJNYMD-CHK-EXT
           END-IF
      *
           MOVE    WRK-SYMD        TO  SPA-NAI-KJNYMD
           MOVE    WRK-HENYMDG     TO  SPA-GMN-KJNYMD
      *
           .
       4101-KJNYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェック区分チェック処理
      *****************************************************************
       4101-CHKKBN-CHK-SEC             SECTION.
      *
           IF    ( SPA-GMN-CHKKBN-MAX  =   1 )
               MOVE    SPA-GMN-CHKKBNLST (1)
                                       TO  SPA-GMN-CHKKBN-G
               GO  TO  4101-CHKKBN-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-CHKKBN  TO  WRK-CMB-CD
           PERFORM 4200-CHKKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM     NOT =   SPACE )
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-CHKKBN-G
           ELSE
               MOVE    "0002"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-GMN-PAGE
               PERFORM 3101-SELCHKLST-HEN-SEC
               GO  TO  4101-CHKKBN-CHK-EXT
           END-IF
      *
           IF    ( WRK-MCP-WIDGET      =   "CHKKBN" )
               MOVE    1               TO  SPA-GMN-PAGE
               PERFORM 3101-SELCHKLST-HEN-SEC
           END-IF
      *
           .
       4101-CHKKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェック名称チェック処理
      *****************************************************************
       4101-SELNAME-CHK-SEC            SECTION.
      *
           IF    ( SPA-GMN-SELNAME NOT =   SPACE )
               MOVE    SPA-GMN-SELNAME TO  WRK-KANACHK-MAE-INPUT
               PERFORM 800-ENRTRY-CHK-SEC
               MOVE    WRK-KANACHK-OUT-INPUT
                                           TO  SPA-GMN-SELNAME
           END-IF
      *
           IF    ( WRK-MCP-WIDGET          =   "SELNAME" )
               MOVE    1                   TO  SPA-GMN-PAGE
      *
               IF  ( SPA-ERRCD             =   SPACE )
                   PERFORM 3101-SELCHKLST-HEN-SEC
               END-IF
           END-IF
      *
           .
       4101-SELNAME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェック元リスト選択処理
      *****************************************************************
       4101-LCHKLST-SEL-SEC            SECTION.
      *
           PERFORM VARYING IDX4    FROM    1   BY  1
                   UNTIL ( IDX4   >   SPA-GMN-LCHKLST-MAX )
      *
               IF    ( X03-LCHKLST-SELECT (IDX4)   =   "T" )
                   IF    ( SPA-NAI-TLCHK-DATAKBN (IDX4)
                                                   =   "1" )
                       MOVE    ZERO    TO  SPA-GMN-LCHKLST-FLG(IDX4)
                   ELSE
                       MOVE    1       TO  SPA-GMN-LCHKLST-FLG(IDX4)
                   END-IF
               ELSE
                   MOVE    ZERO    TO  SPA-GMN-LCHKLST-FLG (IDX4)
               END-IF
      *
           END-PERFORM
      *
      *    チェック先リストの選択状態を解除
           PERFORM VARYING IDX4    FROM    1   BY  1
                   UNTIL ( IDX4   >   SPA-GMN-RCHKLST-MAX )
      *
               MOVE    ZERO    TO  SPA-GMN-RCHKLST-FLG (IDX4)
      *
           END-PERFORM
      *
           .
       4101-RCHKLST-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェック先リスト選択処理
      *****************************************************************
       4101-RCHKLST-SEL-SEC            SECTION.
      *
           PERFORM VARYING IDX4    FROM    1   BY  1
                   UNTIL ( IDX4   >   SPA-GMN-RCHKLST-MAX )
      *
               IF    ( X03-RCHKLST-SELECT (IDX4)   =   "T" )
                AND  ( SPA-NAI-TRCHK-DELFLG  (IDX4)
                                                   =   ZERO )
                AND  ( SPA-NAI-TRCHK-SRYCD   (IDX4)
                                               NOT =   SPA-X01-SRYCD )
                   MOVE    1       TO  SPA-GMN-RCHKLST-FLG (IDX4)
               ELSE
                   MOVE    ZERO    TO  SPA-GMN-RCHKLST-FLG (IDX4)
               END-IF
      *
           END-PERFORM
      *
      *    チェック元リストの選択状態を解除
           PERFORM VARYING IDX4    FROM    1   BY  1
                   UNTIL ( IDX4   >   SPA-GMN-LCHKLST-MAX )
      *
               MOVE    ZERO    TO  SPA-GMN-LCHKLST-FLG (IDX4)
      *
           END-PERFORM
      *
           .
       4101-RCHKLST-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェック名称リスト選択処理
      *****************************************************************
       4101-SELCHKLST-SEL-SEC          SECTION.
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   SPA-GMN-SELCHKLST-MAX )
      *
               IF    ( X03-SELCHKLST-SELECT (IDX2) =   "T" )
                   MOVE    IDX2    TO  SPA-GMN-SELCHKLST-SEL
      *
                   MOVE    SPA-GMN-CHKKBN-G
                                   TO  SPA-GMN-RCHKKBNMEI
      *
                   MOVE    SPA-GMN-TSEL-NAME (IDX2)
                                   TO  SPA-GMN-RNAME
      *
                   MOVE    SPA-GMN-TSEL-YUKOSTYMD (IDX2)
                                   TO  SPA-GMN-RYUKOSTYMD
                   MOVE    SPA-NAI-TSEL-YUKOSTYMD (IDX2)
                                   TO  SPA-NAI-RYUKOSTYMD
      *
                   MOVE    SPA-GMN-TSEL-YUKOEDYMD (IDX2)
                                   TO  SPA-GMN-RYUKOEDYMD
                   MOVE    SPA-NAI-TSEL-YUKOEDYMD (IDX2)
                                   TO  SPA-NAI-RYUKOEDYMD
      *
                   MOVE    SPA-NAI-TSEL-SRYCD (IDX2)
                                   TO  WRK-RCHKLST-SRYCD
      *
                   MOVE    SPA-NAI-TSEL-ID (IDX2)
                                   TO  WRK-RCHKLST-ID
      *            チェック先リスト編集処理
                   EVALUATE    SPA-GMN-CHKKBN
                   WHEN  "1"
                   WHEN  "2"
                   WHEN  "6"
                       PERFORM 41011-RCHKLST-CHKSND-HEN-SEC
                   WHEN  "M"
                       PERFORM 41011-RCHKLST-TEKBYO-HEN-SEC
                   WHEN  "7"
                       PERFORM 41011-RCHKLST-CHKTRD-HEN-SEC
                   WHEN  "8"
                   WHEN  "9"
                       PERFORM 41011-RCHKLST-CHK005-HEN-SEC
                   WHEN  OTHER
                       PERFORM 41011-RCHKLST-CHKMST-HEN-SEC
                   END-EVALUATE
                   MOVE    SPA-GMN-SELCHKLST-MAX
                                       TO  IDX2
               END-IF
      *
           END-PERFORM
      *
           .
       4101-SELCHKLST-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェック先リスト編集処理（チェックマスタ２より)
      *****************************************************************
       41011-RCHKLST-CHKSND-HEN-SEC    SECTION.
      *
           MOVE    1                   TO  FLG-RCHKLST-ROW
      *
           INITIALIZE                      SPA-GMN-RCHKLST
                                           SPA-NAI-RCHKLST
      *
           MOVE    ZERO                TO  FLG-CHKSND
      *
      *    チェックマスタ読み込み 
           INITIALIZE                  CHKSND-REC
           MOVE    SPA-GMN-CHKKBN  TO  CHKSND-CHKKBN
           MOVE    WRK-RCHKLST-SRYCD   TO  CHKSND-SRYCD
           MOVE    SPA-NAI-KJNYMD  TO  CHKSND-YUKOSTYMD
                                           CHKSND-YUKOEDYMD
           MOVE    SPA-HOSPNUM         TO  CHKSND-HOSPNUM
           MOVE    CHKSND-REC          TO  MCPDATA-REC
           MOVE    "tbl_chksnd"            TO  MCP-TABLE
           MOVE    "key2"                  TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHKSND-REC
           ELSE
               MOVE    1               TO  FLG-CHKSND
               INITIALIZE                  CHKSND-REC
           END-IF
      *
           PERFORM UNTIL ( FLG-CHKSND  NOT =   ZERO )
                   OR    ( SPA-GMN-RCHKLST-MAX
                                          >=   CONST-RCHKLST-MAX )
      *
             IF    ( CHKSND-BYOMEI     NOT =   SPACE )
      *
               COMPUTE SPA-GMN-RCHKLST-MAX
                   =   SPA-GMN-RCHKLST-MAX +   1
      *
               MOVE    SPA-GMN-RCHKLST-MAX
                                           TO  IDXA
      *
               MOVE    IDXA                TO  SPA-GMN-TRCHK-NUM
                                                                  (IDXA)
               MOVE    CHKSND-CD           TO  WRK-KEY-CD-G
               MOVE    WRK-KEY-CD (1)      TO  SPA-NAI-TRCHK-SRYCD
                                                                  (IDXA)
               PERFORM VARYING IDX7    FROM    1   BY  1
                       UNTIL ( IDX7    >   CONST-MKCD-MAX )
      *
                   MOVE    WRK-KEY-CD (IDX7)
                                           TO  SPA-NAI-TRCHK-BYOCD
                                                             (IDXA IDX7)
      *
               END-PERFORM
      *
               MOVE    CHKSND-BYOMEI       TO  SPA-GMN-TRCHK-NAME
                                                               (IDXA)
               MOVE    ZERO                TO  SPA-NAI-TRCHK-DELFLG
                                                               (IDXA)
      *
               MOVE    CHKSND-COMPKBN      TO  SPA-GMN-TRCHK-JYKN
                                                               (IDXA)
      *
               MOVE    CHKSND-GRPID        TO  SPA-GMN-TRCHK-GRPID
                                                               (IDXA)
      *
               MOVE    CHKSND-BYOMEIHENFLG 
                                       TO  SPA-NAI-TRCHK-BYOSTATUS
                                                               (IDXA)
               IF    ( SPA-X01-CHKKBN  =   "1" OR "2" OR "6" )
                   IF    ( SPA-X01-CHKKBN  =  SPA-GMN-CHKKBN )
                       MOVE    CHKSND-UTAGAIKBN
                                       TO  SPA-NAI-TRCHK-UTAGAI
                                                               (IDXA)
                   ELSE
                       MOVE    SPA-GMN-DEFAULT-UTAGAI
                                       TO  SPA-NAI-TRCHK-UTAGAI
                                                               (IDXA)
                   END-IF
               END-IF
      *
               MOVE    CHKSND-HOSOKU    TO  SPA-GMN-TRCHK-HOSOKU
                                                               (IDXA)
               MOVE    CHKSND-HOSOKU-COMPKBN
                                        TO SPA-GMN-TRCHK-HOSOKU-JYKN
                                                               (IDXA)
             END-IF
      *
               MOVE    "tbl_chksnd"            TO  MCP-TABLE
               MOVE    "key2"                  TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC TO  CHKSND-REC
               ELSE
                   MOVE    1           TO  FLG-CHKSND
                   INITIALIZE          CHKSND-REC
               END-IF
      *
           END-PERFORM
      *
      *    カーソルクロース
           MOVE    "tbl_chksnd"            TO  MCP-TABLE
           MOVE    "key2"                  TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       41011-RCHKLST-CHKSND-HEN-EXT.
           EXIT.
      *****************************************************************
      *    チェック先リスト編集処理（チェックマスタ３より)
      *****************************************************************
       41011-RCHKLST-CHKTRD-HEN-SEC    SECTION.
      *
           MOVE    1                   TO  FLG-RCHKLST-ROW
      *
           INITIALIZE                      SPA-GMN-RCHKLST
                                           SPA-NAI-RCHKLST
      *
           MOVE    ZERO                TO  FLG-CHKTRD
      *
      *    チェックマスタ読み込み 
           INITIALIZE                  CHKTRD-REC
           MOVE    SPA-GMN-CHKKBN  TO  CHKTRD-CHKKBN
           MOVE    WRK-RCHKLST-SRYCD   TO  CHKTRD-SRYCD
           MOVE    SPA-NAI-KJNYMD  TO  CHKTRD-YUKOSTYMD
                                           CHKTRD-YUKOEDYMD
           MOVE    SPA-HOSPNUM         TO  CHKTRD-HOSPNUM
           MOVE    CHKTRD-REC          TO  MCPDATA-REC
           MOVE    "tbl_chktrd"            TO  MCP-TABLE
           MOVE    "key2"                  TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHKTRD-REC
           ELSE
               MOVE    1               TO  FLG-CHKTRD
               INITIALIZE                  CHKTRD-REC
           END-IF
      *
           PERFORM UNTIL ( FLG-CHKTRD  NOT =   ZERO )
                   OR    ( SPA-GMN-RCHKLST-MAX
                                          >=   CONST-RCHKLST-MAX )
      *
               COMPUTE SPA-GMN-RCHKLST-MAX
                   =   SPA-GMN-RCHKLST-MAX +   1
      *
               MOVE    SPA-GMN-RCHKLST-MAX
                                           TO  IDXA
      *
               MOVE    IDXA                TO  SPA-GMN-TRCHK-NUM
                                                                  (IDXA)
               MOVE    CHKTRD-CD           TO  SPA-NAI-TRCHK-SRYCD
                                                                  (IDXA)
      *
      *        点数マスタ読み込み
               MOVE    CHKTRD-CD           TO  WRK-CHK-CD
               PERFORM 41011-TNS-NAME-HEN-SEC
               MOVE    WRK-NAME            TO  SPA-GMN-TRCHK-NAME
                                                               (IDXA)
               MOVE    FLG-SRYCD-DEL
                                           TO  SPA-NAI-TRCHK-DELFLG
                                                               (IDXA)
      *
               MOVE    CHKTRD-DAYMONTHKBN  TO  SPA-GMN-TRCHK-JYKN
                                                               (IDXA)
      *
               MOVE    "tbl_chktrd"            TO  MCP-TABLE
               MOVE    "key2"                  TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC TO  CHKTRD-REC
               ELSE
                   MOVE    1           TO  FLG-CHKTRD
                   INITIALIZE          CHKTRD-REC
               END-IF
      *
           END-PERFORM
      *
      *    カーソルクロース
           MOVE    "tbl_chktrd"            TO  MCP-TABLE
           MOVE    "key2"                  TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       41011-RCHKLST-CHKTRD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    チェック先リスト編集処理（チェックマスタ３より)
      *****************************************************************
       41011-RCHKLST-CHK005-HEN-SEC    SECTION.
      *
           MOVE    1                   TO  FLG-RCHKLST-ROW
      *
           INITIALIZE                      SPA-GMN-RCHKLST
                                           SPA-NAI-RCHKLST
      *
           MOVE    ZERO                TO  FLG-CHK005
      *
      *    チェックマスタ読み込み 
           INITIALIZE                  CHK005-REC
           MOVE    SPA-GMN-CHKKBN  TO  CHK005-CHKKBN
           MOVE    WRK-RCHKLST-ID      TO  CHK005-ID
           MOVE    SPA-NAI-KJNYMD  TO  CHK005-YUKOSTYMD
                                           CHK005-YUKOEDYMD
           MOVE    "2"                 TO  CHK005-CDKBN
           MOVE    SPA-HOSPNUM         TO  CHK005-HOSPNUM
           MOVE    CHK005-REC          TO  MCPDATA-REC
           MOVE    "tbl_chk005"            TO  MCP-TABLE
           MOVE    "key3"                  TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHK005-REC
           ELSE
               MOVE    1               TO  FLG-CHK005
               INITIALIZE                  CHK005-REC
           END-IF
      *
           PERFORM UNTIL ( FLG-CHK005  NOT =   ZERO )
                   OR    ( SPA-GMN-RCHKLST-MAX
                                          >=   CONST-RCHKLST-MAX )
      *
               COMPUTE SPA-GMN-RCHKLST-MAX
                   =   SPA-GMN-RCHKLST-MAX +   1
      *
               MOVE    SPA-GMN-RCHKLST-MAX
                                           TO  IDXA
      *
               MOVE    IDXA                TO  SPA-GMN-TRCHK-NUM
                                                                  (IDXA)
               MOVE    CHK005-SRYCD        TO  SPA-NAI-TRCHK-SRYCD
                                                                  (IDXA)
      *
      *        点数マスタ読み込み
               MOVE    CHK005-SRYCD        TO  WRK-CHK-CD
               PERFORM 41011-TNS-NAME-HEN-SEC
               MOVE    WRK-NAME            TO  SPA-GMN-TRCHK-NAME
                                                               (IDXA)
               MOVE    FLG-SRYCD-DEL
                                           TO  SPA-NAI-TRCHK-DELFLG
                                                               (IDXA)
      *
               MOVE    CHK005-DAYMONTHKBN  TO  SPA-GMN-TRCHK-JYKN
                                                               (IDXA)
      *
               MOVE    "tbl_chk005"            TO  MCP-TABLE
               MOVE    "key3"                  TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC TO  CHK005-REC
               ELSE
                   MOVE    1           TO  FLG-CHK005
                   INITIALIZE          CHK005-REC
               END-IF
      *
           END-PERFORM
      *
      *    カーソルクロース
           MOVE    "tbl_chk005"            TO  MCP-TABLE
           MOVE    "key3"                  TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       41011-RCHKLST-CHK005-HEN-EXT.
           EXIT.
      *****************************************************************
      *    チェック先リスト編集処理（チェックマスタより)
      *****************************************************************
       41011-RCHKLST-CHKMST-HEN-SEC        SECTION.
      *
           MOVE    1                   TO  FLG-RCHKLST-ROW
      *
           INITIALIZE                      SPA-GMN-RCHKLST
                                           SPA-NAI-RCHKLST
      *
           MOVE    ZERO                TO  FLG-CHK
      *
      *    チェックマスタ読み込み 
           INITIALIZE                  CHK-REC
           MOVE    SPA-GMN-CHKKBN  TO  CHK-CHKKBN
           MOVE    WRK-RCHKLST-SRYCD   TO  CHK-SRYCD
           MOVE    SPA-NAI-KJNYMD  TO  CHK-YUKOSTYMD
                                           CHK-YUKOEDYMD
           MOVE    SPA-HOSPNUM         TO  CHK-HOSPNUM
           MOVE    CHK-REC             TO  MCPDATA-REC
           MOVE    "tbl_chk"           TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHK-REC
           ELSE
               MOVE    1               TO  FLG-CHK
               INITIALIZE              CHK-REC
           END-IF
      *
           PERFORM UNTIL ( FLG-CHK     NOT =   ZERO )
                   OR    ( SPA-GMN-RCHKLST-MAX
                                          >=   CONST-RCHKLST-MAX )
      *
               COMPUTE SPA-GMN-RCHKLST-MAX
                   =   SPA-GMN-RCHKLST-MAX +   1
      *
               MOVE    SPA-GMN-RCHKLST-MAX
                                       TO  IDXA
      *
               MOVE    IDXA            TO  SPA-GMN-TRCHK-NUM  (IDXA)
               MOVE    CHK-CD          TO  SPA-NAI-TRCHK-SRYCD(IDXA)
      *
               EVALUATE    SPA-GMN-CHKKBN
               WHEN    "4"
               WHEN    "5"
               WHEN    "A"
               WHEN    "B"
      *            点数マスタ読み込み
                   MOVE    CHK-CD      TO  WRK-CHK-CD
                   PERFORM 41011-TNS-NAME-HEN-SEC
                   MOVE    WRK-NAME    TO  SPA-GMN-TRCHK-NAME
                                                               (IDXA)
                   MOVE    FLG-SRYCD-DEL
                                       TO  SPA-NAI-TRCHK-DELFLG
                                                               (IDXA)
                   MOVE    SPACE       TO  SPA-NAI-TRCHK-NGBNDKBN
                                                               (IDXA)
                   MOVE    CHK-NYUGAIBNDKBN
                                       TO  SPA-NAI-TRCHK-NGBNDKBN
                                                               (IDXA)
                   MOVE    CHK-CHKERR  TO  SPA-NAI-TRCHK-CHKERR
                                                               (IDXA)
                   MOVE    SPACE       TO  SPA-NAI-TRCHK-DATAKBN
                                                               (IDXA)
               END-EVALUATE
      *
               MOVE    "tbl_chk"       TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC TO  CHK-REC
               ELSE
                   MOVE    1           TO  FLG-CHK
                   INITIALIZE          CHK-REC
               END-IF
      *
           END-PERFORM
      *
      *    カーソルクロース
           MOVE    "tbl_chk"           TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       41011-RCHKLST-CHKMST-HEN-EXT.
           EXIT.
      *****************************************************************
      *    チェック先リスト編集処理（適応病名マスタより)
      *****************************************************************
       41011-RCHKLST-TEKBYO-HEN-SEC    SECTION.
      *
           MOVE    1                   TO  FLG-RCHKLST-ROW
      *
           INITIALIZE                      SPA-GMN-RCHKLST
                                           SPA-NAI-RCHKLST
      *
           MOVE    ZERO                TO  FLG-TEKIOUBYOMEI
      *
      *    適応病名マスタ読み込み 
           INITIALIZE                  TEKIOUBYOMEI-REC
           MOVE    SPA-X01-CHKKBN      TO  TEKBYO-CHKKBN
           MOVE    WRK-RCHKLST-SRYCD   TO  TEKBYO-SRYCD
      *****MOVE    SPA-HOSPNUM         TO  TEKBYO-HOSPNUM
           MOVE    TEKIOUBYOMEI-REC    TO  MCPDATA-REC
           MOVE    "tbl_tekioubyomei"  TO  MCP-TABLE
           MOVE    "key2"                  TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  TEKIOUBYOMEI-REC
           ELSE
               MOVE    1               TO  FLG-TEKIOUBYOMEI
               INITIALIZE                  TEKIOUBYOMEI-REC
           END-IF
      *
           PERFORM UNTIL ( FLG-TEKIOUBYOMEI    NOT =   ZERO )
                   OR    ( SPA-GMN-RCHKLST-MAX
                                          >=   CONST-RCHKLST-MAX )
      *
               COMPUTE SPA-GMN-RCHKLST-MAX
                   =   SPA-GMN-RCHKLST-MAX +   1
      *
               MOVE    SPA-GMN-RCHKLST-MAX
                                           TO  IDXA
      *
               MOVE    IDXA                TO  SPA-GMN-TRCHK-NUM
                                                                  (IDXA)
               MOVE    TEKBYO-BYOMEICD (1) TO  SPA-NAI-TRCHK-SRYCD
                                                                  (IDXA)
               PERFORM VARYING IDX7    FROM    1   BY  1
                       UNTIL ( IDX7    >   CONST-MKCD-MAX )
      *
                   MOVE    TEKBYO-BYOMEICD (IDX7)
                                           TO  SPA-NAI-TRCHK-BYOCD
                                                             (IDXA IDX7)
      *
               END-PERFORM
      *
               IF    ( TEKBYO-BYOMEI       =   SPACE )
                   MOVE    TEKBYO-BYOMEICD (1)
                                       TO  SPA-GMN-TRCHK-NAME
                                                               (IDXA)
                   MOVE    1           TO  SPA-NAI-TRCHK-DELFLG
                                                               (IDXA)
               ELSE
                   MOVE    TEKBYO-BYOMEI
                                       TO  SPA-GMN-TRCHK-NAME
                                                               (IDXA)
                   MOVE    ZERO        TO  SPA-NAI-TRCHK-DELFLG
                                                               (IDXA)
               END-IF
      *
               MOVE    "1"             TO  SPA-GMN-TRCHK-JYKN
                                                               (IDXA)
               MOVE    TEKBYO-BYOMEIHENFLG
                                       TO  SPA-NAI-TRCHK-BYOSTATUS
                                                               (IDXA)
      *
               IF    ( SPA-X01-CHKKBN  =   "1" OR "2" OR "6" )
                   MOVE    SPA-GMN-DEFAULT-UTAGAI
                                       TO  SPA-NAI-TRCHK-UTAGAI
                                                               (IDXA)
               END-IF
      *
               MOVE    "1"             TO  SPA-GMN-TRCHK-HOSOKU-JYKN
                                                               (IDXA)
      *
               MOVE    "tbl_tekioubyomei"      TO  MCP-TABLE
               MOVE    "key2"                  TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC TO  TEKIOUBYOMEI-REC
               ELSE
                   MOVE    1           TO  FLG-TEKIOUBYOMEI
                   INITIALIZE          TEKIOUBYOMEI-REC
               END-IF
      *
           END-PERFORM
      *
      *    カーソルクロース
           MOVE    "tbl_tekioubyomei"      TO  MCP-TABLE
           MOVE    "key2"                  TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       41011-RCHKLST-TEKBYO-HEN-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ読み込み処理
      *****************************************************************
       41011-TNS-NAME-HEN-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           MOVE    SPACE               TO  WRK-TNS-NAME
           MOVE    ZERO                TO  FLG-SRYCD-DEL
      *
           MOVE    ZERO                TO  FLG-TENSU
      *
           INITIALIZE                      TNS-TENSU-REC
           MOVE    WRK-CHK-CD          TO  TNS-SRYCD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
           ELSE
               MOVE    1               TO  FLG-TENSU
               INITIALIZE                  TNS-TENSU-REC
           END-IF
      *
           PERFORM UNTIL ( FLG-TENSU       =   1 )
                     OR  ( FLG-OK          =   1 )
      *
               IF    ( TNS-SRYCD(1:1)      =   "1" )
                AND  ( TNS-ROUTEKKBN       =   2   )
                   STRING  "［老］"        DELIMITED   BY  SIZE
                           TNS-NAME        DELIMITED   BY  SIZE
                   INTO    WRK-TNS-NAME
                   END-STRING
               ELSE
                   MOVE    TNS-NAME    TO  WRK-TNS-NAME
               END-IF
      *
      *        有効終了日が対象のマスタを対象とする
               IF    ( SPA-X01-KJNYMD <=   TNS-YUKOEDYMD )
                   MOVE    1               TO  FLG-OK
               END-IF
      *
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
               ELSE
                   MOVE    1               TO  FLG-TENSU
                   INITIALIZE                  TNS-TENSU-REC
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           MOVE    WRK-TNS-NAME        TO  WRK-NAME
      *
           IF    ( FLG-OK              =   ZERO )
      *
               MOVE    1               TO  FLG-SRYCD-DEL
      *
               IF    ( WRK-TNS-NAME    NOT =   SPACE)
      *
                   MOVE    SPACE       TO  WRK-NAME
                   STRING  "★削除済★"    DELIMITED   BY  SIZE
                           WRK-TNS-NAME    DELIMITED   BY  SIZE
                   INTO    WRK-NAME
                   END-STRING
               ELSE
                   MOVE    SPACE       TO  WRK-NAME
                   STRING  "★削除済★"    DELIMITED   BY  SIZE
                           WRK-CHK-CD      DELIMITED   BY  SIZE
                   INTO    WRK-NAME
                   END-STRING
               END-IF
           END-IF
      *
           .
       41011-TNS-NAME-HEN-EXT.
           EXIT.
      *****************************************************************
      *    チェック元リスト追加処理
      *****************************************************************
       4101-LCHKLST-INS-SEC            SECTION.
      *
           INITIALIZE                  WRK-LCHKLST-AREA
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SPA-GMN-RCHKLST-MAX )
      *
               IF    ( SPA-GMN-RCHKLST-FLG (IDX0)
                                                   =   1 )
      *
                   MOVE    ZERO            TO  FLG-HIT
                   PERFORM VARYING IDX6    FROM    1   BY  1
                           UNTIL ( IDX6    >   SPA-GMN-LCHKLST-MAX )
                            OR   ( FLG-HIT =   1 )
      *
                       EVALUATE    SPA-GMN-CHKKBN
                       WHEN    "1"
                       WHEN    "2"
                           IF    ( SPA-GMN-TRCHK-NAME  (IDX0)
                               =   SPA-GMN-TLCHK-NAME  (IDX6) )
                            AND  ( SPA-GMN-TRCHK-JYKN  (IDX0)
                               =   SPA-GMN-TLCHK-JYKN  (IDX6) )
                            AND  ( SPA-GMN-TRCHK-GRPID (IDX0)
                               =   SPA-GMN-TLCHK-GRPID (IDX6) )
                            AND  ( SPA-GMN-TRCHK-HOSOKU  (IDX0)
                               =   SPA-GMN-TLCHK-HOSOKU  (IDX6))
                            AND  ( SPA-GMN-TRCHK-HOSOKU-JYKN (IDX0)
                               =   SPA-GMN-TLCHK-HOSOKU-JYKN (IDX6))
                               MOVE    1   TO  FLG-HIT
                           END-IF
                       WHEN    "6"
                       WHEN    "M"
                           IF    ( SPA-GMN-TRCHK-NAME  (IDX0)
                               =   SPA-GMN-TLCHK-NAME  (IDX6) )
                            AND  ( SPA-GMN-TRCHK-JYKN  (IDX0)
                               =   SPA-GMN-TLCHK-JYKN  (IDX6) )
                               MOVE    1   TO  FLG-HIT
                           END-IF
                       WHEN    OTHER
                           IF    ( SPA-NAI-TRCHK-SRYCD (IDX0)
                               =   SPA-NAI-TLCHK-SRYCD (IDX6) )
                               MOVE    1   TO  FLG-HIT
                           END-IF
                       END-EVALUATE
      *
                   END-PERFORM
      *
                   IF    ( FLG-HIT         =   ZERO )
                    AND  ( SPA-X03-LCHKLST-MAX    >   WRK-LCHKLST-MAX )
      *
                       COMPUTE WRK-LCHKLST-MAX =   WRK-LCHKLST-MAX +   1
      *
                       MOVE    SPA-GMN-TRCHK-NAME (IDX0)
                                       TO  WRK-LCHKLST-NAME 
                                                      (WRK-LCHKLST-MAX)
                       MOVE    SPA-NAI-TRCHK-SRYCD (IDX0)
                                       TO  WRK-LCHKLST-SRYCD
                                                      (WRK-LCHKLST-MAX)
      *
                       MOVE    SPA-GMN-TRCHK-JYKN (IDX0)
                                       TO  WRK-LCHKLST-JYKN
                                                      (WRK-LCHKLST-MAX)
                       MOVE    SPA-GMN-TRCHK-GRPID (IDX0)
                                       TO  WRK-LCHKLST-GRPID
                                                      (WRK-LCHKLST-MAX)
                       PERFORM VARYING IDX7    FROM    1   BY  1
                           UNTIL ( IDX7    >   CONST-MKCD-MAX )
                           MOVE    SPA-NAI-TRCHK-BYOCD(IDX0 IDX7)
                                       TO  WRK-LCHKLST-BYOCD
                                                 (WRK-LCHKLST-MAX IDX7)
                       END-PERFORM
      *
                       MOVE    SPA-NAI-TRCHK-BYOSTATUS (IDX0)
                                       TO  WRK-LCHKLST-BYOSTATUS
                                                      (WRK-LCHKLST-MAX)
      *
                       MOVE    SPA-NAI-TRCHK-UTAGAI (IDX0)
                                       TO  WRK-LCHKLST-UTAGAI
                                                      (WRK-LCHKLST-MAX)
                       MOVE    SPA-NAI-TRCHK-SYORIKBN (IDX0)
                                       TO  WRK-LCHKLST-SYORIKBN
                                                      (WRK-LCHKLST-MAX)
                       MOVE    SPA-NAI-TRCHK-NGBNDKBN (IDX0)
                                       TO  WRK-LCHKLST-NGBNDKBN
                                                      (WRK-LCHKLST-MAX)
                       MOVE    SPA-NAI-TRCHK-CHKERR (IDX0)
                                       TO  WRK-LCHKLST-CHKERR
                                                      (WRK-LCHKLST-MAX)
                       MOVE    SPA-NAI-TRCHK-DATAKBN (IDX0)
                                       TO  WRK-LCHKLST-DATAKBN
                                                      (WRK-LCHKLST-MAX)
                       MOVE    SPA-GMN-TRCHK-HOSOKU (IDX0)
                                       TO  WRK-LCHKLST-HOSOKU
                                                      (WRK-LCHKLST-MAX)
                       MOVE    SPA-GMN-TRCHK-HOSOKU-JYKN (IDX0)
                                       TO  WRK-LCHKLST-HOSOKU-JYKN
                                                      (WRK-LCHKLST-MAX)
      *
                   END-IF
               END-IF
           END-PERFORM
      *
           IF    ( WRK-LCHKLST-MAX +   SPA-GMN-LCHKLST-MAX
                                       >   SPA-X03-LCHKLST-MAX )
               MOVE    "1001"      TO  SPA-ERRCD
               GO  TO  4101-LCHKLST-INS-EXT
           END-IF
      *
      *    チェック元リスト追加処理
           PERFORM 41011-LCHKLST-INS-SEC
      *
           .
       4101-LCHKLST-INS-EXT.
           EXIT.
      *****************************************************************
      *    チェック元リスト追加処理
      *****************************************************************
       41011-LCHKLST-INS-SEC           SECTION.
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   WRK-LCHKLST-MAX )
                     OR  ( SPA-GMN-LCHKLST-MAX
                                   >=  SPA-X03-LCHKLST-MAX )
      *
               COMPUTE SPA-GMN-LCHKLST-MAX
                   =   SPA-GMN-LCHKLST-MAX +   1
      *
               MOVE    SPA-GMN-LCHKLST-MAX
                                       TO  IDXA
      *
               MOVE    IDXA            TO  SPA-GMN-TLCHK-NUM  (IDXA)
      *
               MOVE    WRK-LCHKLST-NAME (IDX0)
                                       TO  SPA-GMN-TLCHK-NAME (IDXA)
               MOVE    WRK-LCHKLST-SRYCD(IDX0)
                                       TO  SPA-NAI-TLCHK-SRYCD(IDXA)
               MOVE    WRK-LCHKLST-JYKN (IDX0)
                                       TO  SPA-GMN-TLCHK-JYKN (IDXA)
               IF    ( SPA-GMN-CHKKBN  =   "1" OR "2" )
                   MOVE    WRK-LCHKLST-GRPID(IDX0)
                                       TO  SPA-GMN-TLCHK-GRPID(IDXA)
               END-IF
               PERFORM VARYING IDX7    FROM    1   BY  1
                       UNTIL ( IDX7    >   CONST-MKCD-MAX )
                   MOVE    WRK-LCHKLST-BYOCD (IDX0 IDX7)
                               TO  SPA-NAI-TLCHK-BYOCD (IDXA IDX7)
               END-PERFORM
      *
               MOVE    WRK-LCHKLST-BYOSTATUS (IDX0)
                               TO  SPA-NAI-TLCHK-BYOSTATUS (IDXA)
      *
               MOVE    WRK-LCHKLST-UTAGAI (IDX0)
                               TO  SPA-NAI-TLCHK-UTAGAI (IDXA)
               MOVE    WRK-LCHKLST-SYORIKBN (IDX0)
                               TO  SPA-NAI-TLCHK-SYORIKBN (IDXA)
               MOVE    WRK-LCHKLST-NGBNDKBN (IDX0)
                               TO  SPA-NAI-TLCHK-NGBNDKBN (IDXA)
               MOVE    WRK-LCHKLST-CHKERR (IDX0)
                               TO  SPA-NAI-TLCHK-CHKERR (IDXA)
               MOVE    WRK-LCHKLST-DATAKBN (IDX0)
                               TO  SPA-NAI-TLCHK-DATAKBN (IDXA)
               MOVE    WRK-LCHKLST-HOSOKU(IDX0)
                               TO  SPA-GMN-TLCHK-HOSOKU(IDXA)
               MOVE    WRK-LCHKLST-HOSOKU-JYKN(IDX0)
                               TO  SPA-GMN-TLCHK-HOSOKU-JYKN (IDXA)
      *
           END-PERFORM
      *
           IF    ( SPA-GMN-CHKKBN  =   "1" OR "2" )
      *
               COMPUTE IDXS = SPA-GMN-LCHKLST-MAX  +   1
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   SPA-GMN-LCHKLST-MAX - 1 )
                   COMPUTE IDX-ST  =   IDX0    +   1
                   PERFORM VARYING IDX1    FROM    IDX-ST  BY  1
                           UNTIL ( IDX1    >   SPA-GMN-LCHKLST-MAX )
                       IF    ( SPA-GMN-TLCHK-GRPID (IDX0)
                                   >   SPA-GMN-TLCHK-GRPID (IDX1))
                           MOVE    SPA-GMN-LCHKLST-OCC (IDX0)
                                   TO  SPA-GMN-LCHKLST-OCC (IDXS)
                           MOVE    SPA-NAI-LCHKLST-OCC (IDX0)
                                   TO  SPA-NAI-LCHKLST-OCC (IDXS)
                           MOVE    SPA-GMN-LCHKLST-OCC (IDX1)
                                   TO  SPA-GMN-LCHKLST-OCC (IDX0)
                           MOVE    SPA-NAI-LCHKLST-OCC (IDX1)
                                   TO  SPA-NAI-LCHKLST-OCC (IDX0)
                           MOVE    SPA-GMN-LCHKLST-OCC (IDXS)
                                   TO  SPA-GMN-LCHKLST-OCC (IDX1)
                           MOVE    SPA-NAI-LCHKLST-OCC (IDXS)
                                   TO  SPA-NAI-LCHKLST-OCC (IDX1)
                       END-IF
                   END-PERFORM
               END-PERFORM
           END-IF
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SPA-GMN-LCHKLST-MAX )
      *
               MOVE    IDX0        TO  SPA-GMN-TLCHK-NUM  (IDX0)
               MOVE    ZERO        TO  SPA-GMN-LCHKLST-FLG (IDX0)
      *
           END-PERFORM
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SPA-GMN-RCHKLST-MAX )
      *
               MOVE    ZERO        TO  SPA-GMN-RCHKLST-FLG (IDX0)
      *
           END-PERFORM
      *
           .
       41011-LCHKLST-INS-EXT.
           EXIT.
      *****************************************************************
      *    チェック元リスト全追加処理
      *****************************************************************
       4101-LCHKLST-ALLINS-SEC         SECTION.
      *
           INITIALIZE                  WRK-LCHKLST-AREA
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SPA-GMN-RCHKLST-MAX )
      *
               IF    ( SPA-NAI-TRCHK-DELFLG  (IDX0)
                                                   =   ZERO )
                AND  ( SPA-NAI-TRCHK-SRYCD   (IDX0)
                                               NOT =   SPA-X01-SRYCD )
      *
                   MOVE    ZERO            TO  FLG-HIT
                   PERFORM VARYING IDX6    FROM    1   BY  1
                           UNTIL ( IDX6    >   SPA-GMN-LCHKLST-MAX )
                            OR   ( FLG-HIT =   1 )
      *
                       EVALUATE    SPA-GMN-CHKKBN
                       WHEN    "1"
                       WHEN    "2"
                           IF    ( SPA-GMN-TRCHK-NAME  (IDX0)
                               =   SPA-GMN-TLCHK-NAME  (IDX6) )
                            AND  ( SPA-GMN-TRCHK-JYKN  (IDX0)
                               =   SPA-GMN-TLCHK-JYKN  (IDX6) )
                            AND  ( SPA-GMN-TRCHK-GRPID (IDX0)
                               =   SPA-GMN-TLCHK-GRPID (IDX6) )
                            AND  ( SPA-GMN-TRCHK-HOSOKU(IDX0)
                               =   SPA-GMN-TLCHK-HOSOKU(IDX6) )
                            AND  ( SPA-GMN-TRCHK-HOSOKU-JYKN (IDX0)
                               =   SPA-GMN-TLCHK-HOSOKU-JYKN (IDX6))
                               MOVE    1   TO  FLG-HIT
                           END-IF
                       WHEN    "6"
                       WHEN    "M"
                           IF    ( SPA-GMN-TRCHK-NAME  (IDX0)
                               =   SPA-GMN-TLCHK-NAME  (IDX6) )
                            AND  ( SPA-GMN-TRCHK-JYKN  (IDX0)
                               =   SPA-GMN-TLCHK-JYKN  (IDX6) )
                               MOVE    1   TO  FLG-HIT
                           END-IF
                       WHEN    OTHER
                           IF    ( SPA-NAI-TRCHK-SRYCD (IDX0)
                               =   SPA-NAI-TLCHK-SRYCD (IDX6) )
                               MOVE    1   TO  FLG-HIT
                           END-IF
                       END-EVALUATE
      *
                   END-PERFORM
      *
                   IF    ( FLG-HIT         =   ZERO )
                    AND  ( SPA-X03-LCHKLST-MAX   >   WRK-LCHKLST-MAX )
      *
                       COMPUTE WRK-LCHKLST-MAX =   WRK-LCHKLST-MAX +   1
      *
                       MOVE    SPA-GMN-TRCHK-NAME (IDX0)
                                       TO  WRK-LCHKLST-NAME 
                                                      (WRK-LCHKLST-MAX)
                       MOVE    SPA-NAI-TRCHK-SRYCD (IDX0)
                                       TO  WRK-LCHKLST-SRYCD
                                                      (WRK-LCHKLST-MAX)
      *
                       MOVE    SPA-GMN-TRCHK-JYKN (IDX0)
                                       TO  WRK-LCHKLST-JYKN
                                                      (WRK-LCHKLST-MAX)
                       MOVE    SPA-GMN-TRCHK-GRPID (IDX0)
                                       TO  WRK-LCHKLST-GRPID
                                                      (WRK-LCHKLST-MAX)
                       PERFORM VARYING IDX7    FROM    1   BY  1
                           UNTIL ( IDX7    >   CONST-MKCD-MAX )
                           MOVE    SPA-NAI-TRCHK-BYOCD(IDX0 IDX7)
                                       TO  WRK-LCHKLST-BYOCD
                                                 (WRK-LCHKLST-MAX IDX7)
                       END-PERFORM
      *
                       MOVE    SPA-NAI-TRCHK-BYOSTATUS (IDX0)
                                       TO  WRK-LCHKLST-BYOSTATUS
                                                      (WRK-LCHKLST-MAX)
                       MOVE    SPA-NAI-TRCHK-UTAGAI (IDX0)
                                       TO  WRK-LCHKLST-UTAGAI
                                                      (WRK-LCHKLST-MAX)
                       MOVE    SPA-NAI-TRCHK-SYORIKBN (IDX0)
                                       TO  WRK-LCHKLST-SYORIKBN
                                                      (WRK-LCHKLST-MAX)
                       MOVE    SPA-NAI-TRCHK-NGBNDKBN (IDX0)
                                       TO  WRK-LCHKLST-NGBNDKBN
                                                      (WRK-LCHKLST-MAX)
                       MOVE    SPA-NAI-TRCHK-CHKERR (IDX0)
                                       TO  WRK-LCHKLST-CHKERR
                                                      (WRK-LCHKLST-MAX)
                       MOVE    SPA-NAI-TRCHK-DATAKBN (IDX0)
                                       TO  WRK-LCHKLST-DATAKBN
                                                      (WRK-LCHKLST-MAX)
                       MOVE    SPA-GMN-TRCHK-HOSOKU (IDX0)
                                       TO  WRK-LCHKLST-HOSOKU
                                                      (WRK-LCHKLST-MAX)
                       MOVE    SPA-GMN-TRCHK-HOSOKU-JYKN (IDX0)
                                       TO  WRK-LCHKLST-HOSOKU-JYKN
                                                      (WRK-LCHKLST-MAX)
      *
                   END-IF
               END-IF
           END-PERFORM
      *
           IF    ( WRK-LCHKLST-MAX +   SPA-GMN-LCHKLST-MAX
                                       >   SPA-X03-LCHKLST-MAX )
               MOVE    "1001"      TO  SPA-ERRCD
               GO  TO  4101-LCHKLST-ALLINS-EXT
           END-IF
      *
      *    チェック元リスト追加処理
           PERFORM 41011-LCHKLST-INS-SEC
      *
           .
       4101-LCHKLST-ALLINS-EXT.
           EXIT.
      *****************************************************************
      *    チェック元リスト削除処理
      *****************************************************************
       4101-LCHKLST-DEL-SEC            SECTION.
      *
           INITIALIZE                  WRK-LCHKLST-AREA
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SPA-GMN-LCHKLST-MAX )
      *
               IF    ( SPA-GMN-LCHKLST-FLG (IDX0)    = ZERO )
                   COMPUTE WRK-LCHKLST-MAX =   WRK-LCHKLST-MAX +   1
      *
                   MOVE    SPA-GMN-TLCHK-NAME (IDX0)
                                       TO  WRK-LCHKLST-NAME 
                                                      (WRK-LCHKLST-MAX)
                   MOVE    SPA-NAI-TLCHK-SRYCD (IDX0)
                                       TO  WRK-LCHKLST-SRYCD
                                                      (WRK-LCHKLST-MAX)
      *
                   MOVE    SPA-GMN-TLCHK-JYKN (IDX0)
                                       TO  WRK-LCHKLST-JYKN
                                                      (WRK-LCHKLST-MAX)
                   MOVE    SPA-GMN-TLCHK-GRPID (IDX0)
                                       TO  WRK-LCHKLST-GRPID
                                                      (WRK-LCHKLST-MAX)
                   PERFORM VARYING IDX7    FROM    1   BY  1
                           UNTIL ( IDX7    >   CONST-MKCD-MAX )
                       MOVE    SPA-NAI-TLCHK-BYOCD (IDX0 IDX7)
                                       TO  WRK-LCHKLST-BYOCD
                                                 (WRK-LCHKLST-MAX IDX7)
                   END-PERFORM
      *
                   MOVE    SPA-NAI-TLCHK-BYOSTATUS (IDX0)
                                       TO  WRK-LCHKLST-BYOSTATUS
                                                      (WRK-LCHKLST-MAX)
      *
                   MOVE    SPA-NAI-TLCHK-UTAGAI (IDX0)
                                       TO  WRK-LCHKLST-UTAGAI
                                                      (WRK-LCHKLST-MAX)
                   MOVE    SPA-NAI-TLCHK-SYORIKBN (IDX0)
                                       TO  WRK-LCHKLST-SYORIKBN
                                                      (WRK-LCHKLST-MAX)
                   MOVE    SPA-NAI-TLCHK-NGBNDKBN (IDX0)
                                       TO  WRK-LCHKLST-NGBNDKBN
                                                      (WRK-LCHKLST-MAX)
                   MOVE    SPA-NAI-TLCHK-CHKERR (IDX0)
                                       TO  WRK-LCHKLST-CHKERR
                                                      (WRK-LCHKLST-MAX)
                   MOVE    SPA-NAI-TLCHK-DATAKBN (IDX0)
                                       TO  WRK-LCHKLST-DATAKBN
                                                      (WRK-LCHKLST-MAX)
                   MOVE    SPA-GMN-TLCHK-HOSOKU (IDX0)
                                       TO  WRK-LCHKLST-HOSOKU
                                                      (WRK-LCHKLST-MAX)
                   MOVE    SPA-GMN-TLCHK-HOSOKU-JYKN (IDX0)
                                       TO  WRK-LCHKLST-HOSOKU-JYKN
                                                      (WRK-LCHKLST-MAX)
      *
               END-IF
      *
           END-PERFORM
      *
           INITIALIZE                  SPA-GMN-LCHKLST
                                       SPA-NAI-LCHKLST
      *
           MOVE    WRK-LCHKLST-MAX     TO  SPA-GMN-LCHKLST-MAX
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   WRK-LCHKLST-MAX )
      *
               MOVE    IDX0            TO  SPA-GMN-TLCHK-NUM (IDX0)
               MOVE    WRK-LCHKLST-NAME (IDX0)
                                       TO  SPA-GMN-TLCHK-NAME (IDX0)
               MOVE    WRK-LCHKLST-SRYCD(IDX0)
                                       TO  SPA-NAI-TLCHK-SRYCD(IDX0)
               MOVE    WRK-LCHKLST-JYKN (IDX0)
                                       TO  SPA-GMN-TLCHK-JYKN (IDX0)
               MOVE    WRK-LCHKLST-GRPID (IDX0)
                                       TO  SPA-GMN-TLCHK-GRPID(IDX0)
               PERFORM VARYING IDX7    FROM    1   BY  1
                       UNTIL ( IDX7    >   CONST-MKCD-MAX )
                   MOVE    WRK-LCHKLST-BYOCD (IDX0 IDX7)
                               TO  SPA-NAI-TLCHK-BYOCD (IDX0 IDX7)
               END-PERFORM
      *
               MOVE    WRK-LCHKLST-BYOSTATUS (IDX0)
                               TO  SPA-NAI-TLCHK-BYOSTATUS (IDX0)
               MOVE    WRK-LCHKLST-UTAGAI (IDX0)
                               TO  SPA-NAI-TLCHK-UTAGAI (IDX0)
               MOVE    WRK-LCHKLST-SYORIKBN (IDX0)
                               TO  SPA-NAI-TLCHK-SYORIKBN (IDX0)
               MOVE    WRK-LCHKLST-NGBNDKBN (IDX0)
                               TO  SPA-NAI-TLCHK-NGBNDKBN (IDX0)
               MOVE    WRK-LCHKLST-CHKERR (IDX0)
                               TO  SPA-NAI-TLCHK-CHKERR (IDX0)
               MOVE    WRK-LCHKLST-DATAKBN (IDX0)
                               TO  SPA-NAI-TLCHK-DATAKBN(IDX0)
               MOVE    WRK-LCHKLST-HOSOKU (IDX0)
                               TO  SPA-GMN-TLCHK-HOSOKU(IDX0)
               MOVE    WRK-LCHKLST-HOSOKU-JYKN (IDX0)
                               TO  SPA-GMN-TLCHK-HOSOKU-JYKN(IDX0)
      *
           END-PERFORM
      *
           .
       4101-LCHKLST-DEL-EXT.
           EXIT.
      *****************************************************************
      *    チェック区分コンボチェック処理
      *****************************************************************
       4200-CHKKBNLST-CHK-SEC          SECTION.
      *
           MOVE    SPACE               TO  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
               UNTIL ( IDX-CMB     >   SPA-GMN-CHKKBN-MAX )
                OR   ( WRK-CMB-CD  =   SPA-GMN-CHKKBNL (IDX-CMB) )
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX-CMB     <=  SPA-GMN-CHKKBN-MAX )
               MOVE    SPA-GMN-CHKKBNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-CHKKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    一致条件名チェック処理
      *****************************************************************
       4300-JYKNLST-CHK-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
               UNTIL ( IDX-CMB     >   CONST-JYKN-MAX )
                OR   ( WRK-CMB-CD  =   CONST-JYKN-CD (IDX-CMB) )
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX-CMB     <=  CONST-JYKN-MAX )
               MOVE    CONST-JYKN-MEI  (IDX-CMB)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4300-JYKNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    日・月別区分名チェック処理
      *****************************************************************
       4300-DMKBNLST-CHK-SEC           SECTION.
      *
           MOVE    SPACE               TO  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
               UNTIL ( IDX-CMB     >   CONST-DMKBN-MAX )
                OR   ( WRK-CMB-CD  =   CONST-DMKBN-CD (IDX-CMB) )
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX-CMB     <=  CONST-DMKBN-MAX )
               MOVE    CONST-DMKBN-MEI  (IDX-CMB)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4300-DMKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           EVALUATE    SPA-X01-CHKKBN
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
               MOVE    "X08"           TO  SPA-SAKIPG
      * 
           WHEN    OTHER
               MOVE    "X02"           TO  SPA-SAKIPG
           END-EVALUATE
      *
           MOVE    "X03"               TO  SPA-MOTOPG
      *
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           MOVE    1                   TO  FLG-CHANGE
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    前頁　処理
      *****************************************************************
       490-PAGE-PREV-SEC               SECTION.
      *
           IF    ( SPA-GMN-PAGE    >   1 )
               COMPUTE SPA-GMN-PAGE
                   =   SPA-GMN-PAGE    -   1
               PERFORM 3101-SELCHKLST-HEN-SEC
           END-IF
      *
           .
       490-PAGE-PREV-EXT.
           EXIT.
      *****************************************************************
      *    次頁　処理
      *****************************************************************
       490-PAGE-NEXT-SEC               SECTION.
      * 
           IF    ( SPA-GMN-NEXT    =   1 )
               COMPUTE SPA-GMN-PAGE
                   =   SPA-GMN-PAGE    +   1
               PERFORM 3101-SELCHKLST-HEN-SEC
           END-IF
      *
           .
       490-PAGE-NEXT-EXT.
           EXIT.
      *****************************************************************
      *    チェック先リスト選択解除処理
      *****************************************************************
       490-CANCEL-SEL-SEC              SECTION.
      *
           INITIALIZE                  SPA-GMN-RCHKLST-FLG-G
      *
           .
       490-CANCEL-EXT.
           EXIT.
      *****************************************************************
      *    確定　処理
      *****************************************************************
       490-KAKUTEI-SEC                 SECTION.
      *
           MOVE    1               TO  SPA-X03-GMN-KAKUTEI
      *
           MOVE    SPA-GMN-LCHKKBNMEI
                                   TO  SPA-X03-GMN-LCHKKBNMEI
           MOVE    SPA-GMN-LYUKOSTYMD
                                   TO  SPA-X03-GMN-LYUKOSTYMD
           MOVE    SPA-GMN-LYUKOEDYMD
                                   TO  SPA-X03-GMN-LYUKOEDYMD
           MOVE    SPA-GMN-LNAME
                                   TO  SPA-X03-GMN-LNAME
           MOVE    SPA-GMN-LCHKLST-MAX
                                   TO  SPA-X03-GMN-LCHKLST-MAX
           PERFORM VARYING IDXE    FROM    1   BY  1
                   UNTIL ( IDXE    >   SPA-X03-GMN-LCHKLST-MAX )
               MOVE    SPA-GMN-TLCHK-NUM (IDXE)
                                   TO  SPA-X03-GMN-TLCHK-NUM (IDXE)
               MOVE    SPA-GMN-TLCHK-NAME (IDXE)
                                   TO  SPA-X03-GMN-TLCHK-NAME (IDXE)
               MOVE    SPA-GMN-TLCHK-JYKN (IDXE)
                                   TO  SPA-X03-GMN-TLCHK-JYKN (IDXE)
               MOVE    SPA-GMN-TLCHK-GRPID (IDXE)
                                   TO  SPA-X03-GMN-TLCHK-GRPID (IDXE)
               MOVE    SPA-GMN-TLCHK-HOSOKU (IDXE)
                                   TO  SPA-X03-GMN-TLCHK-HOSOKU (IDXE)
               MOVE    SPA-GMN-TLCHK-HOSOKU-JYKN (IDXE)
                               TO  SPA-X03-GMN-TLCHK-HOSOKU-JYKN (IDXE)
               MOVE    SPA-NAI-TLCHK-SRYCD (IDXE)
                                   TO  SPA-X03-NAI-TLCHK-SRYCD (IDXE)
               PERFORM VARYING IDXF    FROM    1   BY  1
                       UNTIL ( IDXF    >   CONST-MKCD-MAX )
                   MOVE    SPA-NAI-TLCHK-BYOCD (IDXE IDXF)
                               TO  SPA-X03-NAI-TLCHK-BYOCD (IDXE IDXF)
               END-PERFORM
               MOVE    SPA-NAI-TLCHK-BYOSTATUS (IDXE)
                                   TO  SPA-X03-NAI-TLCHK-BYOSTATUS(IDXE)
               MOVE    SPA-NAI-TLCHK-UTAGAI (IDXE)
                                   TO  SPA-X03-NAI-TLCHK-UTAGAI (IDXE)
               MOVE    SPA-NAI-TLCHK-SYORIKBN (IDXE)
                                   TO  SPA-X03-NAI-TLCHK-SYORIKBN (IDXE)
               MOVE    SPA-NAI-TLCHK-NGBNDKBN (IDXE)
                                   TO  SPA-X03-NAI-TLCHK-NGBNDKBN (IDXE)
               MOVE    SPA-NAI-TLCHK-CHKERR (IDXE)
                                   TO  SPA-X03-NAI-TLCHK-CHKERR (IDXE)
               MOVE    SPA-NAI-TLCHK-DATAKBN (IDXE)
                                   TO  SPA-X03-NAI-TLCHK-DATAKBN (IDXE)
           END-PERFORM
      *
           EVALUATE    SPA-X01-CHKKBN
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
               MOVE    "X08"           TO  SPA-SAKIPG
      * 
           WHEN    OTHER
               MOVE    "X02"           TO  SPA-SAKIPG
           END-EVALUATE
      *
           MOVE    "X03"               TO  SPA-MOTOPG
      *
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           MOVE    1                   TO  FLG-CHANGE
      *
           .
       490-KAKUTEI-EXT.
           EXIT.
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
           MOVE    "X03    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    X03-LCHKLST-ROW         TO  WRK-LCHKLST-ROW
           MOVE    X03-RCHKLST-ROW         TO  WRK-RCHKLST-ROW
           MOVE    X03-SELCHKLST-ROW       TO  WRK-SELCHKLST-ROW
      *
           INITIALIZE                      X03
      *
           MOVE    SPA-GMN-LCHKKBNMEI  TO  X03-LCHKKBNMEI
           MOVE    SPA-GMN-LYUKOSTYMD  TO  X03-LYUKOSTYMD
           MOVE    SPA-GMN-LYUKOEDYMD  TO  X03-LYUKOEDYMD
           MOVE    SPA-GMN-LNAME       TO  X03-LNAME
           MOVE    SPA-GMN-LCHKLST-MAX TO  X03-LCHKLST-COUNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-LCHKLST-MAX )
      *
               INITIALIZE                  WRK-PRT-NUM-G
               MOVE    SPA-GMN-TLCHK-NUM  (IDX1)
                                       TO  WRK-PRT-NUM
               MOVE    SPA-GMN-TLCHK-GRPID(IDX1)
                                       TO  WRK-PRT-GRPID(2:1)
               MOVE    WRK-PRT-NUM-G   TO  X03-TLCHK-NUM  (IDX1)
               IF    ( SPA-NAI-TLCHK-DATAKBN(IDX1)
                                           =   "1" )
                   MOVE    "◎"        TO  X03-TLCHK-NUM  (IDX1) (1:2)
               END-IF
      *
               IF    ( SPA-X01-CHKKBN  =   "1" OR "2" )
                AND  ( SPA-GMN-TLCHK-HOSOKU (IDX1)
                                   NOT =   SPACE )
                   STRING  SPA-GMN-TLCHK-NAME (IDX1)
                                                   DELIMITED BY SPACE
                           "（"                    DELIMITED BY SIZE
                           SPA-GMN-TLCHK-HOSOKU (IDX1)
                                                   DELIMITED BY SPACE
                           "）"                    DELIMITED BY SIZE
                   INTO  X03-TLCHK-NAME (IDX1)
                   END-STRING
               ELSE
                   MOVE    SPA-GMN-TLCHK-NAME (IDX1)
                                       TO  X03-TLCHK-NAME (IDX1)
               END-IF
      *
               EVALUATE    SPA-X01-CHKKBN
               WHEN    "1"
               WHEN    "2"
               WHEN    "6"
                   MOVE    SPACE       TO  WRK-BYOMEI-JYKN
                                           WRK-HOSOKU-JYKN
      *
                   MOVE    SPA-GMN-TLCHK-JYKN (IDX1)
                                       TO  WRK-CMB-CD
                   PERFORM 4300-JYKNLST-CHK-SEC
                   MOVE    WRK-CMB-ITM TO  WRK-BYOMEI-JYKN
      *
                   MOVE    SPA-GMN-TLCHK-HOSOKU-JYKN (IDX1)
                                       TO  WRK-CMB-CD
                   PERFORM 4300-JYKNLST-CHK-SEC
                   MOVE    WRK-CMB-ITM TO  WRK-HOSOKU-JYKN
      *
                   IF    ( SPA-X01-CHKKBN  =   "1" OR "2" )
                    AND  ( SPA-GMN-TLCHK-HOSOKU (IDX1)
                                       NOT =   SPACE )
                       STRING  WRK-BYOMEI-JYKN     DELIMITED BY SPACE
                               "（"                DELIMITED BY SIZE
                               WRK-HOSOKU-JYKN     DELIMITED BY SPACE
                               "）"                DELIMITED BY SIZE
                       INTO    X03-TLCHK-JYKN(IDX1)
                       END-STRING
                   ELSE
                       MOVE    WRK-BYOMEI-JYKN
                                       TO  X03-TLCHK-JYKN(IDX1)
                   END-IF
               WHEN    "7"
               WHEN    "8"
               WHEN    "9"
                   MOVE    SPA-GMN-TLCHK-JYKN (IDX1)
                                           TO  WRK-CMB-CD
                   PERFORM 4300-DMKBNLST-CHK-SEC
                   MOVE    WRK-CMB-ITM     TO  X03-TLCHK-JYKN(IDX1)
               END-EVALUATE
      *
               IF    ( SPA-GMN-LCHKLST-FLG (IDX1)  =   ZERO )
                   MOVE    "F"         TO  X03-LCHKLST-SELECT (IDX1)
               ELSE
                   MOVE    "T"         TO  X03-LCHKLST-SELECT (IDX1)
               END-IF
      *
           END-PERFORM
      *
           MOVE    ZERO                TO  FLG-RCHKLST-SELECT
      *
           MOVE    SPA-GMN-RCHKKBNMEI  TO  X03-RCHKKBNMEI
           MOVE    SPA-GMN-RYUKOSTYMD  TO  X03-RYUKOSTYMD
           MOVE    SPA-GMN-RYUKOEDYMD  TO  X03-RYUKOEDYMD
           MOVE    SPA-GMN-RNAME       TO  X03-RNAME
           MOVE    SPA-GMN-RCHKLST-MAX TO  X03-RCHKLST-COUNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-RCHKLST-MAX )
      *
               INITIALIZE                  WRK-PRT-NUM-G
               MOVE    SPA-GMN-TRCHK-NUM  (IDX1)
                                       TO  WRK-PRT-NUM
               MOVE    SPA-GMN-TRCHK-GRPID(IDX1)
                                       TO  WRK-PRT-GRPID(2:1)
               MOVE    WRK-PRT-NUM-G   TO  X03-TRCHK-NUM  (IDX1)
      *
               IF    ( SPA-X01-CHKKBN  =   "1" OR "2" )
                AND  ( SPA-GMN-TRCHK-HOSOKU (IDX1)
                                   NOT =   SPACE )
                   STRING  SPA-GMN-TRCHK-NAME (IDX1)
                                                   DELIMITED BY SPACE
                           "（"                    DELIMITED BY SIZE
                           SPA-GMN-TRCHK-HOSOKU (IDX1)
                                                   DELIMITED BY SPACE
                           "）"                    DELIMITED BY SIZE
                   INTO  X03-TRCHK-NAME (IDX1)
                   END-STRING
               ELSE
                   MOVE    SPA-GMN-TRCHK-NAME (IDX1)
                                       TO  X03-TRCHK-NAME (IDX1)
               END-IF
      *
               EVALUATE    SPA-X01-CHKKBN
               WHEN    "1"
               WHEN    "2"
               WHEN    "6"
                   MOVE    SPACE       TO  WRK-BYOMEI-JYKN
                                           WRK-HOSOKU-JYKN
      *
                   MOVE    SPA-GMN-TRCHK-JYKN (IDX1)
                                       TO  WRK-CMB-CD
                   PERFORM 4300-JYKNLST-CHK-SEC
                   MOVE    WRK-CMB-ITM TO  WRK-BYOMEI-JYKN
      *
                   MOVE    SPA-GMN-TRCHK-HOSOKU-JYKN (IDX1)
                                       TO  WRK-CMB-CD
                   PERFORM 4300-JYKNLST-CHK-SEC
                   MOVE    WRK-CMB-ITM TO  WRK-HOSOKU-JYKN
      *
                   IF    ( SPA-X01-CHKKBN  =   "1" OR "2" )
                    AND  ( SPA-GMN-TRCHK-HOSOKU (IDX1)
                                       NOT =   SPACE )
                       STRING  WRK-BYOMEI-JYKN     DELIMITED BY SPACE
                               "（"                DELIMITED BY SIZE
                               WRK-HOSOKU-JYKN     DELIMITED BY SPACE
                               "）"                DELIMITED BY SIZE
                       INTO    X03-TRCHK-JYKN(IDX1)
                       END-STRING
                   ELSE
                       MOVE    WRK-BYOMEI-JYKN
                                       TO  X03-TRCHK-JYKN(IDX1)
                   END-IF
               WHEN    "7"
               WHEN    "8"
               WHEN    "9"
                   MOVE    SPA-GMN-TRCHK-JYKN (IDX1)
                                           TO  WRK-CMB-CD
                   PERFORM 4300-DMKBNLST-CHK-SEC
                   MOVE    WRK-CMB-ITM     TO  X03-TRCHK-JYKN(IDX1)
               END-EVALUATE
      *
               IF    ( SPA-GMN-RCHKLST-FLG (IDX1)  =   ZERO )
                   MOVE    "F"         TO  X03-RCHKLST-SELECT (IDX1)
               ELSE
                   MOVE    "T"         TO  X03-RCHKLST-SELECT (IDX1)
                   MOVE    1           TO  FLG-RCHKLST-SELECT
               END-IF
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-CHKKBN-G    TO  X03-CHKKBN
      *
           MOVE    SPA-GMN-CHKKBN-MAX  TO  X03-CHKKBN-COUNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-CHKKBN-MAX )
                   MOVE    SPA-GMN-CHKKBNLST (IDX1)
                                           TO  X03-CHKKBN-ITEM (IDX1)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-SELNAME     TO  X03-SELNAME
      *
           MOVE    SPA-GMN-SELCHKLST-MAX
                                           TO  X03-SELCHKLST-COUNT
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-SELCHKLST-MAX )
      *
               MOVE    SPA-GMN-TSEL-CHKNUM (IDX1)
                                       TO  WRK-ZZZZ
               MOVE    WRK-ZZZZ        TO  X03-TSEL-CHKNUM (IDX1)
               MOVE    SPA-GMN-TSEL-NAME   (IDX1)
                                       TO  X03-TSEL-NAME   (IDX1)
               MOVE    SPA-GMN-TSEL-YUKOSTYMD (IDX1)
                                       TO  X03-TSEL-YUKOSTYMD (IDX1)
               MOVE    SPA-GMN-TSEL-YUKOEDYMD (IDX1)
                                       TO  X03-TSEL-YUKOEDYMD (IDX1)
      *
               IF    ( SPA-GMN-SELCHKLST-SEL   =   IDX1 )
                   MOVE    "T"         TO  X03-SELCHKLST-SELECT (IDX1)
               ELSE
                   MOVE    "F"         TO  X03-SELCHKLST-SELECT (IDX1)
               END-IF
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-KJNYMD  TO  X03-KJNYMD
      *
           EVALUATE    SPA-X01-CHKKBN
           WHEN    "1"
           WHEN    "2"
           WHEN    "6"
               MOVE    "一致"          TO  X03-LBLLJYKN
                                           X03-LBLRJYKN
           WHEN    "7"
           WHEN    "8"
           WHEN    "9"
               MOVE    "日／月"        TO  X03-LBLLJYKN
                                           X03-LBLRJYKN
           END-EVALUATE
      *
           IF    ( FLG-LCHKLST-ROW     =   1 )
               MOVE    ZERO            TO  X03-LCHKLST-ROW
               MOVE    ZERO            TO  X03-LCHKLST-ROWATTR
           ELSE
               MOVE    WRK-LCHKLST-ROW TO  X03-LCHKLST-ROW
               MOVE    ZERO            TO  X03-LCHKLST-ROWATTR
           END-IF
      *
           IF    ( FLG-RCHKLST-ROW     =   1 )
               MOVE    ZERO            TO  X03-RCHKLST-ROW
               MOVE    ZERO            TO  X03-RCHKLST-ROWATTR
           ELSE
               MOVE    WRK-RCHKLST-ROW TO  X03-RCHKLST-ROW
               MOVE    ZERO            TO  X03-RCHKLST-ROWATTR
           END-IF
      *
           IF    ( FLG-SELCHKLST-ROW   =   1 )
               MOVE    ZERO            TO  X03-SELCHKLST-ROW
               MOVE    ZERO            TO  X03-SELCHKLST-ROWATTR
           ELSE
               MOVE    WRK-SELCHKLST-ROW
                                       TO  X03-SELCHKLST-ROW
               MOVE    ZERO            TO  X03-SELCHKLST-ROWATTR
           END-IF
      *
           MOVE    WIDGET-NORMAL       TO  X03-B09-STATE
                                           X03-B09-2-STATE
                                           X03-B10-STATE
                                           X03-B10-2-STATE
                                           X03-BTN-INS-STATE
      *
           IF    ( SPA-GMN-PAGE   <=  1 )
               MOVE    WIDGET-INSENSITIVE  TO  X03-B09-STATE
               MOVE    WIDGET-INSENSITIVE  TO  X03-B09-2-STATE
           END-IF
      *
           IF    ( SPA-GMN-NEXT     =  ZERO )
               MOVE    WIDGET-INSENSITIVE  TO  X03-B10-STATE
               MOVE    WIDGET-INSENSITIVE  TO  X03-B10-2-STATE
           END-IF
      *
           IF    ( FLG-RCHKLST-SELECT  =   ZERO )
               MOVE    WIDGET-INSENSITIVE  TO  X03-BTN-INS-STATE
           END-IF
      *
      *    カーソル位置
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    カーソル位置処理
      *****************************************************************
       5001-MAPCUR-SEC                 SECTION.
      *
           IF    ( SPA-ERRCD           =   SPACE )
             AND ( SPA-GMN-CUR     =   ZERO  )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-CUR
           WHEN    01
               MOVE    "CHKKBN"        TO  MCP-WIDGET
           WHEN    02
               MOVE    "SELNAME"       TO  MCP-WIDGET
           WHEN    03
               MOVE    "KJNYMD"        TO  MCP-WIDGET
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
           WHEN    "CHKKBN"
               MOVE    02              TO  SPA-GMN-CUR
           WHEN    "SELNAME"
               MOVE    02              TO  SPA-GMN-CUR
           WHEN    "KJNYMD"
               MOVE    02              TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
      *
       5011-INPUT-CUR-EXT.
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
               MOVE    "基準日入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0002"
               MOVE    "チェック区分入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0003"
               MOVE    "チェック名称入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0012"
               MOVE    "セットコードは入力できません"
                                           TO  SPA-ERRMSG
           WHEN    "1001"
               MOVE    "登録上限数を超えています"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  XERR
           MOVE    SPA-ERRCD           TO  XERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  XERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "X03"               TO  SPA-MOTOPG
           MOVE    "XERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "XERR"              TO  MCP-WINDOW
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
      *    西暦日本語変換処理
      *****************************************************************
       800-SEIWA-HEN-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-HIZUKE-ERR
           MOVE    SPACE               TO  WRK-SYMD
                                           WRK-HENYMDG
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
       800-SEIWA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    全角半角混在チェック
      *****************************************************************
       800-ENRTRY-CHK-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-ENTRYCHK
      *
           IF  ( WRK-KANACHK-MAE-INPUT =   SPACE )
               GO  TO  800-ENRTRY-CHK-EXT
           END-IF
      *
           MOVE    SPACE               TO  ORCSKANACHKAREA
           INITIALIZE                      ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    WRK-KANACHK-MAE-INPUT
                                       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT   TO  WRK-KANACHK-OUT-INPUT
           IF      ( KANACHK-RC    NOT =   ZERO )
               MOVE    9               TO  FLG-ENTRYCHK
               GO  TO  800-ENRTRY-CHK-EXT
           END-IF
      *
           MOVE    KANACHK-SYUBETU     TO  FLG-ENTRYCHK
      *
           .
       800-ENRTRY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ検索処理（ＫＥＹ１１）
      *****************************************************************
       900-CHK-KEY11-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
      *
           INITIALIZE                      CHK-REC
           MOVE    SPA-GMN-CHKKBN  TO  CHK-CHKKBN
           MOVE    SPA-NAI-KJNYMD  TO  CHK-YUKOSTYMD
                                           CHK-YUKOEDYMD
      *
           PERFORM VARYING IDX0    FROM    70  BY  -1
                   UNTIL ( IDX0    <   1 )
                    OR   ( SPA-GMN-SELNAME (IDX0:1)
                               NOT =   SPACE )
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX0    >   ZERO )
               STRING  SPA-GMN-SELNAME (1:IDX0)
                                           DELIMITED   BY  SIZE
                       "%"                 DELIMITED   BY  SIZE
               INTO    CHK-SELNAME
               END-STRING
           ELSE
               MOVE    "%"             TO  CHK-SELNAME
           END-IF
      *
           INSPECT CHK-SELNAME
                   REPLACING   ALL "＊"    BY  "%%"
      *
           MOVE    SPA-HOSPNUM         TO  CHK-HOSPNUM
           MOVE    CHK-REC             TO  MCPDATA-REC
           MOVE    "tbl_chk"           TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    1               TO  FLG-CHK
               INITIALIZE                  CHK-REC
           END-IF
      *
           .
       900-CHK-KEY11-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ検索処理
      *****************************************************************
       900-CHK-KEY15-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
      *
           INITIALIZE                      CHK-REC
           MOVE    SPA-GMN-CHKKBN  TO  CHK-CHKKBN
           MOVE    SPA-NAI-KJNYMD  TO  CHK-YUKOSTYMD
                                           CHK-YUKOEDYMD
           MOVE    WRK-CDSYUBETU       TO  CHK-SELCDSYU
           MOVE    SPA-GMN-SELNAME TO  CHK-SELINPUTCD
           MOVE    SPA-HOSPNUM         TO  CHK-HOSPNUM
           MOVE    CHK-REC             TO  MCPDATA-REC
           MOVE    "tbl_chk"           TO  MCP-TABLE
           MOVE    "key15"             TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    1               TO  FLG-CHK
               INITIALIZE                  CHK-REC
           END-IF
      *
           .
       900-CHK-KEY15-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ検索処理
      *****************************************************************
       900-CHK-KEY16-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
      *
           INITIALIZE                      CHK-REC
           MOVE    SPA-GMN-CHKKBN  TO  CHK-CHKKBN
           MOVE    SPA-NAI-KJNYMD  TO  CHK-YUKOSTYMD
                                           CHK-YUKOEDYMD
           MOVE    WRK-CDSYUBETU       TO  CHK-SELCDSYU
           STRING  SPA-GMN-SELNAME DELIMITED   BY  SPACE
                   "%"                 DELIMITED   BY  SIZE
           INTO    CHK-SELINPUTCD
           END-STRING
           MOVE    SPA-HOSPNUM         TO  CHK-HOSPNUM
           MOVE    CHK-REC             TO  MCPDATA-REC
           MOVE    "tbl_chk"           TO  MCP-TABLE
           MOVE    "key16"             TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    1               TO  FLG-CHK
               INITIALIZE                  CHK-REC
           END-IF
      *
           .
       900-CHK-KEY16-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ検索処理
      *****************************************************************
       900-CHK-KEY17-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
      *
           INITIALIZE                      CHK-REC
           MOVE    SPA-GMN-CHKKBN  TO  CHK-CHKKBN
           MOVE    SPA-NAI-KJNYMD  TO  CHK-YUKOSTYMD
                                           CHK-YUKOEDYMD
           MOVE    SPA-GMN-SELNAME TO  CHK-SRYCD
           MOVE    SPA-HOSPNUM         TO  CHK-HOSPNUM
           MOVE    CHK-REC             TO  MCPDATA-REC
           MOVE    "tbl_chk"           TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    1               TO  FLG-CHK
               INITIALIZE                  CHK-REC
           END-IF
      *
           .
       900-CHK-KEY17-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ読込処理
      *****************************************************************
       900-CHK-FET-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
      *
           MOVE    WRK-TABLE           TO  MCP-TABLE
           MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  CHK-REC
           ELSE
               MOVE    1               TO  FLG-CHK
               INITIALIZE                  CHK-REC
           END-IF
      *
           .
       900-CHK-FET-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２検索処理（ＫＥＹ１１）
      *****************************************************************
       900-CHKSND-KEY11-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKSND
      *
           INITIALIZE                      CHKSND-REC
           MOVE    SPA-GMN-CHKKBN  TO  CHKSND-CHKKBN
           MOVE    SPA-NAI-KJNYMD  TO  CHKSND-YUKOSTYMD
                                       CHKSND-YUKOEDYMD
           MOVE    ZERO            TO  CHKSND-CDKBN
      *
           PERFORM VARYING IDX0    FROM    70  BY  -1
                   UNTIL ( IDX0    <   1 )
                    OR   ( SPA-GMN-SELNAME (IDX0:1)
                               NOT =   SPACE )
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX0    >   ZERO )
               STRING  SPA-GMN-SELNAME (1:IDX0)
                                           DELIMITED   BY  SIZE
                       "%"                 DELIMITED   BY  SIZE
               INTO    CHKSND-SELNAME
               END-STRING
           ELSE
               MOVE    "%"             TO  CHKSND-SELNAME
           END-IF
      *
           INSPECT CHKSND-SELNAME
                   REPLACING   ALL "＊"    BY  "%%"
      *
           MOVE    SPA-HOSPNUM         TO  CHKSND-HOSPNUM
           MOVE    CHKSND-REC          TO  MCPDATA-REC
           MOVE    "tbl_chksnd"            TO  MCP-TABLE
           MOVE    "key11"                 TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    1               TO  FLG-CHKSND
               INITIALIZE                  CHKSND-REC
           END-IF
      *
           .
       900-CHKSND-KEY11-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２検索処理
      *****************************************************************
       900-CHKSND-KEY16-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKSND
      *
           INITIALIZE                      CHKSND-REC
           MOVE    SPA-GMN-CHKKBN  TO  CHKSND-CHKKBN
           MOVE    SPA-NAI-KJNYMD  TO  CHKSND-YUKOSTYMD
                                       CHKSND-YUKOEDYMD
           MOVE    ZERO             TO CHKSND-CDKBN
           MOVE    WRK-CDSYUBETU       TO  CHKSND-SELCDSYU
           MOVE    SPA-GMN-SELNAME TO  CHKSND-SELINPUTCD
           MOVE    SPA-HOSPNUM         TO  CHKSND-HOSPNUM
           MOVE    CHKSND-REC             TO  MCPDATA-REC
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key16"             TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    1               TO  FLG-CHKSND
               INITIALIZE                  CHKSND-REC
           END-IF
      *
           .
       900-CHKSND-KEY16-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２検索処理
      *****************************************************************
       900-CHKSND-KEY17-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKSND
      *
           INITIALIZE                      CHKSND-REC
           MOVE    SPA-GMN-CHKKBN  TO  CHKSND-CHKKBN
           MOVE    SPA-NAI-KJNYMD  TO  CHKSND-YUKOSTYMD
                                       CHKSND-YUKOEDYMD
           MOVE    ZERO             TO CHKSND-CDKBN
           MOVE    WRK-CDSYUBETU       TO  CHKSND-SELCDSYU
           STRING  SPA-GMN-SELNAME DELIMITED   BY  SPACE
                   "%"                 DELIMITED   BY  SIZE
           INTO    CHKSND-SELINPUTCD
           END-STRING
           MOVE    SPA-HOSPNUM         TO  CHKSND-HOSPNUM
           MOVE    CHKSND-REC             TO  MCPDATA-REC
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    1               TO  FLG-CHKSND
               INITIALIZE                  CHKSND-REC
           END-IF
      *
           .
       900-CHKSND-KEY17-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２検索処理
      *****************************************************************
       900-CHKSND-KEY18-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKSND
      *
           INITIALIZE                      CHKSND-REC
           MOVE    SPA-GMN-CHKKBN  TO  CHKSND-CHKKBN
           MOVE    SPA-NAI-KJNYMD  TO  CHKSND-YUKOSTYMD
                                           CHKSND-YUKOEDYMD
           MOVE    SPA-GMN-SELNAME TO  CHKSND-SRYCD
           MOVE    ZERO             TO CHKSND-CDKBN
           MOVE    SPA-HOSPNUM         TO  CHKSND-HOSPNUM
           MOVE    CHKSND-REC             TO  MCPDATA-REC
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key18"             TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    1               TO  FLG-CHKSND
               INITIALIZE                  CHKSND-REC
           END-IF
      *
           .
       900-CHKSND-KEY18-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２読込処理
      *****************************************************************
       900-CHKSND-FET-SEC                  SECTION.
      *
           MOVE    WRK-TABLE            TO  MCP-TABLE
           MOVE    WRK-PATHNAME         TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  CHKSND-REC
           ELSE
               MOVE    1               TO  FLG-CHKSND
               INITIALIZE                  CHKSND-REC
           END-IF
      *
           .
       900-CHKSND-FET-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ３検索処理（ＫＥＹ１１）
      *****************************************************************
       900-CHKTRD-KEY11-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKTRD
      *
           INITIALIZE                      CHKTRD-REC
           MOVE    SPA-GMN-CHKKBN  TO  CHKTRD-CHKKBN
           MOVE    SPA-NAI-KJNYMD  TO  CHKTRD-YUKOSTYMD
                                           CHKTRD-YUKOEDYMD
      *
           PERFORM VARYING IDX0    FROM    70  BY  -1
                   UNTIL ( IDX0    <   1 )
                    OR   ( SPA-GMN-SELNAME (IDX0:1)
                               NOT =   SPACE )
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX0    >   ZERO )
               STRING  SPA-GMN-SELNAME (1:IDX0)
                                           DELIMITED   BY  SIZE
                       "%"                 DELIMITED   BY  SIZE
               INTO    CHKTRD-SELNAME
               END-STRING
           ELSE
               MOVE    "%"             TO  CHKTRD-SELNAME
           END-IF
      *
           INSPECT CHKTRD-SELNAME
                   REPLACING   ALL "＊"    BY  "%%"
      *
           MOVE    SPA-HOSPNUM         TO  CHKTRD-HOSPNUM
           MOVE    CHKTRD-REC          TO  MCPDATA-REC
           MOVE    "tbl_chktrd"            TO  MCP-TABLE
           MOVE    "key11"                 TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    1               TO  FLG-CHKTRD
               INITIALIZE                  CHKTRD-REC
           END-IF
      *
           .
       900-CHKTRD-KEY11-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ３検索処理
      *****************************************************************
       900-CHKTRD-KEY15-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKTRD
      *
           INITIALIZE                      CHKTRD-REC
           MOVE    SPA-GMN-CHKKBN  TO  CHKTRD-CHKKBN
           MOVE    SPA-NAI-KJNYMD  TO  CHKTRD-YUKOSTYMD
                                           CHKTRD-YUKOEDYMD
           MOVE    WRK-CDSYUBETU       TO  CHKTRD-SELCDSYU
           MOVE    SPA-GMN-SELNAME TO  CHKTRD-SELINPUTCD
           MOVE    SPA-HOSPNUM         TO  CHKTRD-HOSPNUM
           MOVE    CHKTRD-REC             TO  MCPDATA-REC
           MOVE    "tbl_chktrd"            TO  MCP-TABLE
           MOVE    "key15"             TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    1               TO  FLG-CHKTRD
               INITIALIZE                  CHKTRD-REC
           END-IF
      *
           .
       900-CHKTRD-KEY15-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ３検索処理
      *****************************************************************
       900-CHKTRD-KEY16-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKTRD
      *
           INITIALIZE                      CHKTRD-REC
           MOVE    SPA-GMN-CHKKBN  TO  CHKTRD-CHKKBN
           MOVE    SPA-NAI-KJNYMD  TO  CHKTRD-YUKOSTYMD
                                           CHKTRD-YUKOEDYMD
           MOVE    WRK-CDSYUBETU       TO  CHKTRD-SELCDSYU
           STRING  SPA-GMN-SELNAME DELIMITED   BY  SPACE
                   "%"                 DELIMITED   BY  SIZE
           INTO    CHKTRD-SELINPUTCD
           END-STRING
           MOVE    SPA-HOSPNUM         TO  CHKTRD-HOSPNUM
           MOVE    CHKTRD-REC             TO  MCPDATA-REC
           MOVE    "tbl_chktrd"            TO  MCP-TABLE
           MOVE    "key16"             TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    1               TO  FLG-CHKTRD
               INITIALIZE                  CHKTRD-REC
           END-IF
      *
           .
       900-CHKTRD-KEY16-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ３検索処理
      *****************************************************************
       900-CHKTRD-KEY17-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKTRD
      *
           INITIALIZE                      CHKTRD-REC
           MOVE    SPA-GMN-CHKKBN  TO  CHKTRD-CHKKBN
           MOVE    SPA-NAI-KJNYMD  TO  CHKTRD-YUKOSTYMD
                                           CHKTRD-YUKOEDYMD
           MOVE    SPA-GMN-SELNAME TO  CHKTRD-SRYCD
           MOVE    SPA-HOSPNUM         TO  CHKTRD-HOSPNUM
           MOVE    CHKTRD-REC             TO  MCPDATA-REC
           MOVE    "tbl_chktrd"            TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    1               TO  FLG-CHKTRD
               INITIALIZE                  CHKTRD-REC
           END-IF
      *
           .
       900-CHKTRD-KEY17-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ３読込処理
      *****************************************************************
       900-CHKTRD-FET-SEC        SECTION.
      *
           MOVE    WRK-TABLE            TO  MCP-TABLE
           MOVE    WRK-PATHNAME         TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  CHKTRD-REC
           ELSE
               MOVE    1               TO  FLG-CHKTRD
               INITIALIZE                  CHKTRD-REC
           END-IF
      *
           .
       900-CHKTRD-FET-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ５検索処理
      *****************************************************************
       900-CHK005-KEY7-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK005
      *
           INITIALIZE                      CHK005-REC
           MOVE    SPA-GMN-CHKKBN  TO  CHK005-CHKKBN
           MOVE    SPA-NAI-KJNYMD  TO  CHK005-YUKOSTYMD
                                           CHK005-YUKOEDYMD
      *
           STRING  SPA-GMN-SELNAME     DELIMITED   BY  SPACE
                   "%"                     DELIMITED   BY  SIZE
           INTO    CHK005-BYOMEI
           END-STRING
      *
           INSPECT CHK005-BYOMEI
                   REPLACING   ALL "＊"    BY  "%%"
      *
           MOVE    SPA-HOSPNUM         TO  CHK005-HOSPNUM
           MOVE    CHK005-REC          TO  MCPDATA-REC
           MOVE    "tbl_chk005"        TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    1               TO  FLG-CHK005
               INITIALIZE                  CHK005-REC
           END-IF
      *
           .
       900-CHK005-KEY7-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ５読込処理
      *****************************************************************
       900-CHK005-KEY7-FET-SEC         SECTION.
      *
           MOVE    "tbl_chk005"        TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  CHK005-REC
           ELSE
               MOVE    1               TO  FLG-CHK005
               INITIALIZE                  CHK005-REC
           END-IF
      *
           .
       900-CHK005-KEY7-FET-EXT.
           EXIT.
      *****************************************************************
      *    適応病名マスタ検索処理（ＫＥＹ４）
      *****************************************************************
       900-TEKIOUBYOMEI-KEY4-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-TEKIOUBYOMEI
      *
           INITIALIZE                      TEKIOUBYOMEI-REC
           MOVE    SPA-X01-CHKKBN      TO  TEKBYO-CHKKBN
           MOVE    SPA-NAI-KJNYMD  TO  TEKBYO-SELYUKOSTYMD
                                           TEKBYO-SELYUKOEDYMD
      *
           PERFORM VARYING IDX0    FROM    70  BY  -1
                   UNTIL ( IDX0    <   1 )
                    OR   ( SPA-GMN-SELNAME (IDX0:1)
                               NOT =   SPACE )
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX0    >   ZERO )
               STRING  SPA-GMN-SELNAME (1:IDX0)
                                           DELIMITED   BY  SIZE
                       "%"                 DELIMITED   BY  SIZE
               INTO    TEKBYO-SELNAME
               END-STRING
           ELSE
               MOVE    "%"             TO  TEKBYO-SELNAME
           END-IF
      *
      **** MOVE    SPA-HOSPNUM         TO  TEKBYO-HOSPNUM
           MOVE    TEKIOUBYOMEI-REC         TO  MCPDATA-REC
           MOVE    "tbl_tekioubyomei"       TO  MCP-TABLE
           MOVE    "key4"                   TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    1               TO  FLG-TEKIOUBYOMEI
               INITIALIZE                  TEKIOUBYOMEI-REC
           END-IF
      *
           .
       900-TEKIOUBYOMEI-KEY4-SEL-EXT.
           EXIT.
      *****************************************************************
      *    適応病名マスタ検索処理(KEY4)
      *****************************************************************
       900-TEKIOUBYOMEI-KEY4-FET-SEC        SECTION.
      *
           MOVE    "tbl_tekioubyomei"       TO  MCP-TABLE
           MOVE    "key4"                   TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  TEKIOUBYOMEI-REC
           ELSE
               MOVE    1               TO  FLG-TEKIOUBYOMEI
               INITIALIZE                  TEKIOUBYOMEI-REC
           END-IF
      *
           .
       900-TEKIOUBYOMEI-KEY4-FET-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ検索処理
      *****************************************************************
       900-TENSU-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
           INITIALIZE                      TNS-TENSU-REC
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           MOVE    WRK-YMD             TO  TNS-YUKOSTYMD
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
      *    入力コードマスター読込処理
      *****************************************************************
       900-INPUTCD-SEL-KEY2-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-INPUTCD
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF  ( MCP-RC        =   ZERO )
               MOVE    MCPDATA-REC TO  INPUTCD-REC
           ELSE
               MOVE    1           TO  FLG-INPUTCD
           END-IF
      *
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-INPUTCD-SEL-KEY2-EXT.
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
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
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
           MOVE   "PUTWINDOW"          TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       900-PUT-WINDOW-EXT.
           EXIT.
