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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCGC50.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセコメント入力
      *  コンポーネント名  : レセコメント入力（Ｃ５０）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  06/06/15    NACL-多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  00.00.01    NACL-多々納  06/08/29  詳記区分初期表示の追加
      *  03.04.00    NACL-多々納  07/04/12  詳記区分 ５０、５１追加
      *  03.05.00    NACL-多々納  07/05/XX  グループ診療対応
      *  03.05.00    NACL-多々納  07/06/05  コメント内容の全角変換
      *  04.06.00    NACL-多々納  10/10/01  コメント文字　３０００、
      *                                     連番対応他
      *  04.07.00    NACL-多々納  13/09/XX  第三者行為対応
      *  04.07.00    NACL-多々納  15/07/XX  診療科・保険組合せ件数追加
      *  04.08.00    NACL-多々納  16/01/27  文字数確認追加
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
      *FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    画面色等
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    画面スパ領域
           COPY    "C50COMMON-SPA".
      *
           COPY    "C50SCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-RECECOM         PIC 9(01).
           03  FLG-EXAMPLE         PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-PTRSIINF        PIC 9(01).
           03  FLG-PTNYUINRRK      PIC 9(01).
           03  FLG-NYUINACCT       PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
      *
           03  FLG-HKNCOMBICHK     PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-COPY-END        PIC 9(01).
      *
           03  FLG-HENOK           PIC 9(01).
           03  FLG-HENOK2          PIC 9(01).
           03  FLG-BUNREI          PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDH                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX-D1              PIC 9(04).
           03  IDX-D2              PIC 9(04).
           03  IDX-D3              PIC 9(04).
           03  IDX-Z               PIC 9(04).
           03  IDX-S               PIC 9(04).
           03  IDX-S2              PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD            PIC X(08).
           03  WRK-HKNCOMBI        PIC 9(04).
           03  WRK-HKNCOMBIMEI     PIC X(100).
           03  WRK-HKNCOMBIMEI-H   PIC X(100).
           03  WRK-HKNKBN-MEI      PIC X(02).
      *
           03  WRK-NYUGAIKBN       PIC X(01).
           03  WRK-SRYKA           PIC X(02).
           03  WRK-SRYKAMEI        PIC X(20).
           03  WRK-SRYYMD          PIC X(08).
      *    保険組合せ再設定
           03  WRK-HKN-IDX         PIC 9(04).
           03  WRK-HKN-IDX1        PIC 9(04).
      *
           03  WRK-SELNUM-CHK-G.
               05  WRK-SELNUM-CHK  PIC 9(03)   OCCURS  100.
           03  WRK-HEN-SELNUM      PIC X(40).
           03  WRK-HEN-SELNUM-2    PIC X(40).
           03  WRK-HEN-SELNUM-HZN  PIC X(40).
      *
           03  WRK-SELNUM          PIC 9(03).
           03  WRK-SELNUM2         PIC 9(03).
      *
           03  WRK-SELNUM-X        PIC X(03).
           03  WRK-C50-SELNUM-X   PIC X(40).
      *
           03  WRK-ZZZZ            PIC ZZZZ.
           03  WRK-ZZZ             PIC ZZZ.
           03  WRK-ZZ              PIC ZZ.
      *
           03  WRK-DBPATH          PIC X(20).
           03  WRK-MCP-WIDGET      PIC X(64).
      *
           03  WRK-KEY-AREA.
               05  WRK-KEY-SRYYM       PIC X(06).
               05  WRK-KEY-NYUGAIKBN   PIC X(01).
               05  WRK-KEY-SRYKA       PIC X(02).
               05  WRK-KEY-HKNCOMBI    PIC X(04).
               05  WRK-KEY-SRYDD       PIC X(02).
           03  WRK-OLD-AREA.
               05  WRK-OLD-SRYYM       PIC X(06).
               05  WRK-OLD-NYUGAIKBN   PIC X(01).
               05  WRK-OLD-SRYKA       PIC X(02).
               05  WRK-OLD-HKNCOMBI    PIC X(04).
               05  WRK-OLD-SRYDD       PIC X(02).
      *
           03  WRK-RENNUM-MAX          PIC 9(02).
      *
           03  WRK-ERRCD               PIC X(04).
           03  WRK-C50IDMSG            PIC X(80).
      *
           03  WRK-COPYYM.
               05  WRK-COPYYY          PIC 9(04).
               05  WRK-COPYMM          PIC 9(02).
      *
           03  WRK-SRYDD               PIC X(02).
           03  WRK-NYUINYMD            PIC X(08).
           03  WRK-TAIINYMD            PIC X(08). 
      *
      *    コラムリスト位置
           03  WRK-C50-RECE-ROW        PIC S9(09).
           03  FLG-GMN-INIT            PIC  9(01).
      *
           03  WRK-C50-COMM-ROW        PIC S9(09).
           03  FLG-GMN-INIT2           PIC  9(01).
      *
      *    保険組合せワーク
           03  WRK-GMN-HKNCOMB-COMB.
               05  WRK-GMN-HKN-LIST    OCCURS  50.
                   07  WRK-GMN-THKNCOMBINUM    PIC X(04).
                   07  WRK-GMN-THKNCOMBY1      PIC X(01).
                   07  WRK-GMN-THKNCOMBIMEI    PIC X(100).
                   07  WRK-GMN-FLG             PIC X(01).
                   07  WRK-GMN-RSI-HKNKBN      PIC X(01).
                05  WRK-GMN-HKNMAX              PIC 9(04).
      *    一括削除編集
           03  WRK-SELNUM-G.
               05  WRK-SEL-OCC         OCCURS   100.
                   07  WRK-TSELNUM-X    PIC X(03).
                   07  WRK-TSELNUM      PIC 9(03).
                   07  WRK-TSELH        PIC X(01).
           03  WRK-SELNUM-AREA.
               05  WRK-SELNUM-OCC    OCCURS   100.
                   07  WRK-SEL         PIC 9(01).
      *
           03  WRK-MOJI-MAX            PIC 9(04).
      *
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
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *
           COPY    "CPSK1001.INC".
      *
           COPY    "CPSK1005.INC".
      *
           COPY    "CPSK1042.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    レセプトコメント
       01  RECECOM-REC.
           COPY    "CPRECECOM.INC".
      *    保存用レセプトコメント
       01  HZN-RECECOM-REC.
           COPY    "CPRECECOM.INC"  REPLACING
                                     //RECECOM-// BY //HZN-RECECOM-//.
      *
      *    雛形
       01  EXAMPLE-REC.
           COPY    "CPEXAMPLE.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *    入院会計マスタ−
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
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
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      **** COPY    "CPORCXKANACONV.INC".
           COPY    "CPORCXKANACONV2.INC".
      *
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    保険組合せセット領域
           COPY    "CPORCSHKNSET.INC".
      *
      *    レセプト作成管理マスタ更新サブ
           COPY    "CPORCSRECEUPD.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
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
           COPY    "C50.INC".
           COPY    "C501.INC".
           COPY    "C50ERR.INC".
           COPY    "C50ID1.INC".
           COPY    "CPAPIMODV2.INC".
      *
       PROCEDURE                   DIVISION    USING
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
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-WORK            TO  SPA-C50KYOUTU
           MOVE    SPA-FREE            TO  SPA-C50
      *
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    ZERO                TO  FLG-END
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN     "PUTG"           ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-C50KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-C50             TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "C50ERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
      *        文例登録時の４００文字以上は確認メッセージ表示
               IF      SPA-NAI-KEIFLG      =   1
                   MOVE    SPA-NAI-KEIMSG      TO  SPA-C50IDCD
                   MOVE    SPACE               TO  SPA-NAI-KEIMSG
                   MOVE    ZERO                TO  SPA-NAI-KEIFLG
                   PERFORM 520-C50IDSET-SEC
               END-IF
           ELSE
      *        初期画面編集
               PERFORM 300-SCREEN-SEC
      *        画面編集
               IF      FLG-END             =   ZERO
                   PERFORM 500-GMNHEN-SEC
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SPACE               TO  LNK-KYOUTU
      *
               MOVE   SPACE                TO  MCP-PUTTYPE
               MOVE   "C50"                TO  MCP-WINDOW
               PERFORM 900-PUT-WINDOW
      *
               MOVE    1                   TO  FLG-END
           END-IF
           .
      *
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC             SECTION.
      *
           IF      LNK-KYOUTU      NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-AREA
           END-IF
      *
      *    内部画面より
           EVALUATE    SPA-MOTOPG
      *            文例画面より
               WHEN    "C501"
                   IF      SPA-C501-SELNUM     NOT =   ZERO
                       MOVE    1                   TO  SPA-GMN-CUR
                       MOVE    SPACE               TO  SPA-C50IDCD
                   ELSE
                       MOVE    1                   TO  SPA-GMN-CUR
                   END-IF
               WHEN    "C50ID1"
      *            確認画面より
                   PERFORM 3002-C50ID1-SET-SEC
               WHEN    OTHER
                   PERFORM 3001-SPA-INIT-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＸＤＩＤ１）ＯＫ処理
      *****************************************************************
       3002-C50ID1-SET-SEC          SECTION.
      *
           EVALUATE    SPA-C50IDCD
               WHEN    "0101"
               WHEN    "0105"
               WHEN    "0107"
               WHEN    "0110"
               WHEN    "0201"
               WHEN    "0205"
      *            コメント登録の確認
                   IF      SPA-C50ID1-FLG      =   "OK"
                       PERFORM 2121-TOUROKU-SYORI-SEC
                   ELSE
                       MOVE    1               TO  SPA-GMN-CUR
                   END-IF
               WHEN    "0108"
               WHEN    "0109"
               WHEN    "0206"
      *            コメント空白の登録確認
                   IF      SPA-C50ID1-FLG      =   "OK"
                       PERFORM 2122-ALLTOUROKU-SYORI-SEC
                   ELSE
                       MOVE    1               TO  SPA-GMN-CUR
                   END-IF
               WHEN    "0104"
      *            コメント削除の確認
                   IF      SPA-C50ID1-FLG      =   "OK"
                       PERFORM 2401-COMMENT-DELSYORI-SEC
                   ELSE
                       MOVE    1                   TO  SPA-GMN-CUR
                   END-IF
               WHEN    "0102"
               WHEN    "0103"
               WHEN    "0106"
      *            文例の確認
                   IF      SPA-C50ID1-FLG      =   "OK"
                       PERFORM 2801-BUNREI-SYORI-SEC
                   ELSE
                       MOVE    1                   TO  SPA-GMN-CUR
                   END-IF
           END-EVALUATE
           MOVE    SPACE               TO  SPA-C50IDCD
      *
           .
       3002-C50ID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面編集処理
      *****************************************************************
       3001-SPA-INIT-SEC             SECTION.
      *
           INITIALIZE                  SPA-C50KYOUTU
           MOVE    SPA-MOTOPG          TO  SPA-HZN-MOTOPG
      *
           MOVE    SPA-PTID            TO  SPA-GMN-PTID
           MOVE    SPA-PTNUM           TO  SPA-GMN-PTNUM
           MOVE    SPA-NAME            TO  SPA-GMN-NAME
           MOVE    SPA-BIRTHDAYW       TO  SPA-GMN-BIRTHDAYW
           MOVE    SPA-SEX             TO  SPA-GMN-SEX
      *
           MOVE    SPA-SRYYMD(1:6)     TO  SPA-NAI-SRYYM
           MOVE    SPA-SRYYMDW(1:6)    TO  SPA-GMN-SRYYM
      *
           IF      SPA-PTID            =   ZERO
      *        文例登録のみ
               MOVE    2               TO  SPA-GMN-SYORI
           ELSE
               MOVE    SPACE               TO  WRK-NYUGAIKBN
               IF      SPA-NYUGAIKBN       =   SPACE
                   PERFORM 3104-NYUGAIKBN-HEN-SEC
               END-IF
      *        症状詳記初期表示区分
               PERFORM 3109-SJKBN-SET-SEC
           END-IF
      *
           PERFORM 310-SPA-SET-SEC
           .
       3001-SPA-INIT-EXT.
           EXIT.
      *****************************************************************
      *    症状詳記初期表示区分処理
      *****************************************************************
       3109-SJKBN-SET-SEC          SECTION.
      *
           MOVE    SPACE               TO  SYS-1042-REC
           MOVE    "1042"              TO  SYS-1042-KANRICD
           MOVE    "*"                 TO  SYS-1042-KBNCD
           MOVE    SPA-NAI-SRYYM       TO  SYS-1042-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-1042-STYUKYMD(7:2)
           MOVE    SYS-1042-STYUKYMD   TO  SYS-1042-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1042-HOSPNUM
           MOVE    SYS-1042-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1042-REC
               MOVE    SYS-1042-SJKBN-SET  TO  SPA-SJKBN-SET
           END-IF
           IF      SPA-SJKBN-SET       =   SPACE
               MOVE    "01"                TO  SPA-SJKBN-SET
           END-IF
      *
           .
       3109-SJKBN-SET-EXT.
           EXIT.
      *****************************************************************
      *    入外区分編集処理
      *****************************************************************
       3104-NYUGAIKBN-HEN-SEC          SECTION.
      *
           MOVE    SPACE               TO  WRK-TAIINYMD
           MOVE    SPACE               TO  WRK-NYUINYMD
      *    入院履歴判定
           MOVE    SPACE               TO  PTNYUINRRK-REC
           INITIALIZE                  PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-PTID            TO  PTNYUINRRK-PTID
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 910-PTNYUINRRK-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
      *
      *    入院が１件もないとき、外来とする
           IF      FLG-PTNYUINRRK      =   1
               MOVE    "2"             TO   SPA-NYUGAIKBN
           ELSE
               MOVE    ZERO                TO  FLG-OK
               PERFORM UNTIL      (FLG-PTNYUINRRK      =   1 )
      *                転入日
                   IF      PTNYUINRRK-TENNYUYMD(1:6)
                                               <=  SPA-NAI-SRYYM
      *                転出日
                       IF     ( PTNYUINRRK-TENSTUYMD   =   SPACE )  OR
                              ((PTNYUINRRK-TENSTUYMD   NOT =
                                                           SPACE ) AND
                               (PTNYUINRRK-TENSTUYMD(1:6)  >=
                                                       SPA-NAI-SRYYM))
                           MOVE    PTNYUINRRK-NYUINYMD TO  WRK-NYUINYMD
                           MOVE    PTNYUINRRK-TAIINYMD TO  WRK-TAIINYMD
                           MOVE    1                   TO  FLG-OK
                       END-IF
                   END-IF
      *
                   MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
                   MOVE    "key8"              TO  MCP-PATHNAME
                   PERFORM 910-PTNYUINRRK-READ-SEC
               END-PERFORM
      *
               IF      FLG-OK              =   1
      *            入院中の場合入院とする
                   IF      (WRK-NYUINYMD(1:6)  <   SPA-NAI-SRYYM  )  AND
                           (WRK-TAIINYMD(1:6)  >   SPA-NAI-SRYYM  )
                       MOVE    "1"             TO  SPA-NYUGAIKBN
                   END-IF
               END-IF
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      SPA-NYUGAIKBN       =   SPACE
               MOVE    "2"             TO   SPA-NYUGAIKBN
           END-IF
      *
           .
       3104-NYUGAIKBN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面編集処理
      *****************************************************************
       310-SPA-SET-SEC             SECTION.
      *
           INITIALIZE                  SPA-C50
           INITIALIZE                  SPA-C50ID-AREA
      *
           MOVE    SPA-SRYYMD(1:6)     TO  SPA-NAI-SRYYM
           MOVE    SPA-SRYYMDW(1:6)    TO  SPA-GMN-SRYYM
           IF      WRK-NYUGAIKBN       =   SPACE
               MOVE    SPA-NYUGAIKBN       TO  SPA-GMN-NYUGAIKBN
           ELSE
               MOVE    WRK-NYUGAIKBN       TO  SPA-GMN-NYUGAIKBN
           END-IF
           IF      SPA-PTID            =   ZERO
      *        文例登録のみ
               MOVE    2               TO  SPA-GMN-SYORI
               MOVE    SPACE           TO  SPA-GMN-NYUGAIKBN
           ELSE
      *        患者情報初期編集
               PERFORM 3100-SPA-INIT-HEN-SEC
               MOVE    1               TO  SPA-GMN-SYORI
           END-IF
           MOVE    1                   TO  SPA-GMN-CUR
           .
       310-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報初期編集処理
      *****************************************************************
       3100-SPA-INIT-HEN-SEC        SECTION.
      *
      *    保険組合せ
           PERFORM 3101-HKNCOMBI-HEN-SEC
      *    診療科
           PERFORM 3102-SRYKA-HEN-SEC
      *    入院判定
           PERFORM 3105-NYUGAI-HEN-SEC
      *    症状詳記区分
           PERFORM 3106-SJKBN-HEN-SEC
      *
      *    コメント編集編集
           MOVE    ZERO                TO  SPA-GMN-NEXT
           MOVE    1                   TO  SPA-GMN-PAGE
           PERFORM 3103-RECECOM-HEN-SEC
      *
      *    初期編集
           PERFORM 314-SYOKI-SEC
      *
           .
       3100-SPA-INIT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ名称編集処理
      *****************************************************************
       3101-HKNCOMBI-HEN-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-HKNCOMB-COMB
           MOVE    ZERO                TO  SPA-GMN-HKNMAX
           MOVE    ZERO                TO  FLG-HKNCOMBICHK
      *
           MOVE    SPA-SRYYMD          TO  WRK-SRYYMD
      *
           MOVE    SPA-NAI-SRYYM       TO  SPA-SRYYMD(1:6)
           MOVE    "00"                TO  SPA-SRYYMD(7:2)
           MOVE    SPA-GMN-PTID        TO  SPA-PTID
           MOVE    SPA-GMN-NYUGAIKBN   TO  SPA-NYUGAIKBN
      *
           INITIALIZE                  ORCSHKNSETAREA
           MOVE    "1"                 TO  ORCSHKN-KBN
           MOVE    SPACE               TO  ORCSHKN-KBN2
           CALL    "ORCSHKNSET"        USING
                                       SPA-AREA
                                       ORCSHKNSETAREA
      *
           MOVE    ORCSHKN-HKN-MAX     TO  SPA-GMN-HKNMAX
      *
           MOVE    ZERO                TO  IDH
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL      (IDX     >   ORCSHKN-HKN-MAX )  OR
                              (IDH     >=  50              )
               ADD     1                   TO  IDH
               MOVE    ORCSHKN-GMN-HKNCOMBINUM (IDX)
                                           TO  SPA-GMN-THKNCOMBINUM
                                                                (IDH)
               MOVE    ORCSHKN-GMN-HKNCOMBIMEI (IDX)
                                           TO  SPA-GMN-THKNCOMBIMEI
                                                               (IDH)
               MOVE    ORCSHKN-NAI-RSI-HKNKBN  (IDX)
                                           TO  SPA-GMN-RSI-THKNKBN
                                                               (IDH)
      *
               MOVE    IDH             TO  SPA-GMN-HKNMAX
           END-PERFORM
           MOVE    WRK-SRYYMD          TO  SPA-SRYYMD
      *
           .
       3101-HKNCOMBI-HEN-EXT.
      *
           EXIT.
      *
      *****************************************************************
      *    保険組合せ名称編集処理
      *****************************************************************
       31011-HKNCOMBI-HEN-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-HKNCOMBIMEI
      *
           STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
                   " "                     DELIMITED  BY  SIZE
                   COMB-KOH1-TANSEIDONAME  DELIMITED  BY  SPACE
                   " "                     DELIMITED  BY  SIZE
                   COMB-KOH2-TANSEIDONAME  DELIMITED  BY  SPACE
                   " "                     DELIMITED  BY  SIZE
                   COMB-KOH3-TANSEIDONAME  DELIMITED  BY  SPACE
                   " "                     DELIMITED  BY  SIZE
                   COMB-KOH4-TANSEIDONAME  DELIMITED  BY  SPACE
                         INTO              WRK-HKNCOMBIMEI
           END-STRING
      *    主保険がない時
           IF      COMB-SYU-TANSEIDONAME   =   SPACE
               MOVE    SPACE               TO  WRK-HKNCOMBIMEI
               STRING  COMB-KOH1-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH2-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH3-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH4-TANSEIDONAME  DELIMITED  BY  SPACE
                         INTO              WRK-HKNCOMBIMEI
               END-STRING
           END-IF
      *
      *    労災の時、コメントの追加
           IF      COMB-HKNNUM         =   "971"  OR  "973"
      *        患者保険情報検索
               INITIALIZE                      PTRSIINF-REC
               MOVE    SPA-HOSPNUM         TO  PTRSI-HOSPNUM
               MOVE    SPA-GMN-PTID        TO  PTRSI-PTID
               MOVE    COMB-HKNID          TO  PTRSI-HKNID
               MOVE    PTRSIINF-REC        TO  MCPDATA-REC
               MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 993-PTRSIINF-NEXT-SEC
               ELSE
                   INITIALIZE                      PTRSIINF-REC
                   MOVE    1                   TO  FLG-PTRSIINF
               END-IF
               MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
      *
               MOVE    SPACE               TO  WRK-HKNKBN-MEI
               EVALUATE    PTRSI-HKNKBN
                   WHEN    "1"
                       MOVE    "短"        TO  WRK-HKNKBN-MEI
                   WHEN    "2"
                       MOVE    "傷"        TO  WRK-HKNKBN-MEI
                   WHEN    "3"
                       MOVE    "ア"        TO  WRK-HKNKBN-MEI
                   WHEN    "5"
                       MOVE    "公"        TO  WRK-HKNKBN-MEI
               END-EVALUATE
               IF      PTRSI-COMMENT       =   SPACE
                   MOVE    SPACE               TO  WRK-HKNCOMBIMEI
                   STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
                           " "                     DELIMITED  BY  SIZE
                           WRK-HKNKBN-MEI          DELIMITED  BY  SPACE
                           INTO              WRK-HKNCOMBIMEI
                   END-STRING
               ELSE
                   MOVE    SPACE               TO  WRK-HKNCOMBIMEI
                   STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
                           " "                     DELIMITED  BY  SIZE
                           WRK-HKNKBN-MEI          DELIMITED  BY  SPACE
                           "（"                    DELIMITED  BY  SIZE
                           PTRSI-COMMENT           DELIMITED  BY  SPACE
                           "）"                    DELIMITED  BY  SIZE
                           INTO              WRK-HKNCOMBIMEI
                   END-STRING
               END-IF
      *
      *******  MOVE    PTRSI-HKNKBN        TO  SPA-GMN-RSI-THKNKBN
      *******                                         (SPA-GMN-HKNMAX)
           END-IF
      *
           .
       31011-HKNCOMBI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科編集処理
      *****************************************************************
       3102-SRYKA-HEN-SEC        SECTION.
      *
           INITIALIZE                   SPA-GMN-SRYKA-AREA
      *
      *    最初に全科指定を挿入
           ADD     1                   TO  SPA-GMN-SRYKAMAX
           MOVE    "00"                TO  SPA-GMN-SRYKAL
                                                  (SPA-GMN-SRYKAMAX)
           MOVE    "全科指定"          TO  SPA-GMN-SRYKAMEIL
                                                  (SPA-GMN-SRYKAMAX)
      *    診療科
           MOVE    SPACE               TO  SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    SPA-NAI-SRYYM       TO  SYS-1005-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-1005-STYUKYMD(7:2)
           MOVE    SYS-1005-STYUKYMD   TO  SYS-1005-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
      *    システム管理検索
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-1005-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           PERFORM
                   UNTIL      (FLG-SYSKANRI   =   1    )
                       OR     (SPA-GMN-SRYKAMAX   >=  100 )
               ADD     1                   TO  SPA-GMN-SRYKAMAX
      *
               MOVE    MCPDATA-REC         TO  SYS-1005-REC
               MOVE    SYS-1005-KBNCD(1:2) TO  SPA-GMN-SRYKAL
                                                   (SPA-GMN-SRYKAMAX)
               MOVE    SYS-1005-SRYKANAME1 TO  SPA-GMN-SRYKAMEIL
                                                   (SPA-GMN-SRYKAMAX)
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       3102-SRYKA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院判定編集処理
      *****************************************************************
       3105-NYUGAI-HEN-SEC        SECTION.
      *
           IF     (SPA-BEDKBN          =   ZERO )
               CONTINUE
           ELSE
               INITIALIZE                  SYS-1001-REC
               MOVE    "1001"              TO  SYS-1001-KANRICD
               MOVE    "*"                 TO  SYS-1001-KBNCD
               MOVE    SPA-NAI-SRYYM       TO  SYS-1001-STYUKYMD(1:6)
               MOVE    "01"                TO  SYS-1001-STYUKYMD(7:2)
               MOVE    SYS-1001-STYUKYMD   TO  SYS-1001-EDYUKYMD
               MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
               MOVE    SYS-1001-REC        TO  MCPDATA-REC
      *        システム管理検索
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key10"             TO  MCP-PATHNAME
                   PERFORM 900-SYSKANRI-READ-SEC
               ELSE
                   INITIALIZE                  SYS-1001-REC
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    MCPDATA-REC         TO  SYS-1001-REC
      *            病床数
                   MOVE    SYS-1001-BEDSU          TO  SPA-BEDSU
               END-IF
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
           END-IF
      *
           IF      SPA-BEDSU           >   ZERO
               MOVE    "1 入院"            TO  SPA-GMN-NYUGAIKBN-LST(1)
               MOVE    "2 外来"            TO  SPA-GMN-NYUGAIKBN-LST(2)
               MOVE    2                   TO  SPA-GMN-NYUGAI-MAX
           ELSE
               MOVE    "2 外来"            TO  SPA-GMN-NYUGAIKBN-LST(1)
               MOVE    1                   TO  SPA-GMN-NYUGAI-MAX
           END-IF
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-NYUGAI-MAX
               IF      SPA-GMN-NYUGAIKBN   =   SPA-GMN-NYUGAIKBN-LST
                                                           (IDX)(1:1)
                   MOVE    SPA-GMN-NYUGAIKBN-LST (IDX)
                                           TO  SPA-GMN-NYUGAIKBN-G
               END-IF
           END-PERFORM
      *
           .
       3105-NYUGAI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    症状詳記区分編集処理
      *****************************************************************
       3106-SJKBN-HEN-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-SJKBN-AREA
           MOVE    "01 主たる疾患の臨床症状"
                                       TO  SPA-GMN-SJKBNLST (01)
           MOVE    "02 主たる疾患の診療・検査所見"
                                       TO  SPA-GMN-SJKBNLST (02)
           MOVE    "03 主な治療行為の必要性"
                                       TO  SPA-GMN-SJKBNLST (03)
           MOVE    "04 主な治療行為の経過"
                                       TO  SPA-GMN-SJKBNLST (04)
           MOVE    "05 １００万点以上の薬剤に係る症状等"
                                       TO  SPA-GMN-SJKBNLST (05)
           MOVE    "06 １００万点以上の処置に係る症状等"
                                       TO  SPA-GMN-SJKBNLST (06)
           MOVE    "07 その他（１）"
                                       TO  SPA-GMN-SJKBNLST (07)
           MOVE    "08 その他（２）"
                                       TO  SPA-GMN-SJKBNLST (08)
           MOVE    "09 その他（３）"
                                       TO  SPA-GMN-SJKBNLST (09)
           MOVE    "99 レセプト摘要欄コメント"
                                       TO  SPA-GMN-SJKBNLST (10)
           MOVE    10                  TO  SPA-SJKBN-MAX
      *
           IF      SPA-NAI-SRYYM       >=  "200704"
               MOVE    "50 治験概要"
                                           TO  SPA-GMN-SJKBNLST (10)
               MOVE    "51 疾患別リハビリテーションに係る治療継続理由"
                                           TO  SPA-GMN-SJKBNLST (11)
               MOVE    "90 上記以外の診療報酬明細書"
                                           TO  SPA-GMN-SJKBNLST (12)
               MOVE    "99 レセプト摘要欄コメント"
                                           TO  SPA-GMN-SJKBNLST (13)
               MOVE    13                  TO  SPA-SJKBN-MAX
           END-IF
      *
      *
           IF      SPA-NAI-SRYYM       >=  "200804"
               MOVE    "52 廃用症候群に係る評価表"
                                           TO  SPA-GMN-SJKBNLST (12)
               MOVE    "90 上記以外の診療報酬明細書"
                                           TO  SPA-GMN-SJKBNLST (13)
               MOVE    "99 レセプト摘要欄コメント"
                                           TO  SPA-GMN-SJKBNLST (14)
               MOVE    14                  TO  SPA-SJKBN-MAX
           END-IF
      *
           IF      SPA-NAI-SRYYM       >=  "201307"
               MOVE    "AA 労災レセプト「傷病の経過」"
                                           TO  SPA-GMN-SJKBNLST (15)
               MOVE    15                  TO  SPA-SJKBN-MAX
           END-IF
      *!!!
           ADD     1                   TO  SPA-SJKBN-MAX
           MOVE    "BB 自賠責（第三者行為）レセプト摘要欄コメント"
                                       TO  SPA-GMN-SJKBNLST
                                                (SPA-SJKBN-MAX)
      *
           IF      SPA-GMN-SJKBN   NOT =   SPACE
               MOVE    ZERO                TO  FLG-OK
               PERFORM VARYING    IDX      FROM    1   BY  1
                       UNTIL     (IDX      >   SPA-SJKBN-MAX )
                              OR (FLG-OK   =   1       )
                   IF      SPA-GMN-SJKBN       =   SPA-GMN-SJKBNLST
                                                         (IDX)(1:2)
                       MOVE    SPA-GMN-SJKBN-LIST  (IDX)
                                               TO  SPA-GMN-SJKBN-G
                       MOVE    1               TO  FLG-OK
                   END-IF
               END-PERFORM
               IF      FLG-OK              =   ZERO
                   PERFORM 31031-SJKBN-INI-SEC
               END-IF
           END-IF
      *
           .
       3106-SJKBN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメント編集処理
      *****************************************************************
        3103-RECECOM-HEN-SEC        SECTION.
      *
           MOVE    1                   TO  FLG-GMN-INIT
           INITIALIZE                  SPA-GMN-RECECOMLIST-G
      *
           MOVE    SPACE               TO  RECECOM-REC
           INITIALIZE                  RECECOM-REC
           MOVE    SPA-HOSPNUM         TO  RECECOM-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  RECECOM-PTID
      *****MOVE    SPA-GMN-NYUGAIKBN   TO  RECECOM-NYUGAIKBN
      *
           MOVE    RECECOM-REC         TO  MCPDATA-REC
           MOVE    "tbl_rececom"       TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_rececom"       TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 920-RECECOM-READ-SEC
           ELSE
               MOVE    1               TO  FLG-RECECOM
               INITIALIZE                  RECECOM-REC
           END-IF
           MOVE    ZERO                TO  SPA-GMN-NEXT
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDX-S2
           IF      SPA-GMN-PAGE        =   ZERO
               MOVE    1               TO  SPA-GMN-PAGE
           END-IF
           COMPUTE IDX-S               =  (SPA-GMN-PAGE  -  1 )
                                       *   100
                                       +   1
           MOVE    SPACE               TO  WRK-KEY-AREA
           PERFORM
                   UNTIL       FLG-RECECOM        =   1
               MOVE    ZERO                TO  FLG-HENOK
      *        症状詳記区分以外で編集
               IF     (RECECOM-SRYYM       =   WRK-KEY-SRYYM  ) AND
                      (RECECOM-NYUGAIKBN   =   WRK-KEY-NYUGAIKBN) AND
                      (RECECOM-SRYKA       =   WRK-KEY-SRYKA   ) AND
                      (RECECOM-HKNCOMBI    =   WRK-KEY-HKNCOMBI) AND
                      (RECECOM-SRYDD       =   WRK-KEY-SRYDD   )
                   CONTINUE
               ELSE
                   MOVE    1                   TO  FLG-HENOK
                   MOVE    RECECOM-NYUGAIKBN   TO  WRK-KEY-NYUGAIKBN
                   MOVE    RECECOM-SRYKA       TO  WRK-KEY-SRYKA
                   MOVE    RECECOM-SRYYM       TO  WRK-KEY-SRYYM
                   MOVE    RECECOM-HKNCOMBI    TO  WRK-KEY-HKNCOMBI
                   MOVE    RECECOM-SRYDD       TO  WRK-KEY-SRYDD
               END-IF
               IF      FLG-HENOK           =   1
                   ADD     1               TO  IDX-S2
                   IF      IDX-S2          >=  IDX-S
                       ADD     1                   TO  IDX
                       IF      IDX                 >   100
                           MOVE    1               TO  SPA-GMN-NEXT
                           MOVE    1               TO  FLG-RECECOM
                       ELSE
                           PERFORM 31031-RECECOM-SPAHEN-SEC
                       END-IF
                   END-IF
               END-IF
      *
               IF      FLG-RECECOM         =   ZERO
                   MOVE    "tbl_rececom"       TO  MCP-TABLE
                   MOVE    "key5"              TO  MCP-PATHNAME
                   PERFORM 920-RECECOM-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_rececom"       TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    SPACE               TO  SPA-GMN-COMMENT
           MOVE    ZERO                TO  SPA-GMN-BUNNUM
           MOVE    SPACE               TO  SPA-GMN-HKNCOMBI-G
           MOVE    ZERO                TO  SPA-GMN-JYURRKNUM
           INITIALIZE                      SPA-GMN-COPY-AREA
           INITIALIZE                      SPA-GMN-SELNUM-X
           INITIALIZE                      SPA-GMN-SELNUM-G
           INITIALIZE                      SPA-GMN-COMMENTLIST-G
           MOVE    ZERO                TO  SPA-GMN-COMSEL
           MOVE    SPACE               TO  SPA-GMN-SJKBN-G
           MOVE    ZERO                TO  SPA-NAI-RENNUM
      *    初期表示
           IF      SPA-GMN-SYORI       =   2
               CONTINUE
           ELSE
               PERFORM 31031-SJKBN-INI-SEC
           END-IF
      *
           .
       3103-RECECOM-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    詳記区分初期表示処理
      *****************************************************************
       31031-SJKBN-INI-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL     (IDX      >   SPA-SJKBN-MAX )
                          OR (FLG-OK   =   1       )
               IF      SPA-SJKBN-SET   =   SPA-GMN-SJKBNLST (IDX)(1:2)
                   MOVE    SPA-GMN-SJKBN-LIST  (IDX)
                                           TO  SPA-GMN-SJKBN-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF     FLG-OK               =   ZERO
               MOVE    SPA-GMN-SJKBNLST (01)   TO  SPA-GMN-SJKBN-G
           END-IF
           .
       31031-SJKBN-INI-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメント編集処理
      *****************************************************************
       31031-RECECOM-SPAHEN-SEC             SECTION.
      *
           MOVE    IDX                 TO  SPA-GMN-RECE-MAX
           MOVE    IDX                 TO  SPA-GMN-TRECNUM (IDX)
           MOVE    RECECOM-SRYKA       TO  SPA-GMN-NSRYKA  (IDX)
           MOVE    RECECOM-SRYYM       TO  SPA-GMN-NSRYYM  (IDX)
           MOVE    RECECOM-HKNCOMBI    TO  SPA-GMN-NHKNCOMBI(IDX)
      **** MOVE    RECECOM-COMMENT     TO  SPA-GMN-TCOMMENT (IDX)
           MOVE    RECECOM-NYUGAIKBN   TO  SPA-GMN-NNYUGAIKBN(IDX)
      *    診療年月
           MOVE    RECECOM-SRYYM       TO  WRK-SYMD(1:6)
           MOVE    "01"                TO  WRK-SYMD(7:2)
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG (1:6)   TO  SPA-GMN-TSRYYM (IDX)
      *    診療科
           MOVE    RECECOM-SRYKA       TO  WRK-SRYKA
           PERFORM 810-SRYKA-SET-SEC
           MOVE    WRK-SRYKAMEI        TO  SPA-GMN-TSRYKA (IDX)
      *    保険組合せ番号
           MOVE    RECECOM-HKNCOMBI    TO  WRK-HKNCOMBI
           PERFORM 820-HKNCOMBI-SET-SEC
           IF      FLG-HKNCOMBI        =   ZERO
               IF     (RECECOM-SRYYM       >=  COMB-TEKSTYMD(1:6)) AND
                      (RECECOM-SRYYM       <=  COMB-TEKEDYMD(1:6))
                   CONTINUE
               ELSE
                   MOVE    WRK-HKNCOMBIMEI TO  WRK-HKNCOMBIMEI-H
                   MOVE    SPACE           TO  WRK-HKNCOMBIMEI
                   STRING  "※"                DELIMITED  BY  SIZE
                           WRK-HKNCOMBIMEI-H   DELIMITED  BY  SIZE
                           INTO                WRK-HKNCOMBIMEI
                   END-STRING
               END-IF
           END-IF
           PERFORM 830-HKNCOMBIMEI-CHK-SEC
           MOVE    WRK-HKNCOMBIMEI     TO  SPA-GMN-THKNCOMBI(IDX)
      *    診療日
           IF      RECECOM-SRYDD       =   ZERO
               MOVE    SPACE               TO  SPA-GMN-TSRYDD (IDX)
           ELSE
               MOVE    RECECOM-SRYDD       TO  SPA-GMN-TSRYDD (IDX)
           END-IF
      *
           IF      RECECOM-NYUGAIKBN  =   "1"
               MOVE    "入"           TO  SPA-GMN-TNYUGAIKBN (IDX)
           ELSE
               MOVE    "外"           TO  SPA-GMN-TNYUGAIKBN (IDX)
           END-IF
      *
           .
       31031-RECECOM-SPAHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期編集処理
      *****************************************************************
       314-SYOKI-SEC             SECTION.
      *
           MOVE    ZERO                TO  WRK-HKN-IDX
           MOVE    ZERO                TO  WRK-HKN-IDX1
      *    保険組合せの再編集
           PERFORM 314-SYOKI-HEN-SEC
      *
           INITIALIZE                  SPA-GMN-JYURRK-LIST
           MOVE    ZERO                TO  SPA-GMN-JYURRKNUM
      *    初期編集
           MOVE    SPA-GMN-SRYKA-LIST(01)
                                       TO  SPA-GMN-SRYKA-G
      *    保険組合せの初期表示
           IF      WRK-HKN-IDX         =   ZERO
               MOVE    WRK-HKN-IDX1        TO  WRK-HKN-IDX
           END-IF
           IF      WRK-HKN-IDX         =   ZERO
               MOVE    1                   TO  WRK-HKN-IDX
           END-IF
           MOVE    SPA-GMN-HKN-LIST (WRK-HKN-IDX)
                                       TO  SPA-GMN-HKNCOMBI-G
      *    労災保険区分がアフターケアの時、履歴日を表示する
           MOVE    SPA-GMN-RSI-THKNKBN (WRK-HKN-IDX)
                                       TO  SPA-GMN-RSI-HKNKBN
           IF      SPA-GMN-RSI-HKNKBN =   "3"
               PERFORM 31401-AFTER-JYURRK-SEC
           END-IF
      *
           .
       314-SYOKI-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せの再編集処理
      *****************************************************************
       314-SYOKI-HEN-SEC             SECTION.
      *
      *    保険組合せの再編集
           INITIALIZE                  WRK-GMN-HKNCOMB-COMB
           MOVE    ZERO                TO  WRK-GMN-HKNMAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >    SPA-GMN-HKNMAX
               MOVE    SPA-GMN-THKNCOMBINUM(IDX)
                                       TO  WRK-GMN-THKNCOMBINUM(IDX)
               MOVE    SPA-GMN-THKNCOMBIMEI(IDX)
                                       TO  WRK-GMN-THKNCOMBIMEI(IDX)
               MOVE    SPA-GMN-RSI-THKNKBN  (IDX)
                                       TO  WRK-GMN-RSI-HKNKBN  (IDX)
           END-PERFORM
           MOVE    SPA-GMN-HKNMAX     TO  WRK-GMN-HKNMAX
      *
      *    受診歴を検索する  医療機関、患者ＩＤ
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  JYURRK-PTID
           MOVE    SPA-GMN-NYUGAIKBN   TO  JYURRK-NYUGAIKBN
           MOVE    SPA-NAI-SRYYM       TO  JYURRK-SRYYMD(1:6)
           MOVE    "01"                TO  JYURRK-SRYYMD(7:2)
           MOVE    SPA-NAI-SRYYM       TO  JYURRK-UPSRYYMD(1:6)
           MOVE    "31"                TO  JYURRK-UPSRYYMD(7:2)
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key13"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key13"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           MOVE    ZERO                TO  WRK-HKNCOMBI
           MOVE    SPACE               TO  WRK-SRYKA
           PERFORM
               UNTIL    FLG-JYURRK     =   1
      *
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   WRK-GMN-HKNMAX
                   IF      JYURRK-HKNCOMBINUM
                                       =   WRK-GMN-THKNCOMBINUM(IDX)
                       MOVE    "1"             TO  WRK-GMN-FLG (IDX)
                       MOVE    WRK-GMN-HKNMAX TO  IDX
                   END-IF
               END-PERFORM
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key13"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           END-PERFORM
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key13"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    入院の時、入院の保険組合せ編集
           IF      SPA-GMN-NYUGAIKBN   =   "1"
               PERFORM 3141-NYUINACCT-SYOKI-SEC
           END-IF
      *
      *    受診のあった保険組合せのみ表示する
           INITIALIZE                  SPA-GMN-HKNCOMB-COMB
           MOVE    ZERO                TO  SPA-GMN-HKNMAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   WRK-GMN-HKNMAX
      *        入院調剤料のみの履歴判定の判定
               IF     (SPA-NAI-SRYYM       <   SPA-SYSYMD(1:6) ) AND
                      (WRK-GMN-FLG (IDX)   =   SPACE           )
                   IF      SPA-GMN-NYUGAIKBN   =   "1"
                       PERFORM 3142-SRYACCT-CHOZAI-SEC
                   END-IF
               END-IF
      *        診療年月がシステム年月以前の時のみ、受診履歴の判定
               IF     (SPA-NAI-SRYYM       <   SPA-SYSYMD(1:6) ) AND
                      (WRK-GMN-FLG (IDX)   =   SPACE           )
                   CONTINUE
               ELSE
                   ADD     1                TO  SPA-GMN-HKNMAX
                   MOVE    WRK-GMN-THKNCOMBINUM(IDX)
                                      TO  SPA-GMN-THKNCOMBINUM
                                                (SPA-GMN-HKNMAX)
                   MOVE    WRK-GMN-THKNCOMBIMEI(IDX)
                                      TO  SPA-GMN-THKNCOMBIMEI
                                                (SPA-GMN-HKNMAX)
                   MOVE    WRK-GMN-RSI-HKNKBN  (IDX)
                                      TO  SPA-GMN-RSI-THKNKBN
                                                (SPA-GMN-HKNMAX)
               END-IF
               IF      WRK-GMN-FLG  (IDX)  =   "2"
      *            入院の保険組合せを初期表示
                   IF      WRK-HKN-IDX      =   ZERO
                       MOVE    SPA-GMN-HKNMAX  TO  WRK-HKN-IDX
                   END-IF
               END-IF
               IF      WRK-GMN-FLG  (IDX)  =   "1"
      *            保険組合せを初期表示
                   IF      WRK-HKN-IDX1     =   ZERO
                       MOVE    SPA-GMN-HKNMAX  TO  WRK-HKN-IDX1
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       314-SYOKI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入院調剤料のみの診療会計（受診履歴なし）処理
      *****************************************************************
       3142-SRYACCT-CHOZAI-SEC          SECTION.
      *
      *    診療会計
           INITIALIZE                  SRYACCT-REC
           MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SPA-GMN-NYUGAIKBN   TO  ACCT-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  ACCT-PTID
           MOVE    SPA-NAI-SRYYM       TO  ACCT-SRYYM
           MOVE    WRK-GMN-THKNCOMBINUM(IDX)
                                       TO  ACCT-HKNCOMBI
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key22"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key22"             TO  MCP-PATHNAME
               PERFORM 980-SRYACCT-READ-SEC
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1               TO  FLG-SRYACCT
           END-IF
      *
           PERFORM
               UNTIL  (FLG-SRYACCT     =   1   )
               IF      ACCT-ZAIKAISU       >   ZERO
                   MOVE    "1"             TO  WRK-GMN-FLG (IDX)
                   MOVE    1               TO  FLG-SRYACCT
               ELSE
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    "key22"             TO  MCP-PATHNAME
                   PERFORM 980-SRYACCT-READ-SEC
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key22"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       3142-SRYACCT-CHOZAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院の保険組合せ編集処理
      *****************************************************************
       3141-NYUINACCT-SYOKI-SEC          SECTION.
      *    入院の保険組合せ編集
      *    入院会計を検索する  医療機関、患者ＩＤ、年月
           INITIALIZE                  NYUINACCT-REC.
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  NACCT-PTID
           MOVE    SPA-NAI-SRYYM       TO  NACCT-SRYYM
      *    保険レコード
           MOVE    "5"                 TO  NACCT-ZAISKBKBN
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key17"             TO  MCP-PATHNAME
               PERFORM 980-NYUINACCT-READ-SEC
           ELSE
               INITIALIZE                  NYUINACCT-REC
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
           PERFORM UNTIL    FLG-NYUINACCT     =   1
               PERFORM VARYING     IDX1    FROM    1   BY  1
                       UNTIL       IDX1    >   31
                   MOVE    ZERO                TO  WRK-HKNCOMBI
                   MOVE    NACCT-DAY (IDX1)    TO  WRK-HKNCOMBI
                   PERFORM VARYING     IDX     FROM    1   BY  1
                           UNTIL       IDX     >   WRK-GMN-HKNMAX
                       IF      WRK-GMN-THKNCOMBINUM(IDX)
                                                   =   WRK-HKNCOMBI
                           MOVE    "2"             TO  WRK-GMN-FLG(IDX)
                           MOVE    WRK-GMN-HKNMAX TO  IDX
                       END-IF
                   END-PERFORM
               END-PERFORM
      *
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key17"             TO  MCP-PATHNAME
               PERFORM 980-NYUINACCT-READ-SEC
           END-PERFORM.
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       3141-NYUINACCT-SYOKI-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災保険区分がアフターケアの時、履歴日編集処理
      *****************************************************************
       31401-AFTER-JYURRK-SEC          SECTION.
      *
      *    受診歴を検索する  医療機関、患者ＩＤ
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  JYURRK-PTID
           MOVE    SPA-GMN-NYUGAIKBN   TO  JYURRK-NYUGAIKBN
           MOVE    SPA-GMN-SRYKA       TO  JYURRK-SRYKA
           MOVE    SPA-NAI-SRYYM       TO  JYURRK-SRYYMD(1:6)
           MOVE    "01"                TO  JYURRK-SRYYMD(7:2)
           MOVE    SPA-NAI-SRYYM       TO  JYURRK-UPSRYYMD(1:6)
           MOVE    "31"                TO  JYURRK-UPSRYYMD(7:2)
           MOVE    SPA-GMN-HKNCOMBINUM
                                       TO  JYURRK-HKNCOMBINUM
           IF      SPA-GMN-SRYKA      =   "00"
               MOVE    "key17"             TO  WRK-DBPATH
           ELSE
               MOVE    "key18"             TO  WRK-DBPATH
           END-IF
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    WRK-DBPATH          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    WRK-DBPATH          TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           INITIALIZE                  SPA-GMN-JYURRK-LIST
           PERFORM
               UNTIL    FLG-JYURRK     =   1
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL       IDY     >   10
                   IF      SPA-GMN-JYURRKYMD (IDY)
                                          =    JYURRK-SRYYMD
                       MOVE    10              TO  IDY
                   ELSE
                       IF      SPA-GMN-JYURRKYMD (IDY)  =   SPACE
                           MOVE    JYURRK-SRYYMD   TO
                                               SPA-GMN-JYURRKYMD(IDY)
                           MOVE    JYURRK-SRYYMD   TO  WRK-SYMD
                           PERFORM 800-SEIWA-HEN-SEC
                           MOVE    WRK-HENYMDG     TO
                                               SPA-GMN-JYURRKYMDW(IDY)
                           MOVE    10              TO  IDY
                       END-IF
                   END-IF
               END-PERFORM
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    WRK-DBPATH          TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           END-PERFORM
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    WRK-DBPATH          TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    ZERO                TO  SPA-GMN-RRKMAX
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   10
               IF      SPA-GMN-JYURRKYMD (IDY)    NOT =   SPACE
                   MOVE    IDY        TO  SPA-GMN-TRRKNUM (IDY)
                   MOVE    IDY        TO  SPA-GMN-RRKMAX
               END-IF
           END-PERFORM
      *
           .
       31401-AFTER-JYURRK-EXT.
           EXIT.
      *
      *****************************************************************
      *   受診履歴より診療科決定処理
      *****************************************************************
       3141-JYURRK-CHK-SEC             SECTION.
      *
      *    受診歴を検索する  医療機関、患者ＩＤ
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  JYURRK-PTID
           MOVE    SPA-GMN-NYUGAIKBN   TO  JYURRK-NYUGAIKBN
           MOVE    SPA-NAI-SRYYM       TO  JYURRK-SRYYMD(1:6)
           MOVE    "01"                TO  JYURRK-SRYYMD(7:2)
           MOVE    SPA-NAI-SRYYM       TO  JYURRK-UPSRYYMD(1:6)
           MOVE    "31"                TO  JYURRK-UPSRYYMD(7:2)
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key13"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key13"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           MOVE    ZERO                TO  WRK-HKNCOMBI
           MOVE    SPACE               TO  WRK-SRYKA
           PERFORM
               UNTIL    FLG-JYURRK     =   1
               MOVE    JYURRK-HKNCOMBINUM  TO  WRK-HKNCOMBI
               MOVE    JYURRK-SRYKA        TO  WRK-SRYKA
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key13"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key13"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       3141-JYURRK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科名編集処理
      *****************************************************************
       810-SRYKA-SET-SEC             SECTION.
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   SPA-GMN-SRYKAMAX
               IF      WRK-SRYKA           =   SPA-GMN-SRYKAL  (IDY)
                   MOVE    SPA-GMN-SRYKAMEIL (IDY)
                                               TO  WRK-SRYKAMEI
                   MOVE    SPA-GMN-SRYKAMAX   TO  IDY
               END-IF
           END-PERFORM
      *
           IF      WRK-SRYKA           =   ZERO
               MOVE    SPACE               TO  WRK-SRYKAMEI
           END-IF
      *
           .
       810-SRYKA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ名編集処理
      *****************************************************************
       820-HKNCOMBI-SET-SEC             SECTION.
      *
      *    保険組合マスター編集
           MOVE    SPACE               TO  HKNCOMBI-REC
           INITIALIZE                  HKNCOMBI-REC
           MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  COMB-PTID
           MOVE    WRK-HKNCOMBI        TO  COMB-HKNCOMBINUM
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-HKNCOMBI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
               INITIALIZE                  HKNCOMBI-REC
           END-IF
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-HKNCOMBI        =   ZERO
               IF      COMB-DELKBN             =   SPACE
                   PERFORM 31011-HKNCOMBI-HEN-SEC
               ELSE
                   MOVE    1                   TO  FLG-HKNCOMBI
                   MOVE    WRK-HKNCOMBI        TO  WRK-HKNCOMBIMEI
               END-IF
           ELSE
               MOVE    SPACE               TO  WRK-HKNCOMBIMEI
               MOVE    WRK-HKNCOMBI        TO  WRK-HKNCOMBIMEI
           END-IF
      *
           .
       820-HKNCOMBI-SET-EXT.
           EXIT.
      *****************************************************************
      *    コメントチェック処理
      *****************************************************************
       830-HKNCOMBIMEI-CHK-SEC                   SECTION.
      *
           INITIALIZE                  ORCXKANACONVAREA
           MOVE    50                  TO  KANACONV-LEN
           MOVE    5000                TO  KANACONV-STLEN
           MOVE    0                   TO  KANACONV-CONV-FLAG
           MOVE    0                   TO  KANACONV-CHAR-TYPE
           MOVE    WRK-HKNCOMBIMEI     TO  KANACONV-INDATA
           CALL    "kanaconv"          USING ORCXKANACONVAREA
           IF     (KANACONV-RETURN     =   ZERO )
               MOVE    KANACONV-OUTDATA
                                       TO  WRK-HKNCOMBIMEI
           ELSE
               MOVE    KANACONV-OUTDATA
                                       TO  WRK-HKNCOMBIMEI
           END-IF
           .
      *
       830-COMMENT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦和暦編集処理
      *****************************************************************
       800-SEIWA-HEN-SEC             SECTION.
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
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF
           .
       800-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
      *    コラムリスト位置保存
           IF      FLG-GMN-INIT        =   1
               MOVE    ZERO                TO  WRK-C50-RECE-ROW
           ELSE
               MOVE    C50-RECE-ROW        TO  WRK-C50-RECE-ROW
           END-IF
           IF      FLG-GMN-INIT2       =   1
               MOVE    ZERO                TO  WRK-C50-COMM-ROW
           ELSE
               MOVE    C50-COMM-ROW        TO  WRK-C50-COMM-ROW
           END-IF
      *
           MOVE    SPACE               TO  C50
      *
      *    コラムリストの位置指定
           INITIALIZE                      C50
           MOVE    ZERO                TO  C50-RECE-ROW
           MOVE    ZERO                TO  C50-RECE-ROWATTR
           IF      WRK-C50-RECE-ROW    NOT =   ZERO
               MOVE    WRK-C50-RECE-ROW    TO  C50-RECE-ROW
           END-IF
           MOVE    ZERO                TO  C50-COMM-ROW
           MOVE    ZERO                TO  C50-COMM-ROWATTR
           IF      WRK-C50-COMM-ROW    NOT =   ZERO
               MOVE    WRK-C50-COMM-ROW    TO  C50-COMM-ROW
           END-IF
      *
      *    画面編集
           MOVE    SPA-GMN-PTNUM      TO  C50-PTNUM
           MOVE    SPA-GMN-NAME       TO  C50-NAME
           MOVE    SPA-GMN-SEX        TO  C50-SEX
           MOVE    SPA-GMN-BIRTHDAYW  TO  C50-BIRTHDAY
      *
           MOVE    SPA-GMN-SRYYM      TO  C50-SRYYM
           MOVE    SPA-GMN-COMMENT    TO  C50-COMMENT
      *
      *    診療科
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SRYKAMAX
               MOVE    SPA-GMN-SRYKA-LIST (IDX)
                                           TO  C50-SRYKA-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-SRYKAMAX   TO  C50-SRYKA-COUNT
           MOVE    SPA-GMN-SRYKA-G    TO  C50-SRYKA
      *    保険組合せ
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-HKNMAX
               MOVE    SPA-GMN-HKN-LIST(IDX)
                                           TO  C50-HKNCOMBI-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-HKNMAX     TO  C50-HKNCOMBI-COUNT
           MOVE    SPA-GMN-HKNCOMBI-G TO  C50-HKNCOMBI
      *
      *    入外区分
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-NYUGAI-MAX
               MOVE    SPA-GMN-NYUGAIKBN-LST(IDX)
                                           TO  C50-NYUGAIKBN-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-NYUGAI-MAX     TO  C50-NYUGAIKBN-COUNT
           MOVE    SPA-GMN-NYUGAIKBN-G    TO  C50-NYUGAIKBN
      *
           EVALUATE    SPA-GMN-SYORI
               WHEN    0
                   MOVE    SPACE               TO  C50-MIDASI
                   MOVE    SPACE               TO  C50-MIDASI-STYLE
               WHEN    1
               WHEN    5
                   MOVE    "＜変　更＞"        TO  C50-MIDASI
                   MOVE    "red"               TO  C50-MIDASI-STYLE
               WHEN    6
                   MOVE    "＜削　除＞"        TO  C50-MIDASI
                   MOVE    "blue"              TO  C50-MIDASI-STYLE
               WHEN    2
                   MOVE    "＜文　例＞"        TO  C50-MIDASI
                   MOVE    "red"               TO  C50-MIDASI-STYLE
           END-EVALUATE
      *    複写
           MOVE    SPA-GMN-COPYYM     TO  C50-COPYSRYYM
           MOVE    SPA-GMN-COPYTUKI   TO  C50-COPYTUKI
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-RECE-MAX
               MOVE    SPA-GMN-TRECNUM (IDX)  TO  WRK-ZZZ
               MOVE    WRK-ZZZ                TO  C50-TRECNUM (IDX)
               MOVE    SPA-GMN-TSRYYM  (IDX)  TO  C50-TSRYYM  (IDX)
               MOVE    SPA-GMN-TSRYKA  (IDX)  TO  C50-TSRYKA  (IDX)
               MOVE    SPA-GMN-THKNCOMBI(IDX) TO  C50-THKNCOMBI(IDX)
               IF      SPA-GMN-TSRYDD  (IDX)  =   ZERO
                   MOVE    SPACE               TO  C50-TSRYDD  (IDX)
               ELSE
                   MOVE    SPA-GMN-TSRYDD  (IDX)
                                               TO  C50-TSRYDD  (IDX)
               END-IF
               MOVE    SPA-GMN-TNYUGAIKBN(IDX) TO  C50-TNYUGAIKBN(IDX)
      *
               MOVE    "F"             TO  C50-RECE-SELECT (IDX)
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL       IDX2    >   SPA-GMN-SELNUM-MAX
                   IF      SPA-GMN-SELNUM (IDX2)
                                           =   SPA-GMN-TRECNUM (IDX)
                       MOVE    "T"         TO  C50-RECE-SELECT (IDX)
                       IF      C50-RECE-ROW    =   ZERO
                           MOVE    IDX             TO  C50-RECE-ROW
                           MOVE    2               TO  C50-RECE-ROWATTR
                       END-IF
                       MOVE    SPA-GMN-SELNUM-MAX TO  IDX2
                   END-IF
               END-PERFORM
           END-PERFORM
           MOVE    SPA-GMN-RECE-MAX   TO  C50-RECE-COUNT
      *
           MOVE    SPA-GMN-SELNUM-X   TO  C50-SELNUM
           MOVE    SPA-GMN-BUNNUM     TO  C50-BUNNUM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-COMMENT-MAX
               MOVE    SPA-GMN-TCOMREN (IDX)  TO  WRK-ZZZ
               MOVE    WRK-ZZZ                TO  C50-TCOMREN (IDX)
               MOVE    SPA-GMN-TSJKBN  (IDX)  TO  C50-TSJKBN  (IDX)
               MOVE    SPA-GMN-TRENNUM (IDX)  TO  WRK-ZZ
               MOVE    WRK-ZZ                 TO  C50-TRENNUM (IDX)
               MOVE    SPA-GMN-TCOMMENT(IDX)  TO  C50-TCOMMENT (IDX)
      *
               MOVE    "F"                 TO  C50-COMM-SELECT (IDX)
               IF      SPA-GMN-COMSEL      =   SPA-GMN-TCOMREN (IDX)
                   MOVE    "T"             TO  C50-COMM-SELECT (IDX)
                   MOVE    IDX             TO  C50-COMM-ROW
                   MOVE    2               TO  C50-COMM-ROWATTR
               END-IF
           END-PERFORM
           MOVE    SPA-COMMENT-MAX     TO  C50-COMM-COUNT
           MOVE    SPA-GMN-COMSEL      TO  C50-COMSEL
      *    症状詳記区分
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SJKBN-MAX
               MOVE    SPA-GMN-SJKBN-LIST(IDX)
                                           TO  C50-SJKBN-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-SJKBN-MAX       TO  C50-SJKBN-COUNT
           MOVE    SPA-GMN-SJKBN-G     TO  C50-SJKBN
      *
      *    受診履歴
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-RRKMAX
               MOVE    SPA-GMN-TRRKNUM   (IDX)  TO  WRK-ZZ
               MOVE    WRK-ZZ                  TO  C50-TJYURRKNUM(IDX)
               MOVE    SPA-GMN-JYURRKYMDW(IDX)
                                               TO  C50-TJYURRKYMD(IDX)
      *
               IF      SPA-GMN-JYURRKNUM  =   SPA-GMN-TRRKNUM (IDX)
                   MOVE    "T"             TO  C50-JYURRK-SELECT (IDX)
               ELSE
                   MOVE    "F"             TO  C50-JYURRK-SELECT (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-GMN-RRKMAX     TO  C50-JYURRK-COUNT
      *
           MOVE    SPA-GMN-JYURRKNUM  TO  C50-JYURRKNUM
      *!!
      *    IF      SPA-NAI-COMMAX     >   ZERO
      *        MOVE    SPA-NAI-COMMAX     TO  WRK-ZZZZ
      *        STRING  "("
      *                WRK-ZZZZ
      *                "/2400)"
      *                                   INTO  C50-MCNT
      *        END-STRING
      **** END-IF
      *
           PERFORM 5001-MAPCUR-SEC
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF      SPA-GMN-CUR        =   ZERO
               PERFORM 50011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-CUR
               WHEN    1
                   MOVE    "COMMENT"           TO  MCP-WIDGET
               WHEN    2
                   MOVE    "SRYKA"            TO  MCP-WIDGET
               WHEN    3
                   MOVE    "HKNCOMBI"          TO  MCP-WIDGET
               WHEN    4
                   MOVE    "SELNUM"            TO  MCP-WIDGET
               WHEN    5
                   MOVE    "BUNNUM"            TO  MCP-WIDGET
               WHEN    6
                   MOVE    "B12"               TO  MCP-WIDGET
               WHEN    7
                   MOVE    "B08"               TO  MCP-WIDGET
               WHEN    8
                   MOVE    "B05"               TO  MCP-WIDGET
               WHEN    9
                   MOVE    "JYURRKNUM"         TO  MCP-WIDGET
               WHEN    10
                   MOVE    "COPYSRYYM"         TO  MCP-WIDGET
               WHEN    11
                   MOVE    "COPYTUKI"          TO  MCP-WIDGET
               WHEN    12
                   MOVE    "NYUGAIKBN"         TO  MCP-WIDGET
               WHEN    13
                   MOVE    "B03"               TO  MCP-WIDGET
               WHEN    14
                   MOVE    "COMSEL"            TO  MCP-WIDGET
               WHEN    15
                   MOVE    "SJKBN"             TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       50011-INPUT-CUR-SEC          SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "COPYSRYYM"
                   MOVE    11          TO  SPA-GMN-CUR
               WHEN    "COPYTUKI"
                   MOVE    6           TO  SPA-GMN-CUR
               WHEN    "SJKBN"
                   MOVE    1           TO  SPA-GMN-CUR
               WHEN    OTHER
                   MOVE    6           TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *        クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 220-CLEA-SEC
      *        コメント削除
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM 240-COMMENT-DEL-SEC
      *        変更
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM 210-HENKOU-SEC
      *        コピー
               WHEN    "CLICKED"       ALSO    "B05"
                   PERFORM 250-COPY-SEC
      *        文例選択画面へ
               WHEN    "CLICKED"       ALSO    "B09"
                   PERFORM 250-BUNREI-C501-SEC
      *        文例登録
               WHEN    "CLICKED"       ALSO    "B08"
                   PERFORM 280-BUNREI-ADD-SEC
      *        内容確定
               WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM 209-COMMENT-SEC
      *        コメント行
               WHEN    "CLICKED"       ALSO    "B11"
                   PERFORM 210-DOWN-SYORI-SEC
      *        登録
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 212-TOUROKU-SEC
      *        前月 
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 270-BACK-MONTH-SEC
      *        次月 
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 280-NEXT-MONTH-SEC
      *        前頁
               WHEN    "CLICKED"       ALSO    "B06S"
                   PERFORM 290-BACK-PAGE-SEC
      *        次月頁
               WHEN    "CLICKED"       ALSO    "B07S"
                   PERFORM 291-NEXT-PAGE-SEC
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
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        診療科
               WHEN    "ACTIVATE"      ALSO    "SRYKA"
                   PERFORM 400-SRYKA-SEC
      *        保険組合せ
               WHEN    "ACTIVATE"      ALSO    "HKNCOMBI"
                   PERFORM 410-HKNCOMBI-SEC
      *        入外区分
               WHEN    "ACTIVATE"      ALSO    "NYUGAIKBN"
                   PERFORM 410-NYUGAIKBN-SEC
      *        選択番号
               WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 420-SELNUM-SEC
      *        文例番号
               WHEN    "ACTIVATE"      ALSO    "BUNNUM"
                   PERFORM 430-BUNNUM-SEC
      *        明細選択
               WHEN    ANY             ALSO    "RECELIST"
                   PERFORM 440-RECELIST-SEC
      *        受診履歴選択番号
               WHEN    "ACTIVATE"      ALSO    "JYURRKNUM"
                   PERFORM 450-JYURRKNUM-SEC
      *        受診履歴選択
               WHEN    ANY             ALSO    "JYURRKLIST"
                   PERFORM 460-JYURRKLIST-SEC
      *        選択番号
               WHEN    "ACTIVATE"      ALSO    "COMSEL"
                   PERFORM 420-COMSEL-SEC
      *        受診履歴選択
               WHEN    ANY             ALSO    "COMMENTLIST"
                   PERFORM 460-COMMNTLIST-SEC
      *        その他
               WHEN    OTHER
                   PERFORM 490-ENTRY-SYORI-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメント確定処理
      *****************************************************************
       209-COMMENT-SEC                    SECTION.
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           MOVE    C50-COMMENT         TO  SPA-GMN-COMMENT
           MOVE    ZERO                TO  FLG-BUNREI
           PERFORM 800-COMMENT-CHK-SEC
           IF      KANACHK-MAX         >   2400
               MOVE    "0090"              TO  SPA-ERRCD
           END-IF
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-GMN-PTID        =   ZERO
                   MOVE    7                   TO  SPA-GMN-CUR
               ELSE
                   MOVE    6                   TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       209-COMMENT-EXT.
           EXIT.
      *****************************************************************
      *    コメント行処理
      *****************************************************************
       210-DOWN-SYORI-SEC                    SECTION.
      *
           IF      SPA-COMMENT-MAX     =   ZERO
               CONTINUE
           ELSE
               COMPUTE SPA-GMN-COMSEL  =   SPA-GMN-COMSEL  +  1
               IF      SPA-GMN-COMSEL  >   SPA-COMMENT-MAX
                   MOVE    1               TO  SPA-GMN-COMSEL
               END-IF
               MOVE    SPA-GMN-SELNUM(1)   TO  WRK-SELNUM
               PERFORM 21031-COMMENT-HENSYU-SEC
           END-IF
      *
           .
       210-DOWN-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    診療科処理
      *****************************************************************
       400-SRYKA-SEC                    SECTION.
      *
           MOVE    2                   TO  SPA-GMN-CUR
      *
           MOVE    C50-SRYKA           TO  SPA-GMN-SRYKA-G
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL      IDX      >   SPA-GMN-SRYKAMAX
               IF      SPA-GMN-SRYKA      =   SPA-GMN-SRYKAL (IDX)
                   MOVE    SPA-GMN-SRYKA-LIST (IDX)
                                           TO  SPA-GMN-SRYKA-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0001"              TO  SPA-ERRCD
           ELSE
               MOVE    3                   TO  SPA-GMN-CUR
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *    労災保険区分がアフターケアの時、履歴日を表示する
             IF      SPA-GMN-RSI-HKNKBN =   "3"
               PERFORM 31401-AFTER-JYURRK-SEC
               IF      SPA-GMN-RRKMAX     =   ZERO
                   MOVE    "0012"              TO  SPA-ERRCD
               ELSE
                   MOVE    9                   TO  SPA-GMN-CUR
               END-IF
             ELSE
               INITIALIZE                  SPA-GMN-JYURRK-LIST
               MOVE    ZERO                TO  SPA-GMN-RRKMAX
               MOVE    ZERO                TO  SPA-GMN-JYURRKNUM
               MOVE    3                   TO  SPA-GMN-CUR
             END-IF
           END-IF
      *
           .
       400-SRYKA-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ処理
      *****************************************************************
       410-HKNCOMBI-SEC                 SECTION.
      *
           MOVE    3                   TO  SPA-GMN-CUR
      *
           MOVE    C50-HKNCOMBI       TO  SPA-GMN-HKNCOMBI-G
      *
           MOVE    ZERO                TO  FLG-OK
           MOVE    SPACE               TO  SPA-GMN-RSI-HKNKBN
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL      IDX      >   SPA-GMN-HKNMAX
               IF      SPA-GMN-HKNCOMBINUM    =
                                           SPA-GMN-THKNCOMBINUM (IDX)
                   MOVE    SPA-GMN-HKN-LIST  (IDX)
                                           TO  SPA-GMN-HKNCOMBI-G
                   MOVE    SPA-GMN-RSI-THKNKBN (IDX)
                                           TO  SPA-GMN-RSI-HKNKBN
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0002"              TO  SPA-ERRCD
           ELSE
      *
      *    労災保険区分がアフターケアの時、履歴日を表示する
           IF      SPA-GMN-RSI-HKNKBN =   "3"
               PERFORM 31401-AFTER-JYURRK-SEC
               IF      SPA-GMN-RRKMAX     =   ZERO
                   MOVE    "0012"              TO  SPA-ERRCD
               ELSE
                   MOVE    9                   TO  SPA-GMN-CUR
               END-IF
           ELSE
               INITIALIZE                  SPA-GMN-JYURRK-LIST
               MOVE    ZERO                TO  SPA-GMN-RRKMAX
               MOVE    ZERO                TO  SPA-GMN-JYURRKNUM
               MOVE    6                   TO  SPA-GMN-CUR
           END-IF
      *
           END-IF
      *
           .
       410-HKNCOMBI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入外区分処理
      *****************************************************************
       410-NYUGAIKBN-SEC                 SECTION.
      *
           MOVE    12                  TO  SPA-GMN-CUR
      *
           MOVE    C50-NYUGAIKBN      TO  SPA-GMN-NYUGAIKBN-G
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-NYUGAI-MAX
               IF      SPA-GMN-NYUGAIKBN  =   SPA-GMN-NYUGAIKBN-LST
                                                           (IDX)(1:1)
                   MOVE    SPA-GMN-NYUGAIKBN-LST (IDX)
                                           TO  SPA-GMN-NYUGAIKBN-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0008"              TO  SPA-ERRCD
           ELSE
      *        保険組合せ
               PERFORM 3101-HKNCOMBI-HEN-SEC
      *        初期編集
               PERFORM 314-SYOKI-SEC
               IF      SPA-ERRCD           =   SPACE
                   MOVE    1               TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       410-NYUGAIKBN-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号処理
      *****************************************************************
       420-SELNUM-SEC                 SECTION.
      *
           MOVE    4                   TO  SPA-GMN-CUR
      *
           MOVE    C50-SELNUM         TO  SPA-GMN-SELNUM-X
      *
           IF      SPA-GMN-SELNUM-X   =   SPACE
               INITIALIZE                      SPA-GMN-SELNUM-G
           ELSE
               PERFORM 4201-SELNUM-CHK-SEC
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-GMN-SELNUM-MAX =   1     )
               MOVE    SPA-GMN-SELNUM(1)   TO  WRK-SELNUM
               EVALUATE    SPA-GMN-SYORI
      *        変更処理
               WHEN    1
               WHEN    5
                   PERFORM 2101-HENKOU-SYORI-SEC
      *        コピー処理
               WHEN    3
                   PERFORM 2501-COPY-SYORI-SEC
      *
               WHEN    OTHER
                   MOVE    8                   TO  SPA-GMN-CUR
               END-EVALUATE
           ELSE
               IF     (SPA-GMN-SELNUM-MAX >   1 ) AND
                      (SPA-ERRCD           =   SPACE )
                   MOVE    13                  TO  SPA-GMN-CUR
               END-IF
           END-IF
           .
       420-SELNUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号処理
      *****************************************************************
       4201-SELNUM-CHK-SEC                 SECTION.
      *
           INITIALIZE                      WRK-SELNUM-G
           MOVE    ZERO                TO  IDX-D1
           MOVE    ZERO                TO  IDX-D3
           MOVE    1                   TO  IDX-D2
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   40  )   OR
                              (SPA-ERRCD   NOT =   SPACE )
               EVALUATE    SPA-GMN-SELNUM-X(IDX:1)
                   WHEN    ","
                   WHEN    "."
                       IF      IDX-D1          >   ZERO
                           ADD     1               TO  IDX-D2
                           MOVE    ZERO            TO  IDX-D1
                           MOVE    ZERO            TO  IDX-D3
                       END-IF
                   WHEN    "0"  THRU  "9" 
                       ADD     1               TO  IDX-D1
                       IF      IDX-D1          >   3
                           MOVE    "0003"          TO  SPA-ERRCD
                       ELSE
                           MOVE    SPA-GMN-SELNUM-X(IDX:1)
                                               TO  WRK-TSELNUM-X(IDX-D2)
                                                            (IDX-D1:1)
                       END-IF
                   WHEN    "-"
                       MOVE    "-"             TO  WRK-TSELH(IDX-D2)
                       IF      IDX-D1          >   ZERO
                           ADD     1               TO  IDX-D2
                           MOVE    ZERO            TO  IDX-D1
                           MOVE    ZERO            TO  IDX-D3
                       END-IF
                   WHEN    SPACE
                       CONTINUE
                   WHEN    OTHER
                       MOVE    "0003"          TO  SPA-ERRCD
               END-EVALUATE
           END-PERFORM
      *
           PERFORM VARYING     IDX-D2      FROM    1   BY  1
                   UNTIL      (IDX-D2      >   100  )  OR
                              (WRK-TSELNUM-X(IDX-D2)    =   SPACE  )  OR
                              (SPA-ERRCD   NOT =   SPACE  )
               INITIALIZE                      ORCSNUMAREA
               MOVE    WRK-TSELNUM-X (IDX-D2)  TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN     NOT =   SPACE )   OR
                      (SNUM-SYOKBN     NOT =   SPACE )
                   MOVE    "0003"              TO  SPA-ERRCD
               ELSE
                   MOVE    SNUM-OUTNUM         TO  WRK-TSELNUM(IDX-D2)
                   IF      WRK-TSELNUM (IDX-D2) >=  01  AND
                                               <=  SPA-GMN-RECE-MAX
                       CONTINUE
                   ELSE
                       MOVE    "0003"          TO  SPA-ERRCD
                   END-IF
      *
                   IF     (IDX-D2          >   1   )  AND
                          (WRK-TSELH(IDX-D2 - 1 )   =   "-" )
                       IF      WRK-TSELNUM (IDX-D2 - 1)  >
                                                   WRK-TSELNUM(IDX-D2)
                           MOVE    "0003"              TO  SPA-ERRCD
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
           INITIALIZE                      SPA-GMN-SELNUM-G
           INITIALIZE                      WRK-SELNUM-AREA
           MOVE    ZERO                TO  IDX
           PERFORM VARYING     IDX-D2      FROM    1   BY  1
                   UNTIL      (IDX-D2      >   100  )  OR
                              (WRK-TSELNUM-X(IDX-D2)
                                               =   SPACE  )  OR
                              (SPA-ERRCD   NOT =   SPACE  )
               IF     (IDX-D2                  >   1   )  AND
                      (WRK-TSELH (IDX-D2 - 1)   =   "-" )
                   COMPUTE IDX-Z           =   WRK-TSELNUM(IDX-D2 - 1)
                                           +   1
                   PERFORM VARYING     IDX2    FROM    IDX-Z  BY  1
                           UNTIL       IDX2    >=  WRK-TSELNUM(IDX-D2)
                       MOVE    1               TO  WRK-SEL (IDX2)
                   END-PERFORM
               END-IF
               MOVE    WRK-TSELNUM(IDX-D2)     TO  IDX2
               MOVE    1                       TO  WRK-SEL (IDX2)
           END-PERFORM
           MOVE    ZERO                TO  IDX
           PERFORM VARYING     IDX-D2  FROM    1   BY  1
                   UNTIL      (IDX-D2  >   100  )  OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF      WRK-SEL (IDX-D2)    =   1
                   ADD     1               TO  IDX
                   IF      IDX             >   12
                       MOVE    "0038"          TO  SPA-ERRCD
                   ELSE
                       MOVE    IDX-D2          TO  SPA-GMN-SELNUM(IDX)
                       MOVE    IDX             TO  SPA-GMN-SELNUM-MAX
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       4201-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    明細選択処理
      *****************************************************************
       440-RECELIST-SEC                 SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
           MOVE    ZERO                TO  WRK-SELNUM-CHK-G
           MOVE    ZERO                TO  IDX2
      *
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >   SPA-GMN-RECE-MAX
               IF      C50-RECE-SELECT (IDX)   =  "T"
                   ADD     1               TO  IDX2
                   MOVE    SPA-GMN-TRECNUM  (IDX)
                                           TO  WRK-SELNUM-CHK (IDX2)
              END-IF
           END-PERFORM
      *
           MOVE    SPACE               TO  WRK-HEN-SELNUM
           MOVE    ZERO                TO  IDX2
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL      (IDX     >   100 )  OR
                              (WRK-SELNUM-CHK (IDX)   =   ZERO) OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF      SPA-GMN-SYORI       =   1  OR  3
                                                  OR  5
      *            変更・コピー処理の時、番号置換え
                   IF      SPA-GMN-SELNUM (1) =   WRK-SELNUM-CHK (IDX)
                       CONTINUE
                   ELSE
                       MOVE    WRK-SELNUM-CHK (IDX)    TO  WRK-SELNUM
                       PERFORM 4401-SELNUM-CHK-HEN-SEC
                   END-IF
               ELSE
                   ADD     1                   TO  IDX2
                   IF      IDX2                >   12
                       MOVE    "0038"              TO  SPA-ERRCD
                   ELSE
                       MOVE    WRK-SELNUM-CHK (IDX)    TO  WRK-SELNUM
                       PERFORM 4401-SELNUM-CHK-HEN-SEC
                   END-IF
               END-IF
           END-PERFORM
      *
           MOVE    WRK-HEN-SELNUM      TO  SPA-GMN-SELNUM-X
      *
           IF      SPA-GMN-SELNUM-X   =   SPACE
               INITIALIZE                  SPA-GMN-SELNUM-G
           ELSE
               MOVE    SPA-ERRCD           TO  WRK-ERRCD
               MOVE    SPACE               TO  SPA-ERRCD
               PERFORM 4201-SELNUM-CHK-SEC
               IF      WRK-ERRCD       NOT =   SPACE
                   MOVE    WRK-ERRCD           TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-GMN-SELNUM-MAX =   1     )
               EVALUATE    SPA-GMN-SYORI
      *        変更処理
               WHEN    1
               WHEN    5
                   PERFORM 2101-HENKOU-SYORI-SEC
      *        コピー処理
               WHEN    3
                   PERFORM 2501-COPY-SYORI-SEC
               WHEN    OTHER
                   MOVE    8                   TO  SPA-GMN-CUR
               END-EVALUATE
           ELSE
               MOVE    13                  TO  SPA-GMN-CUR
           END-IF
      *
           .
       440-RECELIST-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号編集処理
      *****************************************************************
       4401-SELNUM-CHK-HEN-SEC             SECTION.
      *
           MOVE    WRK-SELNUM          TO  WRK-ZZZ
           MOVE    SPACE               TO  WRK-SELNUM-X
           MOVE    ZERO                TO  IDX-D1
           PERFORM VARYING     IDX-D2  FROM    1   BY  1
                   UNTIL       IDX-D2  >   3
               IF      WRK-ZZZ(IDX-D2:1)    NOT =   SPACE
                   ADD     1                    TO  IDX-D1
                   MOVE    WRK-ZZZ(IDX-D2:1) TO  WRK-SELNUM-X(IDX-D1:1)
               END-IF
           END-PERFORM
           COMPUTE IDX-D2              =   40  -  IDX-D1
           IF      WRK-HEN-SELNUM (IDX-D2:)    NOT =   SPACE
               MOVE    "0037"              TO  SPA-ERRCD
               MOVE    SPACE               TO  WRK-SELNUM-X
           END-IF
      *
           MOVE    WRK-HEN-SELNUM      TO  WRK-HEN-SELNUM-2
           MOVE    SPACE               TO  WRK-HEN-SELNUM
           IF      WRK-HEN-SELNUM-2    =   SPACE
               STRING  WRK-SELNUM-X        DELIMITED   BY  SPACE
                       INTO                WRK-HEN-SELNUM
               END-STRING
           ELSE
               IF      WRK-SELNUM-X        =   SPACE
                   MOVE    WRK-HEN-SELNUM-2    TO  WRK-HEN-SELNUM
               ELSE
                   STRING  WRK-HEN-SELNUM-2    DELIMITED   BY  SPACE
                           ","                 DELIMITED   BY  SIZE
                           WRK-SELNUM-X        DELIMITED   BY  SPACE
                           INTO                WRK-HEN-SELNUM
                   END-STRING
               END-IF
           END-IF
      *
           .
       4401-SELNUM-CHK-HEN-EXT.
           EXIT.
      *****************************************************************
      *    文例番号処理
      *****************************************************************
       430-BUNNUM-SEC                 SECTION.
      *
           MOVE    5                   TO  SPA-GMN-CUR
      *
           MOVE    C50-BUNNUM         TO  SPA-GMN-BUNNUM
      *
           MOVE    7                   TO  SPA-GMN-CUR
      *
           .
       430-BUNNUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴選択番号処理
      *****************************************************************
       450-JYURRKNUM-SEC                 SECTION.
      *
           MOVE    9                   TO  SPA-GMN-CUR
      *
           MOVE    C50-JYURRKNUM      TO  SPA-GMN-JYURRKNUM
      *
           IF      SPA-GMN-JYURRKNUM  =   ZERO
               CONTINUE
           ELSE
               IF      SPA-GMN-JYURRKNUM  >   SPA-GMN-RRKMAX
                   MOVE    "0015"              TO  SPA-ERRCD
               ELSE
                   MOVE    6                   TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       450-JYURRKNUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細選択処理
      *****************************************************************
       460-JYURRKLIST-SEC                 SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
           MOVE    ZERO                TO  WRK-SELNUM2
      *
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >   SPA-GMN-RRKMAX
              IF      C50-JYURRK-SELECT (IDX)   =  "T"
                  IF      SPA-GMN-TRRKNUM(IDX)
                                                 =   SPA-GMN-JYURRKNUM
                      MOVE    SPA-GMN-TRRKNUM(IDX)  TO  WRK-SELNUM
                  ELSE
                      MOVE    SPA-GMN-TRRKNUM(IDX)  TO  WRK-SELNUM2
                  END-IF
              END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM2     NOT =   ZERO
               MOVE    WRK-SELNUM2         TO  SPA-GMN-JYURRKNUM
           ELSE
               MOVE    WRK-SELNUM          TO  SPA-GMN-JYURRKNUM
           END-IF
      *
           MOVE    SPA-GMN-JYURRKNUM  TO  C50-JYURRKNUM
           PERFORM 450-JYURRKNUM-SEC
      *
           .
       460-JYURRKLIST-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメント選択番号処理
      *****************************************************************
       420-COMSEL-SEC                 SECTION.
      *
           MOVE    14                  TO  SPA-GMN-CUR
      *
           MOVE    C50-COMSEL          TO  SPA-GMN-COMSEL
      *
           IF     (SPA-GMN-COMSEL      >=  1   )  AND
                  (SPA-GMN-COMSEL      <=  SPA-COMMENT-MAX )
      *        コメント編集
               MOVE    SPA-GMN-SELNUM(1)  TO  WRK-SELNUM
               PERFORM 21031-COMMENT-HENSYU-SEC
           ELSE
               IF      SPA-GMN-COMSEL      =   ZERO
                   MOVE    1                   TO  SPA-GMN-CUR
                   MOVE    ZERO                TO  SPA-NAI-RENNUM
               ELSE
                   MOVE    "0040"              TO  SPA-ERRCD
               END-IF
           END-IF
           .
       420-COMSEL-EXT.
           EXIT.
      *****************************************************************
      *    コメント明細選択処理
      *****************************************************************
       460-COMMNTLIST-SEC                 SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
           MOVE    ZERO                TO  WRK-SELNUM2
      *
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >   SPA-COMMENT-MAX
              IF      C50-COMM-SELECT (IDX)    =  "T"
                  IF      SPA-GMN-TCOMREN(IDX) =   SPA-GMN-COMSEL
                      MOVE    SPA-GMN-TCOMREN(IDX)  TO  WRK-SELNUM
                  ELSE
                      MOVE    SPA-GMN-TCOMREN(IDX)  TO  WRK-SELNUM2
                  END-IF
              END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM2     NOT =   ZERO
               MOVE    WRK-SELNUM2         TO  SPA-GMN-COMSEL
           ELSE
               MOVE    WRK-SELNUM          TO  SPA-GMN-COMSEL
           END-IF
      *
      *    コメント編集
           MOVE    SPA-GMN-SELNUM(1)  TO  WRK-SELNUM
           IF     (WRK-SELNUM         >   ZERO )
              AND (SPA-GMN-COMSEL     >   ZERO )
               PERFORM 21031-COMMENT-HENSYU-SEC
           ELSE
               MOVE    ZERO               TO  SPA-NAI-RENNUM
           END-IF
      *
           .
       460-COMMNTLIST-EXT.
           EXIT.
      *
      *****************************************************************
      *    その他処理
      *****************************************************************
       490-ENTRY-SYORI-SEC                   SECTION.
      *
      *    入力個所のセット
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
      *    入力チェック処理
           PERFORM 4901-INPUT-CHK-SEC
           .
       490-ENTRY-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェックと別画面処理
      *****************************************************************
       4901-INPUT-CHK-SEC          SECTION.
      *
      *    入力個所のセット
           MOVE    C50-COMMENT        TO  SPA-GMN-COMMENT
           MOVE    C50-COPYSRYYM      TO  SPA-GMN-COPYYM
           MOVE    C50-COPYTUKI       TO  SPA-GMN-COPYTUKI
           MOVE    C50-SJKBN          TO  SPA-GMN-SJKBN-G
      *
      *    症状詳記区分
           PERFORM 4902-SJKBN-CHK-SEC
      *    コメント
           MOVE    ZERO                TO  FLG-BUNREI
           PERFORM 800-COMMENT-CHK-SEC
           IF      KANACHK-MAX         >   2400
               MOVE    "0090"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
      *
      *    複写年月
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-GMN-COPYYM     =   SPACE
                   MOVE    SPACE           TO  SPA-GMN-NAI-COPYYM
               ELSE
                   PERFORM 49011-COPYYM-CHK-SEC
               END-IF
           END-IF
      *    複写月数
           IF      SPA-ERRCD           =   SPACE
               PERFORM 49012-COPYTUKI-CHK-SEC
           END-IF
      *
           .
       4901-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    症状詳記区分チェック処理
      *****************************************************************
       4902-SJKBN-CHK-SEC                   SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL     (IDX      >   SPA-SJKBN-MAX )
                          OR (FLG-OK   =   1       )
               IF      SPA-GMN-SJKBN   =   SPA-GMN-SJKBNLST (IDX)(1:2)
                   MOVE    SPA-GMN-SJKBN-LIST  (IDX)
                                           TO  SPA-GMN-SJKBN-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      SPA-GMN-SJKBN       =   SPACE
               MOVE    SPACE           TO  SPA-GMN-SJKBN-G
           ELSE
               IF      FLG-OK              =   ZERO
                   MOVE    "0009"              TO  SPA-ERRCD
                   MOVE    16                  TO  SPA-GMN-CUR
               ELSE
      *            選択判定
                   IF      SPA-GMN-COMSEL      >   ZERO
                       IF      SPA-GMN-SJKBN   NOT =   SPA-GMN-TSJKBN
                                                       (SPA-GMN-COMSEL)
                           MOVE    ZERO            TO  SPA-NAI-RENNUM
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           .
       4902-SJKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    複写年月チェック処理
      *****************************************************************
       49011-COPYYM-CHK-SEC                   SECTION.
      *
           INITIALIZE                      ORCSGDAYAREA
           MOVE    SPA-GMN-COPYYM      TO  SGDAY-INDAY
           MOVE    "1"                 TO  SGDAY-INTYPE
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-OTDAY(1:6)    TO  SPA-GMN-COPYYM
               MOVE    SGDAY-SDAY(1:6)     TO  SPA-GMN-NAI-COPYYM
           ELSE
               MOVE    "0030"              TO  SPA-ERRCD
               MOVE    10                  TO  SPA-GMN-CUR
           END-IF
      *
           IF      SPA-GMN-RSI-HKNKBN =   "3"
               MOVE    "0033"              TO  SPA-ERRCD
               MOVE    10                  TO  SPA-GMN-CUR
           END-IF
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-GMN-NAI-COPYYM <=  SPA-NAI-SRYYM
                   MOVE    "0034"              TO  SPA-ERRCD
                   MOVE    10                  TO  SPA-GMN-CUR
               END-IF
           END-IF
           .
       49011-COPYYM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    複写月数チェック処理
      *****************************************************************
       49012-COPYTUKI-CHK-SEC                   SECTION.
      *
           IF      SPA-GMN-COPYTUKI   >   12
               MOVE    "0031"              TO  SPA-ERRCD
               MOVE    11                  TO  SPA-GMN-CUR
           ELSE
               IF      SPA-GMN-COPYTUKI   >   ZERO
                   IF      SPA-GMN-NAI-COPYYM     =   SPACE
                       MOVE    "0032"              TO  SPA-ERRCD
                       MOVE    10                  TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
           .
       49012-COPYTUKI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア　処理
      *****************************************************************
       220-CLEA-SEC                   SECTION.
      *
           IF      SPA-GMN-COMSEL      =   ZERO
               PERFORM 310-SPA-SET-SEC
               MOVE    1                   TO  SPA-GMN-CUR
           ELSE
               MOVE    ZERO                TO  SPA-GMN-COMSEL
               MOVE    SPACE               TO  SPA-GMN-COMMENT
               MOVE    ZERO                TO  SPA-GMN-BUNNUM
               MOVE    ZERO                TO  SPA-GMN-COMSEL
               MOVE    ZERO                TO  SPA-NAI-RENNUM
               MOVE    SPACE               TO  SPA-GMN-SJKBN-G
               MOVE    ZERO                TO  SPA-NAI-COMMAX
      *        初期表示
               IF      SPA-GMN-SYORI       =   2
                   CONTINUE
               ELSE
                   PERFORM 31031-SJKBN-INI-SEC
               END-IF
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
           .
       220-CLEA-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録　処理
      *****************************************************************
       212-TOUROKU-SEC                   SECTION.
      *
      *    入力チェック処理
           PERFORM 4901-INPUT-CHK-SEC
      *
           IF      SPA-GMN-PTID        =   ZERO
               MOVE    "0011"              TO  SPA-ERRCD
               GO      TO      212-TOUROKU-EXT
           END-IF
      *
      *    入外区分チェック
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-NYUGAI-MAX
               IF      SPA-GMN-NYUGAIKBN  =   SPA-GMN-NYUGAIKBN-LST
                                                           (IDX)(1:1)
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0008"              TO  SPA-ERRCD
               MOVE    12                  TO  SPA-GMN-CUR
               GO      TO      212-TOUROKU-EXT
           END-IF
      *    診療科チェック
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL      IDX      >   SPA-GMN-SRYKAMAX
               IF      SPA-GMN-SRYKA       =   SPA-GMN-SRYKAL (IDX)
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-CUR
               GO      TO      212-TOUROKU-EXT
           END-IF
      *
           IF     (SPA-GMN-HKNCOMBINUM    =   SPACE ) AND
                  (SPA-GMN-HKNMAX         =   ZERO  )
               MOVE    "0006"              TO  SPA-ERRCD
           END-IF
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-GMN-HKNCOMBINUM    =   SPACE
                   MOVE    "0002"              TO  SPA-ERRCD
               END-IF
           END-IF
      *    アフターケアの時、受診日を選択すること
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-GMN-RSI-HKNKBN     =   "3"
                   IF      SPA-GMN-JYURRKNUM  =   ZERO
                       MOVE    "0014"              TO  SPA-ERRCD
                       MOVE    9                   TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
      *    コメント空白チェック
           MOVE    ZERO                TO  FLG-HENOK2
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-C50IDCD         =   SPACE )  AND
                  (SPA-GMN-COMMENT     =   SPACE )
               PERFORM 2124-UPDATE-CHK1-SEC
           END-IF
      *
      *H28.1
           IF      SPA-ERRCD           =   SPACE
      *        選択番号は１件
               IF      SPA-GMN-SELNUM-MAX  >  1
                   MOVE    "0036"              TO  SPA-ERRCD
                   MOVE    13                  TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        複写月がある時
               IF     (SPA-GMN-COPYYM      NOT =   SPACE ) AND
                      (SPA-GMN-RSI-HKNKBN  NOT =   "3" )
                   IF     (SPA-GMN-COMMENT     =   SPACE )  AND
                          (SPA-GMN-SELNUM (1)  >   ZERO  )
                       MOVE    "0206"              TO  SPA-C50IDCD
                   END-IF
               END-IF
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE ) AND
                  (SPA-C50IDCD         =   SPACE )
      *        症状詳記区分
               IF     (SPA-GMN-SJKBN       =   SPACE )  AND
                      (SPA-GMN-COMMENT NOT =   SPACE )
                   MOVE    "0042"              TO  SPA-ERRCD
                   MOVE    15                  TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF     (SPA-ERRCD           =   SPACE ) AND
                  (SPA-C50IDCD         =   SPACE )
               IF      SPA-GMN-COMMENT    =   SPACE
                   IF      FLG-HENOK2          =   ZERO
                       MOVE    "0007"              TO  SPA-ERRCD
                       MOVE    1                   TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE ) AND
                  (SPA-C50IDCD         =   SPACE )
               PERFORM 2130-HKNCOMBI-CHK-SEC
           END-IF
      *    症状詳記区分チェック
           IF     (SPA-ERRCD           =   SPACE ) AND
                  (SPA-C50IDCD         =   SPACE )
               PERFORM 2131-SJKBNCOPYCHK-SEC
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE ) AND
                  (SPA-C50IDCD         =   SPACE )
               IF      FLG-HENOK2      =   1
      *        選択番号分の追加
                   PERFORM 2120-RECECOM-SEL06-SEC
                   IF      FLG-RECECOM         =   ZERO
                       MOVE    "0108"              TO  SPA-C50IDCD
                   ELSE
                       MOVE    "0109"              TO  SPA-C50IDCD
                   END-IF
               END-IF
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE ) AND
                  (SPA-C50IDCD         =   SPACE )
               PERFORM 2120-RECECOM-SEL-SEC
      *H26.12
               IF     (SPA-NAI-RENNUM      >   ZERO )  AND
                      (FLG-RECECOM         =   1    )
      *            連番で登録がない時、最大連番で追加
                   PERFORM 21201-RECECOM-CHKSEL-SEC
               END-IF
      *
               IF      FLG-RECECOM         =   ZERO
                   MOVE    "0105"              TO  SPA-C50IDCD
                   IF      SPA-GMN-SYORI       =   1  OR  5
                                                      OR  ZERO
                                                      OR  3
                       PERFORM 2124-UPDATE-CHK-SEC
                   END-IF
               ELSE
                   IF      SPA-GMN-SYORI       =   5
                       MOVE    "0105"              TO  SPA-C50IDCD
                   ELSE
                       MOVE    "0101"              TO  SPA-C50IDCD
                   END-IF
               END-IF
      *        複写月
               IF      SPA-GMN-COPYYM     NOT =   SPACE
                   IF      FLG-RECECOM         =   ZERO
                       MOVE    "0205"              TO  SPA-C50IDCD
                   ELSE
                       MOVE    "0201"              TO  SPA-C50IDCD
                   END-IF
               END-IF
           END-IF
      *
           .
       212-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新のチェック処理
      *****************************************************************
       2124-UPDATE-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-HENOK
           IF      SPA-GMN-SELNUM (1)  >   ZERO
               MOVE    SPA-GMN-SELNUM (1)  TO  WRK-SELNUM
               IF      SPA-GMN-RSI-HKNKBN     =   "3"
                   MOVE    SPA-GMN-JYURRKYMD (SPA-GMN-JYURRKNUM)(7:2)
                                               TO  WRK-SRYDD
               ELSE
                   MOVE    SPACE               TO  WRK-SRYDD
               END-IF
               IF     (SPA-GMN-NNYUGAIKBN (WRK-SELNUM)
                                           =   SPA-GMN-NYUGAIKBN) AND
                      (SPA-GMN-NSRYYM  (WRK-SELNUM)
                                           =   SPA-NAI-SRYYM    ) AND
                      (SPA-GMN-NSRYKA  (WRK-SELNUM)
                                           =   SPA-GMN-SRYKA    ) AND
                      (SPA-GMN-NHKNCOMBI  (WRK-SELNUM)
                                           =   SPA-GMN-HKNCOMBINUM) AND
                      (SPA-GMN-TSRYDD  (WRK-SELNUM)
                                           =   WRK-SRYDD    )
      *        キーに変更がない
                   IF      SPA-GMN-COMSEL      >   ZERO
                       IF      SPA-GMN-TSJKBN(SPA-GMN-COMSEL)
                                               =   SPA-GMN-SJKBN
                           MOVE    1               TO  FLG-HENOK
                       END-IF
                   END-IF
               END-IF
           END-IF
      *    詳記区分登録あり
           IF      WRK-RENNUM-MAX      >   ZERO
               MOVE    2               TO  FLG-HENOK
           END-IF
      *    最大連番
           IF      WRK-RENNUM-MAX      =   99
               MOVE    1                   TO  FLG-HENOK
               MOVE    "0099"              TO  SPA-ERRCD
               MOVE    SPACE               TO  SPA-C50IDCD
           END-IF
      *
           IF      FLG-HENOK           =   ZERO
               MOVE    "0107"              TO  SPA-C50IDCD
           END-IF
           IF      FLG-HENOK           =   2
               MOVE    "0110"              TO  SPA-C50IDCD
           END-IF
           .
       2124-UPDATE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新のチェック処理（コメント空白）
      *****************************************************************
       2124-UPDATE-CHK1-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-HENOK2
           IF     (SPA-GMN-SYORI       =   1  OR  5  )  AND
                  (SPA-GMN-SELNUM (1)  >   ZERO )  AND
                  (SPA-GMN-COMSEL      =   ZERO )
               MOVE    SPA-GMN-SELNUM (1)  TO  WRK-SELNUM
               IF      SPA-GMN-RSI-HKNKBN     =   "3"
                   MOVE    SPA-GMN-JYURRKYMD (SPA-GMN-JYURRKNUM)(7:2)
                                               TO  WRK-SRYDD
               ELSE
                   MOVE    SPACE               TO  WRK-SRYDD
               END-IF
               IF     (SPA-GMN-NNYUGAIKBN (WRK-SELNUM)
                                           =   SPA-GMN-NYUGAIKBN) AND
                      (SPA-GMN-NSRYYM  (WRK-SELNUM)
                                           =   SPA-NAI-SRYYM    ) AND
                      (SPA-GMN-NSRYKA  (WRK-SELNUM)
                                           =   SPA-GMN-SRYKA    ) AND
                      (SPA-GMN-NHKNCOMBI  (WRK-SELNUM)
                                           =   SPA-GMN-HKNCOMBINUM) AND
                      (SPA-GMN-TSRYDD  (WRK-SELNUM)
                                           =   WRK-SRYDD    )
                   CONTINUE
               ELSE
                   MOVE    1               TO  FLG-HENOK2
               END-IF
           END-IF
           .
       2124-UPDATE-CHK2-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診のチェック処理
      *****************************************************************
       2130-HKNCOMBI-CHK-SEC          SECTION.
      *
      *    受診歴を検索する  医療機関、患者ＩＤ
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  JYURRK-PTID
           MOVE    SPA-GMN-NYUGAIKBN   TO  JYURRK-NYUGAIKBN
           MOVE    SPA-NAI-SRYYM       TO  JYURRK-SRYYMD(1:6)
           MOVE    "01"                TO  JYURRK-SRYYMD(7:2)
           MOVE    SPA-NAI-SRYYM       TO  JYURRK-UPSRYYMD(1:6)
           MOVE    "31"                TO  JYURRK-UPSRYYMD(7:2)
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key13"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key13"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM
               UNTIL   (FLG-JYURRK     =   1  )  OR
                       (FLG-OK         =   1  )
               IF      SPA-GMN-SRYKA      =   "00"
                   IF      SPA-GMN-HKNCOMBINUM
                                           =   JYURRK-HKNCOMBINUM
                       PERFORM 21301-HKNCOBMI-RSCHK-SEC
                   END-IF
               ELSE
                   IF     (SPA-GMN-HKNCOMBINUM
                                           =   JYURRK-HKNCOMBINUM) AND
                          (SPA-GMN-SRYKA  =   JYURRK-SRYKA      )
                       PERFORM 21301-HKNCOBMI-RSCHK-SEC
                   END-IF
               END-IF
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key13"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           END-PERFORM
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key13"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-OK              =   ZERO
      *    入院の時、入院の保険組合せ
               IF      SPA-GMN-NYUGAIKBN   =   "1"
                   PERFORM 21301-NYUINACCT-CHK-SEC
               END-IF
           END-IF
      *
           IF      FLG-OK              =   ZERO
               IF     (SPA-NAI-SRYYM     <   SPA-SYSYMD(1:6)) OR
                      (SPA-GMN-RSI-HKNKBN =   "3"           )
                   MOVE    "0012"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       2130-HKNCOMBI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院の保険組合せチェック処理
      *****************************************************************
       21301-NYUINACCT-CHK-SEC        SECTION.
      *
           INITIALIZE                  NYUINACCT-REC.
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  NACCT-PTID
           MOVE    SPA-NAI-SRYYM       TO  NACCT-SRYYM
      *    保険レコード
           MOVE    "5"                 TO  NACCT-ZAISKBKBN
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key17"             TO  MCP-PATHNAME
               PERFORM 980-NYUINACCT-READ-SEC
           ELSE
               INITIALIZE                  NYUINACCT-REC
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
           PERFORM UNTIL   (FLG-NYUINACCT     =   1 )  OR
                           (FLG-OK            =   1 )
               PERFORM VARYING     IDX1    FROM    1   BY  1
                       UNTIL      (IDX1    >   31   )  OR
                                  (FLG-OK      =   1 )
                   MOVE    ZERO                TO  WRK-HKNCOMBI
                   MOVE    NACCT-DAY (IDX1)    TO  WRK-HKNCOMBI
                   IF      SPA-GMN-HKNCOMBINUM
                                               =   WRK-HKNCOMBI
                       MOVE    1                   TO  FLG-OK
                   END-IF
               END-PERFORM
      *
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key17"             TO  MCP-PATHNAME
               PERFORM 980-NYUINACCT-READ-SEC
           END-PERFORM.
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-OK              =   ZERO
      *        調剤料のみ判定
               PERFORM 213011-SRYACCT-CHOZAICHK-SEC
           END-IF
      *
           .
       21301-NYUINACCT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院調剤料のみの診療会計（受診履歴なし）処理
      *****************************************************************
       213011-SRYACCT-CHOZAICHK-SEC          SECTION.
      *
      *    診療会計
           INITIALIZE                  SRYACCT-REC
           MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SPA-GMN-NYUGAIKBN   TO  ACCT-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  ACCT-PTID
           MOVE    SPA-NAI-SRYYM       TO  ACCT-SRYYM
           MOVE    SPA-GMN-HKNCOMBINUM
                                       TO  ACCT-HKNCOMBI
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key22"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key22"             TO  MCP-PATHNAME
               PERFORM 980-SRYACCT-READ-SEC
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1               TO  FLG-SRYACCT
           END-IF
      *
           PERFORM
               UNTIL   (FLG-SRYACCT    =   1   )
                   OR  (FLG-OK         =   1   )
               IF      ACCT-ZAIKAISU       >   ZERO
                   IF      SPA-GMN-SRYKA       =   "00"
                       MOVE    1               TO  FLG-OK
                   ELSE
                       IF     (SPA-GMN-SRYKA       =   ACCT-SRYKA   )
                           MOVE    1               TO  FLG-OK
                       END-IF
                   END-IF
               END-IF
               IF      FLG-OK              =   ZERO
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    "key22"             TO  MCP-PATHNAME
                   PERFORM 980-SRYACCT-READ-SEC
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key22"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       213011-SRYACCT-CHOZAICHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険チェック処理
      *****************************************************************
       21301-HKNCOBMI-RSCHK-SEC        SECTION.
      *
           IF      SPA-GMN-RSI-HKNKBN =   "3"
               IF      SPA-GMN-JYURRKYMD (SPA-GMN-JYURRKNUM)
                                           =   JYURRK-SRYYMD
                   MOVE    1                   TO  FLG-OK
               END-IF
           ELSE
               MOVE    1                   TO  FLG-OK
           END-IF
      *
           .
       21301-HKNCOBMI-RSCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプトコメントチェック処理
      *****************************************************************
       2131-SJKBNCOPYCHK-SEC          SECTION.
      *
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL     (IDX          >   SPA-COMMENT-MAX )
                          OR (SPA-ERRCD    NOT =   SPACE  )
               MOVE    ZERO                TO  FLG-OK
               PERFORM VARYING    IDY      FROM    1   BY  1
                       UNTIL     (IDY      >   SPA-SJKBN-MAX )
                              OR (FLG-OK   =   1       )
                   IF      SPA-GMN-TSJKBN (IDX)    =   SPA-GMN-SJKBNLST
                                                             (IDY)(1:2)
                       MOVE    1               TO  FLG-OK
                   END-IF
               END-PERFORM
               IF      FLG-OK              =   ZERO
                   MOVE    "0016"          TO  SPA-ERRCD
                   MOVE    14              TO  SPA-GMN-CUR
               END-IF
           END-PERFORM
           .
       2131-SJKBNCOPYCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプトコメントチェック処理
      *****************************************************************
       2120-RECECOM-SEL-SEC          SECTION.
      *
           MOVE    SPACE               TO  RECECOM-REC
           INITIALIZE                  RECECOM-REC
           MOVE    SPA-HOSPNUM         TO  RECECOM-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  RECECOM-PTID
           MOVE    SPA-GMN-NYUGAIKBN   TO  RECECOM-NYUGAIKBN
           MOVE    SPA-GMN-SRYKA       TO  RECECOM-SRYKA
           MOVE    SPA-NAI-SRYYM       TO  RECECOM-SRYYM
           MOVE    SPA-GMN-HKNCOMBINUM
                                       TO  RECECOM-HKNCOMBI
           IF      SPA-GMN-RSI-HKNKBN     =   "3"
               MOVE    SPA-GMN-JYURRKYMD (SPA-GMN-JYURRKNUM)(7:2)
                                           TO  RECECOM-SRYDD
           ELSE
               MOVE    ZERO                TO  RECECOM-SRYDD
           END-IF
           MOVE    SPA-GMN-SJKBN       TO  RECECOM-SJKBN
           IF      SPA-NAI-RENNUM      =   ZERO
      *        新規追加
               MOVE    "key14"             TO  WRK-DBPATH
           ELSE
               MOVE    SPA-NAI-RENNUM      TO  RECECOM-RENNUM
               MOVE    "key"               TO  WRK-DBPATH
           END-IF
      *
           MOVE    RECECOM-REC         TO  MCPDATA-REC
           MOVE    "tbl_rececom"       TO  MCP-TABLE
           MOVE    WRK-DBPATH          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_rececom"       TO  MCP-TABLE
               MOVE    WRK-DBPATH          TO  MCP-PATHNAME
               PERFORM 920-RECECOM-READ-SEC
           ELSE
               MOVE    1               TO  FLG-RECECOM
               INITIALIZE                  RECECOM-REC
           END-IF
           MOVE    "tbl_rececom"       TO  MCP-TABLE
           MOVE    WRK-DBPATH          TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    ZERO                TO  WRK-RENNUM-MAX
           IF     (SPA-NAI-RENNUM      =   ZERO )  AND
                  (FLG-RECECOM         =   ZERO )
               MOVE    RECECOM-RENNUM      TO  WRK-RENNUM-MAX
            END-IF
           .
       2120-RECECOM-SEL-EXT.
           EXIT.
      *
      *H26.12
      *****************************************************************
      *    レセプトコメントチェック処理
      *****************************************************************
       21201-RECECOM-CHKSEL-SEC          SECTION.
      *
           MOVE    SPACE               TO  RECECOM-REC
           INITIALIZE                  RECECOM-REC
           MOVE    SPA-HOSPNUM         TO  RECECOM-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  RECECOM-PTID
           MOVE    SPA-GMN-NYUGAIKBN   TO  RECECOM-NYUGAIKBN
           MOVE    SPA-GMN-SRYKA       TO  RECECOM-SRYKA
           MOVE    SPA-NAI-SRYYM       TO  RECECOM-SRYYM
           MOVE    SPA-GMN-HKNCOMBINUM
                                       TO  RECECOM-HKNCOMBI
           IF      SPA-GMN-RSI-HKNKBN     =   "3"
               MOVE    SPA-GMN-JYURRKYMD (SPA-GMN-JYURRKNUM)(7:2)
                                           TO  RECECOM-SRYDD
           ELSE
               MOVE    ZERO                TO  RECECOM-SRYDD
           END-IF
           MOVE    SPA-GMN-SJKBN       TO  RECECOM-SJKBN
      *
           MOVE    "key14"             TO  WRK-DBPATH
      *
           MOVE    RECECOM-REC         TO  MCPDATA-REC
           MOVE    "tbl_rececom"       TO  MCP-TABLE
           MOVE    WRK-DBPATH          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_rececom"       TO  MCP-TABLE
               MOVE    WRK-DBPATH          TO  MCP-PATHNAME
               PERFORM 920-RECECOM-READ-SEC
           ELSE
               MOVE    1               TO  FLG-RECECOM
               INITIALIZE                  RECECOM-REC
           END-IF
           MOVE    "tbl_rececom"       TO  MCP-TABLE
           MOVE    WRK-DBPATH          TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    ZERO                TO  WRK-RENNUM-MAX
           IF     (FLG-RECECOM         =   ZERO )
               MOVE    RECECOM-RENNUM      TO  WRK-RENNUM-MAX
               MOVE    ZERO                TO  SPA-NAI-RENNUM
           END-IF
      *
           .
       21201-RECECOM-CHKSEL-EXT.
           EXIT.
      *****************************************************************
      *    レセプトコメントチェック処理
      *****************************************************************
       2120-RECECOM-SEL06-SEC          SECTION.
      *
           MOVE    SPACE               TO  RECECOM-REC
           INITIALIZE                  RECECOM-REC
           MOVE    SPA-HOSPNUM         TO  RECECOM-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  RECECOM-PTID
           MOVE    SPA-GMN-NYUGAIKBN   TO  RECECOM-NYUGAIKBN
           MOVE    SPA-GMN-SRYKA       TO  RECECOM-SRYKA
           MOVE    SPA-NAI-SRYYM       TO  RECECOM-SRYYM
           MOVE    SPA-GMN-HKNCOMBINUM
                                       TO  RECECOM-HKNCOMBI
           IF      SPA-GMN-RSI-HKNKBN     =   "3"
               MOVE    SPA-GMN-JYURRKYMD (SPA-GMN-JYURRKNUM)(7:2)
                                           TO  RECECOM-SRYDD
           ELSE
               MOVE    ZERO                TO  RECECOM-SRYDD
           END-IF
      *
           MOVE    RECECOM-REC         TO  MCPDATA-REC
           MOVE    "tbl_rececom"       TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_rececom"       TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 920-RECECOM-READ-SEC
           ELSE
               MOVE    1               TO  FLG-RECECOM
               INITIALIZE                  RECECOM-REC
           END-IF
      *
           MOVE    "tbl_rececom"       TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       2120-RECECOM-SEL06-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録　処理
      *****************************************************************
       2121-TOUROKU-SYORI-SEC          SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           PERFORM 2120-RECECOM-SEL-SEC
           IF     (FLG-RECECOM         =   ZERO )
              AND (SPA-NAI-RENNUM      >   ZERO )
      *        更新
               MOVE    SPA-GMN-COMMENT    TO  RECECOM-COMMENT
      *
               MOVE    SMCNDATE-YMD        TO  RECECOM-UPYMD
               MOVE    SMCNDATE-HMS        TO  RECECOM-UPHMS
               MOVE    SPA-OPID            TO  RECECOM-OPID
      *
               MOVE    RECECOM-REC         TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_rececom"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "C50 RECECOM UPD ERR:" MCP-RC
                           ",KEY:"   RECECOM-KEY    "."
                   MOVE    "0020"              TO  SPA-ERRCD
               END-IF
           ELSE
      *        最大連番
               IF      WRK-RENNUM-MAX      =   99
                   MOVE    "0099"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-GMN-CUR
               ELSE
      *        追加
               MOVE    SPACE               TO  RECECOM-REC
               INITIALIZE                  RECECOM-REC
               MOVE    SPA-HOSPNUM         TO  RECECOM-HOSPNUM
               MOVE    SPA-GMN-PTID        TO  RECECOM-PTID
               MOVE    SPA-GMN-NYUGAIKBN   TO  RECECOM-NYUGAIKBN
               MOVE    SPA-GMN-SRYKA       TO  RECECOM-SRYKA
               MOVE    SPA-NAI-SRYYM       TO  RECECOM-SRYYM
               MOVE    SPA-GMN-HKNCOMBINUM
                                           TO  RECECOM-HKNCOMBI
               IF      SPA-GMN-RSI-HKNKBN     =   "3"
                   MOVE    SPA-GMN-JYURRKYMD (SPA-GMN-JYURRKNUM)(7:2)
                                               TO  RECECOM-SRYDD
               ELSE
                   MOVE    ZERO                TO  RECECOM-SRYDD
               END-IF
               MOVE    SPA-GMN-SJKBN       TO  RECECOM-SJKBN
               ADD     1                   TO  WRK-RENNUM-MAX
               MOVE    WRK-RENNUM-MAX      TO  RECECOM-RENNUM
               MOVE    SPA-GMN-COMMENT     TO  RECECOM-COMMENT
      *
               MOVE    SMCNDATE-YMD        TO  RECECOM-CREYMD
               MOVE    SMCNDATE-YMD        TO  RECECOM-UPYMD
               MOVE    SMCNDATE-HMS        TO  RECECOM-UPHMS
               MOVE    SPA-OPID            TO  RECECOM-OPID
      *
      *
               MOVE    RECECOM-REC         TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_rececom"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "C50 RECECOM INS ERR:" MCP-RC
                           ",KEY:"   RECECOM-KEY    "."
                   MOVE    "0020"              TO  SPA-ERRCD
               END-IF
               END-IF
           END-IF
      *    変更前の保険組合せが無効の時、削除
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-GMN-SYORI       =   5     )  AND
                  (SPA-GMN-SELNUM (1)  NOT =   ZERO)
               MOVE     SPA-GMN-SELNUM (1)     TO  WRK-SELNUM
               IF      (SPA-GMN-NHKNCOMBI(WRK-SELNUM)
                                       NOT =   SPA-GMN-HKNCOMBINUM)
                   MOVE    1               TO  IDX2
                   PERFORM 24011-COMMENT-DELETE-SEC
               END-IF
           END-IF
      *    複写処理
           IF     (SPA-ERRCD           =   SPACE ) AND
                  (SPA-GMN-COPYYM  NOT =   SPACE )  AND
                  (SPA-GMN-RSI-HKNKBN NOT =   "3")
               PERFORM 21213-COPY-RECECOME-SEC
           END-IF
      *
      *    レセプト作成管理マスタ更新
           IF      SPA-ERRCD           =   SPACE
               PERFORM 21212-RECEUPD-UPDATE-SEC
           END-IF
      *
      *    再表示
           IF      SPA-ERRCD           =   SPACE
               PERFORM 21219-SAIHYOUJI-SEC
           END-IF
      *
           .
       2121-TOUROKU-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    登録後再表示処理
      *****************************************************************
       21219-SAIHYOUJI-SEC           SECTION.
      *
           MOVE    SPACE               TO  WRK-OLD-AREA
           MOVE    SPA-GMN-NYUGAIKBN   TO  WRK-OLD-NYUGAIKBN
           MOVE    SPA-GMN-SRYKA       TO  WRK-OLD-SRYKA
           MOVE    SPA-NAI-SRYYM       TO  WRK-OLD-SRYYM
           MOVE    SPA-GMN-HKNCOMBINUM TO  WRK-OLD-HKNCOMBI
           IF      SPA-GMN-RSI-HKNKBN     =   "3"
               MOVE    SPA-GMN-JYURRKYMD (SPA-GMN-JYURRKNUM)(7:2)
                                           TO  WRK-OLD-SRYDD
           ELSE
               MOVE    SPACE               TO  WRK-OLD-SRYDD
           END-IF
           MOVE    SPA-GMN-NYUGAIKBN  TO  WRK-NYUGAIKBN
           PERFORM 310-SPA-SET-SEC
      *
           MOVE    ZERO                TO  WRK-SELNUM
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >   SPA-GMN-RECE-MAX
               IF     (SPA-GMN-NSRYKA (IDX)    =   WRK-OLD-SRYKA )
                  AND (SPA-GMN-NSRYYM (IDX)    =   WRK-OLD-SRYYM )
                  AND (SPA-GMN-NNYUGAIKBN (IDX)
                                               =   WRK-OLD-NYUGAIKBN )
                  AND (SPA-GMN-NHKNCOMBI (IDX) =   WRK-OLD-HKNCOMBI )
                  AND (SPA-GMN-TSRYDD    (IDX) =   WRK-OLD-SRYDD )
                   MOVE    IDX                 TO  WRK-SELNUM
                   MOVE    SPA-GMN-RECE-MAX    TO  IDX
               END-IF
           END-PERFORM
           PERFORM UNTIL   (WRK-SELNUM     NOT =   ZERO )
                        OR (SPA-GMN-NEXT       =   ZERO )
      *
               PERFORM 291-NEXT-PAGE-SEC
      *
               PERFORM VARYING     IDX     FROM   1   BY  1
                       UNTIL       IDX     >   SPA-GMN-RECE-MAX
                   IF     (SPA-GMN-NSRYKA (IDX)    =   WRK-OLD-SRYKA )
                      AND (SPA-GMN-NSRYYM (IDX)    =   WRK-OLD-SRYYM )
                      AND (SPA-GMN-NNYUGAIKBN (IDX)
                                               =   WRK-OLD-NYUGAIKBN )
                      AND (SPA-GMN-NHKNCOMBI(IDX)  =   WRK-OLD-HKNCOMBI)
                      AND (SPA-GMN-TSRYDD   (IDX)  =   WRK-OLD-SRYDD )
                       MOVE    IDX                 TO  WRK-SELNUM
                       MOVE    SPA-GMN-RECE-MAX    TO  IDX
                   END-IF
               END-PERFORM
           END-PERFORM
           IF      WRK-SELNUM          >   ZERO
               MOVE    1                   TO  SPA-GMN-SYORI
               MOVE    SPACE               TO  SPA-GMN-SELNUM-X
               MOVE    SPACE               TO  WRK-HEN-SELNUM
               PERFORM 4401-SELNUM-CHK-HEN-SEC
               MOVE    WRK-HEN-SELNUM      TO  SPA-GMN-SELNUM-X
               PERFORM 4201-SELNUM-CHK-SEC
      *        コメント
               MOVE    SPA-GMN-SELNUM(1)   TO  WRK-SELNUM
               PERFORM 2101-HENKOU-SYORI-SEC
      *        コメント選択なし
               MOVE    ZERO                TO  SPA-GMN-COMSEL
               MOVE    SPACE               TO  SPA-GMN-COMMENT
               MOVE    ZERO                TO  SPA-NAI-RENNUM
               MOVE    ZERO                TO  SPA-NAI-COMMAX
               PERFORM 31031-SJKBN-INI-SEC
           ELSE
               INITIALIZE                  SPA-GMN-SELNUM-G
           END-IF
           MOVE    1                   TO  SPA-GMN-CUR
      *
           .
       21219-SAIHYOUJI-EXT.
           EXIT.
      *****************************************************************
      *    レセコメント複写処理
      *****************************************************************
       21213-COPY-RECECOME-SEC         SECTION.
      *
           MOVE    RECECOM-REC         TO  HZN-RECECOM-REC
      *
           MOVE    RECECOM-HKNCOMBI    TO  WRK-HKNCOMBI
           PERFORM 820-HKNCOMBI-SET-SEC
      *
           MOVE    SPA-GMN-NAI-COPYYM TO  WRK-COPYYM
           IF      SPA-GMN-COPYTUKI   =   ZERO
               MOVE    1                   TO  SPA-GMN-COPYTUKI
           END-IF
           IF     (WRK-COPYYM          >=  COMB-TEKSTYMD(1:6)) AND
                  (WRK-COPYYM          <=  COMB-TEKEDYMD(1:6))
               MOVE    ZERO                TO  FLG-COPY-END
           ELSE
               MOVE    1                   TO  FLG-COPY-END
           END-IF
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN-COPYTUKI )  OR
                              (FLG-COPY-END   =   1    )   OR
                              (SPA-ERRCD  NOT =   SPACE)
      *        複写
               PERFORM 212131-RECECOME-COPY-SEC
      *
               COMPUTE WRK-COPYMM      =   WRK-COPYMM
                                       +   1
               IF      WRK-COPYMM      >   12
                   COMPUTE WRK-COPYYY      =   WRK-COPYYY
                                           +   1
                   MOVE    1               TO  WRK-COPYMM
               END-IF
      *        保険組合せの有効期間外は複写なし
               IF     (WRK-COPYYM          >=  COMB-TEKSTYMD(1:6)) AND
                      (WRK-COPYYM          <=  COMB-TEKEDYMD(1:6))
                   MOVE    ZERO                TO  FLG-COPY-END
               ELSE
                   MOVE    1                   TO  FLG-COPY-END
               END-IF
           END-PERFORM
      *
           .
       21213-COPY-RECECOME-EXT.
           EXIT.
      *****************************************************************
      *    レセコメント複写処理
      *****************************************************************
       212131-RECECOME-COPY-SEC           SECTION.
      *
           MOVE    HZN-RECECOM-REC     TO  RECECOM-REC
           MOVE    WRK-COPYYM          TO  RECECOM-SRYYM
      *
           MOVE    RECECOM-REC         TO  MCPDATA-REC
           MOVE    "tbl_rececom"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_rececom"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-RECECOM-READ-SEC
           ELSE
               MOVE    1               TO  FLG-RECECOM
               INITIALIZE                  RECECOM-REC
           END-IF
           MOVE    "tbl_rececom"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-RECECOM         =   ZERO
      *        更新
               IF      SPA-GMN-COMMENT     =   SPACE
                   MOVE    HZN-RECECOM-COMMENT TO  RECECOM-COMMENT
               ELSE
                   MOVE    SPA-GMN-COMMENT     TO  RECECOM-COMMENT
               END-IF
      *
               MOVE    SMCNDATE-YMD        TO  RECECOM-UPYMD
               MOVE    SMCNDATE-HMS        TO  RECECOM-UPHMS
               MOVE    SPA-OPID            TO  RECECOM-OPID
      *
               MOVE    RECECOM-REC         TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_rececom"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "C50 RECECOM UPD ERR:" MCP-RC
                           ",KEY:"   RECECOM-KEY    "."
                   MOVE    "0020"              TO  SPA-ERRCD
               END-IF
           ELSE
      *        追加
               MOVE    HZN-RECECOM-REC     TO  RECECOM-REC
               MOVE    WRK-COPYYM          TO  RECECOM-SRYYM
      *
               MOVE    SMCNDATE-YMD        TO  RECECOM-CREYMD
               MOVE    SMCNDATE-YMD        TO  RECECOM-UPYMD
               MOVE    SMCNDATE-HMS        TO  RECECOM-UPHMS
               MOVE    SPA-OPID            TO  RECECOM-OPID
      *
               MOVE    RECECOM-REC         TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_rececom"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "C50 RECECOM INS ERR:" MCP-RC
                           ",KEY:"   RECECOM-KEY    "."
                   MOVE    "0020"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       212131-RECECOME-COPY-EXT.
           EXIT.
      *****************************************************************
      *    レセプト作成管理マスタ更新処理
      *****************************************************************
        21212-RECEUPD-UPDATE-SEC           SECTION.
      *
           INITIALIZE                      ORCSRECEUPDAREA
           MOVE    "1"                 TO  LNK-UPD-SYORI
           MOVE    SPA-HOSPNUM         TO  LNK-UPD-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  LNK-UPD-PTID
           MOVE    SPA-TERMID          TO  LNK-UPD-TERMID
           MOVE    SPA-OPID            TO  LNK-UPD-OPID
           CALL    "ORCSRECEUPD"       USING    ORCSRECEUPDAREA
                                                SPA-AREA
           IF      LNK-UPD-RC      NOT =   ZERO
               MOVE    "0020"              TO  SPA-ERRCD
               DISPLAY "LNK-UPD-RC =>" LNK-UPD-RC
           END-IF
      *
           .
        21212-RECEUPD-UPDATE-EXT.
           EXIT.
      *****************************************************************
      *    一括登録　処理
      *****************************************************************
       2122-ALLTOUROKU-SYORI-SEC        SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE    SPA-GMN-SELNUM (1)  TO  WRK-SELNUM
      *
           MOVE    SPACE               TO  RECECOM-REC
           INITIALIZE                  RECECOM-REC
           MOVE    SPA-HOSPNUM         TO  RECECOM-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  RECECOM-PTID
           MOVE    SPA-GMN-NNYUGAIKBN (WRK-SELNUM)
                                       TO  RECECOM-NYUGAIKBN
           MOVE    SPA-GMN-NSRYYM  (WRK-SELNUM)
                                       TO  RECECOM-SRYYM
           MOVE    SPA-GMN-NSRYKA  (WRK-SELNUM)
                                       TO  RECECOM-SRYKA
           MOVE    SPA-GMN-NHKNCOMBI(WRK-SELNUM)
                                       TO  RECECOM-HKNCOMBI
           IF      SPA-GMN-TSRYDD  (WRK-SELNUM)   =   SPACE
               MOVE    ZERO            TO  RECECOM-SRYDD
           ELSE
               MOVE    SPA-GMN-TSRYDD  (WRK-SELNUM)
                                       TO  RECECOM-SRYDD
           END-IF
      *
           MOVE    RECECOM-REC         TO  MCPDATA-REC
           MOVE    "tbl_rececom"       TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_rececom"       TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 920-RECECOM-READ-SEC
           ELSE
               MOVE    1               TO  FLG-RECECOM
               INITIALIZE                  RECECOM-REC
           END-IF
           PERFORM UNTIL  (FLG-RECECOM         =   1 )
      *
               MOVE    RECECOM-REC         TO  HZN-RECECOM-REC
               PERFORM 21221-ALL-KOUSIN-SEC
      *
      *        複写処理
               IF     (SPA-ERRCD           =   SPACE ) AND
                      (SPA-GMN-COPYYM  NOT =   SPACE )  AND
                      (SPA-GMN-RSI-HKNKBN NOT =   "3")
                   PERFORM 21213-COPY-RECECOME-SEC
               END-IF
      *
               MOVE    "tbl_rececom"       TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 920-RECECOM-READ-SEC
           END-PERFORM
           MOVE    "tbl_rececom"       TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    変更前の保険組合せが無効の時、削除
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-GMN-SYORI       =   5     )  AND
                  (SPA-GMN-SELNUM (1)  NOT =   ZERO)
               MOVE     SPA-GMN-SELNUM (1)     TO  WRK-SELNUM
               IF      (SPA-GMN-NHKNCOMBI(WRK-SELNUM)
                                       NOT =   SPA-GMN-HKNCOMBINUM)
                   MOVE    1               TO  IDX2
                   PERFORM 24011-COMMENT-DELETE-SEC
               END-IF
           END-IF
      *    レセプト作成管理マスタ更新
           IF      SPA-ERRCD           =   SPACE
               PERFORM 21212-RECEUPD-UPDATE-SEC
           END-IF
      *
           MOVE    SPA-GMN-NYUGAIKBN   TO  WRK-NYUGAIKBN
           PERFORM 310-SPA-SET-SEC
           MOVE    1                   TO  SPA-GMN-CUR
           .
        2129-COPYONLY-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    文例選択画面へ　処理
      *****************************************************************
       21221-ALL-KOUSIN-SEC                   SECTION.
      *
           MOVE    SPACE               TO  RECECOM-REC
           INITIALIZE                  RECECOM-REC
           MOVE    SPA-HOSPNUM         TO  RECECOM-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  RECECOM-PTID
           MOVE    SPA-GMN-NYUGAIKBN   TO  RECECOM-NYUGAIKBN
           MOVE    SPA-GMN-SRYKA       TO  RECECOM-SRYKA
           MOVE    SPA-NAI-SRYYM       TO  RECECOM-SRYYM
           MOVE    SPA-GMN-HKNCOMBINUM
                                       TO  RECECOM-HKNCOMBI
           IF      SPA-GMN-RSI-HKNKBN     =   "3"
               MOVE    SPA-GMN-JYURRKYMD (SPA-GMN-JYURRKNUM)(7:2)
                                           TO  RECECOM-SRYDD
           ELSE
               MOVE    ZERO                TO  RECECOM-SRYDD
           END-IF
           MOVE    HZN-RECECOM-SJKBN       TO  RECECOM-SJKBN
           MOVE    HZN-RECECOM-RENNUM      TO  RECECOM-RENNUM
      *
           MOVE    RECECOM-REC         TO  MCPDATA-REC
           MOVE    "tbl_rececom"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_rececom"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-RECECOM-READ-SEC
           ELSE
               MOVE    1               TO  FLG-RECECOM
               INITIALIZE                  RECECOM-REC
           END-IF
           MOVE    "tbl_rececom"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-RECECOM         =   ZERO
      *        更新
               MOVE    HZN-RECECOM-COMMENT TO  RECECOM-COMMENT
      *
               MOVE    SMCNDATE-YMD        TO  RECECOM-UPYMD
               MOVE    SMCNDATE-HMS        TO  RECECOM-UPHMS
               MOVE    SPA-OPID            TO  RECECOM-OPID
      *
               MOVE    RECECOM-REC         TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_rececom"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "C50 RECECOM UPD ERR:" MCP-RC
                           ",KEY:"   RECECOM-KEY    "."
                   MOVE    "0020"              TO  SPA-ERRCD
               END-IF
           ELSE
      *        追加
               MOVE    SPACE               TO  RECECOM-REC
               INITIALIZE                  RECECOM-REC
               MOVE    SPA-HOSPNUM         TO  RECECOM-HOSPNUM
               MOVE    SPA-GMN-PTID        TO  RECECOM-PTID
               MOVE    SPA-GMN-NYUGAIKBN   TO  RECECOM-NYUGAIKBN
               MOVE    SPA-GMN-SRYKA       TO  RECECOM-SRYKA
               MOVE    SPA-NAI-SRYYM       TO  RECECOM-SRYYM
               MOVE    SPA-GMN-HKNCOMBINUM
                                           TO  RECECOM-HKNCOMBI
               IF      SPA-GMN-RSI-HKNKBN     =   "3"
                   MOVE    SPA-GMN-JYURRKYMD (SPA-GMN-JYURRKNUM)(7:2)
                                               TO  RECECOM-SRYDD
               ELSE
                   MOVE    ZERO                TO  RECECOM-SRYDD
               END-IF
               MOVE    HZN-RECECOM-SJKBN   TO  RECECOM-SJKBN
               MOVE    HZN-RECECOM-RENNUM  TO  RECECOM-RENNUM
               MOVE    HZN-RECECOM-COMMENT TO  RECECOM-COMMENT
      *
               MOVE    SMCNDATE-YMD        TO  RECECOM-CREYMD
               MOVE    SMCNDATE-YMD        TO  RECECOM-UPYMD
               MOVE    SMCNDATE-HMS        TO  RECECOM-UPHMS
      *
               MOVE    SPA-OPID            TO  RECECOM-OPID
      *
               MOVE    RECECOM-REC         TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_rececom"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "C50 RECECOM INS ERR:" MCP-RC
                           ",KEY:"   RECECOM-KEY    "."
                   MOVE    "0020"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       21221-ALL-KOUSIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    文例選択画面へ　処理
      *****************************************************************
       250-BUNREI-C501-SEC                   SECTION.
      *
      *    コメント内容確定
           PERFORM 209-COMMENT-SEC
      *
           MOVE    SPACE               TO  SPA-MOTOPG
      *
           MOVE    SPA-C50KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-C50             TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGC501"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-C50
           MOVE    SPA-WORK        TO  SPA-C50KYOUTU
      *
      *    カーソル移動
           MOVE    "SELNUM"            TO  MCP-WIDGET
      *
           MOVE    "C50"              TO  SPA-MOTOPG
           MOVE    "C501"             TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "C501"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
      *
       250-BUNREI-C501-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメントチェック処理
      *****************************************************************
       800-COMMENT-CHK-SEC             SECTION.
      *
           IF      FLG-BUNREI          =   1
               MOVE    800                 TO  WRK-MOJI-MAX
           ELSE
      *!!      MOVE    2400                TO  WRK-MOJI-MAX
               MOVE    2420                TO  WRK-MOJI-MAX
           END-IF
           INITIALIZE                  ORCXKANACONVAREA
           MOVE    WRK-MOJI-MAX        TO  KANACONV-LEN
           MOVE    5000                TO  KANACONV-STLEN
           MOVE    0                   TO  KANACONV-CONV-FLAG
           MOVE    0                   TO  KANACONV-CHAR-TYPE
           MOVE    SPA-GMN-COMMENT     TO  KANACONV-INDATA
           CALL    "kanaconv"          USING ORCXKANACONVAREA
           IF     (KANACONV-RETURN     =   ZERO )
               MOVE    KANACONV-OUTDATA
                                       TO  SPA-GMN-COMMENT
           ELSE
               MOVE    KANACONV-OUTDATA
                                       TO  SPA-GMN-COMMENT
           END-IF
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
      *    出力桁数（全角の最大桁数の指定（省略可））
           MOVE    WRK-MOJI-MAX        TO  KANACHK-MAX-CNT
           MOVE    SPA-GMN-COMMENT     TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  SPA-GMN-COMMENT
           MOVE    KANACHK-MAX         TO  SPA-NAI-COMMAX
           IF     (KANACHK-RC      NOT =   ZERO  )
      *D       OR (KANACHK-SYUBETU     =   1     )
      *        全角以外エラー
      *        MOVE    "0013"          TO  SPA-ERRCD
      *        IF      KANACHK-RC2         =   7
      *            MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-COMMENT
      *D       END-IF
               CONTINUE
           ELSE
               MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-COMMENT
           END-IF
      *
           .
       800-COMMENT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    文例登録　処理
      *****************************************************************
       280-BUNREI-ADD-SEC                   SECTION.
      *
           MOVE    C50-COMMENT         TO  SPA-GMN-COMMENT
      *
           IF      SPA-GMN-COMMENT    =   SPACE
               MOVE    "0007"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
               MOVE    ZERO                TO  SPA-NAI-COMMAX
           ELSE
               MOVE    ZERO                TO  FLG-BUNREI
               PERFORM 800-COMMENT-CHK-SEC
               IF      KANACHK-MAX         >   800
                   MOVE    "K090"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-NAI-KEIFLG
      *            MOVE    1                   TO  FLG-BUNREI
      *            PERFORM 800-COMMENT-CHK-SEC
               END-IF
           END-IF
           IF     (SPA-ERRCD           =   SPACE )
              OR  (SPA-ERRCD(1:1)      =   "K"   )
               IF      SPA-GMN-BUNNUM      =   ZERO
                   MOVE    "0102"              TO  SPA-C50IDCD
               ELSE
                   PERFORM 28001-EXAMPLE-CHK-SEC
                   IF      FLG-EXAMPLE         =   ZERO
                       MOVE    "0103"              TO  SPA-C50IDCD
                   ELSE
                       MOVE    "0106"              TO  SPA-C50IDCD
                   END-IF
               END-IF
               IF      SPA-ERRCD(1:1)      =   "K"
                   MOVE    SPA-C50IDCD         TO  SPA-NAI-KEIMSG
                   MOVE    SPACE               TO  SPA-C50IDCD
               END-IF
           END-IF
           .
       280-BUNREI-ADD-EXT.
           EXIT.
      *
      *****************************************************************
      *    雛形有無チェック処理
      *****************************************************************
       28001-EXAMPLE-CHK-SEC                   SECTION.
      *
      *    雛形有無
           MOVE    SPACE               TO  EXAMPLE-REC
           INITIALIZE                  EXAMPLE-REC
           MOVE    SPA-HOSPNUM         TO  EXAMPLE-HOSPNUM
           MOVE    "1"                 TO  EXAMPLE-KUBUN
           MOVE    SPA-GMN-BUNNUM     TO  EXAMPLE-RENNUM
      *
           MOVE    EXAMPLE-REC         TO  MCPDATA-REC
           MOVE    "tbl_example"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_example"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 930-EXAMPLE-READ-SEC
           ELSE
               MOVE    1               TO  FLG-EXAMPLE
               INITIALIZE                  EXAMPLE-REC
           END-IF
           MOVE    "tbl_example"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       28001-EXAMPLE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    文例登録　処理
      *****************************************************************
       2801-BUNREI-SYORI-SEC                   SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           IF      SPA-GMN-BUNNUM     =   ZERO
      *        最大連番決定
               MOVE    SPACE               TO  EXAMPLE-REC
               INITIALIZE                  EXAMPLE-REC
               MOVE    SPA-HOSPNUM         TO  EXAMPLE-HOSPNUM
               MOVE    "1"                 TO  EXAMPLE-KUBUN
      *
               MOVE    EXAMPLE-REC         TO  MCPDATA-REC
               MOVE    "tbl_example"       TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_example"       TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 930-EXAMPLE-READ-SEC
               ELSE
                   MOVE    1               TO  FLG-EXAMPLE
                   INITIALIZE                  EXAMPLE-REC
               END-IF
               MOVE    "tbl_example"       TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
      *
               MOVE    EXAMPLE-RENNUM      TO  SPA-GMN-BUNNUM
               IF      SPA-GMN-BUNNUM     =   999
                   MOVE    "0010"              TO  SPA-ERRCD
                   GO      TO      2801-BUNREI-SYORI-EXT
               END-IF
               ADD     1                   TO  SPA-GMN-BUNNUM
           END-IF
      *
           MOVE    1                   TO  FLG-BUNREI
           PERFORM 800-COMMENT-CHK-SEC
      *
      *    雛形有無
           PERFORM 28001-EXAMPLE-CHK-SEC
           IF      FLG-EXAMPLE         =   ZERO
      *        更新
               MOVE    SPA-GMN-COMMENT    TO  EXAMPLE-COMMENT
      *
               MOVE    SMCNDATE-YMD        TO  EXAMPLE-UPYMD
               MOVE    SMCNDATE-HMS        TO  EXAMPLE-UPHMS
      *
               MOVE    SPA-OPID            TO  EXAMPLE-OPID
      *
               MOVE    EXAMPLE-REC         TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_example"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "C50 EXAMPLE UPD ERR:" MCP-RC
                           ",KEY:"   EXAMPLE-KEY    "."
                   MOVE    "0020"          TO  SPA-ERRCD
               END-IF
           ELSE
      *        追加
               MOVE    SPACE               TO  EXAMPLE-REC
               INITIALIZE                  EXAMPLE-REC
               MOVE    SPA-HOSPNUM         TO  EXAMPLE-HOSPNUM
               MOVE    "1"                 TO  EXAMPLE-KUBUN
               MOVE    SPA-GMN-BUNNUM      TO  EXAMPLE-RENNUM
               MOVE    SPA-GMN-COMMENT     TO  EXAMPLE-COMMENT
      *
               MOVE    SMCNDATE-YMD        TO  EXAMPLE-CREYMD
               MOVE    SMCNDATE-YMD        TO  EXAMPLE-UPYMD
               MOVE    SMCNDATE-HMS        TO  EXAMPLE-UPHMS
               MOVE    SPA-OPID            TO  EXAMPLE-OPID
      *
               MOVE    EXAMPLE-REC         TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_example"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "C50 EXAMPLE INS ERR:" MCP-RC
                           ",KEY:"   EXAMPLE-KEY    "."
                   MOVE    "0020"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  SPA-GMN-COMMENT
           MOVE    ZERO                TO  SPA-GMN-BUNNUM
           MOVE    ZERO                TO  SPA-GMN-COMSEL
           MOVE    ZERO                TO  SPA-NAI-RENNUM
           MOVE    ZERO                TO  SPA-NAI-COMMAX
           PERFORM 31031-SJKBN-INI-SEC
      *    コメントへ
           MOVE    1                   TO  SPA-GMN-CUR
      *
           .
       2801-BUNREI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメント削除　処理
      *****************************************************************
       240-COMMENT-DEL-SEC                   SECTION.
      *
           IF      SPA-GMN-PTID        =   ZERO
               MOVE    "0011"              TO  SPA-ERRCD
           ELSE
      *
           IF      SPA-GMN-SELNUM-MAX  =   ZERO
               MOVE    4               TO  SPA-GMN-CUR
               MOVE    6               TO  SPA-GMN-SYORI
           ELSE
               IF      SPA-GMN-SELNUM(1)   =   ZERO
                   MOVE    "0003"              TO  SPA-ERRCD
               ELSE
                   MOVE    "0104"              TO  SPA-C50IDCD
               END-IF
           END-IF
      *
           END-IF
           .
       240-COMMENT-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメント削除処理
      *****************************************************************
       2401-COMMENT-DELSYORI-SEC                   SECTION.
      *
           IF     (SPA-GMN-SELNUM-MAX  =   1  )  AND
                  (SPA-GMN-COMSEL      >   ZERO )
      *        選択中のコメントのみ
               PERFORM 24012-COMMENT-SJKBNDELETE-SEC
           ELSE
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL       IDX2    >   SPA-GMN-SELNUM-MAX
                   IF      SPA-GMN-SELNUM (IDX2)  NOT =   ZERO
                       PERFORM 24011-COMMENT-DELETE-SEC
                   END-IF
               END-PERFORM
           END-IF
      *
      *    レセプト作成管理マスタ更新
           IF      SPA-ERRCD           =   SPACE
               PERFORM 21212-RECEUPD-UPDATE-SEC
           END-IF
      *
      *    再表示
           IF     (SPA-GMN-SELNUM-MAX  =   1  )  AND
                  (SPA-GMN-COMSEL      >   ZERO )  AND
                  (SPA-COMMENT-MAX     >   1    )
      *        コメント一覧のみ更新
               MOVE    SPA-GMN-SELNUM(1)  TO  WRK-SELNUM
               PERFORM 2103-RECECOM-HEN-SEC
           ELSE
               MOVE    SPA-GMN-NYUGAIKBN  TO  WRK-NYUGAIKBN
               PERFORM 310-SPA-SET-SEC
           END-IF
           .
       2401-COMMENT-DELSYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメント削除処理
      *****************************************************************
       24012-COMMENT-SJKBNDELETE-SEC                   SECTION.
      *
           MOVE    SPA-GMN-SELNUM (01) TO  IDX
      *
           MOVE    SPACE               TO  RECECOM-REC
           INITIALIZE                  RECECOM-REC
           MOVE    SPA-HOSPNUM         TO  RECECOM-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  RECECOM-PTID
           MOVE    SPA-GMN-NNYUGAIKBN (IDX)
                                       TO  RECECOM-NYUGAIKBN
           MOVE    SPA-GMN-NSRYKA (IDX)
                                       TO  RECECOM-SRYKA
           MOVE    SPA-GMN-NSRYYM (IDX)
                                       TO  RECECOM-SRYYM
           MOVE    SPA-GMN-NHKNCOMBI (IDX)
                                       TO  RECECOM-HKNCOMBI
           MOVE    SPA-GMN-TSRYDD (IDX)
                                       TO  RECECOM-SRYDD
           IF      SPA-GMN-TSRYDD (IDX)    =   SPACE
               MOVE    ZERO                TO  RECECOM-SRYDD
           END-IF
           MOVE    SPA-GMN-TSJKBN (SPA-GMN-COMSEL)
                                       TO  RECECOM-SJKBN
           MOVE    SPA-GMN-TRENNUM(SPA-GMN-COMSEL)
                                       TO  RECECOM-RENNUM
      *
           MOVE    RECECOM-REC         TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_rececom"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "C50 RECECOM DEL ERR:" MCP-RC 
                           ",KEY:"   RECECOM-KEY    "."
               MOVE    "0021"          TO  SPA-ERRCD
               GO      TO      24012-COMMENT-SJKBNDELETE-EXT
           END-IF
      *    連番の再編集
      *
           MOVE    SPACE               TO  RECECOM-REC
           INITIALIZE                  RECECOM-REC
           MOVE    SPA-HOSPNUM         TO  RECECOM-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  RECECOM-PTID
           MOVE    SPA-GMN-NNYUGAIKBN (IDX)
                                       TO  RECECOM-NYUGAIKBN
           MOVE    SPA-GMN-NSRYKA (IDX)
                                       TO  RECECOM-SRYKA
           MOVE    SPA-GMN-NSRYYM (IDX)
                                       TO  RECECOM-SRYYM
           MOVE    SPA-GMN-NHKNCOMBI (IDX)
                                       TO  RECECOM-HKNCOMBI
           MOVE    SPA-GMN-TSRYDD (IDX)
                                       TO  RECECOM-SRYDD
           IF      SPA-GMN-TSRYDD (IDX)    =   SPACE
               MOVE    ZERO                TO  RECECOM-SRYDD
           END-IF
           MOVE    SPA-GMN-TSJKBN (SPA-GMN-COMSEL)
                                       TO  RECECOM-SJKBN
      *
           MOVE    ZERO                TO  WRK-RENNUM-MAX
      *
           MOVE    RECECOM-REC         TO  MCPDATA-REC
           MOVE    "tbl_rececom"       TO  MCP-TABLE
           MOVE    "key15"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_rececom"       TO  MCP-TABLE
               MOVE    "key15"             TO  MCP-PATHNAME
               PERFORM 920-RECECOM-READ-SEC
           ELSE
               MOVE    1               TO  FLG-RECECOM
               INITIALIZE                  RECECOM-REC
           END-IF
           PERFORM UNTIL   ( FLG-RECECOM   =   1  )
                       OR  ( SPA-ERRCD NOT =   SPACE )
               MOVE    RECECOM-KEY         TO  RECECOM-UPD-KEY
               ADD     1                   TO  WRK-RENNUM-MAX
               MOVE    WRK-RENNUM-MAX      TO  RECECOM-RENNUM
      *
               MOVE    SMCNDATE-YMD        TO  RECECOM-UPYMD
               MOVE    SMCNDATE-HMS        TO  RECECOM-UPHMS
               MOVE    SPA-OPID            TO  RECECOM-OPID
      *
               MOVE    RECECOM-REC         TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_rececom"       TO  MCP-TABLE
               MOVE    "upd1"              TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "C50 RECECOM UPD ERR:" MCP-RC
                           ",KEY:"   RECECOM-KEY    "."
                   MOVE    "0020"              TO  SPA-ERRCD
               END-IF
      *
               MOVE    "tbl_rececom"       TO  MCP-TABLE
               MOVE    "key15"             TO  MCP-PATHNAME
               PERFORM 920-RECECOM-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_rececom"       TO  MCP-TABLE
           MOVE    "key15"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       24012-COMMENT-SJKBNDELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメント削除処理
      *****************************************************************
       24011-COMMENT-DELETE-SEC                   SECTION.
      *
           MOVE    SPA-GMN-SELNUM (IDX2)  TO  IDX
      *
           MOVE    SPACE               TO  RECECOM-REC
           INITIALIZE                  RECECOM-REC
           MOVE    SPA-HOSPNUM         TO  RECECOM-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  RECECOM-PTID
           MOVE    SPA-GMN-NNYUGAIKBN (IDX)
                                       TO  RECECOM-NYUGAIKBN
           MOVE    SPA-GMN-NSRYKA (IDX)
                                       TO  RECECOM-SRYKA
           MOVE    SPA-GMN-NSRYYM (IDX)
                                       TO  RECECOM-SRYYM
           MOVE    SPA-GMN-NHKNCOMBI (IDX)
                                       TO  RECECOM-HKNCOMBI
           MOVE    SPA-GMN-TSRYDD (IDX)
                                       TO  RECECOM-SRYDD
           IF      SPA-GMN-TSRYDD (IDX)    =   SPACE
               MOVE    ZERO                TO  RECECOM-SRYDD
           END-IF
      *
           MOVE    RECECOM-REC         TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_rececom"       TO  MCP-TABLE
           MOVE    "del2"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "C50 RECECOM DEL ERR:" MCP-RC 
                           ",KEY:"   RECECOM-KEY    "."
               MOVE    "0021"          TO  SPA-ERRCD
           END-IF
      *
           .
       24011-COMMENT-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    変更　処理
      *****************************************************************
       210-HENKOU-SEC                   SECTION.
      *
           IF      SPA-GMN-PTID        =   ZERO
               MOVE    "0011"              TO  SPA-ERRCD
               GO      TO      210-HENKOU-EXT
           END-IF
      *
           IF     SPA-GMN-SELNUM-MAX  >   1
               MOVE    "0036"              TO  SPA-ERRCD
               GO      TO      210-HENKOU-EXT
           END-IF
      *
           MOVE    1                   TO  SPA-GMN-SYORI
           IF      SPA-GMN-SELNUM-MAX =   ZERO
               MOVE    4               TO  SPA-GMN-CUR
           ELSE
               MOVE    SPA-GMN-SELNUM(1)  TO  WRK-SELNUM
               PERFORM 2101-HENKOU-SYORI-SEC
           END-IF
      *
           .
       210-HENKOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    変更　処理
      *****************************************************************
       2101-HENKOU-SYORI-SEC                   SECTION.
      *
      *    入院判定
           IF      SPA-NAI-SRYYM   NOT =   SPA-GMN-TSRYYM  (WRK-SELNUM)
               MOVE    SPA-GMN-NSRYYM  (WRK-SELNUM)
                                       TO  SPA-NAI-SRYYM
               PERFORM 3105-NYUGAI-HEN-SEC
           END-IF
      *
           MOVE    SPA-GMN-TSRYYM  (WRK-SELNUM)
                                       TO  SPA-GMN-SRYYM
           MOVE    SPA-GMN-NSRYYM  (WRK-SELNUM)
                                       TO  SPA-NAI-SRYYM
      *    入外区分
           MOVE    SPACE               TO  SPA-GMN-NYUGAIKBN-G
           MOVE    SPA-GMN-NNYUGAIKBN(WRK-SELNUM)
                                       TO  SPA-GMN-NYUGAIKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-NYUGAI-MAX
               IF      SPA-GMN-NYUGAIKBN   =   SPA-GMN-NYUGAIKBN-LST
                                                           (IDX)(1:1)
                   MOVE    SPA-GMN-NYUGAIKBN-LST (IDX)
                                           TO  SPA-GMN-NYUGAIKBN-G
               END-IF
           END-PERFORM
      *
      *    保険組合せ
           PERFORM 3101-HKNCOMBI-HEN-SEC
      *
      *    診療科
           PERFORM 3102-SRYKA-HEN-SEC
      *
      *    症状詳記区分
           PERFORM 3106-SJKBN-HEN-SEC
      *    初期編集
           PERFORM 314-SYOKI-SEC
      *
           MOVE    SPA-GMN-NSRYKA  (WRK-SELNUM)
                                       TO  C50-SRYKA
           PERFORM 400-SRYKA-SEC
           MOVE    SPA-GMN-NHKNCOMBI  (WRK-SELNUM)
                                       TO  C50-HKNCOMBI
           PERFORM 410-HKNCOMBI-SEC
           IF      SPA-ERRCD           =   "0002"
               MOVE    "0039"              TO  SPA-ERRCD
               MOVE    3                   TO  SPA-GMN-CUR
               MOVE    5                   TO  SPA-GMN-SYORI
           END-IF
      *
      *    選択行からレセコメント検索編集
           PERFORM 2103-RECECOM-HEN-SEC
      *
      *    アフターケアの時診療日を選択させる
           IF       SPA-GMN-TSRYDD (WRK-SELNUM)       =   "00" OR
                                                           SPACE
               CONTINUE
           ELSE
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   SPA-GMN-RRKMAX
                   IF      SPA-GMN-JYURRKYMD (IDX)(7:2)
                                           =   SPA-GMN-TSRYDD
                                                  (WRK-SELNUM)
                       MOVE    IDX             TO  SPA-GMN-JYURRKNUM
                       MOVE    SPA-GMN-RRKMAX
                                               TO  IDX
                   END-IF
               END-PERFORM
           END-IF
      *
           IF      SPA-ERRCD          =   SPACE
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
           .
       2101-HENKOU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプトコメント編集処理
      *****************************************************************
       2103-RECECOM-HEN-SEC          SECTION.
      *
           INITIALIZE                  SPA-GMN-COMMENTLIST-G
           MOVE    1                   TO  FLG-GMN-INIT2
      *
           MOVE    SPACE               TO  RECECOM-REC
           INITIALIZE                  RECECOM-REC
           MOVE    SPA-HOSPNUM         TO  RECECOM-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  RECECOM-PTID
           MOVE    SPA-GMN-NNYUGAIKBN (WRK-SELNUM)
                                       TO  RECECOM-NYUGAIKBN
           MOVE    SPA-GMN-NSRYYM  (WRK-SELNUM)
                                       TO  RECECOM-SRYYM
           MOVE    SPA-GMN-NSRYKA  (WRK-SELNUM)
                                       TO  RECECOM-SRYKA
           MOVE    SPA-GMN-NHKNCOMBI(WRK-SELNUM)
                                       TO  RECECOM-HKNCOMBI
           IF      SPA-GMN-TSRYDD  (WRK-SELNUM)   =   SPACE
               MOVE    ZERO            TO  RECECOM-SRYDD
           ELSE
               MOVE    SPA-GMN-TSRYDD  (WRK-SELNUM)
                                       TO  RECECOM-SRYDD
           END-IF
      *
           MOVE    RECECOM-REC         TO  MCPDATA-REC
           MOVE    "tbl_rececom"       TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_rececom"       TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 920-RECECOM-READ-SEC
           ELSE
               MOVE    1               TO  FLG-RECECOM
               INITIALIZE                  RECECOM-REC
           END-IF
           PERFORM UNTIL  (FLG-RECECOM         =   1 )
                       OR (SPA-COMMENT-MAX     >=  100  )
               ADD     1                   TO  SPA-COMMENT-MAX
               MOVE    SPA-COMMENT-MAX     TO  SPA-GMN-TCOMREN
                                                    (SPA-COMMENT-MAX)
               MOVE    RECECOM-SJKBN       TO  SPA-GMN-TSJKBN
                                                    (SPA-COMMENT-MAX)
               MOVE    RECECOM-RENNUM      TO  SPA-GMN-TRENNUM
                                                    (SPA-COMMENT-MAX)
      *H29.5   コメント文字数で変換
               PERFORM 21031-RECCOMMENT-HEN-SEC
      **       MOVE    RECECOM-COMMENT     TO  SPA-GMN-TCOMMENT
      **                                            (SPA-COMMENT-MAX)
               MOVE    "tbl_rececom"       TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 920-RECECOM-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_rececom"       TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      SPA-COMMENT-MAX     >   ZERO
               MOVE    14                  TO  SPA-GMN-CUR
               MOVE    ZERO                TO  SPA-GMN-COMSEL
               MOVE    ZERO                TO  SPA-NAI-RENNUM
               MOVE    SPACE               TO  SPA-GMN-COMMENT
               MOVE    ZERO                TO  SPA-NAI-COMMAX
               PERFORM 31031-SJKBN-INI-SEC
           ELSE
               MOVE    ZERO                TO  SPA-GMN-COMSEL
               MOVE    ZERO                TO  SPA-NAI-RENNUM
               MOVE    SPACE               TO  SPA-GMN-COMMENT
               MOVE    ZERO                TO  SPA-NAI-COMMAX
               MOVE    15                  TO  SPA-GMN-CUR
           END-IF
           .
       2103-RECECOM-HEN-EXT.
           EXIT.
      *H29.5
      *****************************************************************
      *    コメントチェック処理
      *****************************************************************
       21031-RECCOMMENT-HEN-SEC                   SECTION.
      *
           INITIALIZE                  ORCXKANACONVAREA
           MOVE    100                 TO  KANACONV-LEN
           MOVE    5000                TO  KANACONV-STLEN
           MOVE    0                   TO  KANACONV-CONV-FLAG
           MOVE    0                   TO  KANACONV-CHAR-TYPE
           MOVE    RECECOM-COMMENT     TO  KANACONV-INDATA
           CALL    "kanaconv"          USING ORCXKANACONVAREA
           IF     (KANACONV-RETURN     =   ZERO )
               MOVE    KANACONV-OUTDATA
                                       TO  SPA-GMN-TCOMMENT
                                                    (SPA-COMMENT-MAX)
           ELSE
               MOVE    KANACONV-OUTDATA
                                       TO  SPA-GMN-TCOMMENT
                                                    (SPA-COMMENT-MAX)
           END-IF
           .
      *
       21031-RECCOMMENT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセコメント編集処理
      *****************************************************************
       21031-COMMENT-HENSYU-SEC        SECTION.
      *
      *    症状詳記区分
           MOVE    SPACE               TO  SPA-GMN-SJKBN-G
           MOVE    SPA-GMN-TSJKBN (SPA-GMN-COMSEL) TO  SPA-GMN-SJKBN
           PERFORM 4902-SJKBN-CHK-SEC
      *    連番
           IF      SPA-GMN-SYORI       =   3
      *        コピー処理は連番なし
               MOVE    ZERO                TO  SPA-NAI-RENNUM
           ELSE
               MOVE    SPA-GMN-TRENNUM(SPA-GMN-COMSEL)
                                           TO  SPA-NAI-RENNUM
           END-IF
      *
           MOVE    SPACE               TO  RECECOM-REC
           INITIALIZE                  RECECOM-REC
           MOVE    SPA-HOSPNUM         TO  RECECOM-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  RECECOM-PTID
           MOVE    SPA-GMN-NNYUGAIKBN (WRK-SELNUM)
                                       TO  RECECOM-NYUGAIKBN
           MOVE    SPA-GMN-NSRYYM  (WRK-SELNUM)
                                       TO  RECECOM-SRYYM
           MOVE    SPA-GMN-NSRYKA  (WRK-SELNUM)
                                       TO  RECECOM-SRYKA
           MOVE    SPA-GMN-NHKNCOMBI(WRK-SELNUM)
                                       TO  RECECOM-HKNCOMBI
           IF      SPA-GMN-TSRYDD  (WRK-SELNUM)   =   SPACE
               MOVE    ZERO            TO  RECECOM-SRYDD
           ELSE
               MOVE    SPA-GMN-TSRYDD  (WRK-SELNUM)
                                       TO  RECECOM-SRYDD
           END-IF
           MOVE    SPA-GMN-TSJKBN (SPA-GMN-COMSEL)
                                       TO  RECECOM-SJKBN
           MOVE    SPA-GMN-TRENNUM(SPA-GMN-COMSEL)
                                       TO  RECECOM-RENNUM
      *
           MOVE    RECECOM-REC         TO  MCPDATA-REC
           MOVE    "tbl_rececom"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_rececom"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-RECECOM-READ-SEC
           ELSE
               MOVE    1               TO  FLG-RECECOM
               INITIALIZE                  RECECOM-REC
           END-IF
           MOVE    "tbl_rececom"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    RECECOM-COMMENT     TO  SPA-GMN-COMMENT
           MOVE    ZERO                TO  SPA-NAI-COMMAX
      *H28.1
           MOVE    ZERO                TO  FLG-BUNREI
           PERFORM 800-COMMENT-CHK-SEC
      *
           MOVE    1                   TO  SPA-GMN-CUR
           .
       21031-COMMENT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    コピー　処理
      *****************************************************************
       250-COPY-SEC                   SECTION.
      *
           IF      SPA-GMN-PTID        =   ZERO
               MOVE    "0011"              TO  SPA-ERRCD
               GO      TO      250-COPY-EXT
           END-IF
      *
           IF     SPA-GMN-SELNUM-MAX  >   1
               MOVE    "0036"              TO  SPA-ERRCD
               GO      TO      250-COPY-EXT
           END-IF
      *
           MOVE    3                   TO  SPA-GMN-SYORI
           IF     SPA-GMN-SELNUM-MAX  =   ZERO
               MOVE    4               TO  SPA-GMN-CUR
           ELSE
               MOVE    SPA-GMN-SELNUM(1)  TO  WRK-SELNUM
               PERFORM 2501-COPY-SYORI-SEC
           END-IF
      *
           .
       250-COPY-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメントコピー処理
      *****************************************************************
       2501-COPY-SYORI-SEC                   SECTION.
      *
           IF      WRK-SELNUM          =   ZERO
               MOVE   1                TO  WRK-SELNUM
           END-IF
      *    選択行からレセコメント検索編集
           PERFORM 2103-RECECOM-HEN-SEC
      *
           MOVE    1                   TO  SPA-GMN-CUR
           .
       2501-COPY-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    前月分処理
      *****************************************************************
       270-BACK-MONTH-SEC               SECTION.
      *
           IF      SPA-GMN-PTID        =   ZERO
               MOVE    "0011"              TO  SPA-ERRCD
               GO      TO      270-BACK-MONTH-EXT
           END-IF
      *
           INITIALIZE                     STS-AREA-DAY
           INITIALIZE                     LNK-DAY6-AREA
           MOVE    "61"                   TO  LNK-DAY6-IRAI
           MOVE    SPA-NAI-SRYYM          TO  LNK-DAY6-KIJUN (1:6)
           MOVE    "01"                   TO  LNK-DAY6-KIJUN (7:2)
           MOVE    "2"                    TO  LNK-DAY6-ZOGENPTN
           MOVE    -1                     TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"              USING   STS-AREA-DAY
                                                  LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN(1:6)   TO  SPA-NAI-SRYYM
      *
           MOVE    SPA-NAI-SRYYM          TO  WRK-SYMD (1:6)
           MOVE    "01"                   TO  WRK-SYMD (7:2)  
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG (1:6)      TO  SPA-GMN-SRYYM
      *
      *    保険組合せ
           PERFORM 3101-HKNCOMBI-HEN-SEC
      *    診療科
           PERFORM 3102-SRYKA-HEN-SEC
      *
      *    入院判定
           PERFORM 3105-NYUGAI-HEN-SEC
      *    症状詳記区分
           PERFORM 3106-SJKBN-HEN-SEC
      *
      *    初期編集
           PERFORM 314-SYOKI-SEC
      *
           .
      *
       270-BACK-MONTH-EXT.
           EXIT.
      *
      *****************************************************************
      *    次月分処理
      *****************************************************************
       280-NEXT-MONTH-SEC               SECTION.
      *
           IF      SPA-GMN-PTID        =   ZERO
               MOVE    "0011"              TO  SPA-ERRCD
               GO      TO      280-NEXT-MONTH-EXT
           END-IF
      * 
      *     次月診療年月 
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    SPA-NAI-SRYYM       TO  LNK-DAY6-KIJUN (1:6)
           MOVE    "01"                TO  LNK-DAY6-KIJUN (7:2)  
           MOVE    "2"                 TO  LNK-DAY6-ZOGENPTN
           MOVE    1                   TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING
                                           STS-AREA-DAY
                                           LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN(1:6)
                                       TO  SPA-NAI-SRYYM
      * 
           MOVE    SPA-NAI-SRYYM     TO  WRK-SYMD (1:6)
           MOVE    "01"                TO  WRK-SYMD (7:2)
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG (1:6)   TO  SPA-GMN-SRYYM
      *
      *    保険組合せ
           PERFORM 3101-HKNCOMBI-HEN-SEC
      *    診療科
           PERFORM 3102-SRYKA-HEN-SEC
      *
      *    入院判定
           PERFORM 3105-NYUGAI-HEN-SEC
      *    症状詳記区分
           PERFORM 3106-SJKBN-HEN-SEC
      *
      *    初期編集
           PERFORM 314-SYOKI-SEC
           .
      *
       280-NEXT-MONTH-EXT.
           EXIT.
      *
      *****************************************************************
      *    前頁処理
      *****************************************************************
       290-BACK-PAGE-SEC               SECTION.
      *
           IF      SPA-GMN-PAGE       >   1
               COMPUTE SPA-GMN-PAGE       =   SPA-GMN-PAGE
                                           -   1
      *        コメント編集編集
               PERFORM 3103-RECECOM-HEN-SEC
            END-IF
           .
      *
       290-BACK-PAGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    次頁処理
      *****************************************************************
       291-NEXT-PAGE-SEC               SECTION.
      *
           IF      SPA-GMN-NEXT       =   1
               COMPUTE SPA-GMN-PAGE       =   SPA-GMN-PAGE
                                          +   1
      *        コメント編集編集
               PERFORM 3103-RECECOM-HEN-SEC
            END-IF
           .
      *
       291-NEXT-PAGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    LNK-COMMON          TO  SPA-KYOUTU
      *
           MOVE    SPA-HZN-MOTOPG      TO  SPA-SAKIPG
           MOVE    "C50"               TO  SPA-MOTOPG
      *
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
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
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-C50IDCD       NOT =   SPACE
               PERFORM 520-C50IDSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
      *****MOVE    SPACE               TO  LINKAREA
           MOVE    SPACE               TO  LNK-KYOUTU
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "C50"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "診療科選択エラー"
                                       TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "保険組合せ選択エラー"
                                       TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "選択番号が有りません"
                                       TO  SPA-ERRMSG
               WHEN    "0036"
                   STRING  "選択番号が複数選択されています。"
                           "削除のみ可能です。"
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0004"
                   MOVE    "文例登録番号エラー"
                                       TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "診療年月に診療行為がありません。"
                                       TO  SPA-ERRMSG
                   MOVE    "コメントは登録できません。"
                                       TO  SPA-ERRMSG(33:)
               WHEN    "0007"
                   MOVE    "コメント内容が未入力です。入力して下さい"
                                       TO  SPA-ERRMSG
               WHEN    "0008"
                   MOVE    "入外区分入力エラー"
                                       TO  SPA-ERRMSG
               WHEN    "0009"
                   MOVE    "症状詳記区分入力エラー"
                                       TO  SPA-ERRMSG
               WHEN    "0016"
                   STRING  "症状詳記区分が診療年月ではありません。"
                           "症状詳記区分を変更して下さい。"
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0010"
                   STRING  "文例の最大登録番号が９９９です。"
                           "これ以上自動追加できません。"
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0011"
                   MOVE
                   "患者が選択されてません。文例の処理しかできません"
                                       TO  SPA-ERRMSG
               WHEN    "0012"
                   MOVE
                   "対象の診療科と保険組合せは受診が有りません。"
                                       TO  SPA-ERRMSG
               WHEN    "0013"
                   MOVE    "コメントは全角で入力して下さい"
                                       TO  SPA-ERRMSG
               WHEN    "0014"
                   MOVE    "アフターケアの時、診療日を選択して下さい"
                                       TO  SPA-ERRMSG
               WHEN    "0015"
                   MOVE    "アフターケア診療日選択番号がありません"
                                       TO  SPA-ERRMSG
               WHEN    "0030"
                   MOVE    "複写開始年月暦日エラー"
                                       TO  SPA-ERRMSG
               WHEN    "0031"
                   MOVE    "複写月数は１２までです。"
                                       TO  SPA-ERRMSG
               WHEN    "0032"
                   MOVE    "複写開始年月が未入力です。"
                                       TO  SPA-ERRMSG
               WHEN    "0033"
                   MOVE    "アフターケアは複写できません。"
                                       TO  SPA-ERRMSG
               WHEN    "0034"
                   MOVE    "複写開始年月＞診療年月で入力して下さい。"
                                       TO  SPA-ERRMSG
               WHEN    "0035"
                   MOVE    "複数選択は削除のみ有効です。"
                                       TO  SPA-ERRMSG
               WHEN    "0037"
                   MOVE    "これ以上選択できません。削除して下さい。"
                                       TO  SPA-ERRMSG
               WHEN    "0038"
                   MOVE    "複数選択は１２行までです。削除して下さい。"
                                       TO  SPA-ERRMSG
               WHEN    "0039"
                   MOVE    "保険組合せでの受診履歴がありません。"
                                       TO  SPA-ERRMSG
                   MOVE    "保険組合せを選択し直して下さい。"
                                       TO  SPA-ERRMSG(37:)
               WHEN    "0040"
                   MOVE    "コメント選択番号がありません。"
                                       TO  SPA-ERRMSG
               WHEN    "0041"
                   MOVE    "症状詳記区分がありません。"
                                       TO  SPA-ERRMSG
               WHEN    "0042"
                   MOVE    "症状詳記区分を選択して下さい。"
                                       TO  SPA-ERRMSG
      *ver.4.6
               WHEN    "K090"
                   STRING  "警告！！"
                           "文例は４００文字までの登録です。"
                           "４００文字以上は登録時に削除します。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0090"
                   STRING  SPA-NAI-COMMAX
                           "文字となります。"
                           "2400文字（全角1200文字）以内に"
                           "文字数を減らして下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0099"
                   STRING  "連番９９まで登録されています。"
                           "これ以上追加はできません。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *
               WHEN    "0020"
                   MOVE    "更新エラーです。"
                                       TO  SPA-ERRMSG
               WHEN    "0021"
                   MOVE    "削除エラーです。"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  C50ERR
           MOVE    SPA-ERRCD           TO  C50ERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  C50ERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "C50"               TO  SPA-MOTOPG
           MOVE    "C50ERR"            TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "C50ERR"            TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       520-C50IDSET-SEC              SECTION.
      *
           EVALUATE    SPA-C50IDCD
               WHEN    "0101"
                   STRING  "診療年月のレセプトコメントを"
                           "追加登録します。よろしいですか？"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-C50IDMSG
                   END-STRING
               WHEN    "0102"
                   STRING  "文例へ追加登録します。"
                           "よろしいですか？"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-C50IDMSG
                   END-STRING
               WHEN    "0103"
                   STRING  "文例登録番号の文例を更新します。"
                           "よろしいですか？"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-C50IDMSG
                   END-STRING
               WHEN    "0106"
                   STRING  "文例を文例登録番号で追加します。"
                           "よろしいですか？"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-C50IDMSG
                   END-STRING
               WHEN    "0104"
                   STRING  "選択されているコメントを削除します。"
                           "よろしいですか？"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-C50IDMSG
                   END-STRING
               WHEN    "0105"
                   STRING  "選択されているコメントを変更します。"
                           "よろしいですか？"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-C50IDMSG
                   END-STRING
               WHEN    "0107"
                   STRING  "既に登録済みです。"
                           "ＯＫでコメント内容を更新します。"
                           "よろしいですか？"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-C50IDMSG
                   END-STRING
               WHEN    "0110"
                   STRING  "詳記区分の連番を追加して登録します。"
                           "よろしいですか？"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-C50IDMSG
                   END-STRING
               WHEN    "0108"
                   STRING  "表示されているコメントをすべて登録します。"
                                       DELIMITED  BY  SIZE
      **                   "登録済みのコメントは内容を更新します。"
                           "登録済みのコメントは連番追加で登録。"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-C50IDMSG
                   END-STRING
               WHEN    "0109"
                   STRING  "表示されているコメントをすべて登録します。"
                                       DELIMITED  BY  SIZE
                           "よろしいですか？"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-C50IDMSG
                   END-STRING
               WHEN    "0201"
                   STRING  "診療年月のレセプトコメントの登録と、"
                           "複写月数分のコメントの複写をします。"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-C50IDMSG
                   END-STRING
               WHEN    "0205"
                   STRING  "選択されているコメントの変更と、"
                           "複写月数分のコメントの複写をします。"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-C50IDMSG
                   END-STRING
               WHEN    "0206"
                   STRING  "選択番号に登録されているすべてのコメントを"
                           "複写月数分複写します。"
                           "よろしいですか？"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-C50IDMSG
                   END-STRING
               WHEN    OTHER
                   MOVE    SPA-ERRCD
                                       TO  WRK-C50IDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-C50ID1-FLG
      *
           MOVE    SPACE               TO  C50ID1
           INITIALIZE                      C50ID1
           MOVE    SPA-C50IDCD         TO  C50ID1-ID1CODE
           MOVE    WRK-C50IDMSG        TO  C50ID1-ID1MSG
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "C50"               TO  SPA-MOTOPG
           MOVE    "C50ID1"            TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "C50ID1"            TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-C50IDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読み込み処理
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読込
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  HKNCOMBI-REC
               MOVE    ZERO                TO  FLG-HKNCOMBI
           ELSE
               INITIALIZE                      HKNCOMBI-REC
               MOVE    1                   TO  FLG-HKNCOMBI
           END-IF
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプトコメント読込
      *****************************************************************
       920-RECECOM-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  RECECOM-REC
               MOVE    ZERO                TO  FLG-RECECOM
           ELSE
               INITIALIZE                      RECECOM-REC
               MOVE    1                   TO  FLG-RECECOM
           END-IF
      *
           .
       920-RECECOM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    雛形読込
      *****************************************************************
       930-EXAMPLE-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  EXAMPLE-REC
               MOVE    ZERO                TO  FLG-EXAMPLE
           ELSE
               INITIALIZE                      EXAMPLE-REC
               MOVE    1                   TO  FLG-EXAMPLE
           END-IF
      *
           .
       930-EXAMPLE-READ-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴マスター読込
      *****************************************************************
       970-JYURRK-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  JYURRK-REC
               MOVE    ZERO            TO  FLG-JYURRK
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           .
       970-JYURRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者労災保険情報読み込み
      *****************************************************************
       993-PTRSIINF-NEXT-SEC        SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTRSIINF-REC
               MOVE    ZERO            TO  FLG-PTRSIINF
           ELSE
               INITIALIZE                  PTRSIINF-REC
               MOVE    1               TO  FLG-PTRSIINF
           END-IF
      *
           .
       993-PTRSIINF-NEXT-EXT.
           EXIT.
      *****************************************************************
      *    入院会計読み込み
      *****************************************************************
       980-NYUINACCT-READ-SEC        SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
               MOVE    ZERO            TO  FLG-NYUINACCT
           ELSE
               INITIALIZE                  NYUINACCT-REC
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
           .
       980-NYUINACCT-READ-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴マスター読込
      *****************************************************************
       910-PTNYUINRRK-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
               MOVE    ZERO            TO  FLG-PTNYUINRRK
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       910-PTNYUINRRK-READ-EXT.
           EXIT.
      *****************************************************************
      *    診療会計マスター次読込
      *****************************************************************
       980-SRYACCT-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACCT-REC
               MOVE    ZERO                TO  FLG-SRYACCT
           ELSE
               INITIALIZE                      SRYACCT-REC
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
      *
           .
       980-SRYACCT-READ-EXT.
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
       990-DBCLOSE-SEC           SECTION.
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
           MOVE   "PUTWINDOW"          TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
