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
       PROGRAM-ID.                 ORCGXF01.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : メモ登録入力
      *  コンポーネント名  : メモ登録（ＸＦ０１）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  07/08/XX    NACL-多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.03.00    NACL-多々納  08/06/10  ＣＬＡＩＭ送信追加
      *  04.04.00    NACL-多々納  08/12/08  確認画面追加
      *  05.00.00    NACL-多々納  18/11/XX  PUSH通信処理追加
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
           COPY    "XF01SCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PTMEMOINF       PIC 9(01).
           03  FLG-EXAMPLE         PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-UKETUKE         PIC 9(01).
      *
           03  FLG-TOROKU          PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-CLAIM               PIC 9(01).
           03  FLG-SOUSIN              PIC 9(01).
           03  FLG-UKETUKE-ARI         PIC 9(01).
      *H30.11
           03  FLG-SYORIKBN            PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX-S               PIC 9(04).
           03  IDX-S2              PIC 9(04).
           03  IDS                 PIC 9(04).
           03  IDX-M               PIC 9(04).
           03  IDX-M2              PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY           PIC X(04).
               05  WRK-SMM           PIC X(02).
               05  WRK-SDD           PIC X(02).
      *
           03  WRK-ZZZ             PIC ZZZ.
      *
           03  WRK-SRYKA-G.
               05  WRK-SRYKA       PIC X(02).
               05  WRK-SRYKAF      PIC X(01).
               05  WRK-SRYKA-MEI   PIC X(12).
           03  WRK-RENNUM          PIC 9(02).
           03  WRK-MEMOKBN         PIC 9(01).
           03  WRK-MEMO            PIC X(1000).
           03  WRK-MEMO-WK         PIC X(1000).
      *
           03  WRK-SELNUM          PIC 9(03).
      *    コラムリスト位置
           03  WRK-XF01-ROW            PIC S9(09).
           03  FLG-GMN-INIT            PIC  9(01).
      *
           03  WRK-MCP-WIDGET          PIC X(68).
           03  WRK-PATHNAME            PIC X(68).
      *
           03  WRK-XFIDMSG             PIC X(80).
      *TEST
      *
            03  WRK-MEMO-01           PIC X(100).
            03  WRK-MEMO-02           PIC X(100).
            03  WRK-MEMO-03           PIC X(100).
            03  WRK-MEMO-04           PIC X(100).
            03  WRK-MEMO-05           PIC X(100).
            03  CTR-1           PIC 9(03).
            03  CTR-2           PIC 9(03).
            03  CTR-3           PIC 9(03).
            03  CTR-4           PIC 9(03).
            03  CTR-5           PIC 9(03).
            03  PTR-A           PIC 9(03).
      *
      *    ＣＬＡＩＭ処理用
           03  WRK-UKE-DATA.
               05  WRK-UKE-UKETIME         PIC X(06).
               05  WRK-UKE-SRYKA           PIC X(02).
               05  WRK-UKE-DRCD            PIC X(05).
               05  WRK-UKE-HKNCOMBI        PIC X(04).
               05  WRK-UKE-SRYNAIYO        PIC X(02).
               05  WRK-UKE-SRYNAIDSP       PIC X(20).
           03  WRK-KBNCD                   PIC 9(02).
           03  WRK-PTID                    PIC X(10).
      *
      *H30.11
      *PUSH通信用
           03  WRK-HEN-DATE.
               05  WRK-HEN-YY          PIC X(04).
               05  WRK-HEN-H1          PIC X(01).
               05  WRK-HEN-MM          PIC X(02).
               05  WRK-HEN-H2          PIC X(01).
               05  WRK-HEN-DD          PIC X(02).
      *    時間編集
           03  WRK-THMS.
               05  WRK-THHH             PIC X(02).
               05  WRK-THMM             PIC X(02).
               05  WRK-THSS             PIC X(02).
           03  WRK-HEN-TIME.
               05  WRK-HEN-THH         PIC X(02).
               05  WRK-HEN-T1          PIC X(01).
               05  WRK-HEN-TMM         PIC X(02).
               05  WRK-HEN-T2          PIC X(01).
               05  WRK-HEN-TSS         PIC X(02).
      *
      *    年齢計算
       01  WRK-NENREI-AREA.
           03  WRK-YMDS1.
               05  WRK-YYS1            PIC 9(04).
               05  WRK-MMS1            PIC 9(02).
               05  WRK-DDS1            PIC 9(02).
           03  WRK-YMDS2.
               05  WRK-YYS2            PIC 9(04).
               05  WRK-MMS2            PIC 9(02).
               05  WRK-DDS2            PIC 9(02).
           03  WRK-NENREI.
               05  WRK-NENREI-YY       PIC 9(04).
               05  WRK-NENREI-MM       PIC 9(02).
               05  WRK-NENREI-DD       PIC 9(02).
      *    年齢表示
       01  WRK-NENREI-G.
           03  WRK-NENREI-Z        PIC ZZ9.
           03  WRK-NENREI-MEI      PIC X(04).
       01  WRK-NENREI-GR           REDEFINES   WRK-NENREI-G.
           03  WRK-NENREI-ZN       PIC Z9.
           03  WRK-NENREI-MEIN     PIC X(08).
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
      *    ＣＬＡＩＭファイル名
       01  CLAIM-FILE.
           03  FILLER              PIC X(30)   VALUE
               "scripts/claim/HL01.sh".
      *
      *    ファイルデイレクトリ位置指定サブ
           COPY  "CPMKPASS.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    COPY    "CPSK1001.INC".
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1005.INC".
      *　　（ＣＬＡＩＭ接続）
           COPY    "CPSK9000.INC".
           COPY    "CPSK9001.INC".
           COPY    "CPSK1012.INC".
      *
      *    患者メモ登録
       01  PTMEMOINF-REC.
           COPY    "CPPTMEMOINF.INC".
      *    受付
       01  UKETUKE-REC.
           COPY    "CPUKETUKE.INC".
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
           COPY    "CPORCXKANACONV.INC".
      *
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *H30.11
      *PUSH通信
           COPY    "CPPUSHACCEPT02.INC".
      *
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *===> For Claim Start
           COPY    "CPSHELLTBL.INC".
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
           COPY    "XF01.INC".
           COPY    "XF011.INC".
           COPY    "XFERR.INC".
           COPY    "XFID1.INC".
           COPY    "XF01X.INC".
           COPY    "XFID2.INC".
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
      *****MOVE    SPA-WORK            TO  SPA-XF01KYOUTU
           MOVE    SPA-FREE            TO  SPA-XF01
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
      **** MOVE    SPA-XF01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA             TO  SPA-COMMON
           MOVE    SPA-XF01             TO  SPA-FREE
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
           IF      SPA-MOTOPG          =   "XFERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
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
               MOVE   "XF01"               TO  MCP-WINDOW
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
               WHEN    "XF011"
      *            文例選択より
                   MOVE    4                   TO  SPA-GMN-CUR
               WHEN    "XFID1"
      *            確認画面より
                   PERFORM 3002-XFID1-SET-SEC
               WHEN    "XFID2"
      *            確認画面より
                   PERFORM 3003-XFID2-SET-SEC
               WHEN    "XF01X"
      *            ＣＬＡＩＭ画面より
                   IF      SPA-XF01X-FLG       =   SPACE
                       MOVE    1                   TO  SPA-GMN-CUR
                   ELSE
      *                送信
                       MOVE    ZERO            TO  SPA-CLAIM-MULTIHOST
                       PERFORM 4906-CLAIM-SOUSIN-SEC
                   END-IF
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
       3002-XFID1-SET-SEC          SECTION.
      *
           EVALUATE    SPA-XFIDCD
               WHEN    "0001"
      *            登録の確認
                   IF      SPA-XFID1-FLG      =   "OK"
                       PERFORM 2121-TOUROKU-SYORI-SEC
                   ELSE
                       MOVE    1               TO  SPA-GMN-CUR
                   END-IF
               WHEN    "0002"
      *            コメント削除の確認
                   IF      SPA-XFID1-FLG      =   "OK"
                       PERFORM 2401-MEMO-DELSYORI-SEC
                   ELSE
                       MOVE    1                   TO  SPA-GMN-CUR
                   END-IF
               WHEN    "0003"
               WHEN    "0004"
      *            当日コメントの確認
                   PERFORM 42011-SELMEMO-SYORI-SEC
               WHEN    "0101"
      *            コメント１登録の確認
                   IF      SPA-XFID1-FLG      =   "OK"
                       PERFORM 21201-MEMO01-ADD-SEC
                   ELSE
                       MOVE    1                   TO  SPA-GMN-CUR
                   END-IF
               WHEN    "0102"
      *            コメント１削除の確認
                   IF      SPA-XFID1-FLG      =   "OK"
                       PERFORM 21201-MEMO01-DEL-SEC
                   ELSE
                       MOVE    1                   TO  SPA-GMN-CUR
                   END-IF
               WHEN    "0103"
                   IF      SPA-XFID1-FLG      =   "OK"
                       PERFORM 21201-MEMO02-ADD-SEC
                   ELSE
                       MOVE    3                   TO  SPA-GMN-CUR
                   END-IF
               WHEN    "0104"
      *            コメント１登録の確認
                   IF      SPA-XFID1-FLG      =   "OK"
                       PERFORM 21201-MEMO02-DEL-SEC
                   ELSE
                       MOVE    3                   TO  SPA-GMN-CUR
                   END-IF
           END-EVALUATE
           MOVE    SPACE               TO  SPA-XFIDCD
      *
           .
       3002-XFID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＸＤＩＤ２）ＯＫ処理
      *****************************************************************
       3003-XFID2-SET-SEC          SECTION.
      *
           EVALUATE    SPA-XFIDCD2
               WHEN    "0001"
                   EVALUATE    SPA-XFID2-FLG
                       WHEN    "B10"
      *                登録・送信
                           PERFORM 2121-TOUROKU-SYORI-SEC
                           IF      SPA-ERRCD            =   SPACE
                               PERFORM 213-SOUSIN-SEC
                           END-IF
                       WHEN    "B12"
      *                登録
                           PERFORM 2121-TOUROKU-SYORI-SEC
                       WHEN    OTHER
                           MOVE    1               TO  SPA-GMN-CUR
                   END-EVALUATE
           END-EVALUATE
           MOVE    SPACE               TO  SPA-XFIDCD2
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 500-GMNHEN-SEC
               PERFORM 510-ERRSET-SEC
           END-IF
      *
           .
       3003-XFID2-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面編集処理
      *****************************************************************
       3001-SPA-INIT-SEC             SECTION.
      *
           INITIALIZE                  SPA-XF01
           MOVE    SPA-MOTOPG          TO  SPA-HZN-MOTOPG
      *
           MOVE    SPA-PTID            TO  SPA-GMN-PTID
           MOVE    SPA-PTNUM           TO  SPA-GMN-PTNUM
           MOVE    SPA-NAME            TO  SPA-GMN-NAME
           MOVE    SPA-BIRTHDAYW       TO  SPA-GMN-BIRTHDAY
           MOVE    SPA-SEX             TO  SPA-GMN-SEX
      *
      *    年齢計算
           MOVE    SPA-BIRTHDAY        TO  WRK-YMDS1
           MOVE    SPA-SYSYMD          TO  WRK-YMDS2
           COMPUTE WRK-NENREI-YY       =   WRK-YYS2
                                       -   WRK-YYS1
           IF      WRK-YMDS1(5:4)      >   WRK-YMDS2(5:4)
               COMPUTE WRK-NENREI-YY   =   WRK-NENREI-YY
                                           -   1
               COMPUTE WRK-NENREI-MM   =  (WRK-MMS2  +  12 )
                                           -   WRK-MMS1
           ELSE
               COMPUTE WRK-NENREI-MM   =   WRK-MMS2
                                           -   WRK-MMS1
           END-IF
           IF      WRK-DDS1            >   WRK-DDS2
               COMPUTE WRK-NENREI-MM   =   WRK-NENREI-MM
                                           -   1
           END-IF
      *    画面表示用
           IF      WRK-NENREI-YY       =   ZERO
               MOVE    WRK-NENREI-MM       TO  WRK-NENREI-ZN
               MOVE    "ヶ月"              TO  WRK-NENREI-MEIN
           ELSE
               MOVE    WRK-NENREI-YY       TO  WRK-NENREI-Z
               MOVE    "才"                TO  WRK-NENREI-MEI
           END-IF
           MOVE    WRK-NENREI-G        TO  SPA-GMN-NENREI
      *
           MOVE    SPA-SYSYMD          TO  SPA-NAI-SYSYMD
      *
      *    受付中
           PERFORM 30011-UKETUKE-HEN-SEC
      *    ＣＬＡＩＭ接続判定
      *v4.8
      *    ginbee対応
      *    IF      SPA-MW              =   SPA-GINBEE
      *        MOVE    ZERO            TO  SPA-CLAIM-FLG
      *    ELSE
               PERFORM 31002-SYS900-READ-SEC
               IF      SYS-9000-SETUZOKU   =   ZERO
                                       OR  SPACE
                   MOVE    ZERO            TO  SPA-CLAIM-FLG
               ELSE
                   MOVE    1               TO  SPA-CLAIM-FLG
               END-IF
      **   END-IF
      *
           PERFORM 310-SPA-SET-SEC
      *
           IF      SPA-HZN-MOTOPG      =   "U02"
               MOVE    3               TO  SPA-GMN-CUR
           ELSE
               MOVE    1               TO  SPA-GMN-CUR
           END-IF
           .
       3001-SPA-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付情報編集処理
      *****************************************************************
       30011-UKETUKE-HEN-SEC             SECTION.
      *
           IF     (SPA-P02-UKEID       =   ZERO  )
               PERFORM 49061-UKETUKE-CHEK-SEC
               IF      FLG-UKETUKE-ARI     =   1
                   MOVE    "【受付有】"        TO  SPA-GMN-UKEMEI
               END-IF
           ELSE
               MOVE    "【受付選択】"        TO  SPA-GMN-UKEMEI
           END-IF
           .
       30011-UKETUKE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＬＡＩＭ接続検索処理
      *****************************************************************
       31002-SYS900-READ-SEC             SECTION.
      *
           MOVE    SPACE               TO  SYS-9000-REC
           INITIALIZE                  SYS-9000-REC
           MOVE    "9000"              TO  SYS-9000-KANRICD
           MOVE    "*"                 TO  SYS-9000-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-9000-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-9000-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-9000-HOSPNUM
           MOVE    SYS-9000-REC        TO  SYSKANRI-REC
           PERFORM 9009-SYSKANRI-KEY-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-9000-REC
           END-IF
      *
           .
       31002-SYS900-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面編集処理
      *****************************************************************
       310-SPA-SET-SEC             SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    SPA-NAI-SYSYMD      TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO  SPA-GMN-SYSYMD
      *
      *    診療科
           PERFORM 3102-SRYKA-HEN-SEC
      *    患者コメント一覧編集
           MOVE    ZERO                TO  SPA-GMN-NEXT
           MOVE    1                   TO  SPA-GMN-PAGE
           MOVE    2                   TO  SPA-NAI-MEMOKBN
           PERFORM 3101-PTMEMO-LIST-SEC
      *    患者コメント初期編集
           PERFORM 3103-PTMEMO-HEN-SEC
      *
           MOVE    1                   TO  SPA-GMN-CUR
           .
       310-SPA-SET-EXT.
           EXIT.
      *****************************************************************
      *    患者コメント一覧編集処理
      *****************************************************************
       3101-PTMEMO-LIST-SEC          SECTION.
      *
           INITIALIZE                      SPA-GMN-TBLG
           MOVE    ZERO                TO  SPA-GMN-SELNUM
      *
      *    患者メモ検索
           MOVE    SPACE               TO  PTMEMOINF-REC
           INITIALIZE                  PTMEMOINF-REC
           MOVE    SPA-HOSPNUM         TO  PTMEMOINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTMEMOINF-PTID
           MOVE    SPA-NAI-MEMOKBN     TO  PTMEMOINF-MEMOKBN
      *
           MOVE    PTMEMOINF-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptmemoinf"     TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptmemoinf"     TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 910-PTMEMOINF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTMEMOINF
               INITIALIZE                  PTMEMOINF-REC
           END-IF
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDX-S2
           MOVE    ZERO                TO  SPA-GMN-NEXT
           IF      SPA-GMN-PAGE        =   ZERO
               MOVE    1               TO  SPA-GMN-PAGE
           END-IF
           COMPUTE IDX-S               =  (SPA-GMN-PAGE  -  1 )
                                       *   100
                                       +   1
           PERFORM UNTIL       (FLG-PTMEMOINF  =   1 )
                           OR  (SPA-GMN-NEXT   =   1 )
               ADD     1               TO  IDX-S2
               IF      IDX-S2          >=  IDX-S
                   ADD     1                   TO  IDX
                   IF      IDX                 >   100
                       MOVE    1               TO  SPA-GMN-NEXT
                       MOVE    1               TO  FLG-PTMEMOINF
                   ELSE
                       PERFORM 3101-PTMEMOINF-SPAHEN-SEC
                   END-IF
               END-IF
      *
               MOVE    "tbl_ptmemoinf"     TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 910-PTMEMOINF-READ-SEC
           END-PERFORM
           MOVE    "tbl_ptmemoinf"     TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       3101-PTMEMO-LIST-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科名称編集処理
      *****************************************************************
       3101-PTMEMOINF-SPAHEN-SEC        SECTION.
      *
           MOVE    IDX                 TO  SPA-TBL-MAX
           MOVE    IDX                 TO  SPA-GMN-TNUM (IDX)
           MOVE    PTMEMOINF-SYSYMD    TO  SPA-NAI-TSYSYMD (IDX)
           MOVE    PTMEMOINF-RENNUM    TO  SPA-NAI-TRENNUM (IDX)
           MOVE    PTMEMOINF-SRYKA     TO  SPA-NAI-TSRYKA  (IDX)
      ***  MOVE    PTMEMOINF-MEMO      TO  SPA-GMN-TMEMO   (IDX)
      *H30.11
      *    メモ内容チェック編集
           PERFORM 31011-MEMO-HEN-SEC
      *
           MOVE    PTMEMOINF-SYSYMD    TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-TSYSYMD (IDX)
      *
           MOVE    PTMEMOINF-SRYKA     TO  WRK-SRYKA
           PERFORM 31011-SRYKAMEI-HEN-SEC
           MOVE    WRK-SRYKA-MEI       TO  SPA-GMN-TSRYKA (IDX)
           .
       3101-PTMEMOINF-SPAHEN-EXT.
           EXIT.
      *H29.5
      *****************************************************************
      *    メモ内容チェック処理
      *****************************************************************
       31011-MEMO-HEN-SEC            SECTION.
      *
           INITIALIZE                  ORCXKANACONVAREA
           MOVE    200                 TO  KANACONV-LEN
           MOVE    1000                TO  KANACONV-STLEN
           MOVE    0                   TO  KANACONV-CONV-FLAG
           MOVE    0                   TO  KANACONV-CHAR-TYPE
           MOVE    PTMEMOINF-MEMO      TO  KANACONV-INDATA
           CALL    "kanaconv"          USING ORCXKANACONVAREA
           IF     (KANACONV-RETURN     =   ZERO )
               MOVE    KANACONV-OUTDATA
                                       TO  SPA-GMN-TMEMO   (IDX)
           ELSE
               MOVE    KANACONV-OUTDATA
                                       TO  SPA-GMN-TMEMO   (IDX)
           END-IF
           .
       31011-MEMO-HEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    診療科名称編集処理
      *****************************************************************
       31011-SRYKAMEI-HEN-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-SRYKA-MEI
           PERFORM VARYING     IDS     FROM    1   BY  1
                   UNTIL       IDS     >    SPA-SRYKA-MAX
               IF      SPA-GMN-SRYKAL (IDS)    =   WRK-SRYKA
                   MOVE    SPA-GMN-SRYKAMEIL (IDS)
                                           TO  WRK-SRYKA-MEI
                   MOVE    SPA-SRYKA-MAX   TO  IDS
               END-IF
           END-PERFORM
           IF      WRK-SRYKA-MEI       =   SPACE
               MOVE    WRK-SRYKA       TO  WRK-SRYKA-MEI
           END-IF
           .
       31011-SRYKAMEI-HEN-EXT.
      *
           EXIT.
      *****************************************************************
      *    患者コメント初期編集処理
      *****************************************************************
       3103-PTMEMO-HEN-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-INPUT
      *    メモ１
           MOVE    1                   TO  WRK-MEMOKBN
           PERFORM 31031-PTMEMOINF-HEN-SEC
           MOVE    WRK-MEMO            TO  SPA-GMN-MEMO01
           IF      WRK-SRYKA       NOT =   SPACE
               PERFORM 31011-SRYKAMEI-HEN-SEC
               MOVE    WRK-SRYKA-G         TO  SPA-GMN-SRYKA01-G
           ELSE
      *        当日登録なし
               MOVE    SPA-GMN-SRYKA-LIST (1)  TO  SPA-GMN-SRYKA01-G
           END-IF
           MOVE    WRK-RENNUM          TO  SPA-NAI-RENNUM01
      *    メモ２
           MOVE    2                   TO  WRK-MEMOKBN
           PERFORM 31031-PTMEMOINF-HEN-SEC
           MOVE    WRK-MEMO            TO  SPA-GMN-MEMO02
           IF      WRK-SRYKA       NOT =   SPACE
               PERFORM 31011-SRYKAMEI-HEN-SEC
               MOVE    WRK-SRYKA-G         TO  SPA-GMN-SRYKA02-G
           ELSE
      *        当日登録なし
               MOVE    SPA-GMN-SRYKA-LIST (1)  TO  SPA-GMN-SRYKA02-G
           END-IF
           MOVE    WRK-RENNUM          TO  SPA-NAI-RENNUM02
      *
           .
       3103-PTMEMO-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者メモ検索編集処理
      *****************************************************************
       31031-PTMEMOINF-HEN-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-MEMO
           MOVE    SPACE               TO  WRK-SRYKA-G
           MOVE    ZERO                TO  WRK-RENNUM
      *
           MOVE    SPACE               TO  PTMEMOINF-REC
           INITIALIZE                  PTMEMOINF-REC
           MOVE    SPA-HOSPNUM         TO  PTMEMOINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTMEMOINF-PTID
           MOVE    WRK-MEMOKBN         TO  PTMEMOINF-MEMOKBN
           MOVE    SPA-NAI-SYSYMD      TO  PTMEMOINF-SYSYMD
      *
           MOVE    PTMEMOINF-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptmemoinf"     TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptmemoinf"     TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-PTMEMOINF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTMEMOINF
               INITIALIZE                  PTMEMOINF-REC
           END-IF
           MOVE    "tbl_ptmemoinf"     TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-PTMEMOINF       =   ZERO
               MOVE    PTMEMOINF-RENNUM    TO  WRK-RENNUM
               MOVE    PTMEMOINF-SRYKA     TO  WRK-SRYKA
               MOVE    PTMEMOINF-MEMO      TO  WRK-MEMO
           END-IF
           .
       31031-PTMEMOINF-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科編集処理
      *****************************************************************
       3102-SRYKA-HEN-SEC        SECTION.
      *
           INITIALIZE                   SPA-GMN-SRYKA-LISTG
      *
      *    最初に全科指定を挿入
           ADD     1                   TO  SPA-SRYKA-MAX
           MOVE    "00"                TO  SPA-GMN-SRYKAL
                                                  (SPA-SRYKA-MAX)
           MOVE    "全科"              TO  SPA-GMN-SRYKAMEIL
                                                  (SPA-SRYKA-MAX)
      *    診療科
           MOVE    SPACE               TO  SYS-1005-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    SPA-NAI-SYSYMD      TO  SYS-1005-STYUKYMD
           MOVE    SYS-1005-STYUKYMD   TO  SYS-1005-EDYUKYMD
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
                       OR     (SPA-SRYKA-MAX   >=  41 )
               ADD     1                   TO  SPA-SRYKA-MAX
      *
               MOVE    MCPDATA-REC         TO  SYS-1005-REC
               MOVE    SYS-1005-KBNCD(1:2) TO  SPA-GMN-SRYKAL
                                                   (SPA-SRYKA-MAX)
               MOVE    SYS-1005-SRYKANAME1 TO  SPA-GMN-SRYKAMEIL
                                                   (SPA-SRYKA-MAX)
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
               MOVE    ZERO                TO  WRK-XF01-ROW
           ELSE
               MOVE    XF01-ROW            TO  WRK-XF01-ROW
           END-IF
      *
           MOVE    SPACE               TO  XF01
           INITIALIZE                      XF01
      *
      *    コラムリストの位置指定
           MOVE    ZERO                TO  XF01-ROW
           IF      WRK-XF01-ROW    NOT =   ZERO
               MOVE    WRK-XF01-ROW        TO  XF01-ROW
           END-IF
      *
      *    画面編集
           MOVE    SPA-GMN-PTNUM       TO  XF01-PTNUM
           MOVE    SPA-GMN-NAME        TO  XF01-NAME
           MOVE    SPA-GMN-SEX         TO  XF01-SEX
           MOVE    SPA-GMN-BIRTHDAY    TO  XF01-BIRTHDAY
           MOVE    SPA-GMN-NENREI      TO  XF01-NENREI
      *
           MOVE    SPA-GMN-SYSYMD      TO  XF01-SYSYMD
      *
           MOVE    SPA-GMN-MEMO01      TO  XF01-MEMO01
           MOVE    SPA-GMN-MEMO02      TO  XF01-MEMO02
      *
           IF      SPA-NAI-RENNUM01    =   ZERO
               MOVE    "（新規）"         TO  XF01-SYORI01-VALUE
               MOVE    "black"            TO  XF01-SYORI01-STYLE
           ELSE
               MOVE    "（更新）"         TO  XF01-SYORI01-VALUE
               MOVE    "blue"             TO  XF01-SYORI01-STYLE
           END-IF
           IF      SPA-NAI-RENNUM02   =   ZERO
               MOVE    "（新規）"         TO  XF01-SYORI02-VALUE
               MOVE    "black"            TO  XF01-SYORI02-STYLE
           ELSE
               MOVE    "（更新）"         TO  XF01-SYORI02-VALUE
               MOVE    "blue"             TO  XF01-SYORI02-STYLE
           END-IF
      *
           MOVE    SPA-GMN-UKEMEI      TO  XF01-UKETUKE-VALUE
               MOVE    "blue"          TO  XF01-UKETUKE-STYLE
      *    診療科１
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SRYKA-MAX
               MOVE    SPA-GMN-SRYKA-LIST (IDX)
                                           TO  XF01-SRYKA01-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-SRYKA-MAX      TO  XF01-SRYKA01-COUNT
           MOVE    SPA-GMN-SRYKA01-G  TO  XF01-SRYKA01
      *    診療科２
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SRYKA-MAX
               MOVE    SPA-GMN-SRYKA-LIST (IDX)
                                           TO  XF01-SRYKA02-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-SRYKA-MAX      TO  XF01-SRYKA02-COUNT
           MOVE    SPA-GMN-SRYKA02-G  TO  XF01-SRYKA02
      *
      *    一覧表
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-TBL-MAX
               MOVE    SPA-GMN-TNUM    (IDX)  TO  WRK-ZZZ
               MOVE    WRK-ZZZ                TO  XF01-TRENNUM (IDX)
               MOVE    SPA-GMN-TSYSYMD (IDX)  TO  XF01-TSYSYMD (IDX)
               MOVE    SPA-GMN-TSRYKA  (IDX)  TO  XF01-TSRYKA  (IDX)
               MOVE    SPA-GMN-TMEMO   (IDX)  TO  XF01-TMEMO   (IDX)
      *
               MOVE    "F"             TO  XF01-MEMOLIST-SELECT (IDX)
               IF      SPA-GMN-TNUM    (IDX)  =    SPA-GMN-SELNUM
                   MOVE    "T"         TO  XF01-MEMOLIST-SELECT (IDX)
                   IF      XF01-ROW        =   ZERO
                       MOVE    IDX             TO  XF01-ROW
                   END-IF
               END-IF
           END-PERFORM
           MOVE    SPA-TBL-MAX        TO  XF01-MEMOLIST-COUNT
           IF      SPA-NAI-MEMOKBN    =   1
               MOVE    "【　メ　モ　１　】"   TO  XF01-MIDASI-VALUE
               MOVE    "blue"                 TO  XF01-MIDASI-STYLE
           ELSE
               MOVE    "【　メ　モ　２　】"   TO  XF01-MIDASI-VALUE
               MOVE    "blue"                 TO  XF01-MIDASI-STYLE
           END-IF
      *
           MOVE    SPA-GMN-SELNUM     TO  XF01-SELNUM
      *
      *    非活性処理
           PERFORM 510-GSRAUTH-SEC
      *
           PERFORM 5001-MAPCUR-SEC
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    非活性処理
      *****************************************************************
       510-GSRAUTH-SEC             SECTION.
      *
           IF      SPA-CLAIM-FLG       =   1
               MOVE    WIDGET-NORMAL       TO  XF01-B12S-STATE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  XF01-B12S-STATE
           END-IF
           .
       51O-GSRAUTH-EXT.
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
                   MOVE    "SRYKA01"           TO  MCP-WIDGET
               WHEN    2
                   MOVE    "MEMO01"            TO  MCP-WIDGET
               WHEN    3
                   MOVE    "SRYKA02"           TO  MCP-WIDGET
               WHEN    4
                   MOVE    "MEMO02"            TO  MCP-WIDGET
               WHEN    5
                   MOVE    "SELNUM"            TO  MCP-WIDGET
               WHEN    99
                   MOVE    "B12"               TO  MCP-WIDGET
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
               WHEN    "SRYKA01"
                   MOVE    2           TO  SPA-GMN-CUR
               WHEN    "MEMO01"
                   MOVE    3           TO  SPA-GMN-CUR
               WHEN    "SRYKA02"
                   MOVE    4           TO  SPA-GMN-CUR
               WHEN    "MEMO02"
                   MOVE    99          TO  SPA-GMN-CUR
               WHEN    OTHER
                   MOVE    1           TO  SPA-GMN-CUR
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
                   PERFORM 220-CLREA-SEC
      *        削除
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM 240-DELETE-SEC
      *        一覧切替え
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM 240-CHENGE-SEC
      *        前日
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 250-BACK-DAY-SEC
      *        次日
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 280-NEXT-DAY-SEC
      *        前頁
               WHEN    "CLICKED"       ALSO    "B06S"
                   PERFORM 260-BACK-PAGE-SEC
      *        次頁
               WHEN    "CLICKED"       ALSO    "B07S"
                   PERFORM 270-NEXT-PAGE-SEC
      *        メモ１登録
               WHEN    "CLICKED"       ALSO    "B05"
                   PERFORM 260-MEMO1-TOROKU-SEC
      *        メモ２登録
               WHEN    "CLICKED"       ALSO    "B08"
                   PERFORM 270-MEMO2-TOROKU-SEC
      *        メモ１削除
               WHEN    "CLICKED"       ALSO    "B05S"
                   PERFORM 260-MEMO1-DEL-SEC
      *        メモ２削除
               WHEN    "CLICKED"       ALSO    "B08S"
                   PERFORM 260-MEMO2-DEL-SEC
      *        メモ１へ
               WHEN    "CLICKED"       ALSO    "B09"
                   PERFORM 290-MEMO01-SEC
      *        メモ複写
               WHEN    "CLICKED"       ALSO    "B11S"
                   PERFORM 2901-MEMOCOPY-SEC
      *        メモ２へ
               WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM 2100-MEMO02-SEC
      *        文例選択画面へ
               WHEN    "CLICKED"       ALSO    "B11"
                   PERFORM 2110-BUNREI-FX011-SEC
      *        登録
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 212-TOUROKU-SEC
      *        送信
               WHEN    "CLICKED"       ALSO    "B12S"
                   PERFORM 213-SOUSIN-SEC
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
               WHEN    "ACTIVATE"      ALSO    "SRYKA01"
                   PERFORM 400-SRYKA01-SEC
      *        診療科
               WHEN    "ACTIVATE"      ALSO    "SRYKA02"
                   PERFORM 400-SRYKA02-SEC
      *        選択番号
               WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 420-SELNUM-SEC
      *        明細選択
               WHEN    ANY             ALSO    "MEMOLIST"
                   PERFORM 440-MEMOLIST-SEC
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
      *    文例選択処理
      *****************************************************************
       2110-BUNREI-FX011-SEC             SECTION.
      *
      *    入力チェック処理
           PERFORM 4901-INPUT-CHK-SEC
           MOVE    1                   TO  SPA-GMN-CUR
      *
           MOVE    SPACE               TO  SPA-MOTOPG
      *
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-XF01            TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGXF011"         USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-XF01
      *
      *    カーソル移動
           MOVE    "SELNUM"            TO  MCP-WIDGET
      *
           MOVE    "XF01"              TO  SPA-MOTOPG
           MOVE    "XF011"             TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "XF011"             TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       2110-BUNREI-FX011-EXT.
           EXIT.
      *
      *****************************************************************
      *    メモ１確定処理
      *****************************************************************
       260-MEMO1-TOROKU-SEC            SECTION.
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *    入力チェック処理
           PERFORM 4901-INPUT-CHK-SEC
      *    診療科チェック
           IF      SPA-ERRCD           =   SPACE
               MOVE    1                   TO  FLG-TOROKU
               PERFORM 400-SRYKA01-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-NAI-SYSYMD  NOT =   SPA-SYSYMD
                   MOVE    "0011"              TO  SPA-ERRCD
               END-IF
           END-IF
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-GMN-MEMO01      =   SPACE
                   MOVE    "0010"              TO  SPA-ERRCD
                   MOVE    2                   TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF      SPA-ERRCD           =   SPACE
               MOVE    "0101"              TO  SPA-XFIDCD
           END-IF
      *
           .
       260-MEMO1-TOROKU-EXT.
           EXIT.
      *****************************************************************
      *    メモ１確定処理
      *****************************************************************
       260-MEMO1-DEL-SEC             SECTION.
      *
      *    入力チェック処理
           PERFORM 4901-INPUT-CHK-SEC
           MOVE    1                   TO  FLG-TOROKU
           PERFORM 400-SRYKA01-SEC
           MOVE    1                   TO  SPA-GMN-CUR
      *
           IF      SPA-NAI-RENNUM01    =   ZERO
               MOVE    "0012"              TO  SPA-ERRCD
           END-IF
           IF      SPA-ERRCD           =   SPACE
               MOVE    "0102"              TO  SPA-XFIDCD
           END-IF
      *
           .
       260-MEMO1-DEL-EXT.
           EXIT.
      *****************************************************************
      *    メモ１確定処理
      *****************************************************************
       260-MEMO2-DEL-SEC             SECTION.
      *
      *    入力チェック処理
           PERFORM 4901-INPUT-CHK-SEC
           MOVE    1                   TO  FLG-TOROKU
           PERFORM 400-SRYKA02-SEC
           MOVE    1                   TO  SPA-GMN-CUR
      *
           IF      SPA-NAI-RENNUM02    =   ZERO
               MOVE    "0012"              TO  SPA-ERRCD
           END-IF
           IF      SPA-ERRCD           =   SPACE
               MOVE    "0104"              TO  SPA-XFIDCD
           END-IF
      *
           .
       260-MEMO2-DEL-EXT.
           EXIT.
      *****************************************************************
      *    メモ２確定処理
      *****************************************************************
       270-MEMO2-TOROKU-SEC             SECTION.
      *
           MOVE    3                   TO  SPA-GMN-CUR
      *    入力チェック処理
           PERFORM 4901-INPUT-CHK-SEC
           IF      SPA-ERRCD           =   SPACE
               MOVE    1                   TO  FLG-TOROKU
               PERFORM 400-SRYKA02-SEC
           END-IF
      *
           IF      SPA-NAI-SYSYMD  NOT =   SPA-SYSYMD
               MOVE    "0011"              TO  SPA-ERRCD
           END-IF
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-GMN-MEMO02      =   SPACE
                   MOVE    "0010"              TO  SPA-ERRCD
                   MOVE    4                   TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    "0103"              TO  SPA-XFIDCD
           END-IF
      *
           .
       270-MEMO2-TOROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    メモ１確定処理
      *****************************************************************
       290-MEMO01-SEC                    SECTION.
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *    入力チェック処理
           PERFORM 4901-INPUT-CHK-SEC
           IF      SPA-ERRCD           =   SPACE
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
      *
           .
       290-MEMO01-EXT.
           EXIT.
      *****************************************************************
      *    メモ２確定処理
      *****************************************************************
       2100-MEMO02-SEC                    SECTION.
      *
           MOVE    3                   TO  SPA-GMN-CUR
      *    入力チェック処理
           PERFORM 4901-INPUT-CHK-SEC
           IF      SPA-ERRCD           =   SPACE
               MOVE    3                   TO  SPA-GMN-CUR
           END-IF
      *
           .
       2100-MEMO02-EXT.
           EXIT.
      *****************************************************************
      *    メモ複写処理
      *****************************************************************
       2901-MEMOCOPY-SEC                 SECTION.
      *
           IF      SPA-TBL-MAX         >   ZERO
               IF      SPA-NAI-MEMOKBN     =   1
      *            メモ１表示
                   MOVE    1                   TO  SPA-GMN-SELNUM
                   IF      SPA-GMN-MEMO01      =   SPACE
                       PERFORM 29011-MEMO-COPY-SEC
                   ELSE
                       MOVE    "0004"              TO  SPA-XFIDCD
                   END-IF
               ELSE
      *            メモ２表示
                   MOVE    1                   TO  SPA-GMN-SELNUM
                   IF      SPA-GMN-MEMO02      =   SPACE
                       PERFORM 29011-MEMO-COPY-SEC
                   ELSE
                       MOVE    "0004"              TO  SPA-XFIDCD
                   END-IF
               END-IF
           END-IF
      *
           .
       2901-MEMOCOPY-EXT.
           EXIT.
      *****************************************************************
      *    メモ複写処理
      *****************************************************************
       29011-MEMO-COPY-SEC                 SECTION.
      *
           MOVE    SPA-GMN-SELNUM      TO  IDX
      *
      *    当日なら内容編集・以外はメモ複写
           MOVE    SPACE               TO  PTMEMOINF-REC
           INITIALIZE                  PTMEMOINF-REC
           MOVE    SPA-HOSPNUM         TO  PTMEMOINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTMEMOINF-PTID
           MOVE    SPA-NAI-MEMOKBN     TO  PTMEMOINF-MEMOKBN
           MOVE    SPA-NAI-TSYSYMD (IDX)
                                       TO  PTMEMOINF-SYSYMD
           MOVE    SPA-NAI-TSRYKA (IDX)
                                       TO  PTMEMOINF-SRYKA
           MOVE    SPA-NAI-TRENNUM (IDX)
                                       TO  PTMEMOINF-RENNUM
      *
           MOVE    PTMEMOINF-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptmemoinf"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptmemoinf"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-PTMEMOINF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTMEMOINF
               INITIALIZE                  PTMEMOINF-REC
           END-IF
           MOVE    "tbl_ptmemoinf"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-PTMEMOINF       =   ZERO
      *        メントの置換
               PERFORM 420112-SELMEMO-CHG-SEC
           END-IF
           .
       29011-MEMO-COPYEXT.
           EXIT.
      *****************************************************************
      *    診療科処理
      *****************************************************************
       400-SRYKA01-SEC                 SECTION.
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           IF     (XF01-SRYKA01    NOT =   SPA-GMN-SRYKA01-G)
             AND  (SPA-NAI-MEMOKBN     =   1                )
               MOVE    ZERO                TO  SPA-GMN-SELNUM
           END-IF
      *
           MOVE    XF01-SRYKA01        TO  SPA-GMN-SRYKA01-G
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL     (IDX      >   SPA-SRYKA-MAX )
                          OR (FLG-OK   =   1    )
               IF      SPA-GMN-SRYKA01     =   SPA-GMN-SRYKAL (IDX)
                   MOVE    SPA-GMN-SRYKA-LIST (IDX)
                                           TO  SPA-GMN-SRYKA01-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0001"              TO  SPA-ERRCD
           ELSE
               MOVE    2                   TO  SPA-GMN-CUR
               MOVE    SPA-GMN-SRYKA01     TO  WRK-SRYKA
               MOVE    1                   TO  WRK-MEMOKBN
               PERFORM 4001-PTMEMO-HEN-SEC
      *        登録前に連番の再設定
               IF      FLG-TOROKU          =   1
                   IF      WRK-RENNUM          =   SPA-NAI-RENNUM01
                       CONTINUE
                   ELSE
                       MOVE    WRK-RENNUM      TO  SPA-NAI-RENNUM01
                   END-IF
               ELSE
                   IF      WRK-RENNUM          =   ZERO
                       MOVE    ZERO                TO  SPA-NAI-RENNUM01
                       MOVE    SPACE               TO  SPA-GMN-MEMO01
                   ELSE
                       MOVE    WRK-MEMO            TO  SPA-GMN-MEMO01
                       MOVE    WRK-RENNUM          TO  SPA-NAI-RENNUM01
                   END-IF
               END-IF
           END-IF
      *
           .
       400-SRYKA01-EXT.
           EXIT.
      *****************************************************************
      *    患者メモ検索編集処理
      *****************************************************************
       4001-PTMEMO-HEN-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-MEMO
           MOVE    ZERO                TO  WRK-RENNUM
      *
           MOVE    SPACE               TO  PTMEMOINF-REC
           INITIALIZE                  PTMEMOINF-REC
           MOVE    SPA-HOSPNUM         TO  PTMEMOINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTMEMOINF-PTID
           MOVE    WRK-MEMOKBN         TO  PTMEMOINF-MEMOKBN
           MOVE    WRK-SRYKA           TO  PTMEMOINF-SRYKA
           MOVE    SPA-NAI-SYSYMD      TO  PTMEMOINF-SYSYMD
      *
           MOVE    PTMEMOINF-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptmemoinf"     TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptmemoinf"     TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 910-PTMEMOINF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTMEMOINF
               INITIALIZE                  PTMEMOINF-REC
           END-IF
           MOVE    "tbl_ptmemoinf"     TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-PTMEMOINF       =   ZERO
               MOVE    PTMEMOINF-RENNUM    TO  WRK-RENNUM
               MOVE    PTMEMOINF-MEMO      TO  WRK-MEMO
           END-IF
           .
       4001-PTMEMO-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療科０２処理
      *****************************************************************
       400-SRYKA02-SEC                 SECTION.
      *
           MOVE    3                   TO  SPA-GMN-CUR
      *
           IF     (XF01-SRYKA02    NOT =   SPA-GMN-SRYKA02-G)
             AND  (SPA-NAI-MEMOKBN     =   2                )
               MOVE    ZERO                TO  SPA-GMN-SELNUM
           END-IF
      *
           MOVE    XF01-SRYKA02        TO  SPA-GMN-SRYKA02-G
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL     (IDX      >   SPA-SRYKA-MAX )
                          OR (FLG-OK   =   1    )
               IF      SPA-GMN-SRYKA02     =   SPA-GMN-SRYKAL (IDX)
                   MOVE    SPA-GMN-SRYKA-LIST (IDX)
                                           TO  SPA-GMN-SRYKA02-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0003"              TO  SPA-ERRCD
           ELSE
               MOVE    4                   TO  SPA-GMN-CUR
               MOVE    SPA-GMN-SRYKA02     TO  WRK-SRYKA
               MOVE    2                   TO  WRK-MEMOKBN
               PERFORM 4001-PTMEMO-HEN-SEC
      *        登録前に連番の再設定
               IF      FLG-TOROKU          =   1
                   IF      WRK-RENNUM          =   SPA-NAI-RENNUM02
                       CONTINUE
                   ELSE
                       MOVE    WRK-RENNUM      TO  SPA-NAI-RENNUM02
                   END-IF
               ELSE
                   IF      WRK-RENNUM          =   ZERO
                       MOVE    ZERO                TO  SPA-NAI-RENNUM02
                       MOVE    SPACE               TO  SPA-GMN-MEMO02
                   ELSE
                       MOVE    WRK-MEMO            TO  SPA-GMN-MEMO02
                       MOVE    WRK-RENNUM          TO  SPA-NAI-RENNUM02
                   END-IF
               END-IF
           END-IF
      *
           .
       400-SRYKA02-EXT.
           EXIT.
      *****************************************************************
      *    選択番号処理
      *****************************************************************
       420-SELNUM-SEC                 SECTION.
      *
           MOVE    5                   TO  SPA-GMN-CUR
      *
           MOVE    XF01-SELNUM         TO  SPA-GMN-SELNUM
      *
           IF      SPA-GMN-SELNUM      >   SPA-TBL-MAX
               MOVE    "0005"              TO  SPA-ERRCD
           ELSE
               IF      SPA-GMN-SELNUM      >   ZERO
                   PERFORM 4201-HENKOU-SYORI-SEC
               END-IF
           END-IF
           .
       420-SELNUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号処理
      *****************************************************************
       4201-HENKOU-SYORI-SEC                 SECTION.
      *
           MOVE    SPA-GMN-SELNUM      TO  IDX
      *
           IF      SPA-SYSYMD          =   SPA-NAI-SYSYMD
               IF      SPA-NAI-TSYSYMD (IDX)   =   SPA-SYSYMD
                   MOVE    "0003"              TO  SPA-XFIDCD
               ELSE
                   MOVE    "0004"              TO  SPA-XFIDCD
               END-IF
           ELSE
               MOVE    "0004"              TO  SPA-XFIDCD
           END-IF
           .
       4201-HENKOU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号処理
      *****************************************************************
       42011-SELMEMO-SYORI-SEC            SECTION.
      *
           MOVE    SPA-GMN-SELNUM      TO  IDX
      *
      *    当日なら内容編集・以外はメモ複写
           MOVE    SPACE               TO  PTMEMOINF-REC
           INITIALIZE                  PTMEMOINF-REC
           MOVE    SPA-HOSPNUM         TO  PTMEMOINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTMEMOINF-PTID
           MOVE    SPA-NAI-MEMOKBN     TO  PTMEMOINF-MEMOKBN
           MOVE    SPA-NAI-TSYSYMD (IDX)
                                       TO  PTMEMOINF-SYSYMD
           MOVE    SPA-NAI-TSRYKA (IDX)
                                       TO  PTMEMOINF-SRYKA
           MOVE    SPA-NAI-TRENNUM (IDX)
                                       TO  PTMEMOINF-RENNUM
      *
           MOVE    PTMEMOINF-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptmemoinf"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptmemoinf"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-PTMEMOINF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTMEMOINF
               INITIALIZE                  PTMEMOINF-REC
           END-IF
           MOVE    "tbl_ptmemoinf"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      SPA-XFIDCD          =   "0003"
      *****IF      SPA-NAI-SYSYMD      =   SPA-NAI-TSYSYMD (IDX)
               IF      SPA-XFID1-FLG      =   "OK"
      *            当日コメントの更新
                   PERFORM 420111-SELMEMO-UPD-SEC
               ELSE
      *            コメントの置換
                   PERFORM 420112-SELMEMO-CHG-SEC
               END-IF
           ELSE
               IF      SPA-XFID1-FLG      =   "OK"
      *            メントの置換
                   PERFORM 420112-SELMEMO-CHG-SEC
               ELSE
      *            メントの追加
                   PERFORM 420113-SELMEMO-ADD-SEC
               END-IF
           END-IF
           MOVE    SPACE               TO  SPA-XFIDCD
           MOVE    SPACE               TO  SPA-XFID1-FLG
      *
           .
       4201-HENKOU-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    当日コメントの更新処理
      *****************************************************************
       420111-SELMEMO-UPD-SEC                 SECTION.
      *
           MOVE    PTMEMOINF-SRYKA     TO  WRK-SRYKA
           PERFORM 31011-SRYKAMEI-HEN-SEC
           IF      SPA-NAI-MEMOKBN     =   1
               MOVE    PTMEMOINF-SRYKA     TO  SPA-GMN-SRYKA01
               MOVE    WRK-SRYKA-G         TO  SPA-GMN-SRYKA01-G
               MOVE    PTMEMOINF-RENNUM    TO  SPA-NAI-RENNUM01
               MOVE    PTMEMOINF-MEMO      TO  SPA-GMN-MEMO01
               MOVE    2                   TO  SPA-GMN-CUR
           ELSE
               MOVE    PTMEMOINF-SRYKA     TO  SPA-GMN-SRYKA02
               MOVE    WRK-SRYKA-G         TO  SPA-GMN-SRYKA02-G
               MOVE    PTMEMOINF-RENNUM    TO  SPA-NAI-RENNUM02
               MOVE    PTMEMOINF-MEMO      TO  SPA-GMN-MEMO02
               MOVE    4                   TO  SPA-GMN-CUR
           END-IF
      *
           .
       420111-SELMEMO-UPD-EXT.
           EXIT.
      *****************************************************************
      *    コメントの置換処理
      *****************************************************************
       420112-SELMEMO-CHG-SEC                 SECTION.
      *
           IF      SPA-NAI-MEMOKBN     =   1
               MOVE    PTMEMOINF-MEMO      TO  SPA-GMN-MEMO01
               MOVE    2                   TO  SPA-GMN-CUR
           ELSE
               MOVE    PTMEMOINF-MEMO      TO  SPA-GMN-MEMO02
               MOVE    4                   TO  SPA-GMN-CUR
           END-IF
           MOVE    ZERO                TO  SPA-GMN-SELNUM
      *
           .
       420112-SELMEMO-CHG-EXT.
           EXIT.
      *****************************************************************
      *    コメントの追加処理
      *****************************************************************
       420113-SELMEMO-ADD-SEC                 SECTION.
      *
           IF      SPA-NAI-MEMOKBN     =   1
               MOVE    SPA-GMN-MEMO01      TO  WRK-MEMO
           ELSE
               MOVE    SPA-GMN-MEMO02      TO  WRK-MEMO
           END-IF
      *    コメント追加
           MOVE    SPACE               TO  WRK-MEMO-WK
           MOVE    ZERO                TO  IDX-M2
           PERFORM VARYING     IDX-M   FROM    1000    BY  -1
                   UNTIL      (IDX-M   <   1   )
               IF      WRK-MEMO(IDX-M:1)   NOT =   SPACE
                   MOVE    WRK-MEMO        TO  WRK-MEMO-WK
                   COMPUTE IDX-M2          =   IDX-M  +  1
                   MOVE    PTMEMOINF-MEMO  TO  WRK-MEMO-WK(IDX-M2:)
                   MOVE    1               TO  IDX-M
               END-IF
           END-PERFORM
           IF      WRK-MEMO-WK         =   SPACE
               MOVE    PTMEMOINF-MEMO  TO  WRK-MEMO-WK
           END-IF
      *
           INITIALIZE                  ORCXKANACONVAREA
           MOVE    1000                TO  KANACONV-LEN
           MOVE    1000                TO  KANACONV-STLEN
           MOVE    0                   TO  KANACONV-CONV-FLAG
           MOVE    0                   TO  KANACONV-CHAR-TYPE
           MOVE    WRK-MEMO-WK         TO  KANACONV-INDATA
           CALL    "kanaconv"          USING ORCXKANACONVAREA
           MOVE    KANACONV-OUTDATA    TO  WRK-MEMO-WK
           IF      SPA-NAI-MEMOKBN     =   1
               MOVE    WRK-MEMO-WK         TO  SPA-GMN-MEMO01
               MOVE    2                   TO  SPA-GMN-CUR
           ELSE
               MOVE    WRK-MEMO-WK         TO  SPA-GMN-MEMO02
               MOVE    4                   TO  SPA-GMN-CUR
           END-IF
           MOVE    ZERO                TO  SPA-GMN-SELNUM
      *
           .
       420113-SELMEMO-ADD-EXT.
           EXIT.
      *****************************************************************
      *    明細選択処理
      *****************************************************************
       440-MEMOLIST-SEC                 SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >   SPA-TBL-MAX
               IF      XF01-MEMOLIST-SELECT (IDX)  =  "T"
                   MOVE    SPA-GMN-TNUM  (IDX)
                                               TO  WRK-SELNUM
              END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM      NOT =   ZERO
               MOVE    WRK-SELNUM          TO  XF01-SELNUM
               PERFORM 420-SELNUM-SEC
           END-IF
      *
           .
       440-MEMOLIST-EXT.
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
           MOVE    XF01-MEMO01         TO  SPA-GMN-MEMO01
           MOVE    XF01-MEMO02         TO  SPA-GMN-MEMO02
      *
      *    メモ０１
           PERFORM 49011-MEMO01-CHK-SEC
      *    メモ０２
           IF      SPA-ERRCD           =   SPACE
               PERFORM 49012-MEMO02-CHK-SEC
           END-IF
      *
           .
       4901-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    メモ０１チェック処理
      *****************************************************************
       49011-MEMO01-CHK-SEC                   SECTION.
      *
           IF      SPA-GMN-MEMO01      =   SPACE
               CONTINUE
           ELSE
               MOVE    SPA-GMN-MEMO01      TO  WRK-MEMO
               PERFORM 800-MEMO-CHK-SEC
               MOVE    WRK-MEMO            TO  SPA-GMN-MEMO01
           END-IF
      *
           .
       49011-MEMO01-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    メモ０２チェック処理
      *****************************************************************
       49012-MEMO02-CHK-SEC                   SECTION.
      *
           IF      SPA-GMN-MEMO02      =   SPACE
               CONTINUE
           ELSE
               MOVE    SPA-GMN-MEMO02      TO  WRK-MEMO
               PERFORM 800-MEMO-CHK-SEC
      *        ＣＬＡＩＭ送信不可文字チェック
               MOVE    ZERO                TO  CTR-1
               MOVE    ZERO                TO  CTR-2
               INSPECT WRK-MEMO    TALLYING
                       CTR-1       FOR   ALL  "<" 
                       CTR-2       FOR   ALL  ">" 
               IF     (CTR-1           >   ZERO )  OR
                      (CTR-2           >   ZERO )
                   MOVE    "0015"          TO  SPA-ERRCD
                   MOVE    4               TO  SPA-GMN-CUR
               END-IF
      *
               MOVE    WRK-MEMO            TO  SPA-GMN-MEMO02
           END-IF
      *
           .
       49012-MEMO02-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    メモ内容チェック処理
      *****************************************************************
       800-MEMO-CHK-SEC             SECTION.
      *
      *    全角変換
      *    INITIALIZE                  ORCSKANACHKAREA
      *    MOVE    "2"                 TO  KANACHK-SYORI
      *    出力桁数
      *    MOVE    800                 TO  KANACHK-MAX-CNT
      *    MOVE    WRK-MEMO            TO  KANACHK-MAE-INPUT
      *    CALL    "ORCSKANACHK"       USING
      *                                ORCSKANACHKAREA
      *    MOVE    KANACHK-MAE-INPUT   TO  WRK-MEMO
      *    IF     (KANACHK-RC      NOT =   ZERO  )
      *        CONTINUE
      *    ELSE
      *        MOVE    KANACHK-OUT-INPUT   TO  WRK-MEMO
      *    END-IF
      *
           INITIALIZE                  ORCXKANACONVAREA
           MOVE    1000                TO  KANACONV-LEN
           MOVE    1000                TO  KANACONV-STLEN
           MOVE    0                   TO  KANACONV-CONV-FLAG
           MOVE    0                   TO  KANACONV-CHAR-TYPE
           MOVE    WRK-MEMO            TO  KANACONV-INDATA
           CALL    "kanaconv"          USING ORCXKANACONVAREA
           MOVE    KANACONV-OUTDATA    TO  WRK-MEMO
      *
           .
       800-MEMO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア　処理
      *****************************************************************
       220-CLREA-SEC                   SECTION.
      *
           IF      SPA-GMN-SELNUM      =   ZERO
               MOVE    SPA-SYSYMD          TO  SPA-NAI-SYSYMD
               PERFORM 310-SPA-SET-SEC
               MOVE    1                   TO  SPA-GMN-CUR
           ELSE
               MOVE    ZERO                TO  SPA-GMN-SELNUM
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
           .
       220-CLREA-EXT.
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
           MOVE    1                   TO  FLG-TOROKU
           IF      SPA-ERRCD           =   SPACE
               PERFORM 400-SRYKA02-SEC
           END-IF
           IF      SPA-ERRCD           =   SPACE
               PERFORM 400-SRYKA02-SEC
           END-IF
      *
           IF      SPA-NAI-SYSYMD  NOT =   SPA-SYSYMD
               MOVE    "0011"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-CUR
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF     (SPA-GMN-MEMO01      =   SPACE )   AND
                      (SPA-GMN-MEMO02      =   SPACE )
                   MOVE    "0010"              TO  SPA-ERRCD
                   MOVE    2                   TO  SPA-GMN-CUR
               ELSE
                   IF      SPA-CLAIM-FLG       =   1
                       MOVE    "0001"              TO  SPA-XFIDCD2
                   ELSE
                       MOVE    "0001"              TO  SPA-XFIDCD
                   END-IF
               END-IF
           END-IF
      *
           .
       212-TOUROKU-EXT.
           EXIT.
      *
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
      *    メモ１
           IF      SPA-GMN-MEMO01  NOT =   SPACE
               MOVE    1                   TO  WRK-MEMOKBN
               MOVE    SPA-GMN-MEMO01      TO  WRK-MEMO
               MOVE    SPA-GMN-SRYKA01     TO  WRK-SRYKA
               MOVE    SPA-NAI-RENNUM01    TO  WRK-RENNUM
               PERFORM 21121-PTMEMOINF-SYORI-SEC
           END-IF
      *    メモ２
           IF      SPA-GMN-MEMO02  NOT =   SPACE
               MOVE    2                   TO  WRK-MEMOKBN
               MOVE    SPA-GMN-MEMO02      TO  WRK-MEMO
               MOVE    SPA-GMN-SRYKA02     TO  WRK-SRYKA
               MOVE    SPA-NAI-RENNUM02    TO  WRK-RENNUM
               PERFORM 21121-PTMEMOINF-SYORI-SEC
      *H30.11
      *        PUSH通信処理
               IF      SPA-ERRCD           =   SPACE
                   PERFORM 600-PUSH-SYORI-SEC
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        患者コメント一覧編集
               MOVE    1                   TO  SPA-GMN-PAGE
               MOVE    2                   TO  SPA-NAI-MEMOKBN
               PERFORM 3101-PTMEMO-LIST-SEC
      *        患者コメント初期編集
               PERFORM 3103-PTMEMO-HEN-SEC
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
      *
           .
       2121-TOUROKU-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    患者メモ登録処理
      *****************************************************************
       21121-PTMEMOINF-SYORI-SEC           SECTION.
      *
           MOVE    SPACE               TO  PTMEMOINF-REC
           INITIALIZE                  PTMEMOINF-REC
           MOVE    SPA-HOSPNUM         TO  PTMEMOINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTMEMOINF-PTID
           MOVE    WRK-MEMOKBN         TO  PTMEMOINF-MEMOKBN
           MOVE    SPA-NAI-SYSYMD      TO  PTMEMOINF-SYSYMD
           MOVE    WRK-SRYKA           TO  PTMEMOINF-SRYKA
           MOVE    WRK-RENNUM          TO  PTMEMOINF-RENNUM
           IF      WRK-RENNUM          =   ZERO
               MOVE    "key2"              TO  WRK-PATHNAME
           ELSE
               MOVE    "key"               TO  WRK-PATHNAME
           END-IF
      *
           MOVE    PTMEMOINF-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptmemoinf"     TO  MCP-TABLE
           MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptmemoinf"     TO  MCP-TABLE
               MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
               PERFORM 910-PTMEMOINF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTMEMOINF
               INITIALIZE                  PTMEMOINF-REC
           END-IF
           MOVE    "tbl_ptmemoinf"     TO  MCP-TABLE
           MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    新規登録
           IF      WRK-RENNUM          =   ZERO
               IF      FLG-PTMEMOINF       =   ZERO
                   MOVE    PTMEMOINF-RENNUM    TO  WRK-RENNUM
               END-IF
               ADD     1                   TO  WRK-RENNUM
               PERFORM 21121-PTMEMOINF-INS-SEC
           ELSE
               IF      FLG-PTMEMOINF       =   ZERO
                   PERFORM 21121-PTMEMOINF-UPD-SEC
               ELSE
                   PERFORM 21121-PTMEMOINF-INS-SEC
               END-IF
           END-IF
      *
           .
       2121-TOUROKU-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    患者メモ登録処理
      *****************************************************************
       21121-PTMEMOINF-INS-SEC           SECTION.
      *
           MOVE    SPACE               TO  PTMEMOINF-REC
           INITIALIZE                  PTMEMOINF-REC
           MOVE    SPA-HOSPNUM         TO  PTMEMOINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTMEMOINF-PTID
           MOVE    WRK-MEMOKBN         TO  PTMEMOINF-MEMOKBN
           MOVE    SPA-NAI-SYSYMD      TO  PTMEMOINF-SYSYMD
           MOVE    WRK-SRYKA           TO  PTMEMOINF-SRYKA
           MOVE    WRK-RENNUM          TO  PTMEMOINF-RENNUM
           MOVE    WRK-MEMO            TO  PTMEMOINF-MEMO
      *
           MOVE    SMCNDATE-YMD        TO  PTMEMOINF-CREYMD
           MOVE    SMCNDATE-YMD        TO  PTMEMOINF-UPYMD
           MOVE    SMCNDATE-HMS        TO  PTMEMOINF-UPHMS
           MOVE    SPA-OPID            TO  PTMEMOINF-OPID
      *
           MOVE    PTMEMOINF-REC       TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_ptmemoinf"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "XF01 PTMEMOINF INS ERR:" MCP-RC
                           ",KEY:"   PTMEMOINF-KEY    "."
               MOVE    "0020"              TO  SPA-ERRCD
           END-IF
      *
      *H30.11
           MOVE    1                   TO  FLG-SYORIKBN
      *
           .
       21121-PTMEMOINF-INS-EXT.
           EXIT.
      *****************************************************************
      *    患者メモ更新処理
      *****************************************************************
       21121-PTMEMOINF-UPD-SEC           SECTION.
      *
      *    更新
           MOVE    WRK-MEMO            TO  PTMEMOINF-MEMO
      *
           MOVE    SMCNDATE-YMD        TO  PTMEMOINF-UPYMD
           MOVE    SMCNDATE-HMS        TO  PTMEMOINF-UPHMS
           MOVE    SPA-OPID            TO  PTMEMOINF-OPID
      *
           MOVE    PTMEMOINF-REC       TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_ptmemoinf"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "XF01 PTMEMOINF UPD ERR:" MCP-RC
                           ",KEY:"   PTMEMOINF-KEY    "."
               MOVE    "0020"              TO  SPA-ERRCD
           END-IF
      *
      *H30.11
           MOVE    2                   TO  FLG-SYORIKBN
           .
       21121-PTMEMOINF-UPD-EXT.
           EXIT.
      *****************************************************************
      *    コメント１登録処理
      *****************************************************************
       21201-MEMO01-ADD-SEC            SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *    メモ１
           MOVE    1                   TO  WRK-MEMOKBN
           MOVE    SPA-GMN-MEMO01      TO  WRK-MEMO
           MOVE    SPA-GMN-SRYKA01     TO  WRK-SRYKA
           MOVE    SPA-NAI-RENNUM01    TO  WRK-RENNUM
           PERFORM 21121-PTMEMOINF-SYORI-SEC
      *
      *    再表示
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-NAI-MEMOKBN     =   1
                   MOVE    1                   TO  SPA-GMN-PAGE
                   PERFORM 3101-PTMEMO-LIST-SEC
               END-IF
      *        患者コメント初期編集
               PERFORM 3103-PTMEMO-HEN-SEC
           END-IF
           .
       21201-MEMO01-ADD-EXT.
           EXIT.
      *****************************************************************
      *    コメント１登録処理
      *****************************************************************
       21201-MEMO01-DEL-SEC            SECTION.
      *
           MOVE    1                   TO  WRK-MEMOKBN
           MOVE    SPA-GMN-MEMO01      TO  WRK-MEMO
           MOVE    SPA-GMN-SRYKA01     TO  WRK-SRYKA
           MOVE    SPA-NAI-RENNUM01    TO  WRK-RENNUM
           PERFORM 21201-PTMEMOINF-DEL-SEC
      *    再表示
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-NAI-MEMOKBN     =   1
                   MOVE    1                   TO  SPA-GMN-PAGE
                   PERFORM 3101-PTMEMO-LIST-SEC
               END-IF
      *        患者コメント初期編集
               PERFORM 3103-PTMEMO-HEN-SEC
           END-IF
      *
           .
       21201-MEMO01-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメント削除処理
      *****************************************************************
       21201-PTMEMOINF-DEL-SEC                   SECTION.
      *
           MOVE    SPACE               TO  PTMEMOINF-REC
           INITIALIZE                  PTMEMOINF-REC
           MOVE    SPA-HOSPNUM         TO  PTMEMOINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTMEMOINF-PTID
           MOVE    WRK-MEMOKBN         TO  PTMEMOINF-MEMOKBN
           MOVE    SPA-NAI-SYSYMD      TO  PTMEMOINF-SYSYMD
           MOVE    WRK-SRYKA           TO  PTMEMOINF-SRYKA
           MOVE    WRK-RENNUM          TO  PTMEMOINF-RENNUM
      *
           MOVE    PTMEMOINF-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptmemoinf"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptmemoinf"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-PTMEMOINF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTMEMOINF
               INITIALIZE                  PTMEMOINF-REC
           END-IF
           MOVE    "tbl_ptmemoinf"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-PTMEMOINF       =   ZERO
               MOVE    PTMEMOINF-REC       TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_ptmemoinf"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                    DISPLAY "XF01 PTMEMOINF DEL ERR:" MCP-RC 
                               ",KEY:"   PTMEMOINF-KEY    "."
                   MOVE    "0021"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
      *H30.11
           MOVE    3                   TO  FLG-SYORIKBN
      *
           .
       21201-PTMEMOINF-DEL-EXT.
           EXIT.
      *****************************************************************
      *    コメント２登録処理
      *****************************************************************
       21201-MEMO02-ADD-SEC            SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *    メモ２
           MOVE    2                   TO  WRK-MEMOKBN
           MOVE    SPA-GMN-MEMO02      TO  WRK-MEMO
           MOVE    SPA-GMN-SRYKA02     TO  WRK-SRYKA
           MOVE    SPA-NAI-RENNUM02    TO  WRK-RENNUM
           PERFORM 21121-PTMEMOINF-SYORI-SEC
      *H30.11
      *PUSH通信処理
           IF      SPA-ERRCD           =   SPACE
               PERFORM 600-PUSH-SYORI-SEC
           END-IF
      *
      *    再表示
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-NAI-MEMOKBN     =   2
                   MOVE    1                   TO  SPA-GMN-PAGE
                   PERFORM 3101-PTMEMO-LIST-SEC
               END-IF
      *        患者コメント初期編集
               PERFORM 3103-PTMEMO-HEN-SEC
           END-IF
           .
       21201-MEMO02-ADD-EXT.
           EXIT.
      *****************************************************************
      *    コメント２登録処理
      *****************************************************************
       21201-MEMO02-DEL-SEC            SECTION.
      *
           MOVE    2                   TO  WRK-MEMOKBN
           MOVE    SPA-GMN-MEMO02      TO  WRK-MEMO
           MOVE    SPA-GMN-SRYKA02     TO  WRK-SRYKA
           MOVE    SPA-NAI-RENNUM02    TO  WRK-RENNUM
           PERFORM 21201-PTMEMOINF-DEL-SEC
      *H30.11
      *PUSH通信処理
      *    当日メモ２の削除のみ
           IF     (SPA-ERRCD           =   SPACE     )
              AND (SPA-NAI-SYSYMD      =   SPA-SYSYMD )
               PERFORM 600-PUSH-SYORI-SEC
           END-IF
      *
      *    再表示
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-NAI-MEMOKBN     =   2
                   MOVE    1                   TO  SPA-GMN-PAGE
                   PERFORM 3101-PTMEMO-LIST-SEC
               END-IF
      *        患者コメント初期編集
               PERFORM 3103-PTMEMO-HEN-SEC
           END-IF
           .
       21201-MEMO02-DEL-EXT.
           EXIT.
      *****************************************************************
      *    一覧切替え　処理
      *****************************************************************
       240-CHENGE-SEC                   SECTION.
      *
           IF      SPA-NAI-MEMOKBN     =   1
               MOVE    2               TO  SPA-NAI-MEMOKBN
           ELSE
               MOVE    1               TO  SPA-NAI-MEMOKBN
           END-IF
           MOVE    1                   TO  SPA-GMN-PAGE
           PERFORM 3101-PTMEMO-LIST-SEC
      *
           .
       240-CHENGE-EXT.
           EXIT.
      *****************************************************************
      *    コメント削除　処理
      *****************************************************************
       240-DELETE-SEC                   SECTION.
      *
           IF     (SPA-NAI-RENNUM01    =   ZERO )  AND
                  (SPA-NAI-RENNUM02    =   ZERO )
               MOVE    "0013"              TO  SPA-ERRCD
           ELSE
               MOVE    "0002"              TO  SPA-XFIDCD
           END-IF
           .
       240-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメント１日分削除処理
      *****************************************************************
       2401-MEMO-DELSYORI-SEC          SECTION.
      *
           PERFORM 24011-PTMEMOINF-ALLDEL-SEC
      *
      *    再表示
           IF      SPA-ERRCD           =   SPACE
               PERFORM 310-SPA-SET-SEC
           END-IF
           .
       2401-MEMO-DELSYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメント削除処理
      *****************************************************************
       24011-PTMEMOINF-ALLDEL-SEC                   SECTION.
      *
           MOVE    SPACE               TO  PTMEMOINF-REC
           INITIALIZE                  PTMEMOINF-REC
           MOVE    SPA-HOSPNUM         TO  PTMEMOINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTMEMOINF-PTID
           MOVE    SPA-NAI-SYSYMD      TO  PTMEMOINF-SYSYMD
      *
           MOVE    PTMEMOINF-REC       TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_ptmemoinf"     TO  MCP-TABLE
           MOVE    "del2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
                DISPLAY "XF01 PTMEMOINF DEL ERR:" MCP-RC 
                           ",KEY:"   PTMEMOINF-KEY    "."
               MOVE    "0021"          TO  SPA-ERRCD
           END-IF
      *
           .
       24011-PTMEMOINF-ALLDEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    前日分処理
      *****************************************************************
       250-BACK-DAY-SEC               SECTION.
      *
           INITIALIZE                     STS-AREA-DAY
           INITIALIZE                     LNK-DAY6-AREA
           MOVE    "61"                   TO  LNK-DAY6-IRAI
           MOVE    SPA-NAI-SYSYMD         TO  LNK-DAY6-KIJUN
           MOVE    "1"                    TO  LNK-DAY6-ZOGENPTN
           MOVE    -1                     TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"              USING   STS-AREA-DAY
                                                  LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN        TO  SPA-NAI-SYSYMD
      *
           PERFORM 310-SPA-SET-SEC
           .
      *
       250-BACK-DAY-EXT.
           EXIT.
      *
      *****************************************************************
      *    次日分処理
      *****************************************************************
       280-NEXT-DAY-SEC               SECTION.
      *
           INITIALIZE                     STS-AREA-DAY
           INITIALIZE                     LNK-DAY6-AREA
           MOVE    "61"                   TO  LNK-DAY6-IRAI
           MOVE    SPA-NAI-SYSYMD         TO  LNK-DAY6-KIJUN
           MOVE    "1"                    TO  LNK-DAY6-ZOGENPTN
           MOVE    1                      TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"              USING   STS-AREA-DAY
                                                  LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN        TO  SPA-NAI-SYSYMD
      *
           PERFORM 310-SPA-SET-SEC
           .
      *
       280-NEXT-DAY-EXT.
           EXIT.
      *
      *****************************************************************
      *    前頁処理
      *****************************************************************
       260-BACK-PAGE-SEC               SECTION.
      *
           IF      SPA-GMN-PAGE       >   1
               COMPUTE SPA-GMN-PAGE       =   SPA-GMN-PAGE
                                           -   1
               PERFORM 3101-PTMEMO-LIST-SEC
            END-IF
           .
      *
       260-BACK-PAGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    次頁処理
      *****************************************************************
       270-NEXT-PAGE-SEC               SECTION.
      *
           IF      SPA-GMN-NEXT       =   1
               COMPUTE SPA-GMN-PAGE       =   SPA-GMN-PAGE
                                          +   1
               PERFORM 3101-PTMEMO-LIST-SEC
            END-IF
           .
      *
       270-NEXT-PAGE-EXT.
           EXIT.
      *****************************************************************
      *    送信　処理
      *****************************************************************
       213-SOUSIN-SEC          SECTION.
      *
      *    当日のメモ２が登録されていること
           MOVE    SPACE               TO  PTMEMOINF-REC
           INITIALIZE                  PTMEMOINF-REC
           MOVE    SPA-HOSPNUM         TO  PTMEMOINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTMEMOINF-PTID
           MOVE    2                   TO  PTMEMOINF-MEMOKBN
           MOVE    SPA-NAI-SYSYMD      TO  PTMEMOINF-SYSYMD
      *
           MOVE    PTMEMOINF-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptmemoinf"     TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptmemoinf"     TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-PTMEMOINF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTMEMOINF
               INITIALIZE                  PTMEMOINF-REC
           END-IF
           MOVE    "tbl_ptmemoinf"     TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *H28.6
      *    ginbee対応
           IF     (SPA-MW              =   SPA-GINBEE )
              AND (FLG-PTMEMOINF       =   ZERO       )
      *        AIP CLAIM送信処理
               PERFORM 49061-API-CLMAI-SEC
               GO  TO  213-SOUSIN-EXT
           END-IF
      *
           IF      FLG-PTMEMOINF       =   ZERO
      *        ＣＬＡＩＭ接続チェック処理
               PERFORM 2131-CLAIM-CHK-SEC
               IF      FLG-CLAIM       =   1
      *            送信
                   PERFORM 4906-CLAIM-SOUSIN-SEC
                   PERFORM 49061-API-CLMAI-SEC
               END-IF
           ELSE
               MOVE    "9000"              TO  SPA-ERRCD
           END-IF
           .
       213-SOUSIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＬＡＩＭ接続情報取得処理
      *****************************************************************
       2131-CLAIM-CHK-SEC               SECTION.
      *
           INITIALIZE                  SPA-XF01X-AREA
      *
           MOVE    ZERO                TO  FLG-CLAIM
           MOVE    ZERO                TO  SPA-CLAIM-MULTIHOST
      *    ＣＬＡＩＭ接続判定
           PERFORM 31002-SYS900-READ-SEC
      *
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYS-9000-CODE       TO  SPA-GMN-SCODE
               MOVE    1                   TO  SPA-XF01X-GMN-SEL
      *        マルチホストフラグ
               IF      SYS-9000-MULTIHOST-FLG  =   "1"
                    MOVE    1                   TO  SPA-CLAIM-MULTIHOST
               END-IF
               IF      SYS-9000-SETUZOKU   =   ZERO
                                           OR  SPACE
                   CONTINUE
               ELSE
                   MOVE    SYS-9000-CODE       TO  SPA-XF01X-CODE
                   IF      SYS-9000-UKEBUTTON (02) =   "T"
                       MOVE    2               TO  FLG-CLAIM
                   ELSE
                       MOVE    1               TO  FLG-CLAIM
                   END-IF
               END-IF
           END-IF
      *
           IF      FLG-CLAIM           =   2
               PERFORM 21211-XF01X-SET-SEC
           END-IF
           .
       2131-CLAIM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ＣＬＡＩＭ画面遷移処理
      *****************************************************************
       21211-XF01X-SET-SEC            SECTION.
      *
      *    ＣＬＡＩＭ接続情報
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-XF01            TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGXF01X"         USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-XF01
      *
           MOVE    "XF01"              TO  SPA-MOTOPG
           MOVE    "XF01X"             TO  SPA-SAKIPG
      *    カーソル移動
           MOVE    "SELNUM"            TO  MCP-WIDGET
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "XF01X"             TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
           MOVE    1                   TO  FLG-END
           .
       21211-XF01X-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＬＡＩＭ接続情報取得処理
      *****************************************************************
       4906-CLAIM-SOUSIN-SEC               SECTION.
      *
           PERFORM 49061-UKETUKE-SET-SEC
      *
           MOVE    SPACE               TO  SYS-9001-REC
           INITIALIZE                  SYS-9001-REC
           MOVE    "9001"              TO  SYS-9001-KANRICD
           MOVE    SPA-SYSYMD          TO  SYS-9001-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-9001-EDYUKYMD
      *    システム管理検索
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
               INITIALIZE                  SYS-9001-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    ZERO                TO  WRK-RENNUM
           PERFORM
                   UNTIL       (FLG-SYSKANRI   =   1    )
               MOVE    MCPDATA-REC         TO  SYS-9001-REC
               MOVE    ZERO                TO  FLG-SOUSIN
               IF      SPA-CLAIM-MULTIHOST =   1
      *            複数送信処理
                   MOVE    1                   TO  FLG-SOUSIN
               ELSE
                   MOVE    SYS-9001-KBNCD (1:2)  TO  WRK-KBNCD
                   IF      WRK-KBNCD            =   SPA-XF01X-GMN-SEL
                       MOVE    1                   TO  FLG-SOUSIN
                       MOVE    1                   TO  FLG-SYSKANRI
                   END-IF
               END-IF
               IF      FLG-SOUSIN          =   1
                   PERFORM 49061-CLAIM-SOUSINSYOSI-SEC
               END-IF
      *
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 900-SYSKANRI-READ-SEC
               END-IF
           END-PERFORM
      *
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       4906-CLAIM-SOUSIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＬＡＩＭ接続情報取得処理
      *****************************************************************
       49061-CLAIM-SOUSINSYOSI-SEC               SECTION.
      *
      *    フィアルデイレクトリ位置指定
           INITIALIZE                  ORCSMKPASSAREA
           MOVE    CLAIM-FILE          TO  MKPASS-IN-01
           CALL    "ORCSMKPASS"        USING   ORCSMKPASSAREA
      *
           MOVE    MKPASS-OT-01        TO  SHELLTBL-NAME
      *
           MOVE    SPA-GMN-SCODE       TO  SHELLTBL-ARG1
           MOVE    SPA-GMN-PTID        TO  WRK-PTID
           MOVE    WRK-PTID            TO  SHELLTBL-ARG2
           IF      WRK-UKE-SRYKA       =   SPACE
               MOVE    "XX"                TO  SHELLTBL-ARG3
           ELSE
               MOVE    UKE-SRYKA           TO  SHELLTBL-ARG3
           END-IF
           MOVE    WRK-UKE-UKETIME         TO  SHELLTBL-ARG4
           MOVE    SPA-HOSPNUM         TO  SHELLTBL-ARG5
      *
           MOVE    SYS-9001-ADDRESS    TO  SHELLTBL-ARG6
           MOVE    SYS-9001-PORT1      TO  SHELLTBL-ARG7
      *    ドクター
           IF      WRK-UKE-DRCD        =   SPACE
                                       OR  ZERO
               MOVE    "@@@@@"             TO  SHELLTBL-ARG8
           ELSE
               MOVE    UKE-DRCD            TO  SHELLTBL-ARG8
           END-IF
      *    保険組合せ
           IF      WRK-UKE-HKNCOMBI    =   SPACE
                                       OR  ZERO
               MOVE    "@@@@"              TO  SHELLTBL-ARG9
           ELSE
               MOVE    WRK-UKE-HKNCOMBI    TO  SHELLTBL-ARG9
           END-IF
      *    診療内容
           IF      WRK-UKE-SRYNAIDSP   =   SPACE
               MOVE    "XX"                TO  SHELLTBL-ARG10
           ELSE
               MOVE    WRK-UKE-SRYNAIDSP   TO  SHELLTBL-ARG10
           END-IF
      *    連番
           ADD     1                   TO  WRK-RENNUM
           MOVE    WRK-RENNUM          TO  SHELLTBL-ARG11 (1:2)
      *    システム日付
           MOVE    SPA-SYSYMD          TO   SHELLTBL-ARG12
      *    メモからの送信
           MOVE    "@@"                TO   SHELLTBL-ARG13
      *
           MOVE    SHELLTBL            TO  MCPDATA-REC
           MOVE    "SHELL"             TO  MCP-FUNC
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    "shell"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       49061-CLAIM-SOUSINSYOSI-EXT.
           EXIT.
      *H28.6
      *****************************************************************
      *    API ＣＬＡＩＭ送信処理
      *****************************************************************
       49061-API-CLMAI-SEC  SECTION.
      *
           MOVE    ZERO                TO  FLG-CLAIM
           MOVE    ZERO                TO  WRK-RENNUM
      *    ＣＬＡＩＭ接続判定
           MOVE    SPACE               TO  SYS-9000-REC
           INITIALIZE                  SYS-9000-REC
           MOVE    "9000"              TO  SYS-9000-KANRICD
           MOVE    "*"                 TO  SYS-9000-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-9000-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-9000-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-9000-HOSPNUM
           MOVE    SYS-9000-REC        TO  SYSKANRI-REC
           PERFORM 9009-SYSKANRI-KEY-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-9000-REC
           END-IF
      *
           IF      FLG-SYSKANRI        =   1
               CONTINUE
           ELSE
               MOVE    SYS-9000-CODE       TO  SPA-GMN-SCODE
               IF      SYS-9000-SETUZOKU   =   ZERO
                                           OR  SPACE
                   CONTINUE
               ELSE
      *            ＣＬＡＩＭ送信あり
                   MOVE    1               TO  FLG-CLAIM
               END-IF
           END-IF
      *
           IF      FLG-CLAIM           =   1
      *
               PERFORM 49061-UKETUKE-SET-SEC
      *        フィアルデイレクトリ位置指定
               INITIALIZE                  ORCSMKPASSAREA
               MOVE    CLAIM-FILE          TO  MKPASS-IN-01
               CALL    "ORCSMKPASS"        USING   ORCSMKPASSAREA
      *
               MOVE    "HL04.sh"           TO  SHELLTBL-NAME
      *
               MOVE    SPA-GMN-SCODE       TO  SHELLTBL-ARG1
               MOVE    SPA-GMN-PTID        TO  WRK-PTID
               MOVE    WRK-PTID            TO  SHELLTBL-ARG2
               IF      WRK-UKE-SRYKA       =   SPACE
                   MOVE    "XX"                TO  SHELLTBL-ARG3
               ELSE
                   MOVE    UKE-SRYKA           TO  SHELLTBL-ARG3
               END-IF
               MOVE    WRK-UKE-UKETIME     TO  SHELLTBL-ARG4
               MOVE    SPA-HOSPNUM         TO  SHELLTBL-ARG5
      *
      *****    MOVE    SYS-9001-ADDRESS    TO  SHELLTBL-ARG6
      *****    MOVE    SYS-9001-PORT1      TO  SHELLTBL-ARG7
      *        ドクター
               IF      WRK-UKE-DRCD        =   SPACE
                                           OR  ZERO
                   MOVE    "@@@@@"             TO  SHELLTBL-ARG8
               ELSE
                   MOVE    WRK-UKE-DRCD        TO  SHELLTBL-ARG8
               END-IF
      *        保険組合せ
               IF      WRK-UKE-HKNCOMBI    =   SPACE
                                           OR  ZERO
                   MOVE    "@@@@"              TO  SHELLTBL-ARG9
               ELSE
                   MOVE    WRK-UKE-HKNCOMBI    TO  SHELLTBL-ARG9
               END-IF
      *        診療内容
               IF      WRK-UKE-SRYNAIDSP   =   SPACE
                   MOVE    "XX"                TO  SHELLTBL-ARG10
               ELSE
                   MOVE    WRK-UKE-SRYNAIDSP   TO  SHELLTBL-ARG10
               END-IF
      *        連番
               ADD     1                   TO  WRK-RENNUM
               MOVE    WRK-RENNUM          TO  SHELLTBL-ARG11 (1:2)
      *        システム日付
               MOVE    SPA-SYSYMD          TO   SHELLTBL-ARG12
      *        メモからの送信
               MOVE    "@@"                TO   SHELLTBL-ARG13
      *        患者番号
               MOVE    SPA-GMN-PTNUM       TO   SHELLTBL-ARG14
      *
               MOVE    SHELLTBL            TO  MCPDATA-REC
               MOVE    "SHELL"             TO  MCP-FUNC
               MOVE    "shell"             TO  MCP-TABLE
               MOVE    "allways"           TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
           END-IF
      *
           .
       49061-API-CLMAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付　処理
      *****************************************************************
       49061-UKETUKE-SET-SEC         SECTION.
      *
           PERFORM 49061-UKETUKE-CHEK-SEC
      *
           IF      WRK-UKE-SRYNAIYO    NOT =   SPACE
      *        診療内容検索
               MOVE    SPACE               TO  SYS-1012-REC
               MOVE    SPA-HOSPNUM         TO  SYS-1012-HOSPNUM
               MOVE    "1012"              TO  SYS-1012-KANRICD
               MOVE    WRK-UKE-SRYNAIYO    TO  SYS-1012-KBNCD
               MOVE    SPA-SYSYMD          TO  SYS-1012-STYUKYMD
               MOVE    SPA-SYSYMD          TO  SYS-1012-EDYUKYMD
               MOVE    SYS-1012-REC        TO  MCPDATA-REC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "key10"             TO  MCP-PATHNAME
                   PERFORM 900-SYSKANRI-READ-SEC
                   IF      FLG-SYSKANRI        =   ZERO
                       MOVE    MCPDATA-REC         TO  SYS-1012-REC
                       MOVE    SYS-1012-SRYNAIYODSP
                                                   TO  WRK-UKE-SRYNAIDSP
                   END-IF
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
           END-IF
      *    受付時間取得
           IF      WRK-UKE-UKETIME     =   SPACE
               INITIALIZE                  ORCSMCNDATEAREA
               CALL    "ORCSMCNDATE"       USING
                                           ORCSMCNDATEAREA
               MOVE    SMCNDATE-HMS        TO  WRK-UKE-UKETIME
           END-IF
           .
       49061-UKETUKE-SET-EXT.
           EXIT.
      *****************************************************************
      *    受付　処理
      *****************************************************************
       49061-UKETUKE-CHEK-SEC         SECTION.
      *
           INITIALIZE                  WRK-UKE-DATA
      *
           MOVE    SPACE               TO  UKETUKE-REC
           INITIALIZE                  UKETUKE-REC
           MOVE    SPA-HOSPNUM         TO  UKE-HOSPNUM
           MOVE    SPA-SYSYMD          TO  UKE-UKEYMD
           MOVE    SPA-PTID            TO  UKE-PTID
      *
           MOVE    UKETUKE-REC         TO  MCPDATA-REC
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-UKETUKE-READ-SEC
           ELSE
               MOVE    SPACE               TO  UKETUKE-REC
               INITIALIZE                  UKETUKE-REC
               MOVE    1                   TO  FLG-UKETUKE
           END-IF
           MOVE    ZERO                TO  FLG-UKETUKE-ARI
           PERFORM UNTIL  (FLG-UKETUKE     =   1  )
                   OR     (FLG-UKETUKE-ARI =   1  )
      *        会計のまだの最初の受付を対象とする
               IF      SPA-P02-UKEID   NOT =   ZERO
                   IF     SPA-P02-UKEID        =   UKE-UKEID
                       MOVE    1               TO  FLG-UKETUKE-ARI
                   END-IF
               ELSE
                   IF     (UKE-KAIKEITIME      =   ZERO )  AND
                          (UKE-UKEID       NOT =   ZERO )
                       MOVE    1               TO  FLG-UKETUKE-ARI
                   END-IF
               END-IF
               IF      FLG-UKETUKE-ARI     =   1
                   MOVE    UKE-UKETIME         TO  WRK-UKE-UKETIME
                   MOVE    UKE-SRYKA           TO  WRK-UKE-SRYKA
                   MOVE    UKE-DRCD            TO  WRK-UKE-DRCD
                   MOVE    UKE-HKNCOMBI        TO  WRK-UKE-HKNCOMBI
                   MOVE    UKE-SRYNAIYO        TO  WRK-UKE-SRYNAIYO
               END-IF
               IF      FLG-UKETUKE-ARI     =   ZERO
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 900-UKETUKE-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       49061-UKETUKE-CHEK-EXT.
           EXIT.
      *H30.11
      *****************************************************************
      *    PUSH処理
      *****************************************************************
       600-PUSH-SYORI-SEC         SECTION.
      *
      *
           INITIALIZE                      PUSHAC02-REC
           MOVE    "patient_memo"      TO  PUSHAC02-EVENT
           EVALUATE    FLG-SYORIKBN
           WHEN    1
      *        追加
               MOVE    "add"               TO  PUSHAC02-PMODE
           WHEN    2
      *        更新
               MOVE    "modify"            TO  PUSHAC02-PMODE
           WHEN    3
      *        削除
               MOVE    "delete"            TO  PUSHAC02-PMODE
           END-EVALUATE
           MOVE    SPA-GMN-PTNUM       TO  PUSHAC02-PTNUM
           MOVE    SPA-SYSYMD          TO  WRK-SYMD
      *??
      *    送信時間
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                            ORCSMCNDATEAREA
           MOVE    SMCNDATE-HMS        TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  PUSHAC02-UKEYMD
           MOVE    WRK-HEN-TIME        TO  PUSHAC02-UKETIME
      *    MOVE    UKE-UKEID           TO  PUSHAC02-UKEID
      *    MOVE    UKE-DRCD            TO  PUSHAC02-DRCD
           MOVE    WRK-SRYKA           TO  PUSHAC02-SRYKA
      *    MOVE    UKE-HKNCOMBI        TO  PUSHAC02-HKNCOMBI
      *
           MOVE    PUSHAC02-REC        TO  MCPDATA-REC
           MOVE    "PUSHEVENT"         TO  MCP-FUNC
           MOVE    "push_accept02"     TO  MCP-TABLE
           MOVE    "push_accept02"     TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       600-PUSH-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       801-DAYHEN01-SEC                SECTION.
      *
           IF      WRK-SYMD            =   SPACE
               MOVE    SPACE           TO  WRK-HEN-DATE
           ELSE
               INITIALIZE                  WRK-HEN-DATE
               MOVE    WRK-SYY             TO  WRK-HEN-YY
               MOVE    WRK-SMM             TO  WRK-HEN-MM
               MOVE    WRK-SDD             TO  WRK-HEN-DD
               MOVE    "-"                 TO  WRK-HEN-H1
               MOVE    "-"                 TO  WRK-HEN-H2
               IF      WRK-SYMD            =   "99999999"
                   MOVE    "12"                TO  WRK-HEN-MM
                   MOVE    "31"                TO  WRK-HEN-DD
               END-IF
           END-IF
      *
           INITIALIZE                  WRK-HEN-TIME
           MOVE    WRK-THHH            TO  WRK-HEN-THH
           MOVE    WRK-THMM            TO  WRK-HEN-TMM
           MOVE    WRK-THSS            TO  WRK-HEN-TSS
           MOVE    ":"                 TO  WRK-HEN-T1
           MOVE    ":"                 TO  WRK-HEN-T2
           .
       801-DAYHEN01-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    LNK-COMMON          TO  SPA-KYOUTU
      *
           MOVE    SPA-HZN-MOTOPG      TO  SPA-SAKIPG
           MOVE    "XF01"              TO  SPA-MOTOPG
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
           IF      SPA-XFIDCD       NOT =   SPACE
               PERFORM 520-XFID1SET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-XFIDCD2      NOT =   SPACE
               PERFORM 530-XFID2SET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
      *****MOVE    SPACE               TO  LINKAREA
           MOVE    SPACE               TO  LNK-KYOUTU
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "XF01"              TO  MCP-WINDOW
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
                   MOVE    "診療科１選択エラー"
                                       TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "メモ１エラー"
                                       TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "診療科２選択エラー"
                                       TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "メモ２エラー"
                                       TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "選択番号がありません。"
                                       TO  SPA-ERRMSG
               WHEN    "0011"
                   STRING  "システム日付ではありません。"
                           "削除のみ行って下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0012"
                   STRING  "新規のメモです。削除はできません。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0010"
                   STRING  "メモの内容が登録されていません。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0013"
                   STRING  "削除するメモがありません。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0015"
                   STRING  "<>はＣＬＡＩＭ送信できません。"
                           "全角文字に変更して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "9000"
                   STRING  "本日のメモ２が登録されていません。"
                           "ＣＬＡＩＭ送信できません。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0020"
                   MOVE    "更新エラーです。"
                                       TO  SPA-ERRMSG
               WHEN    "0021"
                   MOVE    "削除エラーです。"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  XFERR
           MOVE    SPA-ERRCD           TO  XFERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  XFERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "XF01"              TO  SPA-MOTOPG
           MOVE    "XFERR"             TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "XFERR"             TO  MCP-WINDOW.
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
       520-XFID1SET-SEC              SECTION.
      *
           EVALUATE    SPA-XFIDCD
               WHEN    "0001"
                   STRING  "表示されている内容でメモを"
                           "登録します。よろしいですか？"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-XFIDMSG
                   END-STRING
               WHEN    "0002"
                   STRING  "表示されている日のメモをすべて削除します。"
                           "よろしいですか？"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-XFIDMSG
                   END-STRING
               WHEN    "0003"
                   STRING  "メモの更新ならＯＫ を メモ内容複写ならＮＯ"
                           "を選択して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-XFIDMSG
                   END-STRING
               WHEN    "0004"
                   STRING  "メモ内容の置換ならＯＫ を 追加ならＮＯ"
                           "を選択して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-XFIDMSG
                   END-STRING
               WHEN    "0101"
                   STRING  "表示されているメモ１を登録します。"
                           "よろしいですか？"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-XFIDMSG
                   END-STRING
               WHEN    "0102"
                   STRING  "表示されているメモ１を削除します。"
                           "よろしいですか？"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-XFIDMSG
                   END-STRING
               WHEN    "0103"
                   STRING  "表示されているメモ２を登録します。"
                           "よろしいですか？"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-XFIDMSG
                   END-STRING
               WHEN    "0104"
                   STRING  "表示されているメモ２を削除します。"
                           "よろしいですか？"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-XFIDMSG
                   END-STRING
               WHEN    OTHER
                   MOVE    SPA-XFIDCD
                                       TO  WRK-XFIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-XFID1-FLG
      *
           MOVE    SPACE               TO  XFID1
           INITIALIZE                      XFID1
           MOVE    SPA-XFIDCD          TO  XFID1-ID1CODE
           MOVE    WRK-XFIDMSG         TO  XFID1-ID1MSG
      *
           MOVE    "戻る"              TO  XFID1-B01
           MOVE    "ＯＫ"              TO  XFID1-B12
           EVALUATE    SPA-XFIDCD
               WHEN    "0003"
               WHEN    "0004"
                   MOVE    "ＮＯ"          TO  XFID1-B01
           END-EVALUATE
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "XF01"              TO  SPA-MOTOPG
           MOVE    "XFID1"             TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "XFID1"             TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-XFID1SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    警告メッセージ２表示理
      *****************************************************************
       530-XFID2SET-SEC              SECTION.
      *
           EVALUATE    SPA-XFIDCD2
               WHEN    "0001"
                   STRING  "表示されている内容でメモを"
                           "登録します。"
                           "送信押下で登録後、送信します。"
                                       DELIMITED  BY  SIZE
                                       INTO    WRK-XFIDMSG
                   END-STRING
               WHEN    OTHER
                   MOVE    SPA-XFIDCD2
                                       TO  WRK-XFIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-XFID2-FLG
      *
           MOVE    SPACE               TO  XFID2
           INITIALIZE                      XFID2
           MOVE    SPA-XFIDCD2         TO  XFID2-ID2CODE
           MOVE    WRK-XFIDMSG         TO  XFID2-ID2MSG
      *
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "XF01"              TO  SPA-MOTOPG
           MOVE    "XFID2"             TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "XFID2"             TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       530-XFID2SET-EXT.
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
      *    システム管理主キー検索
      *****************************************************************
       9009-SYSKANRI-KEY-READ-SEC         SECTION.
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    MCPDATA-REC     TO  SYSKANRI-REC
               ELSE
                   INITIALIZE                  SYSKANRI-REC
               END-IF
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       9009-SYSKANRI-KEY-READ-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    患者メモ読込処理
      *****************************************************************
       910-PTMEMOINF-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PTMEMOINF-REC
               MOVE    ZERO                TO  FLG-PTMEMOINF
           ELSE
               INITIALIZE                      PTMEMOINF-REC
               MOVE    1                   TO  FLG-PTMEMOINF
           END-IF
      *
           .
       910-PTMEMOINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    受付マスター読込処理
      *****************************************************************
       900-UKETUKE-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-UKETUKE
               MOVE    MCPDATA-REC         TO  UKETUKE-REC
           ELSE
               MOVE    1                   TO  FLG-UKETUKE
               INITIALIZE                      UKETUKE-REC
           END-IF
      *
           .
       910-UKETUKE-READ-EXT.
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
