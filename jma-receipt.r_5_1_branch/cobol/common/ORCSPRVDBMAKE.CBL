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
       PROGRAM-ID.                 ORCSPRVDBMAKE.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : リアルタイムプレビュー用
      *                    : 実DBからtmpDBを作成する
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/05/10    NACL−竹田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.00.00    NACL-竹田    06/06/07  患者コメント取り込み不具合
      *                                     対応
      *  03.05.00    NACL-多々納  07/04/XX  グループ診療対応
      *****************************************************************
      *
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-WKSRYACT        PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-SYUMEI          PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PTCOM           PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-SRYKARRK        PIC 9(01).
           03  FLG-UKETUKE         PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-SANTEI          PIC 9(01).
           03  FLG-PARA            PIC 9(01).
           03  FLG-SYUTTL          PIC 9(01).
           03  FLG-SRYACCTPLUS     PIC 9(01).
      *----> take add start
           03  FLG-PTINFPRV        PIC 9(01).
      *----> take add ed
      *
           03  FLG-OK              PIC 9(01).
           03  FLG-CHK             PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-ERR2            PIC 9(01).
           03  FLG-ARI             PIC 9(01).
           03  FLG-SINKI           PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(02).
           03  IDX-P               PIC 9(04).
           03  IDX-KOUI            PIC 9(04).
           03  IDX-SIN             PIC 9(04).
           03  IDX-ATO             PIC 9(04).
           03  IDX-YAKUZAI         PIC 9(04).
           03  IDX-Y2              PIC 9(02).
      *
           03  IDX-1               PIC 9(04).
           03  IDX-2               PIC 9(02).
           03  IDX-3               PIC 9(02).
      *
           03  IDH                 PIC 9(04).
           03  IDH2                PIC 9(04).
           03  IDH3                PIC 9(04).
           03  IDXF                PIC 9(04).
           03  IDH-GAI             PIC 9(04).
      *
           03  IDW                 PIC 9(04).
           03  IDW2                PIC 9(04).
      *
           03  IDX-GEN4            PIC 9(04).
           03  IDX-GEN5            PIC 9(04).
           03  IDX-GEN6            PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
      *
           03  WRK-PATH                    PIC X(64).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK0041.INC".
      *
           COPY    "CPSK1013.INC".
      *
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    収納マスタワーク
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    診療行為マスター
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    収納明細マスタ−
       01  SYUMEI-REC.
           COPY    "CPSYUMEI.INC".
      *
      *    収納合計マスタ−
       01  SYUTTL-REC.
           COPY    "CPSYUTOTAL.INC".
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *    診療会計附加情報マスタ−
       01  SRYACCTPLUS-REC.
           COPY    "CPSRYACCTPLUS.INC".
      *
      *    患者コメント
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *
       01  WRK-PTCOM-REC.
           COPY    "CPPTCOM.INC"   REPLACING
                                     //PTCOM-// BY //WRK-PTCOM-//.
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    診療科履歴
       01  SRYKARRK-REC.
           COPY    "CPSRYKARRK.INC".
      *
      *    ワーク診療行為マスタ−
       01  WKSRYACT-REC.
           COPY    "CPWKSRYACT.INC".
      *DDD 附加情報を追加
      *DDD 03  WKSRYACTPLUS-REC.
      *    COPY    "CPWKSRYACTPLUS.INC".
      *
      *    受診内容
       01  UKETUKE-REC.
           COPY    "CPUKETUKE.INC".
      *
      *    算定履歴
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *
      *    保存用収納ワーク
       01  WRK-SYUNOU-REC.
           COPY    "CPSYUNOU.INC"  REPLACING
                                     //SYU-// BY //WRK-SYU-//.
      *
      *    受診履歴
       01  TBL-JYURRK-AREA.
           02  TBL-JYURRK-REC        OCCURS   10.
           COPY    "CPJYURRK.INC"  REPLACING
                                     //JYURRK-// BY //TBL-JYURRK-//.
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           .
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
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *    前回のデータが残っている場合は削除
           CALL    "ORCSPRVDBDEL"      USING
                                       SPA-AREA
      *
           PERFORM 100-PRV-DBMAKE-SEC
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *----->take add st
      *****************************************************************
      *    プレビュー用患者登録処理
      *****************************************************************
       100-PRV-DBMAKE-SEC              SECTION.
      *
      *    TMP患者マスタ作成
           PERFORM 100-PTINFPRV-INS-SEC
      *    TMP受診履歴作成
           PERFORM 100-JYURRKPRV-INS-SEC
      *    TMP診療会計作成
           PERFORM 100-ACCTPRV-INS-SEC
      *    TMP診療行為作成
           PERFORM 100-ACTPRV-INS-SEC
      *    TMP算定履歴作成
           PERFORM 100-SANTEIPRV-INS-SEC
      *    TMP診療会計付加情報作成
           PERFORM 100-ACCTPLUSPRV-INS-SEC
           .
      *
       100-PRV-DBMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    プレビュー用患者登録処理
      *****************************************************************
       100-PTINFPRV-INS-SEC            SECTION.
      *    患者マスターより、最終剤番号を再番し、患者マスターを更新する
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTINF-PTID
           PERFORM 940-PTINF-READ-SEC
           IF      FLG-PTINF           =   1
               MOVE    "0002"          TO  SPA-ERRCD
               DISPLAY "K03 ZAINUM-SET ERR:" SPA-PTID
               GO      TO      100-PTINFPRV-INS-EXT
           END-IF
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf_prv"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    "0002"          TO  SPA-ERRCD
               DISPLAY "K03 PTINFPRV-INS ERR:" SPA-PTID
               GO      TO      100-PTINFPRV-INS-EXT
           END-IF
           .
      *
       100-PTINFPRV-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    プレビュー用受診履歴処理
      *****************************************************************
       100-JYURRKPRV-INS-SEC           SECTION.
           INITIALIZE                      JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    SPA-SRYYMD(1:6)     TO  JYURRK-SRYYMD
           MOVE    "% "                TO  JYURRK-SRYYMD(7:2)
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key31"             TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"    TO  MCP-TABLE
               MOVE    "key31"         TO  MCP-PATHNAME
               PERFORM 970-JYURRK-NEXT2-SEC
           ELSE
               MOVE    "0002"          TO  SPA-ERRCD
               MOVE    1               TO  FLG-JYURRK
           END-IF
           PERFORM
                   UNTIL       FLG-JYURRK      =   1
      *
               MOVE    JYURRK-REC          TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_jyurrk_prv"    TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               MOVE    "tbl_jyurrk"    TO  MCP-TABLE
               MOVE    "key31"         TO  MCP-PATHNAME
               PERFORM 970-JYURRK-NEXT2-SEC
           END-PERFORM
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key31"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE2-SEC
           .
      *
       100-JYURRKPRV-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    プレビュー用診療会計処理
      *****************************************************************
       100-ACCTPRV-INS-SEC              SECTION.
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SPA-PTID            TO  ACCT-PTID
           MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SPA-SRYYMD(1:6)     TO  ACCT-SRYYM
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"   TO  MCP-TABLE
               MOVE    "key7"          TO  MCP-PATHNAME
               PERFORM 930-SRYACCT-NEXT2-SEC
           ELSE
               MOVE    "0002"          TO  SPA-ERRCD
               MOVE    1               TO  FLG-SRYACCT
           END-IF
           PERFORM
                   UNTIL       FLG-SRYACCT     =   1
               MOVE    SRYACCT-REC         TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_sryacct_prv"   TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               MOVE    "tbl_sryacct"   TO  MCP-TABLE
               MOVE    "key7"          TO  MCP-PATHNAME
               PERFORM 930-SRYACCT-NEXT2-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE2-SEC
           .
      *
       100-ACCTPRV-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    プレビュー用診療会計処理
      *****************************************************************
       100-ACCTPLUSPRV-INS-SEC         SECTION.
           INITIALIZE                      SRYACCTPLUS-REC
           MOVE    SPA-HOSPNUM         TO  ACCTP-HOSPNUM
           MOVE    SPA-PTID            TO  ACCTP-PTID
           MOVE    SPA-NYUGAIKBN       TO  ACCTP-NYUGAIKBN
           MOVE    SPA-SRYYMD(1:6)     TO  ACCTP-SRYYM
           MOVE    SRYACCTPLUS-REC     TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *****************************************************************
           MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *****************************************************************
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 930-SRYACCTPLUS-NEXT2-SEC
           ELSE
               MOVE    "0002"          TO  SPA-ERRCD
               MOVE    1               TO  FLG-SRYACCTPLUS
           END-IF
           PERFORM
                   UNTIL       FLG-SRYACCTPLUS =   1
               MOVE    SRYACCTPLUS-REC     TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_sryacctplus_prv"   TO  MCP-TABLE
               MOVE    "key"              TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 930-SRYACCTPLUS-NEXT2-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE2-SEC
           .
      *
       100-ACCTPRVPLUS-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    プレビュー用診療行為登録処理
      *****************************************************************
       100-SANTEIPRV-INS-SEC           SECTION.
           INITIALIZE                      SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key13"             TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key13"             TO  MCP-PATHNAME
               PERFORM 915-SANTEI-NEXT2-SEC
           ELSE
               MOVE    "0002"          TO  SPA-ERRCD
               MOVE    1               TO  FLG-SANTEI
           END-IF
           PERFORM
                   UNTIL       FLG-SANTEI     =   1
               MOVE    SANTEI-REC          TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_santei_prv"    TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key13"             TO  MCP-PATHNAME
               PERFORM 915-SANTEI-NEXT2-SEC
           END-PERFORM
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key13"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE2-SEC
           INITIALIZE                      SANTEI-REC
           .
      *
       100-SANTEIPRV-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    プレビュー用診療行為登録処理
      *****************************************************************
       100-ACTPRV-INS-SEC               SECTION.
           INITIALIZE                      SRYACT-REC
           MOVE    SPA-HOSPNUM         TO  SRY-HOSPNUM
           MOVE    SPA-PTID            TO  SRY-PTID
           MOVE    SPA-NYUGAIKBN       TO  SRY-NYUGAIKBN
           MOVE    SPA-SRYYMD(1:6)     TO  SRY-SRYYM
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 915-SRYACT-NEXT2-SEC
           ELSE
               MOVE    "0002"          TO  SPA-ERRCD
               MOVE    1               TO  FLG-SRYACT
           END-IF
           PERFORM
                   UNTIL       FLG-SRYACT     =   1
               MOVE    SRYACT-REC          TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_sryact_prv"    TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
      *
               PERFORM 100-PTCOMPRV-INS-SEC
      *
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 915-SRYACT-NEXT2-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE2-SEC
           .
      *
       100-ACCTPRV-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    プレビュー用診療会計処理
      *****************************************************************
       100-PTCOMPRV-INS-SEC            SECTION.
           INITIALIZE                      SRYACCT-REC
           MOVE    SRY-HOSPNUM         TO  PTCOM-HOSPNUM
           MOVE    SRY-PTID            TO  PTCOM-PTID
           MOVE    SRY-ZAINUM          TO  PTCOM-ZAINUM
           MOVE    SRY-RENNUM          TO  PTCOM-RENNUM
           MOVE    PTCOM-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptcom"         TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 915-PTCOM-NEXT2-SEC
           ELSE
               MOVE    "0002"          TO  SPA-ERRCD
               MOVE    1               TO  FLG-PTCOM
           END-IF
           PERFORM
                   UNTIL       FLG-PTCOM      =   1
               MOVE    PTCOM-REC           TO  MCPDATA-REC
               MOVE    PTCOM-REC           TO  WRK-PTCOM-REC
	       PERFORM 1001-PTCOMPRV-CHK-SEC
	       IF      FLG-PTCOM           =   1
                   MOVE    WRK-PTCOM-REC       TO  MCPDATA-REC
                   MOVE    "DBINSERT"      TO  MCP-FUNC
                   MOVE    "tbl_ptcom_prv" TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
grpsys             CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
               END-IF
               MOVE    "tbl_ptcom"         TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 915-PTCOM-NEXT2-SEC
           END-PERFORM
      *
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE2-SEC
           .
      *
       100-PTCOMPRV-INS-EXT.
           EXIT.
      *
      *----->take add ed
      *
      *****************************************************************
      *    患者コメントチェック処理
      *****************************************************************
       1001-PTCOMPRV-CHK-SEC      SECTION.
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptcom_prv" TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptcom_prv" TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 915-PTCOM-NEXT2-SEC
           END-IF
           MOVE    "tbl_ptcom_prv" TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE2-SEC
           .
      *
       1001-PTCOMPRV-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込
      *****************************************************************
       940-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC        TO  PTINF-REC
                   MOVE    ZERO               TO  FLG-PTINF
               ELSE
                   MOVE    1                  TO  FLG-PTINF
               END-IF
           ELSE
               MOVE    1              TO  FLG-PTINF
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       940-PTINF-READ-EXT.
           EXIT.
      *
      *----> take add start
      *****************************************************************
      *    患者マスター読込
      *****************************************************************
       940-PTINFPRV-READ-SEC      SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf_prv"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_ptinf_prv"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC        TO  PTINF-REC
                   MOVE    ZERO               TO  FLG-PTINFPRV
               ELSE
                   MOVE    1                  TO  FLG-PTINFPRV
               END-IF
           ELSE
               MOVE    1              TO  FLG-PTINFPRV
           END-IF
      *
           MOVE    "tbl_ptinf_prv"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE2-SEC
      *
           .
       940-PTINFPRV-READ-EXT.
           EXIT.
      *
      *----> take add ed
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
      *----> take add st
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE2-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE2-EXT.
           EXIT.
      *
      *----> take add ed
      *----> take add start
      *****************************************************************
      *    受診履歴順読み
      *****************************************************************
       970-JYURRK-NEXT2-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  JYURRK-REC
               MOVE    ZERO                TO  FLG-JYURRK
           ELSE
               MOVE    1                   TO  FLG-JYURRK
           END-IF
      *
           .
       970-JYURRK-NEXT2-EXT.
           EXIT.
      *
      *----> take add start
      *****************************************************************
      *    診療会計マスター読込
      *****************************************************************
       930-SRYACCT-NEXT2-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACCT-REC
               MOVE    ZERO                TO  FLG-SRYACCT
           ELSE
               INITIALIZE                      SRYACCT-REC
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
      *
           .
       930-SRYACCT-NEXT2-EXT.
           EXIT.
      *****************************************************************
      *    診療会計付加情報マスター読込
      *****************************************************************
       930-SRYACCTPLUS-NEXT2-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACCTPLUS-REC
               MOVE    ZERO                TO  FLG-SRYACCTPLUS
           ELSE
               INITIALIZE                      SRYACCTPLUS-REC
               MOVE    1                   TO  FLG-SRYACCTPLUS
           END-IF
      *
           .
       930-SRYACCTPLUS-NEXT2-EXT.
           EXIT.
      *****************************************************************
      *    診療行為マスター読込
      *****************************************************************
       915-SRYACT-NEXT2-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACT-REC
               MOVE    ZERO                TO  FLG-SRYACT
           ELSE
               INITIALIZE                      SRYACT-REC
               MOVE    1                   TO  FLG-SRYACT
           END-IF
      *
           .
       915-SRYACT-NEXT2-EXT.
           EXIT.
      *****************************************************************
      *    患者コメント読込
      *****************************************************************
       915-PTCOM-NEXT2-SEC             SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PTCOM-REC
               MOVE    ZERO                TO  FLG-PTCOM
           ELSE
               INITIALIZE                      PTCOM-REC
               MOVE    1                   TO  FLG-PTCOM
           END-IF
      *
           .
       915-PTCOM-NEXT2-EXT.
           EXIT.
      *****************************************************************
      *    算定履歴マスター読込
      *****************************************************************
       915-SANTEI-NEXT2-SEC            SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SANTEI-REC
               MOVE    ZERO                TO  FLG-SANTEI
           ELSE
               INITIALIZE                      SANTEI-REC
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           .
       915-SANTEI-NEXT2-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       990-DBCLOSE-2-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-2-EXT.
           EXIT.
      *
