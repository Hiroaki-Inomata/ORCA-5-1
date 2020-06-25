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
       PROGRAM-ID.             ORCT0020.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 定期請求
      *  コンポーネント名  : 請求書出力
      *  管理者            :
      *  作成日付    作業者        記述
      *  02/12/19    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-太田    03.03.13  入院請求書複数枚一括印刷対応
      *  01.00.02    NACL-太田    03.03.18  入院カスタマイズ帳票対応
      *  01.00.03    NACL-太田    03.07.01  出力先プリンタサブ追加
      *  03.05.00    NACL-太田    07/05/30  グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    エラーファイル
           SELECT  TEIKIERR-FILE   ASSIGN  TEIKIERR
                                   FILE    STATUS  IS  STS-TEIKIERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
      *    エラーファイル
       FD  TEIKIERR-FILE.
       01  TEIKIERR-REC                    PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPASGNPARA.INC"        REPLACING  //ASGNPARA//
                                           BY         //TEIKIERR//.
      *
           COPY    "ENUM-VALUE".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-TEIKIERR                PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-ERR                     PIC 9(01).
           03  FLG-END                     PIC 9(01).
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-BTPARA                  PIC 9(01).
           03  FLG-PTINF                   PIC 9(01).
           03  FLG-PTNUM                   PIC 9(01).
           03  FLG-SYUNOU                  PIC 9(01).
           03  FLG-SYUMEI                  PIC 9(01).
           03  FLG-PTTEIKIRRK              PIC 9(01).
           03  FLG-TAISYO                  PIC 9(01).
           03  FLG-QTEIKI                  PIC 9(01).
           03  FLG-SELTAIIN                PIC 9(01).
           03  FLG-SUMTAIIN                PIC 9(01).
           03  FLG-SUMKANANAME             PIC 9(01).
           03  FLG-SUMPTNUM                PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-OUT1                    PIC 9(06).
           03  CNT-OUT2                    PIC 9(06).
           03  CNT-SELKBT                  PIC 9(06).
      *
       01  IDX-AREA.
           03  IDX0                        PIC 9(05). 
           03  IDX1                        PIC 9(05). 
           03  IDX2                        PIC 9(05). 
           03  IDX-SELKBT                  PIC 9(05).
           03  IDX-M                       PIC 9(05).
           03  IDX-MAX                     PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PATHNAME                PIC X(100).
      *
           03  WRK-YMD.
               05  WRK-YY                  PIC 9(04).
               05  WRK-MM                  PIC 9(02).
               05  WRK-DD                  PIC 9(02).
           03  WRK-SEIKYU-PG                PIC X(20).
           03  WRK-NYUKIN-HOHO             PIC X(02).
           03  WRK-NYUKIN-JYOTAI           PIC X(01).
           03  WRK-NYUKIN-KBN              PIC X(01).
           03  WRK-NYUKINPRT-KBN           PIC X(01).
           03  WRK-STPAGE                  PIC 9(05).
           03  WRK-EDPAGE                  PIC 9(05).
           03  WRK-MEISAI-PG               PIC X(20).
           03  WRK-MEISAI-PRTNM            PIC X(20).
           03  WRK-PTID                    PIC 9(10).
           03  WRK-PRTPARA-G.
               05  WRK-PTINFREF            PIC X(01).
               05  WRK-SKYPRT              PIC X(01).
               05  WRK-SRYPRT              PIC X(01).
      *
       01  SRH-AREA.
           03  SRH-IDX                     PIC 9(05).
           03  SRH-BTUNUM                  PIC X(02).
           03  SRH-BRMNUM                  PIC X(08).
           03  SRH-1041-DAT.
               05  SRH-1041-JYOTAI         PIC X(01).
               05  SRH-1041-MAX            PIC 9(05).
               05  SRH-1041-OCC            OCCURS  99.
                   07  SRH-1041-KBNCDL     PIC X(02).
                   07  SRH-1041-JYOTAIL    PIC X(01).
           03  SRH-BTUBRM-DAT.
               05  SRH-BTUBRM-MAX          PIC 9(05).
               05  SRH-BTUBRM-OCC          OCCURS  1500.
                   07  SRH-BTUBRM-BTUNUML  PIC X(02).
                   07  SRH-BTUBRM-BRMNUML  PIC X(08).
      *
       01  TSYUGRP-AREA.
           03  TSYUGRP-MAX                 PIC 9(05).
           03  TSYUGRP-OCC                 OCCURS   1000.
               05  TSYUGRP-PTID            PIC 9(10).
               05  TSYUGRP-KBT             PIC X(01).
               05  TSYUGRP-GRP-MAX         PIC 9(05).
               05  TSYUGRP-GRP-OCC         OCCURS   10.
                   07  TSYUGRP-DENPPRTYMD  PIC X(08).
                   07  TSYUGRP-SKYPRT      PIC X(01).
                   07  TSYUGRP-SRYPRT      PIC X(01).
                   07  TSYUGRP-SRYYMD      PIC X(08).
                   07  TSYUGRP-GRP-MAX2    PIC 9(05).
                   07  TSYUGRP-GRP-OCC2    OCCURS   10.
                       09  TSYUGRP-GRP-DENPNUM
                                           PIC 9(07).
      *
       01  ERR-EDIT-AREA.
           03  ERR-WARNING                 PIC 9(01).
           03  ERR-PTID                    PIC 9(10).
           03  ERR-PTNUM                   PIC X(20).
           03  ERR-ERRCD                   PIC X(04).
           03  ERR-ERRMSG                  PIC X(80).
           03  ERR-HEN-ERRMSG              PIC X(80).
      *
       01  CONST-AREA.
           03  CONST-BTUBRM-MAX            PIC 9(05)     VALUE 1500.
           03  CONST-KBT-G.
               05  CONST-KBT-VAL.
                   07                      PIC X(01)   VALUE   "1".
                   07                      PIC X(04)   VALUE   "１期".
                   07                      PIC X(01)   VALUE   "2".
                   07                      PIC X(04)   VALUE   "２期".
                   07                      PIC X(01)   VALUE   "3".
                   07                      PIC X(04)   VALUE   "３期".
               05  CONST-KBT-R REDEFINES   CONST-KBT-VAL.
                   07  CONST-KBT-OCC       OCCURS  3.
                       09  CONST-KBT       PIC X(01).
                       09  CONST-KBTMSG    PIC X(04).
      *
      *
       01  KEY-AREA.
           03  KEY-PTID            PIC 9(10).
           03  KEY-SRYYM           PIC X(06).
           03  KEY-KBT             PIC X(01).
      *
       01  ERR-MSG-AREA.
           03  ERR-MSG01                   PIC X(100)    VALUE
               "出力順の取得に失敗しました".
           03  ERR-MSG02                   PIC X(100)    VALUE
               "収納情報の取得に失敗しました".
           03  ERR-MSG03                   PIC X(100)    VALUE
               "患者定期請求情報の取得に失敗しました".
           03  ERR-MSG04                   PIC X(100)    VALUE
               "処理対象のデータがありませんでした".
      *
           COPY    "MCPAREA".
      *
           COPY    "COMMON-SPA".
      *
      *    定期請求一括処理パラメータ
           COPY    "CPTEIKI2PARA.INC".
      *
       01  WRK-LNK-AREA.
           COPY    "CPORCSPRTLNK.INC".
      *
           COPY    "CPTEIKI2BTPARA.INC".
      *
           COPY    "CPSHELLTBL.INC".
      *
           COPY    "CPORCHCN03.INC"    REPLACING  //ORCHCN03//
                                       BY         //WORCHCN03//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    帳票ＰＧ名
           COPY    "CPSK1035.INC".
      *
      *    プリンタ名
           COPY    "CPSK1034.INC".
      *
      *    入金方法
           COPY  "CPSK1041.INC".
      *
      *    定期請求情報
           COPY    "CPSK5010.INC".
           COPY    "CPSK3103.INC".
      *
      *    収納マスター
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    収納明細
       01  SYUMEI-REC.
           COPY    "CPSYUMEI.INC".
      *
      *    患者定期請求履歴
       01  PTTEIKIRRK-REC.
           COPY    "CPPTTEIKIRRK.INC".
      *
      *    定期請求クエリ
       01  QTEIKI-REC.
           COPY    "CPQTEIKI.INC".
      *
      *    パラメタ
       01  BTPARA-REC.
           COPY    "CPBTPARA.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者番号
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    ファイルデイレクトリ位置指定サブ
           COPY    "CPMKPASS.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    印刷ＤＢ制御サブ
           COPY    "CPORCSPRT.INC".
      *
      *    帳票サブ < 請求書兼領収書 >
           COPY    "CPORCHCN03.INC".
      *    帳票サブ < 診療費明細書 >
           COPY    "CPORCHCN04.INC".
      *
      *     請求書兼領収書まとめサブ
           COPY    "CPORCHCN03S02.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *    一時ディレクトリ編集サブ
           COPY    "CPORCSGETTEMP.INC".
      *
      *    課金チェック
           COPY    "CPORCSBILLINGCHECK.INC".
      *
      ****************************************************************
       LINKAGE                         SECTION.
      *
           COPY    "CPCOMMAND-PARAM.INC".
      *
      ****************************************************************
       PROCEDURE                       DIVISION
               USING
           COMMAND-PARAM.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-TERM-SEC
      *
           PERFORM 990-PROGRAM-FINISH-SEC
      *
           .
       000-PROC-EXT.
           EXIT.
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  CNT-AREA
                                       WRK-AREA
                                       ERR-EDIT-AREA
                                       SRH-AREA
                                       FLG-AREA
                                       IDX-AREA
                                       SPA-AREA
                                       TSYUGRP-AREA
                                       SBC-AREA
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO    LNK-PRTKANRI-RENNUM
                                               LNK-PRTKANRI-TBL-KEY
                                               LNK-PRTKANRI-TBL-GROUP
                                               LNK-PRTKANRI-SHORI-RENNUM
                                               LNK-PRTKANRI-SRYYM
                                               LNK-PRTKANRI-SKYYMD
                                               LNK-PRTKANRI-SHELLID
                                               LNK-PRTKANRI-PRIORITY
                                               LNK-PRTKANRI-TERMID
                                               LNK-PRTKANRI-OPID
                                               TEIKI2P-SYOKBN
                                               TEIKI2P-PRTJYUN
                                               TEIKI2P-SYSYMD
                                               TEIKI2P-TERMID
                                               TEIKI2P-JOBID
                                               TEIKI2P-SHELLID
                                               TEIKI2P-HOSPNUM
                                               TEIKI2P-STAFFCD
                                               TEIKI2P-ERRFILE
                                               WRK-PRTPARA-G
                                               TEIKI2P-PRTKBN
                                               TEIKI2P-SRYYM
                                               TEIKI2P-KBT-G
                                               TEIKI2P-OID1-FLG
                                               TEIKI2P-SUMKBN
           END-UNSTRING
      *
           MOVE    WRK-PTINFREF        TO  TEIKI2P-PTINFREF
           MOVE    WRK-SKYPRT          TO  TEIKI2P-SKYPRT
           MOVE    WRK-SRYPRT          TO  TEIKI2P-SRYPRT
      *
           MOVE    TEIKI2P-HOSPNUM     TO  SPA-HOSPNUM
           MOVE    TEIKI2P-STAFFCD     TO  SPA-STAFFCD
           MOVE    TEIKI2P-SYSYMD      TO  SPA-SYSYMD
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    TEIKI2P-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    TEIKI2P-JOBID       TO  JOB-JOBID
           MOVE    TEIKI2P-SHELLID     TO  JOB-SHELLID
           MOVE    "ORCT0020"          TO  JOB-PGID
           MOVE    "帳票印刷処理"
                                       TO  JOB-SHELLMSG
           CALL    "ORCSJOB"               USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
      *
           MOVE    ZERO                TO  FLG-END
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    TEIKI2P-ERRFILE     TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAME   TO  TEIKIERR
           MOVE    SGETTEMP-ST         TO  TEIKIERR-ST
           MOVE    SGETTEMP-MW         TO  SPA-MW
      *
      *    定期請求情報
           INITIALIZE                      SYS-5010-REC
           MOVE    "5010"              TO  SYS-5010-KANRICD
           MOVE    "*"                 TO  SYS-5010-KBNCD
           MOVE    TEIKI2P-SYSYMD      TO  SYS-5010-STYUKYMD
                                           SYS-5010-EDYUKYMD
           MOVE    SYS-5010-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-5010-REC
      *
           INITIALIZE                      SYS-3103-REC
           MOVE    "3103"              TO  SYS-3103-KANRICD
           MOVE    "5010001"           TO  SYS-3103-KBNCD
           MOVE    TEIKI2P-SYSYMD      TO  SYS-3103-STYUKYMD
                                           SYS-3103-EDYUKYMD
           MOVE    SYS-3103-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-3103-REC
      *
           PERFORM 1001-SRH-1041-HEN-SEC
      *
           MOVE    ZERO                TO  FLG-SUMKANANAME
                                           FLG-SUMPTNUM
                                           FLG-SUMTAIIN
           IF    ( TEIKI2P-SYOKBN      =   "1" )
               PERFORM 1001-SRH-BTUBRM-HEN-SEC
           ELSE
               MOVE    1               TO  FLG-SUMKANANAME
                                           FLG-SUMPTNUM
                                           FLG-SUMTAIIN
           END-IF
      *
           IF    ( SPA-MW              =   SPA-GINBEE )
               CALL    "ORCSBILLINGCHECK"  USING   SBC-AREA
                                                   SPA-AREA
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      * 
      *****************************************************************
      *    入金方法
      *****************************************************************
       1001-SRH-1041-HEN-SEC               SECTION.
      *
      *    入金方法
           MOVE    ZERO            TO  SRH-1041-MAX
      *
           INITIALIZE                  SYS-1041-REC
           MOVE    "1041"          TO  SYS-1041-KANRICD
           MOVE    TEIKI2P-SYSYMD  TO  SYS-1041-STYUKYMD
                                       SYS-1041-EDYUKYMD
           MOVE    SYS-1041-REC    TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-1041-REC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
                    OR   ( SRH-1041-MAX    >=  99 )
      *
               COMPUTE SRH-1041-MAX    =   SRH-1041-MAX    +   1
      *
               MOVE    SYS-1041-KBNCD  TO  SRH-1041-KBNCDL
                                                        (SRH-1041-MAX)
      *
               IF    ( SYS-1041-NYKN-JYOTAI-T  =   SPACE )
                   MOVE    "3"     TO  SRH-1041-JYOTAIL (SRH-1041-MAX)
               ELSE
                   MOVE    SYS-1041-NYKN-JYOTAI-T
                                   TO  SRH-1041-JYOTAIL (SRH-1041-MAX)
               END-IF
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
               MOVE    SYSKANRI-REC    TO  SYS-1041-REC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       1001-SRH-1041-HEN-EXT.
           EXIT.
      *****************************************************************
      *    指定印刷病棟・病室番号
      *****************************************************************
       1001-SRH-BTUBRM-HEN-SEC         SECTION.
      *
           PERFORM 900-BTPARA-KEY5-SEL-SEC
      *
           PERFORM UNTIL ( FLG-BTPARA  NOT =   ZERO )
                    OR   ( SRH-BTUBRM-MAX >=   CONST-BTUBRM-MAX )
      *
               MOVE    BTPARA-INFO-PARA    TO  TEIKI2BTPARA
      *
               EVALUATE    TEIKI2BT-BTUNUM
               WHEN    "A1"
                   MOVE    1               TO  FLG-SUMKANANAME
               WHEN    "A2"
                   MOVE    1               TO  FLG-SUMPTNUM
               WHEN    "A3"
                   MOVE    1               TO  FLG-SUMTAIIN
               WHEN    OTHER
                   COMPUTE SRH-BTUBRM-MAX  =   SRH-BTUBRM-MAX  +   1
      *
                   MOVE    TEIKI2BT-BTUNUM     TO  SRH-BTUBRM-BTUNUML
                                                   (SRH-BTUBRM-MAX)
                   MOVE    TEIKI2BT-BRMNUM     TO  SRH-BTUBRM-BRMNUML
                                                   (SRH-BTUBRM-MAX)
               END-EVALUATE
      *
               PERFORM 900-BTPARA-KEY5-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_btpara"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       1001-SRH-BTUBRM-HEN-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           INITIALIZE  ORCHCN03AREA
           MOVE    TEIKI2P-HOSPNUM     TO  ORCHCN03-HEAD-HOSPNUM
           MOVE    TEIKI2P-STAFFCD     TO  ORCHCN03-STAFFCD
           MOVE    TEIKI2P-SYSYMD      TO  ORCHCN03-SYSYMD
           MOVE    TEIKI2P-TERMID      TO  ORCHCN03-TERMID
           INITIALIZE                      ORCSPRTAREA
           MOVE    "INS"                   TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM     TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY    TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP  TO  SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM      TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD     TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID    TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                           TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY   TO  SPRT-PRIORITY
           MOVE    LNK-PRTKANRI-TERMID
                                           TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID       TO  SPRT-OPID
           MOVE    "請求書兼領収書（入院）"
                                           TO  SPRT-TITLE
           MOVE    "1"                     TO  ORCHCN03-PRTKBN
           MOVE    ORCSPRTAREA             TO  ORCHCN03-PRTAREA
           MOVE    6                       TO  ORCHCN03-MOTOID
      *
           MOVE    ZERO            TO  FLG-SELTAIIN
           PERFORM  2001-HENSYU-SEC
      *
           IF    ( TEIKI2P-SUMKBN  =   "1" )
               IF    ( FLG-SUMTAIIN    =   1 )
                   MOVE    1               TO  FLG-SELTAIIN
                   PERFORM  2001-HENSYU-SEC
               END-IF
           END-IF
      *
           MOVE    CNT-OUT1        TO  ORCHCN03-CNT
      *
      *    帳票プログラム名取得
           PERFORM 20011-PRINT-PG-EDIT-SEC
      *
           MOVE    ORCHCN03AREA    TO  WORCHCN03AREA
           PERFORM 2002-SYUGRP-SEC
           MOVE    WORCHCN03AREA   TO  ORCHCN03AREA
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   TSYUGRP-MAX )
      *
               MOVE    TSYUGRP-PTID (IDX0)
                                       TO  WRK-PTID
               PERFORM 900-PTINF-KEY-SEL-SEC
               PERFORM 900-PTNUM-KEY-SEL-SEC
      *
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL ( IDX1    >   TSYUGRP-GRP-MAX (IDX0))
      *
                   PERFORM VARYING IDX-M   FROM    1   BY  1
                           UNTIL ( IDX-M   >   ORCHCN03-CNT )
      *
                       INITIALIZE      ORCHCN03-OCC (IDX-M)
      *
                   END-PERFORM
      *
                   MOVE    "1"     TO  SPA-NYUGAIKBN
      *
                   MOVE    PTINF-PTID      TO  SPA-PTID
                   MOVE    PTNUM-PTNUM     TO  SPA-PTNUM
                   MOVE    PTINF-KANANAME  TO  SPA-KANANAME
                   MOVE    PTINF-NAME      TO  SPA-NAME
                   MOVE    PTINF-SEX       TO  SPA-SEX
                   MOVE    PTINF-BIRTHDAY  TO  SPA-BIRTHDAY
                   MOVE    TSYUGRP-SRYYMD  (IDX0 IDX1)
                                           TO  SPA-SRYYMD
      *
                   IF    ( TSYUGRP-SKYPRT (IDX0 IDX1)  =   "1" )
                       PERFORM 2001-SEIKYU-PRT-SEC
                   END-IF
      *
                   IF    ( TSYUGRP-SRYPRT (IDX0 IDX1)  =   "1" )
                       PERFORM 2001-MEISAI-PRT-SEC
                   END-IF
      *
               END-PERFORM
      *
           END-PERFORM
      *
           MOVE    1                   TO  FLG-END
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求書兼領収書出力処理
      *****************************************************************
       2001-SEIKYU-PRT-SEC                    SECTION.
      *
           MOVE    ZERO        TO  ORCHCN03-CNT
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   TSYUGRP-GRP-MAX2 (IDX0 IDX1))
      *
               PERFORM VARYING IDX-M   FROM    1   BY  1
                       UNTIL ( IDX-M   >   WORCHCN03-CNT )
      *
                   IF    ( TSYUGRP-GRP-DENPNUM (IDX0 IDX1 IDX2)
                               =   WORCHCN03-DENPNUM (IDX-M))
                       COMPUTE ORCHCN03-CNT
                           =   ORCHCN03-CNT
                           +   1
                       MOVE    WORCHCN03-OCC (IDX-M)
                               TO  ORCHCN03-OCC (ORCHCN03-CNT)
                   END-IF
      *
               END-PERFORM
      *
           END-PERFORM
      *
      *    入院請求書印刷
           IF    ( ORCHCN03-CNT    >    ZERO )
      *
               CALL    WRK-SEIKYU-PG    USING   ORCHCN03AREA
                                                SPA-AREA
               ON  EXCEPTION
                   DISPLAY WRK-SEIKYU-PG " was aborted"
               END-CALL
               COMPUTE CNT-OUT2 = CNT-OUT2 + ORCHCN03-OUTCNT
           END-IF
      *
           .
       2001-SEIKYU-PRT-EXT.
           EXIT.
      *****************************************************************
      *    診療費明細書出力処理
      *****************************************************************
       2001-MEISAI-PRT-SEC                    SECTION.
      *
           INITIALIZE              ORCHCN04AREA
           MOVE    TSYUGRP-PTID (IDX0)
                                       TO  ORCHCN04-PTID
      *
           MOVE    PTNUM-PTNUM         TO  ORCHCN04-PTNUM
           MOVE    PTINF-NAME          TO  ORCHCN04-NAME
           MOVE    TSYUGRP-DENPPRTYMD (IDX0 IDX1)
                                       TO  ORCHCN04-DENPPRTYMD
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   TSYUGRP-GRP-MAX2 (IDX0 IDX1) )
      *
               MOVE    TSYUGRP-GRP-DENPNUM (IDX0 IDX1 IDX2)
                                       TO  ORCHCN04-DENPNUM (IDX2)
      *
           END-PERFORM
      *
           MOVE    "1"                 TO  ORCHCN04-PRTKBN
      *
           MOVE    WRK-MEISAI-PRTNM    TO  SPRT-PRTNM
           MOVE    TSYUGRP-PTID (IDX0) TO  SPRT-PTID
           MOVE    TSYUGRP-SRYYMD  (IDX0 IDX1)(1:6)
                                       TO  SPRT-SRYYM
           MOVE    ORCSPRTAREA         TO  ORCHCN04-PRTAREA
      *
           CALL    WRK-MEISAI-PG    USING
                                    SPA-AREA
                                    ORCHCN04AREA
           ON  EXCEPTION
               DISPLAY WRK-MEISAI-PG " was aborted"
           END-CALL
           COMPUTE CNT-OUT2 = CNT-OUT2 + ORCHCN04-PAGE
      *
           .
       2001-MEISAI-PRT-EXT.
           EXIT.
      *****************************************************************
      *   編集処理
      *****************************************************************
       2001-HENSYU-SEC                 SECTION.
      *
           PERFORM 900-QUERY-TEIKI-SEL-SEC
      *
           PERFORM UNTIL ( FLG-QTEIKI  NOT =   ZERO )
      *
               MOVE    ZERO                TO  FLG-TAISYO
      *
               IF    ( FLG-SELTAIIN    =   ZERO )
                   PERFORM 20011-PRTJYUN-CHK-SEC
               ELSE
                   MOVE    1               TO  FLG-TAISYO
               END-IF
      *
               IF    ( FLG-TAISYO     =    1 )
                   PERFORM 20011-HENSYU-SEC
               END-IF
      *
               PERFORM 900-QUERY-TEIKI-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "query_teiki"   TO  MCP-TABLE
           MOVE    WRK-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2001-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *   出力順チェック処理
      *****************************************************************
       20011-PRTJYUN-CHK-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-TAISYO
      *
           IF    ( TEIKI2P-SYOKBN  =   "0" )
                           MOVE    1   TO  FLG-TAISYO
           ELSE
               EVALUATE    TEIKI2P-PRTJYUN
               WHEN    "01"
               WHEN    "02"
                   PERFORM VARYING SRH-IDX FROM    1   BY  1
                           UNTIL ( SRH-IDX     >   SRH-BTUBRM-MAX )
                            OR   ( FLG-TAISYO  =   1 )
                       IF    ( SRH-BTUBRM-BTUNUML (SRH-IDX)
                                               =   QTEIKI-BTUNUM )
                           MOVE    1   TO  FLG-TAISYO
                       END-IF
                   END-PERFORM
               WHEN    "03"
                   IF    ( FLG-SUMKANANAME =   1 )
                           MOVE    1   TO  FLG-TAISYO
                   END-IF
               WHEN    "04"
                   IF    ( FLG-SUMPTNUM    =   1 )
                           MOVE    1   TO  FLG-TAISYO
                   END-IF
               WHEN    "05"
               WHEN    "06"
                   PERFORM VARYING SRH-IDX FROM    1   BY  1
                           UNTIL ( SRH-IDX     >   SRH-BTUBRM-MAX )
                            OR   ( FLG-TAISYO  =   1 )
                       IF    ( SRH-BTUBRM-BRMNUML (SRH-IDX)
                                               =   QTEIKI-BRMNUM )
                           MOVE    1   TO  FLG-TAISYO
                       END-IF
                   END-PERFORM
               END-EVALUATE
           END-IF
      *
           .
       20011-PRTJYUN-CHK-EXT.
           EXIT.
      *****************************************************************
      *   帳票プログラム名編集処理
      *****************************************************************
       20011-PRINT-PG-EDIT-SEC         SECTION.
      *
           INITIALIZE                      WRK-SEIKYU-PG
      *
           IF    ( SPA-MW                      =   SPA-GINBEE )
            AND  ( SBC-BILLINGUSER             =   "1" )
            AND  ( SYS-3103-PROGRAM-NAME (1)   NOT =   SPACE )
               MOVE    SYS-3103-PROGRAM-NAME (1)
                                           TO  WRK-SEIKYU-PG
           ELSE
               INITIALIZE                      ORCSPRTNMAREA
               MOVE    "1"                 TO  ORCSPRTNM-KBN
               MOVE    "00000003"          TO  ORCSPRTNM-KBNCD
               MOVE    TEIKI2P-SYSYMD      TO  ORCSPRTNM-SRYYMD
               MOVE    "1"                 TO  ORCSPRTNM-NYUGAIKBN
               CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYSKANRI-REC
               IF    ( ORCSPRTNM-RC        =   ZERO )
                  MOVE    ORCSPRTNM-PRTPG  TO  WRK-SEIKYU-PG
               ELSE
                  MOVE    "ORCHCN03"       TO  WRK-SEIKYU-PG
               END-IF
           END-IF
      *
           INITIALIZE                      WRK-MEISAI-PG
      *
           IF    ( SPA-MW                      =   SPA-GINBEE )
            AND  ( SBC-BILLINGUSER             =   "1" )
            AND  ( SYS-3103-PROGRAM-NAME (2)   NOT =   SPACE )
               MOVE    SYS-3103-PROGRAM-NAME (2)
                                           TO  WRK-MEISAI-PG
           ELSE
      *
               INITIALIZE                      ORCSPRTNMAREA
               MOVE    "1"                 TO  ORCSPRTNM-KBN
               MOVE    "00000008"          TO  ORCSPRTNM-KBNCD
               MOVE    TEIKI2P-SYSYMD      TO  ORCSPRTNM-SRYYMD
               MOVE    "1"                 TO  ORCSPRTNM-NYUGAIKBN
               CALL    "ORCSPRTNM"         USING
                                               ORCSPRTNMAREA
                                               SPA-AREA
                                               SYSKANRI-REC
               IF    ( ORCSPRTNM-RC        =   ZERO )
                  MOVE    ORCSPRTNM-PRTPG  TO  WRK-MEISAI-PG
               ELSE
                  MOVE    "ORCHCN04"       TO  WRK-MEISAI-PG
               END-IF
           END-IF
      *
      *    出力先プリンタ名割り当て情報編集
           INITIALIZE                      SYSKANRI-REC
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "2"                 TO  ORCSPRTNM-KBN
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "1"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYSKANRI-REC
           MOVE    SYSKANRI-REC        TO  SYS-1034-REC
           MOVE    SYS-1034-PRTNM(09)  TO  WRK-MEISAI-PRTNM
      *
           .
       20011-PRINT-PG-EDIT-EXT.
           EXIT.
      *****************************************************************
      *   編集処理
      *****************************************************************
       20011-HENSYU-SEC              SECTION.
      *
           MOVE    QTEIKI-PTID         TO  WRK-PTID
      *
      *    患者情報を検索する
           PERFORM 900-PTINF-KEY-SEL-SEC
      *
      *    患者番号を検索する
           PERFORM 900-PTNUM-KEY-SEL-SEC
      *
      *    入金方法取得
           PERFORM 200111-NYUKIN-HOHO-HEN-SEC
      *
      *    収納データを検索する
           PERFORM 900-SYUNOU-KEY-SEL-SEC
           IF    ( FLG-SYUNOU      NOT =   ZERO )
                MOVE  "0002"           TO  ERR-ERRCD
                PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
      *    患者定期請求履歴データを検索する
           PERFORM 900-PTTEIKIRRK-KEY6-SEL-SEC
           IF    ( FLG-PTTEIKIRRK  NOT =   ZERO )
               MOVE  "0003"            TO  ERR-ERRCD
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           INITIALIZE              SYUMEI-REC
           MOVE    PTTEIKIRRK-HOSPNUM       TO  SMEI-HOSPNUM
           MOVE    "1"                     TO  SMEI-NYUGAIKBN
           MOVE    PTTEIKIRRK-PTID         TO  SMEI-PTID
           MOVE    PTTEIKIRRK-DENPNUM      TO  SMEI-DENPNUM
           MOVE    PTTEIKIRRK-DENPEDANUM   TO  SMEI-DENPEDANUM
           PERFORM 900-SYUMEI-KEY-SEL-SEC
      *
           COMPUTE CNT-OUT1    =   CNT-OUT1    +   1
      *
      *    請求書サブ患者情報パラメタ設定
           MOVE    SYU-SKYSTYMD        TO  ORCHCN03-KJNYMD    (CNT-OUT1)
           MOVE    TEIKI2P-HOSPNUM     TO  ORCHCN03-HOSPNUM   (CNT-OUT1)
           MOVE    QTEIKI-PTID         TO  ORCHCN03-PTID      (CNT-OUT1)
           MOVE    PTNUM-PTNUM         TO  ORCHCN03-PTNUM     (CNT-OUT1)
           MOVE    "1"                 TO  ORCHCN03-NYUGAIKBN (CNT-OUT1)
      *
           MOVE    SYU-DENPNUM         TO  ORCHCN03-DENPNUM   (CNT-OUT1)
           MOVE    PTTEIKIRRK-SKYMONEY-TAX-SAI
                                       TO  ORCHCN03-SEIKYU-TAX-SAI
                                                              (CNT-OUT1)
           MOVE    PTTEIKIRRK-SKYMONEY TO  ORCHCN03-SEIKYU    (CNT-OUT1)
      *
           MOVE    ZERO                TO  ORCHCN03-NYUKIN    (CNT-OUT1)
      *
           IF    ( WRK-NYUKIN-KBN      =   "2" )
            AND  ( WRK-NYUKINPRT-KBN   =   "2" )
               IF    ( PTTEIKIRRK-SKYMONEY >   ZERO )
                   MOVE    SMEI-NYUHEN-MONEY
                                       TO  ORCHCN03-NYUKIN    (CNT-OUT1)
               END-IF
           END-IF
      *
           IF    ( PTTEIKIRRK-SKYUPDFLG    =   1 )
               MOVE    2               TO  ORCHCN03-HAKKOFLG  (CNT-OUT1)
           ELSE
               MOVE    ZERO            TO  ORCHCN03-HAKKOFLG  (CNT-OUT1)
           END-IF
      *
           COMPUTE ORCHCN03-KONMISYU (CNT-OUT1)
               = ( SYU-SKYMONEY        -   SYU-NYUKIN-TOTAL )
               - ( PTTEIKIRRK-SKYMONEY -   SMEI-NYUHEN-MONEY )
      *
           .
       20011-HENSYU-EXT.
           EXIT. 
      *
      *****************************************************************
      *    入金方法編集処理
      *****************************************************************
       200111-NYUKIN-HOHO-HEN-SEC      SECTION.
      *
           MOVE    "01"            TO  WRK-NYUKIN-HOHO
           MOVE    "3"             TO  WRK-NYUKIN-JYOTAI
           MOVE    SPACE           TO  WRK-NYUKIN-KBN
           MOVE    SPACE           TO  WRK-NYUKINPRT-KBN
      *
           IF    ( PTINF-NYUKIN-HOHO   NOT =   SPACE )
               MOVE    PTINF-NYUKIN-HOHO
                                   TO  WRK-NYUKIN-HOHO
           END-IF
      *
           MOVE    SPACE           TO  SRH-1041-JYOTAI
           PERFORM VARYING SRH-IDX FROM    1   BY  1
                   UNTIL ( SRH-IDX >   SRH-1041-MAX )
                    OR   ( SRH-1041-JYOTAI NOT =   SPACE )
      *
               IF    ( WRK-NYUKIN-HOHO =   SRH-1041-KBNCDL (SRH-IDX))
                   MOVE    SRH-1041-JYOTAIL (SRH-IDX)
                                   TO  SRH-1041-JYOTAI
               END-IF
      *
           END-PERFORM
      *
           IF    ( SRH-1041-JYOTAI NOT =   SPACE )
               MOVE    SRH-1041-JYOTAI TO  WRK-NYUKIN-JYOTAI
           END-IF
      *
           EVALUATE    WRK-NYUKIN-JYOTAI
           WHEN    "1"
               MOVE    "2"             TO  WRK-NYUKIN-KBN
               IF    ( SYS-5010-NYUKINKBN  =   "2" )
                   MOVE    SYS-5010-NYUKINPRTKBN
                                       TO  WRK-NYUKINPRT-KBN
               ELSE
                   MOVE    "2"         TO  WRK-NYUKINPRT-KBN
               END-IF
           WHEN    "2"
               MOVE    "1"             TO  WRK-NYUKIN-KBN
           WHEN    "3"
               MOVE    SYS-5010-NYUKINKBN
                                       TO  WRK-NYUKIN-KBN
               MOVE    SYS-5010-NYUKINPRTKBN
                                       TO  WRK-NYUKINPRT-KBN
           WHEN    OTHER
               MOVE    SYS-5010-NYUKINKBN
                                       TO  WRK-NYUKIN-KBN
               MOVE    SYS-5010-NYUKINPRTKBN
                                       TO  WRK-NYUKINPRT-KBN
           END-EVALUATE
      *
           .
       200111-NYUKIN-HOHO-HEN-EXT.
           EXIT. 
      *****************************************************************
      *   収納データ集計処理
      *****************************************************************
       2002-SYUGRP-SEC                  SECTION.
      *
           COMPUTE IDX-M   =   1
      *
      *    患者定期履歴検索処理
           PERFORM 901-PTTEIKIRRK-KEY6-SEL-SEC
      *
           PERFORM UNTIL ( IDX-M   >   ORCHCN03-CNT )
      *
               PERFORM 20021-SYUGRP-SEC
      *
           END-PERFORM
      *
           .
       2002-SYUGRP-EXT.
           EXIT. 
      *****************************************************************
      *   収納データ集計処理
      *****************************************************************
       20021-SYUGRP-SEC                SECTION.
      *
           MOVE    ZERO                TO     IDX-MAX
           COMPUTE TSYUGRP-MAX
               =   TSYUGRP-MAX     +      1
      *
           MOVE    ORCHCN03-PTID (IDX-M)
                                       TO  TSYUGRP-PTID
                                                  (TSYUGRP-MAX)
      *
           MOVE    ORCHCN03-PTID (IDX-M)
                                       TO      KEY-PTID
           MOVE    PTTEIKIRRK-SRYYM    TO      KEY-SRYYM
           MOVE    PTTEIKIRRK-KBT      TO      KEY-KBT
      *
           PERFORM UNTIL ( IDX-M   >   ORCHCN03-CNT )
                    OR   ( ORCHCN03-PTID (IDX-M)
                               NOT =   KEY-PTID )
                    OR   ( PTTEIKIRRK-SRYYM
                               NOT =   KEY-SRYYM )
                    OR   ( PTTEIKIRRK-KBT
                               NOT =   KEY-KBT )
      *
      *        収納マスタ検索処理
               PERFORM 901-SYUNOU-KEY-SEL-SEC
      *
               INITIALIZE              ORCHCN03S02AREA
               MOVE    IDX-M       TO  ORCHCN03S02-IDX
               MOVE    1           TO  ORCHCN03S02-CHKPRT
               MOVE    TEIKI2P-PTINFREF
                                   TO  ORCHCN03S02-PTINFREF
               MOVE    TEIKI2P-SKYPRT
                                   TO  ORCHCN03S02-SKYPRT
               MOVE    TEIKI2P-SRYPRT
                                   TO  ORCHCN03S02-SRYPRT
               CALL    "ORCHCN03S02"   USING
                                       ORCHCN03AREA
                                       ORCHCN03S02AREA
                                       SYUNOU-REC
                                       SPA-AREA
               IF    ( ORCHCN03S02-PRTFLG  =   1 )
      *
                   COMPUTE IDX-MAX =   IDX-MAX +   1
      *
                   DISPLAY "PTID[" PTTEIKIRRK-PTID "]"
                           "DENPNUM["  PTTEIKIRRK-DENPNUM "]"
                           "ORCHCN03S02-SKYPRT[" ORCHCN03S02-SKYPRT "]"
                           "ORCHCN03S02-SRYPRT[" ORCHCN03S02-SRYPRT "]"
                   MOVE    SYU-DENPPRTYMD
                                   TO  TSYUGRP-DENPPRTYMD
                                                  (TSYUGRP-MAX IDX-MAX)
                   MOVE    ORCHCN03S02-SKYPRT
                                   TO  TSYUGRP-SKYPRT
                                                  (TSYUGRP-MAX IDX-MAX)
                   MOVE    ORCHCN03S02-SRYPRT
                                   TO  TSYUGRP-SRYPRT
                                                  (TSYUGRP-MAX IDX-MAX)
                   MOVE    ORCHCN03S02-GRP-CNT
                                   TO  TSYUGRP-GRP-MAX2
                                           (TSYUGRP-MAX IDX-MAX)
                   PERFORM VARYING IDX0    FROM    1   BY  1
                           UNTIL ( IDX0    >   ORCHCN03S02-GRP-CNT )
                       MOVE    ORCHCN03S02-GRP-DENPNUM (IDX0)
                                   TO  TSYUGRP-GRP-DENPNUM
                                           (TSYUGRP-MAX IDX-MAX IDX0)
                       MOVE    ORCHCN03-KJNYMD (IDX0)
                                   TO  TSYUGRP-SRYYMD
                                           (TSYUGRP-MAX IDX-MAX)
                   END-PERFORM
               END-IF
      *
               COMPUTE IDX-M   =   IDX-M   +   1
      *
      *        患者定期履歴検索処理
               PERFORM 901-PTTEIKIRRK-KEY6-SEL-SEC
      *
           END-PERFORM
      *
           MOVE    IDX-MAX             TO  TSYUGRP-GRP-MAX (TSYUGRP-MAX)
      *
           .
       20021-SYUGRP-EXT.
           EXIT. 
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC              SECTION.
      *
           PERFORM 800-GET-ERRMSG-SEC
      *
           OPEN    OUTPUT              TEIKIERR-FILE
      *
           MOVE    ERR-ERRMSG      TO  TEIKIERR-REC
           WRITE   TEIKIERR-REC
           CLOSE   TEIKIERR-FILE
      *
      *    ジョブ管理終了処理
           MOVE    "JBE"           TO  SJOBKANRI-MODE
      *
           INITIALIZE                  JOBKANRI-REC
           MOVE    TEIKI2P-HOSPNUM TO  JOB-HOSPNUM
           MOVE    TEIKI2P-JOBID   TO  JOB-JOBID
           MOVE    TEIKI2P-SHELLID
                                   TO  JOB-SHELLID
           MOVE    ERR-ERRMSG      TO  JOB-YOBI
           MOVE    ERR-ERRCD       TO  JOB-ERRCD
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           MOVE    1               TO  FLG-ERR
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
      *    プログラム終了処理
           PERFORM 990-PROGRAM-FINISH-SEC
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ取得処理
      *****************************************************************
       800-GET-ERRMSG-SEC              SECTION.
      *
           EVALUATE    ERR-ERRCD
           WHEN    "0001"
               MOVE    ERR-MSG01       TO  ERR-ERRMSG
           WHEN    "0002"
               MOVE    ERR-MSG02       TO  ERR-ERRMSG
           WHEN    "0003"
               MOVE    ERR-MSG03       TO  ERR-ERRMSG
           WHEN    "0004"
               MOVE    ERR-MSG04       TO  ERR-ERRMSG
           END-EVALUATE
      *
           .
       800-GET-ERRMSG-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           IF    ( CNT-OUT2            =   ZERO  )
                MOVE  "0004"           TO  ERR-ERRCD
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    TEIKI2P-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    TEIKI2P-JOBID   TO  JOB-JOBID
           MOVE    TEIKI2P-SHELLID TO  JOB-SHELLID
           MOVE    CNT-OUT2        TO  JOB-UPDCNT
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           DISPLAY "TEIKI02  CNT " CNT-OUT1 
           DISPLAY "*** ORCT0020 END ***"
      ***  ACCEPT  WRK-IN FROM CONSOLE 
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納マスター読込（主キー）
      *****************************************************************
       900-SYUNOU-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
      *
           INITIALIZE                      SYUNOU-REC
           MOVE    TEIKI2P-HOSPNUM      TO  SYU-HOSPNUM
           MOVE    "1"                 TO  SYU-NYUGAIKBN
           MOVE    QTEIKI-PTID         TO  SYU-PTID
           MOVE    QTEIKI-DENPNUM      TO  SYU-DENPNUM
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               MOVE    1               TO  FLG-SYUNOU
               INITIALIZE                  SYUNOU-REC
           END-IF
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYUNOU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    収納明細読込（主キー）
      *****************************************************************
       900-SYUMEI-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUMEI
      *
           INITIALIZE                  SYUMEI-REC
           MOVE    PTTEIKIRRK-HOSPNUM   TO  SMEI-HOSPNUM
           MOVE    "1"                 TO  SMEI-NYUGAIKBN
           MOVE    PTTEIKIRRK-PTID     TO  SMEI-PTID
           MOVE    PTTEIKIRRK-DENPNUM  TO  SMEI-DENPNUM
           MOVE    PTTEIKIRRK-DENPEDANUM
                                       TO  SMEI-DENPEDANUM
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUMEI-REC
           ELSE
               MOVE    1               TO  FLG-SYUMEI
               INITIALIZE                  SYUMEI-REC
           END-IF
      *
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYUMEI-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者情報検索処理(KEY)
      *****************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTINF
      *
           INITIALIZE                      PTINF-REC
           MOVE    TEIKI2P-HOSPNUM      TO  PTINF-HOSPNUM
           MOVE    WRK-PTID             TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTINF-REC
           ELSE
               MOVE    1               TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者番号検索処理(KEY)
      *****************************************************************
       900-PTNUM-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNUM
      *
           INITIALIZE                      PTNUM-REC
           MOVE    TEIKI2P-HOSPNUM      TO  PTNUM-HOSPNUM
           MOVE    WRK-PTID            TO  PTNUM-PTID
           MOVE    PTNUM-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNUM-REC
           ELSE
               MOVE    1               TO  FLG-PTNUM
               INITIALIZE                  PTNUM-REC
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
      *    システム管理マスタ読込(KEY10)
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    TEIKI2P-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ読込(KEY2)
      *****************************************************************
       900-SYSKANRI-KEY2-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    TEIKI2P-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ読込(KEY2)
      *****************************************************************
       900-SYSKANRI-KEY2-FET-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    患者定期請求履歴検索処理(KEY6)
      *****************************************************************
       900-PTTEIKIRRK-KEY6-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-PTTEIKIRRK
      *
           INITIALIZE                  PTTEIKIRRK-REC
           MOVE    TEIKI2P-HOSPNUM     TO  PTTEIKIRRK-HOSPNUM
           MOVE    QTEIKI-PTID         TO  PTTEIKIRRK-PTID
           MOVE    QTEIKI-DENPNUM      TO  PTTEIKIRRK-DENPNUM
           MOVE    PTTEIKIRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptteikirrk"    TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTTEIKIRRK-REC
           ELSE
               INITIALIZE                  PTTEIKIRRK-REC
               MOVE    1               TO  FLG-PTTEIKIRRK
           END-IF
      *
           MOVE    "tbl_ptteikirrk"    TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTTEIKIRRK-KEY6-SEL-EXT.
           EXIT.
      *****************************************************************
      *    バッチパラメタ検索処理(KEY5)
      *****************************************************************
       900-BTPARA-KEY5-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-BTPARA
           INITIALIZE                      BTPARA-REC
           MOVE    TEIKI2P-HOSPNUM         TO  BTPARA-HOSPNUM
           MOVE    TEIKI2P-JOBID       TO  BTPARA-JOBID
           MOVE    TEIKI2P-SHELLID     TO  BTPARA-SHELLID
           MOVE    BTPARA-REC          TO  MCPDATA-REC
           MOVE    "tbl_btpara"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  BTPARA-REC
           ELSE
               INITIALIZE                  BTPARA-REC
               MOVE    1               TO  FLG-BTPARA
           END-IF
      *
           .
       900-BTPARA-KEY5-SEL-EXT.
           EXIT.
      *****************************************************************
      *    バッチパラメタFETCH処理(KEY5)
      *****************************************************************
       900-BTPARA-KEY5-FET-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-BTPARA
      *
           MOVE    "tbl_btpara"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  BTPARA-REC
           ELSE
               INITIALIZE                  BTPARA-REC
               MOVE    1               TO  FLG-BTPARA
           END-IF
      *
           .
       900-BTPARA-KEY5-FET-EXT.
           EXIT.
      *****************************************************************
      *    定期請求クエリ検索処理
      *****************************************************************
       900-QUERY-TEIKI-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-QTEIKI
           INITIALIZE                  QTEIKI-REC
      *
           IF    ( TEIKI2P-PRTKBN  =   "3" )
               MOVE    TEIKI2P-SRYYM   TO  QTEIKI-SRYYM
               MOVE    ZERO            TO  CNT-SELKBT
               PERFORM VARYING IDX-SELKBT  FROM    1   BY  1
                       UNTIL ( IDX-SELKBT  >   3 )
                   IF    ( TEIKI2P-KBT (IDX-SELKBT)    =   "1" )
                       COMPUTE CNT-SELKBT
                           =   CNT-SELKBT  +   1
                       IF    ( CNT-SELKBT  =   1 )
                           MOVE    CONST-KBT (IDX-SELKBT)
                                   TO  QTEIKI-SELKBT (1)
                                       QTEIKI-SELKBT (2)
                                       QTEIKI-SELKBT (3)
                       ELSE
                           MOVE    CONST-KBT (IDX-SELKBT)
                                   TO  QTEIKI-SELKBT (CNT-SELKBT)
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
           IF    ( TEIKI2P-SUMKBN      =   "1" )
               IF    ( FLG-SELTAIIN    =   ZERO )
                   MOVE    "99999999"  TO  QTEIKI-SELTAIINYMD(1)
                                           QTEIKI-SELTAIINYMD(2)
               ELSE
                   MOVE    "1%"        TO  QTEIKI-SELTAIINYMD(1)
                   MOVE    "2%"        TO  QTEIKI-SELTAIINYMD(2)
               END-IF
           ELSE
                   MOVE    "%"         TO  QTEIKI-SELTAIINYMD(1)
                                           QTEIKI-SELTAIINYMD(2)
           END-IF
           MOVE    TEIKI2P-HOSPNUM TO  QTEIKI-HOSPNUM
           MOVE    QTEIKI-REC      TO  MCPDATA-REC
           PERFORM 9001-EDIT-QTEIKI-KEY-SEC
           IF    ( WRK-PATHNAME    =   SPACE )
                MOVE  "0001"           TO  ERR-ERRCD
                PERFORM 500-ERR-HENSYU-SEC
           END-IF
           MOVE    "query_teiki"   TO  MCP-TABLE
           MOVE    WRK-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  QTEIKI-REC
           ELSE
               MOVE    1           TO  FLG-QTEIKI
               INITIALIZE              QTEIKI-REC
           END-IF
      *
           .
       900-QUERY-TEIKI-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    定期請求クエリキー選択処理
      *****************************************************************
       9001-EDIT-QTEIKI-KEY-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-PATHNAME
      *
           IF    ( TEIKI2P-PRTKBN  =   "1" OR "2" )
               EVALUATE    TEIKI2P-PRTJYUN
               WHEN    "01"
                   MOVE    "key3"      TO  WRK-PATHNAME
               WHEN    "02"
                   MOVE    "key4"      TO  WRK-PATHNAME
               WHEN    "03"
                   MOVE    "key5"      TO  WRK-PATHNAME
               WHEN    "04"
                   MOVE    "key6"      TO  WRK-PATHNAME
               WHEN    "05"
                   MOVE    "key8"      TO  WRK-PATHNAME
               WHEN    "06"
                   MOVE    "key10"     TO  WRK-PATHNAME
               END-EVALUATE
           ELSE
               EVALUATE    TEIKI2P-PRTJYUN
               WHEN    "01"
                   MOVE    "key13"     TO  WRK-PATHNAME
               WHEN    "02"
                   MOVE    "key14"     TO  WRK-PATHNAME
               WHEN    "03"
                   MOVE    "key15"     TO  WRK-PATHNAME
               WHEN    "04"
                   MOVE    "key16"     TO  WRK-PATHNAME
               WHEN    "05"
                   MOVE    "key18"     TO  WRK-PATHNAME
               WHEN    "06"
                   MOVE    "key20"     TO  WRK-PATHNAME
               END-EVALUATE
           END-IF
      *
           IF    ( TEIKI2P-SUMKBN      =   "1" )
            AND  ( FLG-SELTAIIN        =   1   )
               IF    ( TEIKI2P-PRTKBN  =   "1" OR "2" )
                   EVALUATE    TEIKI2P-PRTJYUN
                   WHEN    "01"
                   WHEN    "03"
                   WHEN    "05"
                   WHEN    "06"
                       MOVE    "key5"  TO  WRK-PATHNAME
                   WHEN    "02"
                   WHEN    "04"
                       MOVE    "key6"  TO  WRK-PATHNAME
                   END-EVALUATE
               ELSE
                   EVALUATE    TEIKI2P-PRTJYUN
                   WHEN    "01"
                   WHEN    "03"
                   WHEN    "05"
                   WHEN    "06"
                       MOVE    "key15" TO  WRK-PATHNAME
                   WHEN    "02"
                   WHEN    "04"
                       MOVE    "key16" TO  WRK-PATHNAME
                   END-EVALUATE
               END-IF
           END-IF
      *
           .
       9001-EDIT-QTEIKI-KEY-EXT.
           EXIT.
      *****************************************************************
      *    定期請求クエリFETCH処理
      *****************************************************************
       900-QUERY-TEIKI-FET-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-QTEIKI
      *
           MOVE    "query_teiki"   TO  MCP-TABLE
           MOVE    WRK-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  QTEIKI-REC
           ELSE
               MOVE    1           TO  FLG-QTEIKI
               INITIALIZE              QTEIKI-REC
           END-IF
      *
           .
       900-QUERY-TEIKI-FET-EXT.
           EXIT.
      *****************************************************************
      *    患者定期請求履歴検索処理
      *****************************************************************
       901-PTTEIKIRRK-KEY6-SEL-SEC     SECTION.
      *
           MOVE    ZERO            TO  FLG-PTTEIKIRRK
      *
           INITIALIZE                  PTTEIKIRRK-REC
           MOVE    SPA-HOSPNUM     TO  PTTEIKIRRK-HOSPNUM
           MOVE    ORCHCN03-PTID (IDX-M)
                                   TO  PTTEIKIRRK-PTID
           MOVE    ORCHCN03-DENPNUM (IDX-M)
                                   TO  PTTEIKIRRK-DENPNUM
           MOVE    PTTEIKIRRK-REC  TO  MCPDATA-REC
           MOVE    "tbl_ptteikirrk" TO MCP-TABLE
           MOVE    "key6"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTTEIKIRRK-REC
           ELSE
               INITIALIZE              PTTEIKIRRK-REC
               MOVE    1           TO  FLG-PTTEIKIRRK
           END-IF
      *
           MOVE    "tbl_ptteikirrk"    TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       901-PTTEIKIRRK-KEY6-SEL-EXT.
           EXIT.
      *****************************************************************
      *    収納マスタ検索処理
      *****************************************************************
       901-SYUNOU-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
      *
      *    収納マスタ検索
           INITIALIZE                  SYUNOU-REC
           MOVE    ORCHCN03-HOSPNUM (IDX-M)
                                       TO  SYU-HOSPNUM
           MOVE    ORCHCN03-NYUGAIKBN (IDX-M)
                                       TO  SYU-NYUGAIKBN
           MOVE    ORCHCN03-PTID (IDX-M)
                                       TO  SYU-PTID
           MOVE    ORCHCN03-DENPNUM (IDX-M)
                                       TO  SYU-DENPNUM
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       901-SYUNOU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
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
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHは行わない)
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
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
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *****************************************************************
      *    プログラム終了処理
      *****************************************************************
       990-PROGRAM-FINISH-SEC          SECTION.
      *
            STOP   RUN
      *
           .
       990-PROGRAM-FINISH-EXT.
           EXIT.
