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
       PROGRAM-ID.                 ORCBGS110.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 新保険別請求チェック表 作表用データ作成
      *  管理者            :
      *  作成日付    作業者        記述
      *  11/01/13    NACL-太田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    請求ファイル
           SELECT  BGSSKY-FILE     ASSIGN  ASS-BGSSKY
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  BGSSKY-KEY
                                   FILE    STATUS  IS  STS-BGSSKY.
      *
      *    公費ファイル
           SELECT  BGSKOH-FILE     ASSIGN  ASS-BGSKOH
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  BGSKOH-KEY
                                   FILE    STATUS  IS  STS-BGSKOH.
      *
      *    保険者別請求ファイル
           SELECT  BGSHKNJA-FILE  ASSIGN  ASS-BGSHKNJA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  BGSHKNJA-KEY
                                   FILE    STATUS  IS  STS-BGSHKNJA.
      *
      *    その他請求ファイル
           SELECT  BGSETC-FILE     ASSIGN  ASS-BGSETC
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  BGSETC-KEY
                                   FILE    STATUS  IS  STS-BGSETC.
      *
      *    レセプト別請求ファイル
           SELECT  BGSMEI-FILE     ASSIGN  ASS-BGSMEI
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  BGSMEI-KEY
                                   FILE    STATUS  IS  STS-BGSMEI.
      *
      *    エラーファイル
           SELECT  BGS100ERR-FILE  ASSIGN  ASS-BGS100ERR
                                   FILE    STATUS  IS  STS-BGS100ERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    請求ファイル
       FD  BGSSKY-FILE.
       01  BGSSKY-REC.
           COPY    "CPBGSF01.INC"  REPLACING  //BGSF01-//
                                   BY         //BGSSKY-//.
      *
      *    公費ファイル
       FD  BGSKOH-FILE.
       01  BGSKOH-REC.
           COPY    "CPBGSF01.INC"  REPLACING  //BGSF01-//
                                   BY         //BGSKOH-//.
      *
      *    保険者別請求ファイル
       FD  BGSHKNJA-FILE.
       01  BGSHKNJA-REC.
           COPY    "CPBGSF01.INC"  REPLACING  //BGSF01-//
                                   BY         //BGSHKNJA-//.
      *
      *    その他請求ファイル
       FD  BGSETC-FILE.
       01  BGSETC-REC.
           COPY    "CPBGSF01.INC"  REPLACING  //BGSF01-//
                                   BY         //BGSETC-//.
      *
      *    レセプト別請求ファイル
       FD  BGSMEI-FILE.
       01  BGSMEI-REC.
           COPY    "CPBGSF01.INC"  REPLACING  //BGSF01-//
                                   BY         //BGSMEI-//.
      *
      *    エラーファイル
       FD  BGS100ERR-FILE.
       01  BGS100ERR-REC           PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
           COPY "CPASGNPARA.INC"   REPLACING  //ASGNPARA//
                                   BY         //ASS-BGSSKY//.
      *
           COPY "CPASGNPARA.INC"   REPLACING  //ASGNPARA//
                                   BY         //ASS-BGSKOH//.
      *
           COPY "CPASGNPARA.INC"   REPLACING  //ASGNPARA//
                                   BY         //ASS-BGSHKNJA//.
      *
           COPY "CPASGNPARA.INC"   REPLACING  //ASGNPARA//
                                   BY         //ASS-BGSETC//.
      *
           COPY "CPASGNPARA.INC"   REPLACING  //ASGNPARA//
                                   BY         //ASS-BGSMEI//.
      *
           COPY "CPASGNPARA.INC"   REPLACING  //ASGNPARA//
                                   BY         //ASS-BGS100ERR//.
      *
       01  CNT-AREA.
           03  CNT-SYAHKN          PIC 9(08).
           03  CNT-SYAKOH          PIC 9(08).
           03  CNT-KOKHKN          PIC 9(08).
           03  CNT-KOKKOH          PIC 9(08).
           03  CNT-KOKHKNJA        PIC 9(08).
           03  CNT-KOUHKN          PIC 9(08).
           03  CNT-RSIHKN          PIC 9(08).
           03  CNT-JBIHKN          PIC 9(08).
           03  CNT-KGIHKN          PIC 9(08).
           03  CNT-KOUKOH          PIC 9(08).
           03  CNT-KOUHKNJA        PIC 9(08).
           03  CNT-SYAMEI          PIC 9(08).
           03  CNT-KOKMEI          PIC 9(08).
           03  CNT-KOUMEI          PIC 9(08).
           03  CNT-RSIMEI          PIC 9(08).
           03  CNT-JBIMEI          PIC 9(08).
           03  CNT-KGIMEI          PIC 9(08).
           03  CNT-DSNMEI          PIC 9(08).
           03  CNT-TKYMEI          PIC 9(08).
           03  CNT-BGSMEI          PIC 9(08).
      *
       01  STS-AREA.
           03  STS-BGSSKY          PIC X(02).
           03  STS-BGSKOH          PIC X(02).
           03  STS-BGSHKNJA        PIC X(02).
           03  STS-BGSETC          PIC X(02).
           03  STS-BGSMEI          PIC X(02).
           03  STS-BGS100ERR       PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-BGSSKY          PIC 9(01).
           03  FLG-BGSKOH          PIC 9(01).
           03  FLG-BGSHKNJA        PIC 9(01).
           03  FLG-BGSETC          PIC 9(01).
           03  FLG-BGSMEI          PIC 9(01).
           03  FLG-SEIKYU          PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-LOOPEND         PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                PIC 9(05).
           03  IDX1                PIC 9(05).
           03  IDX-KOH             PIC 9(05).
           03  IDX-NG              PIC 9(05).
           03  IDX-HKN             PIC 9(05).
           03  IDXH                PIC 9(05).
      *
       01  WRK-AREA.
           03  WRK-TABLE           PIC X(64).
           03  WRK-PATHNAME        PIC X(64).
           03  WRK-ERRMSG          PIC X(200).
           03  WRK-HOKEN           PIC 9(2).
           03  WRK-KOHKBN          PIC X(1).
           03  WRK-KOHNUM          PIC X(3).
           03  WRK-TTLID           PIC 9(5).
           03  WRK-RECEKA          PIC X(2).
           03  WRK-ST              PIC 9(5).
           03  WRK-GROUPID         PIC 9(5).
           03  WRK-SBTKBN-G.
               05  WRK-SBTGRPID    PIC 9(02).
               05  WRK-SBTRATE     PIC 9(02).
               05  WRK-SBTHKN      PIC 9(02).
      *
       01  CONST-AREA.
           03  CONST-H2605         PIC X(06)   VALUE "201405".
           03  CONST-SYAHO         PIC 9(02)   VALUE 1.
           03  CONST-KOKUHO        PIC 9(02)   VALUE 2.
           03  CONST-KOUKI         PIC 9(02)   VALUE 3.
           03  CONST-TOKUYO        PIC 9(02)   VALUE 4.
           03  CONST-JIHI          PIC 9(02)   VALUE 5.
           03  CONST-CHIKEN        PIC 9(02)   VALUE 6.
           03  CONST-ROUSAI        PIC 9(02)   VALUE 7.
           03  CONST-JIBAI         PIC 9(02)   VALUE 8.
           03  CONST-DAISAN        PIC 9(02)   VALUE 9.
           03  CONST-KOUGAI        PIC 9(02)   VALUE 10.
           03  CONST-RECEKA-MAX    PIC 9(05)   VALUE 100.
      *
           03  CONST-SYA-KOU9HEI   PIC 9(02)   VALUE 1.
           03  CONST-SYA-KOU9TAN   PIC 9(02)   VALUE 2.
           03  CONST-SYA-KOU7HEI   PIC 9(02)   VALUE 3.
           03  CONST-SYA-KOU7TAN   PIC 9(02)   VALUE 4.
           03  CONST-SYA-HONHEI    PIC 9(02)   VALUE 5.
           03  CONST-SYA-HONTAN    PIC 9(02)   VALUE 6.
           03  CONST-SYA-KZKHEI    PIC 9(02)   VALUE 7.
           03  CONST-SYA-KZKTAN    PIC 9(02)   VALUE 8.
           03  CONST-SYA-MISGKHEI  PIC 9(02)   VALUE 9.
           03  CONST-SYA-MISGKTAN  PIC 9(02)   VALUE 10.
           03  CONST-SYA-KOHTAN    PIC 9(02)   VALUE 11.
           03  CONST-SYA-KOH2HEI   PIC 9(02)   VALUE 12.
           03  CONST-SYA-KOH3HEI   PIC 9(02)   VALUE 13.
      *
           03  CONST-SYA-H01       PIC 9(02)   VALUE 1.
           03  CONST-SYA-H02       PIC 9(02)   VALUE 2.
           03  CONST-SYA-H03       PIC 9(02)   VALUE 3.
           03  CONST-SYA-H04       PIC 9(02)   VALUE 4.
           03  CONST-SYA-H31       PIC 9(02)   VALUE 5.
           03  CONST-SYA-H06       PIC 9(02)   VALUE 6.
           03  CONST-SYA-H07       PIC 9(02)   VALUE 7.
           03  CONST-SYA-H63       PIC 9(02)   VALUE 8.
      *
           03  CONST-KOK-IPNKOU9   PIC 9(02)   VALUE 1.
           03  CONST-KOK-IPNKOU7   PIC 9(02)   VALUE 2.
           03  CONST-KOK-IPN       PIC 9(02)   VALUE 3.
           03  CONST-KOK-IPNMISGK  PIC 9(02)   VALUE 4.
           03  CONST-KOK-TAI       PIC 9(02)   VALUE 5.
           03  CONST-KOK-TAIKZK    PIC 9(02)   VALUE 6.
           03  CONST-KOK-TAIMISGK  PIC 9(02)   VALUE 7.
      *
           03  CONST-KOU-KOU9      PIC 9(02)   VALUE 1.
           03  CONST-KOU-KOU7      PIC 9(02)   VALUE 2.
      *
           03  CONST-TKY-H068      PIC 9(02)   VALUE 1.
           03  CONST-TKY-H069      PIC 9(02)   VALUE 2.
           03  CONST-TKY-H040      PIC 9(02)   VALUE 3.
      *
           03  CONST-RSI-TG        PIC 9(02)   VALUE 1.
           03  CONST-RSI-TT        PIC 9(02)   VALUE 2.
           03  CONST-RSI-SG        PIC 9(02)   VALUE 3.
           03  CONST-RSI-ST        PIC 9(02)   VALUE 4.
           03  CONST-RSI-AC        PIC 9(02)   VALUE 5.
           03  CONST-RSI-KS        PIC 9(02)   VALUE 6.
      *
           03  CONST-JBI-NEW        PIC 9(02)   VALUE 1.
           03  CONST-JBI-USUAL      PIC 9(02)   VALUE 2.
      *
           03  CONST-KGI-GRP        PIC 9(02)   VALUE 1.
           03  CONST-DSN-GRP        PIC 9(02)   VALUE 1.
      *
           03  CONST-TOTAL-GROUPID PIC 9(02)   VALUE 99.
      *
           03  CONST-HKNKBN2-TTL   PIC 9(02)   VALUE 99.
      *
       01  BGSSUM-AREA.
           COPY    "CPBGSF01.INC"  REPLACING  //BGSF01-//
                                   BY         //BGSSUM-//.
      *
       01  TBGSHKNJA-AREA.
           COPY    "CPBGSF01.INC"  REPLACING  //BGSF01-//
                                   BY         //TBGSHKNJA-//.
      *
       01  TBGSMEI-AREA.
           COPY    "CPBGSF01.INC"  REPLACING  //BGSF01-//
                                   BY         //TBGSMEI-//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK2009.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    請求管理
           COPY    "CPRCF010.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    保険別請求チェック表サブ
           COPY    "CPORCSSKYGET.INC".
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      ****************************************************************
       LINKAGE                     SECTION.
       01  BGS100P-AREA.
           COPY    "CPORCSPRTLNK.INC".
           COPY    "CPBGS100PARA.INC".
      *
           COPY    "COMMON-SPA".
      *
      ****************************************************************
       PROCEDURE               DIVISION
                                       USING
                                       BGS100P-AREA
                                       SPA-AREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           IF    ( BGS100P-RC      =   ZERO )
               PERFORM 200-MAIN-SEC
           END-IF
      *
           PERFORM 300-END-SEC
      *
           EXIT PROGRAM
      *
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                      STS-AREA
                                           CNT-AREA
                                           FLG-AREA
                                           IDX-AREA
                                           WRK-AREA
      *
           MOVE    BGS100P-SKYFNAME    TO  ASS-BGSSKY
           MOVE    BGS100P-KOHFNAME    TO  ASS-BGSKOH
           MOVE    BGS100P-HKNJAFNAME  TO  ASS-BGSHKNJA
           MOVE    BGS100P-ETCFNAME    TO  ASS-BGSETC
           MOVE    BGS100P-MEIFNAME    TO  ASS-BGSMEI
           MOVE    BGS100P-ERRFNAME    TO  ASS-BGS100ERR
      *
           MOVE    "STM"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    SPACE               TO  JOB-YOBI
           PERFORM 800-ORCSJOB-SEC
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "2009"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    BGS100P-SRYYMD      TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-2009-REC
      *
           MOVE    ZERO                TO  BGS100P-SYUKA
                                           BGS100P-SYUKA-SYA
                                           BGS100P-SYUKA-KOK
                                           BGS100P-SYUKA-KOU
           INITIALIZE                      BGS100P-RECEKA-G
      *
           IF    ( SYS-2009-SYUKA-KBN  =   "1" )
      *
                   MOVE    1           TO  BGS100P-SYUKA
      *
               IF    ( SYS-2009-SYUKA-SYAHO    =   "1" )
                   MOVE    1           TO  BGS100P-SYUKA-SYA
               END-IF
               IF    ( SYS-2009-SYUKA-KOKUHO   =   "1" )
                   MOVE    1           TO  BGS100P-SYUKA-KOK
               END-IF
               IF    ( SYS-2009-SYUKA-KOUIKI   =   "1" )
                   MOVE    1           TO  BGS100P-SYUKA-KOU
               END-IF
           END-IF
      *
           OPEN    OUTPUT  BGSSKY-FILE
                           BGSKOH-FILE
                           BGSHKNJA-FILE
                           BGSETC-FILE
                           BGSMEI-FILE
      *
           CLOSE           BGSSKY-FILE
                           BGSKOH-FILE
                           BGSHKNJA-FILE
                           BGSETC-FILE
                           BGSMEI-FILE
      *
           PERFORM 900-SEIKYU-KEY79-SEL-SEC
           IF    ( FLG-SEIKYU      =   ZERO )
               MOVE    2   TO  BGS100P-RC
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           OPEN    I-O     BGSSKY-FILE
                           BGSKOH-FILE
                           BGSHKNJA-FILE
                           BGSETC-FILE
                           BGSMEI-FILE
      *
      *    社保分
           IF    ( BGS100P-SYAHO   =   1 )
            OR   ( BGS100P-SOUKATU =   1 )
               MOVE    "tbl_seikyu"        TO  WRK-TABLE
               MOVE    "key88"             TO  WRK-PATHNAME
               PERFORM 2001-SEIKYU-EDIT-SEC
           END-IF
      *
      *    国保分
           IF    ( BGS100P-KOKUHO  =   1 )
            OR   ( BGS100P-SOUKATU =   1 )
               MOVE    "tbl_seikyu"        TO  WRK-TABLE
               MOVE    "key89"             TO  WRK-PATHNAME
               PERFORM 2001-SEIKYU-EDIT-SEC
           END-IF
      *
      *    後期高齢者分
           IF    ( BGS100P-KOUKI   =   1 )
            OR   ( BGS100P-SOUKATU =   1 )
               MOVE    "tbl_seikyu"        TO  WRK-TABLE
               MOVE    "key90"             TO  WRK-PATHNAME
               PERFORM 2001-SEIKYU-EDIT-SEC
           END-IF
      *
      *    その他請求
           IF    ( BGS100P-RC      =   ZERO )
               IF    ( BGS100P-MEISAI  =   ZERO )
                   PERFORM 2001-ETC-SEIKYU-EDIT-SEC
               END-IF
           END-IF
      *
      *    その他請求（請求管理より）
           PERFORM 2002-ETC-SEIKYU-EDIT-SEC
      *
      *    診療科編集
           IF    ( BGS100P-RC      =   ZERO )
               IF    ( BGS100P-RECEKA-MAX  >   ZERO )
                   PERFORM 2001-RECEKA-EDIT-SEC
               END-IF
           END-IF
      *
           CLOSE           BGSSKY-FILE
                           BGSKOH-FILE
                           BGSHKNJA-FILE
                           BGSMEI-FILE
                           BGSETC-FILE
      *
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求データ編集処理
      *****************************************************************
       2001-SEIKYU-EDIT-SEC            SECTION.
      *
           PERFORM 900-SEIKYU-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SEIKYU  NOT =   ZERO )
                    OR   ( BGS100P-RC  NOT =   ZERO )
      *
               PERFORM 900-PTINF-KEY-SEL-SEC
               IF    ( PTINF-TSTPTNUMKBN   NOT =   "1" )
      *
                   INITIALIZE                  ORCSSKYGETAREA
                   MOVE    "1"             TO  LNK-SKYGET-KBN
                   MOVE    BGS100P-SRYYM   TO  LNK-SKYGET-SRYYM
                   MOVE    BGS100P-SKYGET-PARA
                                           TO  LNK-SKYGET-PARA
                   CALL    "ORCSSKYGET"        USING
                                               ORCSSKYGETAREA
                                               RECE10-REC
                                               SPA-AREA
      *
                   IF    ( LNK-SKYGET-RC   =   ZERO )
                       IF    ( BGS100P-MEISAI  =   ZERO )
                           PERFORM 20011-SEIKYU-EDIT-SEC
                       ELSE
                           PERFORM 20012-SEIKYU-EDIT-SEC
                       END-IF
                   ELSE
                       DISPLAY "SKIPPED RC["   LNK-SKYGET-RC
                           "]PTID["            RECE10-PTID
                           "]PTNUM["           RECE10-PTNUM
                           "]SRYYM["           RECE10-SRYYM
                           "]NYUGAIKBN["       RECE10-NYUGAIKBN
                           "]RECEKA["          RECE10-RECEKA
                           "]TEISYUTUSAKI["    RECE10-TEISYUTUSAKI
                           "]RECESYUBETU["     RECE10-RECESYUBETU
                           "]HKNJANUM["        RECE10-HKNJANUM
                           "]HOJOKBN_KEY["     RECE10-HOJOKBN-KEY
                           "]KOHID_KEY["       RECE10-KOHID-KEY
                           "]TEKSTYMD["        RECE10-TEKSTYMD
                           "]"
                   END-IF
               END-IF
      *
               PERFORM 900-SEIKYU-FET-SEC
      *
           END-PERFORM
      *
           MOVE    WRK-TABLE           TO  MCP-TABLE
           MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2001-SEIKYU-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    請求データ編集処理
      *****************************************************************
       20011-SEIKYU-EDIT-SEC           SECTION.
      *
           EVALUATE    RECE10-HKNNUM-S
           WHEN    "060"
           WHEN    "067"
               MOVE    CONST-KOKUHO    TO  WRK-HOKEN
           WHEN    "039"
               MOVE    CONST-KOUKI     TO  WRK-HOKEN
           WHEN    "068"
           WHEN    "069"
           WHEN    "040"
               MOVE    CONST-TOKUYO    TO  WRK-HOKEN
           WHEN    "971"
               MOVE    CONST-ROUSAI    TO  WRK-HOKEN
           WHEN    "973"
               MOVE    CONST-JIBAI     TO  WRK-HOKEN
           WHEN    "975"
               MOVE    CONST-KOUGAI    TO  WRK-HOKEN
           WHEN    "970"
               MOVE    CONST-DAISAN    TO  WRK-HOKEN
           WHEN    OTHER
               MOVE    CONST-SYAHO     TO  WRK-HOKEN
           END-EVALUATE
      *
           PERFORM 200111-BGSSKY-EDIT-SEC
      *
           PERFORM VARYING IDX-KOH     FROM    1   BY  1
                   UNTIL ( IDX-KOH     >   4 )
               IF    ( LNK-SKYGET-KOH-HKNNUM (IDX-KOH)
                                   NOT =   SPACE )
                   MOVE    "1"         TO  WRK-KOHKBN
                   MOVE    LNK-SKYGET-KOH-HKNNUM (IDX-KOH)
                                       TO  WRK-KOHNUM
                   PERFORM 200111-BGSKOH-EDIT-SEC
               END-IF
               IF    ( LNK-SKYGET-KOHETC-HKNNUM (IDX-KOH)
                                   NOT =   SPACE )
                   MOVE    "2"         TO  WRK-KOHKBN
                   MOVE    LNK-SKYGET-KOHETC-HKNNUM (IDX-KOH)
                                       TO  WRK-KOHNUM
                   PERFORM 200111-BGSKOH-EDIT-SEC
               END-IF
           END-PERFORM
      *
           IF    ( WRK-HOKEN    =  CONST-KOKUHO
                                OR CONST-KOUKI )
               PERFORM 200111-BGSHKNJA-EDIT-SEC
           END-IF
      *
           IF    ( RECE10-RECEKA       NOT =   "00" )
               PERFORM VARYING IDX0 FROM    1   BY  1
                      UNTIL ( IDX0 >   CONST-RECEKA-MAX )
                       OR   ( BGS100P-RECEKA (IDX0)
                                           =   RECE10-RECEKA )
      *
                   IF    ( IDX0            >   BGS100P-RECEKA-MAX )
                       MOVE    RECE10-RECEKA
                                           TO  BGS100P-RECEKA (IDX0)
                       MOVE    IDX0        TO  BGS100P-RECEKA-MAX
                       MOVE    CONST-RECEKA-MAX
                                           TO  IDX0
                   END-IF
      *
               END-PERFORM
           END-IF
      *
           .
       20011-SEIKYU-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    請求ファイル編集処理
      *****************************************************************
       200111-BGSSKY-EDIT-SEC          SECTION.
      *
           PERFORM 900-BGSSKY-READ-SEC
      *
           MOVE    BGSSKY-REC      TO  BGSSUM-AREA
           PERFORM 2100-SEIKYU-SUM-SEC
           MOVE    BGSSUM-AREA     TO  BGSSKY-REC
      *
           IF    ( FLG-BGSSKY          =   ZERO )
               REWRITE BGSSKY-REC
           ELSE
               WRITE   BGSSKY-REC
      *
               EVALUATE    WRK-HOKEN
               WHEN    CONST-SYAHO
                   COMPUTE CNT-SYAHKN  =   CNT-SYAHKN  +   1
               WHEN    CONST-KOKUHO
                   COMPUTE CNT-KOKHKN  =   CNT-KOKHKN  +   1
               WHEN    CONST-KOUKI
                   COMPUTE CNT-KOUHKN  =   CNT-KOUHKN  +   1
               END-EVALUATE
      *
           END-IF
      *
           .
       200111-BGSSKY-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    公費ファイル編集処理
      *****************************************************************
       200111-BGSKOH-EDIT-SEC          SECTION.
      *
           PERFORM 900-BGSKOH-READ-SEC
      *
               COMPUTE BGSKOH-KENSU
                   =   BGSKOH-KENSU    + 1
      *
           IF    ( WRK-KOHKBN      =   "1" )
               COMPUTE BGSKOH-NISSU
                   =   BGSKOH-NISSU    + LNK-SKYGET-KOH-NISSU (IDX-KOH)
               COMPUTE BGSKOH-TENSU
                   =   BGSKOH-TENSU    + LNK-SKYGET-KOH-TOTALTEN
                                                              (IDX-KOH)
           ELSE
               COMPUTE BGSKOH-NISSU
                   =   BGSKOH-NISSU    + LNK-SKYGET-KOHETC-NISSU
                                                              (IDX-KOH)
               COMPUTE BGSKOH-TENSU
                   =   BGSKOH-TENSU    + LNK-SKYGET-KOHETC-TOTALTEN
                                                              (IDX-KOH)
           END-IF
      *
           IF    ( FLG-BGSKOH          =   ZERO )
               REWRITE BGSKOH-REC
           ELSE
               WRITE   BGSKOH-REC
      *
               EVALUATE    WRK-HOKEN
               WHEN    CONST-SYAHO
                   COMPUTE CNT-SYAKOH  =   CNT-SYAHKN  +   1
               WHEN    CONST-KOKUHO
                   COMPUTE CNT-KOKKOH  =   CNT-KOKHKN  +   1
               WHEN    CONST-KOUKI
                   COMPUTE CNT-KOUKOH  =   CNT-KOUHKN  +   1
               END-EVALUATE
      *
           END-IF
      *
           .
       200111-BGSKOH-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    保険者別請求ファイル編集処理
      *****************************************************************
       200111-BGSHKNJA-EDIT-SEC       SECTION.
      *
           INITIALIZE                  BGSHKNJA-REC
           MOVE    RECE10-NYUGAIKBN
                                   TO  BGSHKNJA-NYUGAIKBN
      *
           EVALUATE    TRUE
           WHEN  ( BGS100P-EDITKBN =   "0" )
               MOVE    "0"         TO  BGSHKNJA-SKYSBT
           WHEN  ( BGS100P-SRYYM   =   RECE10-SRYYM )
               MOVE    "1"         TO  BGSHKNJA-SKYSBT
           WHEN  ( RECE10-HENREIYM =   "000000" )
               MOVE    "2"         TO  BGSHKNJA-SKYSBT
           WHEN    OTHER
               MOVE    "3"         TO  BGSHKNJA-SKYSBT
           END-EVALUATE
      *
           IF    ( BGS100P-SYUKA-HKN (WRK-HOKEN)
                                   =   1 )
               MOVE    RECE10-RECEKA
                                   TO  BGSHKNJA-RECEKA
           ELSE
                   MOVE    "00"    TO  BGSHKNJA-RECEKA
           END-IF
      *
           MOVE    WRK-HOKEN       TO  BGSHKNJA-HOKEN
           MOVE    RECE10-PREFKBN  TO  BGSHKNJA-PREFKBN
           MOVE    RECE10-HKNJANUM-S
                                   TO  BGSHKNJA-HKNJANUM
      *
           IF    ( WRK-HOKEN       =   CONST-KOKUHO )
      *
               COMPUTE CNT-KOKHKNJA   =   CNT-KOKHKNJA   +   1
      *
               PERFORM 9001-KOKUKO-GROUPID-EDIT-SEC
               MOVE    WRK-GROUPID     TO  BGSHKNJA-GROUPID
           ELSE
               COMPUTE CNT-KOUHKNJA   =   CNT-KOUHKNJA   +   1
      *
               PERFORM 9001-KOUKI-GROUPID-EDIT-SEC
               MOVE    WRK-GROUPID     TO  BGSHKNJA-GROUPID
           END-IF
      *
           MOVE    BGSHKNJA-REC    TO  TBGSHKNJA-AREA
      *
           PERFORM 2001111-BGSHKNJA-EDIT-SEC
      *
           MOVE    TBGSHKNJA-AREA  TO  BGSHKNJA-REC
           MOVE    CONST-TOTAL-GROUPID
                                   TO  BGSHKNJA-GROUPID
           PERFORM 2001111-BGSHKNJA-EDIT-SEC
      *
           IF    ( BGS100P-SYUKA-HKN (WRK-HOKEN)
                                   =   1 )
      *
               MOVE    "GO"            TO  TBGSHKNJA-RECEKA
               MOVE    TBGSHKNJA-AREA  TO  BGSHKNJA-REC
               PERFORM 2001111-BGSHKNJA-EDIT-SEC
      *
               MOVE    TBGSHKNJA-AREA  TO  BGSHKNJA-REC
               MOVE    CONST-TOTAL-GROUPID
                                       TO  BGSHKNJA-GROUPID
               PERFORM 2001111-BGSHKNJA-EDIT-SEC
      *
           END-IF
      *
           .
       200111-BGSHKNJA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    保険者別請求ファイル編集処理
      *****************************************************************
       2001111-BGSHKNJA-EDIT-SEC       SECTION.
      *
           PERFORM 900-BGSHKNJA-READ-SEC
      *
           MOVE    BGSHKNJA-REC    TO  BGSSUM-AREA
           PERFORM 2100-SEIKYU-SUM-SEC
           MOVE    BGSSUM-AREA     TO  BGSHKNJA-REC
      *
           IF    ( FLG-BGSHKNJA          =   ZERO )
               REWRITE BGSHKNJA-REC
           ELSE
               WRITE   BGSHKNJA-REC
           END-IF
      *
           .
       2001111-BGSHKNJA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    請求明細編集処理
      *****************************************************************
       20012-SEIKYU-EDIT-SEC           SECTION.
      *
           EVALUATE    RECE10-HKNNUM-S
           WHEN    "060"
           WHEN    "067"
               MOVE    CONST-KOKUHO    TO  WRK-HOKEN
           WHEN    "039"
               MOVE    CONST-KOUKI     TO  WRK-HOKEN
           WHEN    "068"
           WHEN    "069"
           WHEN    "040"
               MOVE    CONST-TOKUYO    TO  WRK-HOKEN
           WHEN    "971"
               MOVE    CONST-ROUSAI    TO  WRK-HOKEN
           WHEN    "973"
               MOVE    CONST-JIBAI     TO  WRK-HOKEN
           WHEN    "975"
               MOVE    CONST-KOUGAI    TO  WRK-HOKEN
           WHEN    "970"
               MOVE    CONST-DAISAN    TO  WRK-HOKEN
           WHEN    OTHER
               MOVE    CONST-SYAHO     TO  WRK-HOKEN
           END-EVALUATE
      *
           PERFORM 200121-BGSMEI-EDIT-SEC
      *
           IF    ( RECE10-RECEKA       NOT =   "00" )
               PERFORM VARYING IDX0 FROM    1   BY  1
                      UNTIL ( IDX0 >   CONST-RECEKA-MAX )
                       OR   ( BGS100P-RECEKA (IDX0)
                                           =   RECE10-RECEKA )
      *
                   IF    ( IDX0            >   BGS100P-RECEKA-MAX )
                       MOVE    RECE10-RECEKA
                                           TO  BGS100P-RECEKA (IDX0)
                       MOVE    IDX0        TO  BGS100P-RECEKA-MAX
                       MOVE    CONST-RECEKA-MAX
                                           TO  IDX0
                   END-IF
      *
               END-PERFORM
           END-IF
      *
           .
       20012-SEIKYU-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    請求明細ファイル編集処理
      *****************************************************************
       200121-BGSMEI-EDIT-SEC       SECTION.
      *
           INITIALIZE               BGSMEI-REC
      *
           PERFORM 2001211-BGSMEI-KEY-EDIT-SEC
      *
           MOVE    BGSMEI-REC      TO  TBGSMEI-AREA
      *
           MOVE    BGSMEI-REC      TO  BGSSUM-AREA
           PERFORM 2100-SEIKYU-SUM-SEC
           MOVE    BGSSUM-AREA     TO  BGSMEI-REC
           MOVE    LNK-SKYGET-SEIKYUFLG
                                   TO  BGSMEI-SEIKYUFLG
      *
           COMPUTE CNT-BGSMEI = CNT-BGSMEI + 1
           MOVE    CNT-BGSMEI      TO  BGSMEI-SEQ
      *
           WRITE   BGSMEI-REC
      *
           IF    ( WRK-GROUPID       =    CONST-TOTAL-GROUPID )
               CONTINUE
           ELSE
               EVALUATE    WRK-HOKEN
               WHEN    CONST-SYAHO
                   COMPUTE CNT-SYAMEI  =   CNT-SYAMEI  +   1
               WHEN    CONST-KOKUHO
                   COMPUTE CNT-KOKMEI  =   CNT-KOKMEI  +   1
               WHEN    CONST-KOUKI
                   COMPUTE CNT-KOUMEI  =   CNT-KOUMEI  +   1
               WHEN    CONST-ROUSAI
                   COMPUTE CNT-RSIMEI  =   CNT-RSIMEI  +   1
               WHEN    CONST-JIBAI
                   COMPUTE CNT-JBIMEI  =   CNT-JBIMEI  +   1
               WHEN    CONST-KOUGAI
                   COMPUTE CNT-KGIMEI  =   CNT-KGIMEI  +   1
               WHEN    CONST-DAISAN
                   COMPUTE CNT-DSNMEI  =   CNT-DSNMEI  +   1
               WHEN    CONST-TOKUYO
                   COMPUTE CNT-TKYMEI  =   CNT-TKYMEI  +   1
               END-EVALUATE
           END-IF
      *
      ***  保険区分別小計データ作成
           PERFORM 900-BGSMEI-READ-SEC
      *
           MOVE    BGSMEI-REC      TO  BGSSUM-AREA
           PERFORM 2100-SEIKYU-SUM-SEC
           MOVE    BGSSUM-AREA     TO  BGSMEI-REC
      *
           IF    ( FLG-BGSMEI      =   ZERO )
               REWRITE BGSMEI-REC
           ELSE
               WRITE   BGSMEI-REC
           END-IF
      *
           .
       200121-BGSMEI-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    請求明細ファイルキー項目編集処理
      *****************************************************************
       2001211-BGSMEI-KEY-EDIT-SEC     SECTION.
      *
           MOVE    WRK-HOKEN       TO  BGSMEI-HOKEN
      *
           EVALUATE    TRUE
           WHEN  ( BGS100P-EDITKBN =   "0" )
               MOVE    "0"         TO  BGSMEI-SKYSBT
           WHEN  ( BGS100P-SRYYM   =   RECE10-SRYYM )
               MOVE    "1"         TO  BGSMEI-SKYSBT
           WHEN  ( RECE10-HENREIYM =   "000000" )
               MOVE    "2"         TO  BGSMEI-SKYSBT
           WHEN    OTHER
               MOVE    "3"         TO  BGSMEI-SKYSBT
           END-EVALUATE
      *
           MOVE    RECE10-NYUGAIKBN
                                   TO  BGSMEI-NYUGAIKBN
      *
           IF    ( WRK-HOKEN       =   CONST-SYAHO
             OR                        CONST-KOKUHO
             OR                        CONST-KOUKI )
               IF    ( BGS100P-SYUKA-HKN (WRK-HOKEN)
                                   =   1 )
                   MOVE    RECE10-RECEKA
                                   TO  BGSMEI-RECEKA
               ELSE
                   MOVE    "00"    TO  BGSMEI-RECEKA
               END-IF
           ELSE
                   MOVE    "00"    TO  BGSMEI-RECEKA
           END-IF
      *
           INITIALIZE                  WRK-SBTKBN-G
      *
           EVALUATE    WRK-HOKEN
           WHEN    CONST-SYAHO
               PERFORM 9001-SYHAO-HKNKBN-EDIT-SEC
           WHEN    CONST-KOKUHO
               PERFORM 9001-KOKUKO-HKNKBN-EDIT-SEC
           WHEN    CONST-KOUKI
               PERFORM 9001-KOUKI-HKNKBN-EDIT-SEC
           WHEN    CONST-TOKUYO
               PERFORM 9001-TOKUYO-HKNKBN-EDIT-SEC
           WHEN    CONST-ROUSAI
               PERFORM 9001-ROUSAI-HKNKBN-EDIT-SEC
           WHEN    CONST-JIBAI
               PERFORM 9001-JIBAI-HKNKBN-EDIT-SEC
           WHEN    CONST-KOUGAI
               PERFORM 9001-KOUGAI-HKNKBN-EDIT-SEC
           WHEN    CONST-DAISAN
               PERFORM 9001-DAISAN-HKNKBN-EDIT-SEC
           END-EVALUATE
           MOVE    WRK-SBTGRPID     TO  BGSMEI-SBTGRPID
           MOVE    WRK-SBTHKN       TO  BGSMEI-SBTHKN
      *
           EVALUATE    TRUE
           WHEN  ( WRK-HOKEN        =      CONST-SYAHO )
            AND  ( WRK-SBTGRPID     =      CONST-SYA-KOU9TAN
                                      OR   CONST-SYA-KOU9HEI )
           WHEN  ( WRK-HOKEN        =      CONST-KOKUHO )
            AND  ( WRK-SBTGRPID     =      CONST-KOK-IPNKOU9 )
      *
               IF    ( RECE10-SRYYM    >=  CONST-H2605 )
                   COMPUTE WRK-SBTRATE  =  10 - RECE10-KYURATE
               ELSE
                   COMPUTE WRK-SBTRATE  =  1
               END-IF
           END-EVALUATE
      *
           MOVE    WRK-SBTKBN-G    TO  BGSMEI-SBTKBN-G
      *
           MOVE    RECE10-HKNNUM   TO  BGSMEI-HKNNUM
      *
           IF    ( WRK-HOKEN       =   CONST-KOKUHO
                                    OR CONST-KOUKI )
               MOVE    RECE10-PREFKBN
                                   TO  BGSMEI-PREFKBN
           END-IF
      *
           IF    ( WRK-HOKEN       =   CONST-SYAHO
                                    OR CONST-KOKUHO
                                    OR CONST-KOUKI
                                    OR CONST-TOKUYO
                                                    )
               MOVE    RECE10-HKNJANUM-S
                                   TO  BGSMEI-HKNJANUM
           END-IF
      *
           EVALUATE    WRK-HOKEN
           WHEN    CONST-SYAHO
               PERFORM 9001-SYAHO-GROUPID-EDIT-SEC
           WHEN    CONST-KOKUHO
               PERFORM 9001-KOKUKO-GROUPID-EDIT-SEC
           WHEN    CONST-KOUKI
               PERFORM 9001-KOUKI-GROUPID-EDIT-SEC
           WHEN    CONST-TOKUYO
               PERFORM 9001-TOKUYO-GROUPID-EDIT-SEC
           WHEN    CONST-ROUSAI
               PERFORM 9001-ROUSAI-GROUPID-EDIT-SEC
           WHEN    CONST-JIBAI
               PERFORM 9001-JIBAI-GROUPID-EDIT-SEC
           WHEN    CONST-KOUGAI
               PERFORM 9001-KOUGAI-GROUPID-EDIT-SEC
           WHEN    CONST-DAISAN
               PERFORM 9001-DAISAN-GROUPID-EDIT-SEC
           END-EVALUATE
           MOVE    WRK-GROUPID     TO  BGSMEI-GROUPID
      *
           MOVE    RECE10-SRYYM    TO  BGSMEI-SRYYM
           MOVE    PTINF-KANANAME  TO  BGSMEI-KANANAME
           MOVE    PTINF-NAME      TO  BGSMEI-NAME
           MOVE    RECE10-PTID     TO  BGSMEI-PTID
      *
           .
       2001211-BGSMEI-KEY-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    請求集計処理
      *****************************************************************
       2100-SEIKYU-SUM-SEC          SECTION.
      *
           COMPUTE BGSSUM-KENSU
               =   BGSSUM-KENSU    +   1
           COMPUTE BGSSUM-NISSU
               =   BGSSUM-NISSU        + LNK-SKYGET-IRYO-NISSU
           COMPUTE BGSSUM-TENSU
               =   BGSSUM-TENSU        + LNK-SKYGET-IRYO-TOTALTEN
           COMPUTE BGSSUM-FTNMONEY
               =   BGSSUM-FTNMONEY     + LNK-SKYGET-IRYO-ICHIBUFTN
           COMPUTE BGSSUM-HKNMONEY
               =   BGSSUM-HKNMONEY     + LNK-SKYGET-IRYO-HOKENJA
           COMPUTE BGSSUM-KOHMONEY
               =   BGSSUM-KOHMONEY     + LNK-SKYGET-IRYO-KOHFTN
           COMPUTE BGSSUM-TTLMONEY
               =   BGSSUM-TTLMONEY     + LNK-SKYGET-IRYO-ICHIBUFTN
                                       + LNK-SKYGET-IRYO-HOKENJA
                                       + LNK-SKYGET-IRYO-KOHFTN
           COMPUTE BGSSUM-SKJKAISU
               =   BGSSUM-SKJKAISU     + LNK-SKYGET-SKJ-KAISU
           COMPUTE BGSSUM-SKJTTLMONEY
               =   BGSSUM-SKJTTLMONEY  + LNK-SKYGET-SKJ-RYOYOHI
           COMPUTE BGSSUM-SKJFTNMONEY
               =   BGSSUM-SKJFTNMONEY  + LNK-SKYGET-SKJ-ICHIBUFTN
           COMPUTE BGSSUM-SKJHKNMONEY
               =   BGSSUM-SKJHKNMONEY  + LNK-SKYGET-SKJ-HOKENJA
           COMPUTE BGSSUM-SKJKOHMONEY
               =   BGSSUM-SKJKOHMONEY  + LNK-SKYGET-SKJ-KOHFTN
           COMPUTE BGSSUM-TTLTTLMONEY
               =   BGSSUM-TTLTTLMONEY  + LNK-SKYGET-IRYO-ICHIBUFTN
                                       + LNK-SKYGET-IRYO-HOKENJA
                                       + LNK-SKYGET-IRYO-KOHFTN
                                       + LNK-SKYGET-SKJ-RYOYOHI
      *
           .
       2100-SEIKYU-SUM-EXT.
           EXIT.
      *****************************************************************
      *    その他請求データ編集処理
      *****************************************************************
       2001-ETC-SEIKYU-EDIT-SEC        SECTION.
      *
           INITIALIZE                  ORCSSKYGETAREA
           MOVE    "2"             TO  LNK-SKYGET-KBN
           MOVE    BGS100P-SRYYM   TO  LNK-SKYGET-SRYYM
           MOVE    BGS100P-SKYGET-PARA
                                   TO  LNK-SKYGET-PARA
           CALL    "ORCSSKYGET"        USING
                                       ORCSSKYGETAREA
                                       RECE10-REC
                                       SPA-AREA
      *
           PERFORM VARYING IDX-NG    FROM    1   BY  1
                   UNTIL ( IDX-NG    >   2 )
      *
               PERFORM VARYING IDX-HKN    FROM    1   BY  1
                       UNTIL ( IDX-HKN >  10 )
      *
                   INITIALIZE          BGSETC-REC
                   EVALUATE    IDX-HKN
                   WHEN   9
                       MOVE   10       TO  BGSETC-HOKEN
                   WHEN   10
                       MOVE   9        TO  BGSETC-HOKEN
                   WHEN   OTHER
                       MOVE   IDX-HKN  TO  BGSETC-HOKEN
                   END-EVALUATE
                   IF    ( IDX-NG      =   1 )
                       MOVE    "1"     TO  BGSETC-NYUGAIKBN
                   ELSE
                       MOVE    "2"     TO  BGSETC-NYUGAIKBN
                   END-IF
      *
                   IF    ( BGS100P-EDITKBN =   "0" )
                       MOVE    "0"     TO  BGSETC-SKYSBT
                   ELSE
                       MOVE    "1"     TO  BGSETC-SKYSBT
                   END-IF
                   MOVE   "  "         TO  BGSETC-RECEKA
      *
                   IF    ( BGS100P-ETCGETKBN =   "0" )
                    AND  ( BGSETC-HOKEN      =   CONST-ROUSAI
                                              OR CONST-JIBAI
                                              OR CONST-KOUGAI
                                              OR CONST-TOKUYO
                                               )
                       CONTINUE
                   ELSE
      *
                       MOVE   LNK-SKYGET-KENSU (IDX-HKN IDX-NG)
                                       TO  BGSETC-KENSU
                       MOVE   LNK-SKYGET-IRYO2-NISSU (IDX-HKN IDX-NG)
                                       TO  BGSETC-NISSU
                       MOVE   LNK-SKYGET-IRYO2-TOTALTEN (IDX-HKN IDX-NG)
                                       TO  BGSETC-TENSU
                       MOVE   LNK-SKYGET-IRYO2-ICHIBUFTN(IDX-HKN IDX-NG)
                                       TO  BGSETC-FTNMONEY
                       MOVE   LNK-SKYGET-IRYO2-HOKENJA (IDX-HKN IDX-NG)
                                       TO  BGSETC-HKNMONEY
                       MOVE   LNK-SKYGET-IRYO2-KOHFTN (IDX-HKN IDX-NG)
                                       TO  BGSETC-KOHMONEY
      *
                       COMPUTE BGSETC-TTLMONEY
                           = LNK-SKYGET-IRYO2-ICHIBUFTN (IDX-HKN IDX-NG)
                           + LNK-SKYGET-IRYO2-HOKENJA   (IDX-HKN IDX-NG)
                           + LNK-SKYGET-IRYO2-KOHFTN    (IDX-HKN IDX-NG)
      *
                       MOVE    LNK-SKYGET-SKJ2-KAISU (IDX-HKN IDX-NG)
                                       TO  BGSETC-SKJKAISU
                       MOVE    LNK-SKYGET-SKJ2-RYOYOHI  (IDX-HKN IDX-NG)
                                       TO  BGSETC-SKJTTLMONEY
                       MOVE    LNK-SKYGET-SKJ2-ICHIBUFTN(IDX-HKN IDX-NG)
                                       TO  BGSETC-SKJFTNMONEY
                       MOVE    LNK-SKYGET-SKJ2-HOKENJA (IDX-HKN IDX-NG)
                                       TO  BGSETC-SKJHKNMONEY
                       MOVE    LNK-SKYGET-SKJ2-KOHFTN  (IDX-HKN IDX-NG)
                                       TO  BGSETC-SKJKOHMONEY
      *
                       COMPUTE BGSETC-TTLTTLMONEY
                           = LNK-SKYGET-IRYO2-ICHIBUFTN(IDX-HKN IDX-NG)
                           + LNK-SKYGET-IRYO2-HOKENJA  (IDX-HKN IDX-NG)
                           + LNK-SKYGET-IRYO2-KOHFTN (IDX-HKN IDX-NG)
                           + LNK-SKYGET-SKJ2-RYOYOHI (IDX-HKN IDX-NG)
                   END-IF
      *
                   IF    ( IDX-NG            =   1 )
                       MOVE    LNK-SKYGET-JIHI (IDX-HKN 3)
                                       TO  BGSETC-JIHIMONEY
                       MOVE    LNK-SKYGET-DISCOUNT-MONEY (IDX-HKN 3)
                                       TO  BGSETC-GENMEN
                       MOVE    LNK-SKYGET-CHOSEI1 (IDX-HKN 3)
                                       TO  BGSETC-CHOSEI1
                       MOVE    LNK-SKYGET-CHOSEI2 (IDX-HKN 3)
                                       TO  BGSETC-CHOSEI2
                       MOVE    LNK-SKYGET-HASU (IDX-HKN 3)
                                       TO  BGSETC-HASU
                   ELSE
                       MOVE    LNK-SKYGET-JIHI (IDX-HKN IDX-NG)
                                       TO  BGSETC-JIHIMONEY
                       MOVE    LNK-SKYGET-DISCOUNT-MONEY(IDX-HKN IDX-NG)
                                       TO  BGSETC-GENMEN
                       MOVE    LNK-SKYGET-CHOSEI1 (IDX-HKN IDX-NG)
                                       TO  BGSETC-CHOSEI1
                       MOVE    LNK-SKYGET-CHOSEI2 (IDX-HKN IDX-NG)
                                       TO  BGSETC-CHOSEI2
                       MOVE    LNK-SKYGET-HASU (IDX-HKN IDX-NG)
                                       TO  BGSETC-HASU
                   END-IF
      *
                   WRITE   BGSETC-REC
      *
               END-PERFORM
      *
           END-PERFORM
      *
           .
       2001-ETC-SEIKYU-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    請求データ編集処理（労災、自賠責、公害）
      *****************************************************************
       2002-ETC-SEIKYU-EDIT-SEC        SECTION.
      *
           PERFORM 900-SEIKYU-KEY91-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SEIKYU  NOT =   ZERO )
                    OR   ( BGS100P-RC  NOT =   ZERO )
      *
               PERFORM 900-PTINF-KEY-SEL-SEC
               IF    ( PTINF-TSTPTNUMKBN   NOT =   "1" )
      *
                   INITIALIZE                  ORCSSKYGETAREA
                   EVALUATE   RECE10-HKNNUM-S
                   WHEN    "068"
                   WHEN    "069"
                   WHEN    "040"
                       MOVE    "7"         TO  LNK-SKYGET-KBN
                   WHEN    "971"
                       MOVE    "3"         TO  LNK-SKYGET-KBN
                   WHEN    "973"
                       MOVE    "4"         TO  LNK-SKYGET-KBN
                   WHEN    "975"
                       MOVE    "5"         TO  LNK-SKYGET-KBN
                   WHEN    "970"
                       MOVE    "6"         TO  LNK-SKYGET-KBN
                   END-EVALUATE
                   MOVE    BGS100P-SRYYM   TO  LNK-SKYGET-SRYYM
                   MOVE    BGS100P-SKYGET-PARA
                                           TO  LNK-SKYGET-PARA
                   CALL    "ORCSSKYGET"        USING
                                               ORCSSKYGETAREA
                                               RECE10-REC
                                               SPA-AREA
      *
                   IF    ( LNK-SKYGET-RC   =   ZERO )
                       IF    ( BGS100P-MEISAI  =   ZERO )
                           PERFORM 20021-ETC-SEIKYU-EDIT-SEC
                       ELSE
                           PERFORM 20012-SEIKYU-EDIT-SEC
                       END-IF
                   ELSE
                       DISPLAY "SKIPPED RC["   LNK-SKYGET-RC
                           "]PTID["            RECE10-PTID
                           "]PTNUM["           RECE10-PTNUM
                           "]SRYYM["           RECE10-SRYYM
                           "]NYUGAIKBN["       RECE10-NYUGAIKBN
                           "]RECEKA["          RECE10-RECEKA
                           "]TEISYUTUSAKI["    RECE10-TEISYUTUSAKI
                           "]RECESYUBETU["     RECE10-RECESYUBETU
                           "]HKNJANUM["        RECE10-HKNJANUM
                           "]HOJOKBN_KEY["     RECE10-HOJOKBN-KEY
                           "]KOHID_KEY["       RECE10-KOHID-KEY
                           "]TEKSTYMD["        RECE10-TEKSTYMD
                           "]"
                   END-IF
               END-IF
      *
               PERFORM 900-SEIKYU-KEY91-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key91"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2002-ETC-SEIKYU-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    請求データ編集処理（労災、自賠責、公害）
      *****************************************************************
       20021-ETC-SEIKYU-EDIT-SEC       SECTION.
      *
           INITIALIZE              BGSETC-REC
      *
           EVALUATE    RECE10-HKNNUM-S
           WHEN    "068"
           WHEN    "069"
           WHEN    "040"
               MOVE    CONST-TOKUYO    TO  WRK-HOKEN
           WHEN    "971"
               MOVE    CONST-ROUSAI    TO  WRK-HOKEN
           WHEN    "973"
               MOVE    CONST-JIBAI     TO  WRK-HOKEN
           WHEN    "975"
               MOVE    CONST-KOUGAI    TO  WRK-HOKEN
           WHEN    "970"
               MOVE    CONST-DAISAN    TO  WRK-HOKEN
           END-EVALUATE
      *
           MOVE    WRK-HOKEN           TO  BGSETC-HOKEN
           MOVE    RECE10-NYUGAIKBN    TO  BGSETC-NYUGAIKBN
      *
           IF    ( BGS100P-EDITKBN =   "0" )
               MOVE    "0"         TO  BGSETC-SKYSBT
           ELSE
               IF    ( BGS100P-SRYYM   =   RECE10-SRYYM )
                   MOVE    "1"     TO  BGSETC-SKYSBT
               ELSE
                   MOVE    "2"     TO  BGSETC-SKYSBT
               END-IF
           END-IF
      *
           MOVE    "  "            TO  BGSETC-RECEKA
      *
           PERFORM 900-BGSETC-READ-SEC
      *
           MOVE    BGSETC-REC      TO  BGSSUM-AREA
           PERFORM 2100-SEIKYU-SUM-SEC
           MOVE    BGSSUM-AREA     TO  BGSETC-REC
      *
           IF    ( FLG-BGSETC      =   ZERO )
               REWRITE   BGSETC-REC
           ELSE
               WRITE     BGSETC-REC
           END-IF
      *
           .
       20021-ETC-SEIKYU-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    診療科編集処理
      *****************************************************************
       2001-RECEKA-EDIT-SEC        SECTION.
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   BGS100P-RECEKA-MAX  - 1 )
               COMPUTE WRK-ST  =   IDX0    +   1
               PERFORM VARYING IDX1    FROM    WRK-ST  BY  1
                       UNTIL ( IDX1    >   BGS100P-RECEKA-MAX )
                   IF    ( BGS100P-RECEKA (IDX0)
                                       >   BGS100P-RECEKA (IDX1))
                       MOVE    BGS100P-RECEKA (IDX0)
                                       TO  WRK-RECEKA
                       MOVE    BGS100P-RECEKA (IDX1)
                                       TO  BGS100P-RECEKA (IDX0)
                       MOVE    WRK-RECEKA
                                       TO  BGS100P-RECEKA (IDX1)
                   END-IF
               END-PERFORM
           END-PERFORM
      *
           COMPUTE BGS100P-RECEKA-MAX
               =   BGS100P-RECEKA-MAX      +   1
           MOVE    "00"        TO  BGS100P-RECEKA (BGS100P-RECEKA-MAX)
      *
           .
       2001-RECEKA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           DISPLAY "CNT-SYAHKN   ["   CNT-SYAHKN "]"
           DISPLAY "CNT-SYAKOH   ["   CNT-SYAKOH "]"
           DISPLAY "CNT-SYAMEI   ["   CNT-SYAMEI "]"
           DISPLAY "CNT-KOKHKN   ["   CNT-KOKHKN "]"
           DISPLAY "CNT-KOKKOH   ["   CNT-KOKKOH "]"
           DISPLAY "CNT-KOKHKNJA ["   CNT-KOKHKNJA "]"
           DISPLAY "CNT-KOKMEI   ["   CNT-KOKMEI "]"
           DISPLAY "CNT-KOUHKN   ["   CNT-KOUHKN "]"
           DISPLAY "CNT-KOUKOH   ["   CNT-KOUKOH "]"
           DISPLAY "CNT-KOUHKNJA ["   CNT-KOUHKNJA "]"
           DISPLAY "CNT-KOUMEI   ["   CNT-KOUMEI "]"
           DISPLAY "CNT-TKYMEI   ["   CNT-TKYMEI "]"
           DISPLAY "CNT-RSIMEI   ["   CNT-RSIMEI "]"
           DISPLAY "CNT-JBIMEI   ["   CNT-JBIMEI "]"
           DISPLAY "CNT-KGIMEI   ["   CNT-KGIMEI "]"
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT   BGS100ERR-FILE
           IF      STS-BGS100ERR     =   ZERO
               CLOSE   BGS100ERR-FILE
           ELSE
               OPEN    OUTPUT              BGS100ERR-FILE
      *
               MOVE    WRK-ERRMSG      TO  BGS100ERR-REC
               WRITE   BGS100ERR-REC
               CLOSE   BGS100ERR-FILE
      *
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-ERRMSG      TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               PERFORM 800-ORCSJOB-SEC
           END-IF
      *
           MOVE    1                   TO  BGS100P-RC
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    ジョブ管理サブ
      *****************************************************************
       800-ORCSJOB-SEC                 SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    BGS100P-JOBID       TO  JOB-JOBID
           MOVE    BGS100P-SHELLID     TO  JOB-SHELLID
           MOVE    "ORCBGS100"         TO  JOB-PGID
           MOVE    BGS100P-SHELLMSG    TO  JOB-SHELLMSG
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA 
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           .
      *
       800-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理検索処理
      *****************************************************************
       900-SEIKYU-KEY79-SEL-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-SEIKYU
      *
           INITIALIZE                  RECE10-REC
           MOVE    SPA-HOSPNUM     TO  RECE10-HOSPNUM
           MOVE    BGS100P-SRYYM   TO  RECE10-SKYYM
           MOVE    "%"             TO  RECE10-NYUGAIKBN
           MOVE    RECE10-REC      TO  MCPDATA-REC
           MOVE    "tbl_seikyu"    TO  MCP-TABLE
           MOVE    "key79"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  RECE10-REC
           ELSE
               MOVE    1           TO  FLG-SEIKYU
               INITIALIZE              RECE10-REC
           END-IF
      *
           MOVE    "tbl_seikyu"    TO  MCP-TABLE
           MOVE    "key79"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-SEIKYU-KEY79-SEL-EXT.
           EXIT.
      *****************************************************************
      *    請求管理検索処理
      *****************************************************************
       900-SEIKYU-SEL-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-SEIKYU
      *
           INITIALIZE                  RECE10-REC
           MOVE    SPA-HOSPNUM     TO  RECE10-HOSPNUM
           MOVE    BGS100P-SRYYM   TO  RECE10-SKYYM
           IF    ( BGS100P-NYUGAIKBN   =   SPACE )
            OR   ( BGS100P-SOUKATU     =   1     )
               MOVE    "%"         TO  RECE10-NYUGAIKBN
           ELSE
               MOVE    BGS100P-NYUGAIKBN
                                   TO  RECE10-NYUGAIKBN
           END-IF
           MOVE    RECE10-REC      TO  MCPDATA-REC
           MOVE    WRK-TABLE       TO  MCP-TABLE
           MOVE    WRK-PATHNAME    TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               PERFORM 900-SEIKYU-FET-SEC
           ELSE
               MOVE    1           TO  FLG-SEIKYU
               INITIALIZE              RECE10-REC
           END-IF
      *
           .
      *
       900-SEIKYU-SEL-EXT.
           EXIT.
      *****************************************************************
      *    請求管理FETCH処理
      *****************************************************************
       900-SEIKYU-FET-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-SEIKYU
           MOVE    ZERO            TO  FLG-LOOPEND
      *
           PERFORM UNTIL ( FLG-SEIKYU  NOT =   ZERO )
                    OR   ( FLG-LOOPEND NOT =   ZERO )
      *
               MOVE    WRK-TABLE       TO  MCP-TABLE
               MOVE    WRK-PATHNAME    TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
      *
                   MOVE    MCPDATA-REC TO  RECE10-REC
      *
                   MOVE    1           TO  FLG-LOOPEND
      *
                   IF    ( RECE10-SKYKBN-RECEDEN   =   "1" )
                       MOVE    2       TO  BGS100P-RC
                   END-IF
      *
               ELSE
                   MOVE    1           TO  FLG-SEIKYU
                   INITIALIZE              RECE10-REC
               END-IF
      *
           END-PERFORM
      *
           .
      *
       900-SEIKYU-FET-EXT.
           EXIT.
      *****************************************************************
      *    請求管理検索処理
      *****************************************************************
       900-SEIKYU-KEY91-SEL-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-SEIKYU
      *
           INITIALIZE                  RECE10-REC
           MOVE    SPA-HOSPNUM     TO  RECE10-HOSPNUM
           MOVE    BGS100P-SRYYM   TO  RECE10-SKYYM
           IF    ( BGS100P-NYUGAIKBN   =   SPACE )
            OR   ( BGS100P-SOUKATU     =   1     )
               MOVE    "%"         TO  RECE10-NYUGAIKBN
           ELSE
               MOVE    BGS100P-NYUGAIKBN
                                   TO  RECE10-NYUGAIKBN
           END-IF
           MOVE    "97%"           TO  RECE10-HKNNUM-S
           MOVE    RECE10-REC      TO  MCPDATA-REC
           MOVE    "tbl_seikyu"    TO  MCP-TABLE
           MOVE    "key91"         TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
      *
           IF    ( MCP-RC          =   ZERO )
               PERFORM 900-SEIKYU-KEY91-FET-SEC
           ELSE
               MOVE    1           TO  FLG-SEIKYU
               INITIALIZE              RECE10-REC
           END-IF
      *
           .
      *
       900-SEIKYU-KEY91-SEL-EXT.
           EXIT.
      *****************************************************************
      *    請求管理FETCH処理
      *****************************************************************
       900-SEIKYU-KEY91-FET-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-SEIKYU
           MOVE    ZERO            TO  FLG-LOOPEND
      *
           PERFORM UNTIL ( FLG-SEIKYU  NOT =   ZERO )
                    OR   ( FLG-LOOPEND NOT =   ZERO )
      *
               MOVE    "tbl_seikyu"    TO  MCP-TABLE
               MOVE    "key91"         TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
      *
                   MOVE    MCPDATA-REC TO  RECE10-REC
      *
                   MOVE    1           TO  FLG-LOOPEND
      *
                   IF    ( RECE10-HKNNUM-S
                               =   "970" OR "971" OR "973" OR "975"
                                OR "068" OR "069" OR "040"
                                 )
                       CONTINUE
                   ELSE
                       MOVE    ZERO    TO  FLG-LOOPEND
                   END-IF
      *
                   IF    ( BGS100P-MEISAI      =   1 )
                    OR  (( BGS100P-SOUKATU     =   1 )
                    AND  ( BGS100P-ETCGETKBN   =   "0" ))
                    OR   ( RECE10-HKNNUM-S     =   "970" )
                       CONTINUE
                   ELSE
                       MOVE    ZERO    TO  FLG-LOOPEND
                   END-IF
      *
                   IF    ( RECE10-SKYKBN-RECEDEN   =   "1" )
                       MOVE    2       TO  BGS100P-RC
                   END-IF
      *
               ELSE
                   MOVE    1           TO  FLG-SEIKYU
                   INITIALIZE              RECE10-REC
               END-IF
      *
           END-PERFORM
      *
           .
      *
       900-SEIKYU-KEY91-FET-EXT.
           EXIT.
      *****************************************************************
      *    患者情報取得処理
      *****************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTINF
      *
           INITIALIZE                      PTINF-REC
           MOVE    RECE10-HOSPNUM      TO  PTINF-HOSPNUM
           MOVE    RECE10-PTID         TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
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
      *
       900-PTINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    請求ファイル読込処理
      *****************************************************************
       900-BGSSKY-READ-SEC             SECTION.
      *
           MOVE    ZERO            TO  FLG-BGSSKY
      *
           INITIALIZE                  BGSSKY-REC
           MOVE    RECE10-NYUGAIKBN
                                   TO  BGSSKY-NYUGAIKBN
      *
           EVALUATE    TRUE
           WHEN  ( BGS100P-EDITKBN =   "0" )
               MOVE    "0"         TO  BGSSKY-SKYSBT
           WHEN  ( BGS100P-SRYYM   =   RECE10-SRYYM )
               MOVE    "1"         TO  BGSSKY-SKYSBT
           WHEN  ( RECE10-HENREIYM =   "000000" )
               MOVE    "2"         TO  BGSSKY-SKYSBT
           WHEN    OTHER
               MOVE    "3"         TO  BGSSKY-SKYSBT
           END-EVALUATE
      *
           IF    ( WRK-HOKEN       =   CONST-SYAHO
             OR                        CONST-KOKUHO
             OR                        CONST-KOUKI )
               IF    ( BGS100P-SYUKA-HKN (WRK-HOKEN)
                                   =   1 )
                   MOVE    RECE10-RECEKA
                                   TO  BGSSKY-RECEKA
               ELSE
                   MOVE    "00"    TO  BGSSKY-RECEKA
               END-IF
           ELSE
                   MOVE    "00"    TO  BGSSKY-RECEKA
           END-IF
      *
           MOVE    WRK-HOKEN       TO  BGSSKY-HOKEN
           MOVE    RECE10-RECESYUBETU
                                   TO  BGSSKY-RECESBT
           MOVE    RECE10-HKNNUM   TO  BGSSKY-HKNNUM
      *
           EVALUATE    WRK-HOKEN
           WHEN    CONST-SYAHO
               PERFORM 9001-SYAHO-GROUPID-EDIT-SEC
           WHEN    CONST-KOKUHO
               PERFORM 9001-KOKUKO-GROUPID-EDIT-SEC
           WHEN    CONST-KOUKI
               PERFORM 9001-KOUKI-GROUPID-EDIT-SEC
           END-EVALUATE
      *
           MOVE    WRK-GROUPID     TO  BGSSKY-GROUPID
           READ    BGSSKY-FILE
           INVALID
               MOVE    1           TO  FLG-BGSSKY
           END-READ
      *
           .
      *
       900-BGSSKY-READ-EXT.
           EXIT.
      *****************************************************************
      *    公費ファイル読込処理
      *****************************************************************
       900-BGSKOH-READ-SEC             SECTION.
      *
           MOVE    ZERO            TO  FLG-BGSKOH
      *
           INITIALIZE                  BGSKOH-REC
           MOVE    RECE10-NYUGAIKBN
                                   TO  BGSKOH-NYUGAIKBN
      *
           EVALUATE    TRUE
           WHEN  ( BGS100P-EDITKBN =   "0" )
               MOVE    "0"         TO  BGSKOH-SKYSBT
           WHEN  ( BGS100P-SRYYM   =   RECE10-SRYYM )
               MOVE    "1"         TO  BGSKOH-SKYSBT
           WHEN  ( RECE10-HENREIYM =   "000000" )
               MOVE    "2"         TO  BGSKOH-SKYSBT
           WHEN    OTHER
               MOVE    "3"         TO  BGSKOH-SKYSBT
           END-EVALUATE
      *
           IF    ( WRK-HOKEN       =   CONST-SYAHO
             OR                        CONST-KOKUHO
             OR                        CONST-KOUKI )
               IF    ( BGS100P-SYUKA-HKN (WRK-HOKEN)
                                   =   1 )
                   MOVE    RECE10-RECEKA
                                   TO  BGSKOH-RECEKA
               ELSE
                   MOVE    "00"    TO  BGSKOH-RECEKA
               END-IF
           ELSE
                   MOVE    "00"    TO  BGSKOH-RECEKA
           END-IF
           MOVE    WRK-HOKEN       TO  BGSKOH-HOKEN
           MOVE    WRK-KOHKBN      TO  BGSKOH-KOHKBN
           MOVE    WRK-KOHNUM      TO  BGSKOH-KOHNUM
           READ    BGSKOH-FILE
           INVALID
               MOVE    1           TO  FLG-BGSKOH
           END-READ
      *
           .
      *
       900-BGSKOH-READ-EXT.
           EXIT.
      *****************************************************************
      *    保険者別請求ファイル読込処理
      *****************************************************************
       900-BGSHKNJA-READ-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-BGSHKNJA
      *
           READ    BGSHKNJA-FILE
           INVALID
               MOVE    1           TO  FLG-BGSHKNJA
           END-READ
      *
           .
      *
       900-BGSHKNJA-READ-EXT.
           EXIT.
      *****************************************************************
      *    その他請求ファイル読込処理
      *****************************************************************
       900-BGSETC-READ-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-BGSETC
      *
           READ    BGSETC-FILE
           INVALID
               MOVE    1           TO  FLG-BGSETC
           END-READ
      *
           .
      *
       900-BGSETC-READ-EXT.
           EXIT.
      *****************************************************************
      *    レセプト別請求ファイル読込処理
      *****************************************************************
       900-BGSMEI-READ-SEC          SECTION.
      *
           MOVE    ZERO                 TO  FLG-BGSMEI
      *
           INITIALIZE                   BGSMEI-REC
           MOVE    TBGSMEI-HOKEN        TO  BGSMEI-HOKEN
           MOVE    TBGSMEI-SKYSBT       TO  BGSMEI-SKYSBT
           MOVE    TBGSMEI-NYUGAIKBN    TO  BGSMEI-NYUGAIKBN
           MOVE    TBGSMEI-RECEKA       TO  BGSMEI-RECEKA
      *
           MOVE    TBGSMEI-RECESBT-G    TO  BGSMEI-RECESBT-G
           MOVE    CONST-HKNKBN2-TTL    TO  BGSMEI-SBTHKN
      *
           IF    ( BGS100P-HKNJANUM
                                    NOT =   SPACE )
               MOVE    TBGSMEI-HKNJANUM TO  BGSMEI-HKNJANUM
           END-IF
      *
           READ    BGSMEI-FILE
           INVALID
               MOVE    1           TO  FLG-BGSMEI
           END-READ
      *
           .
      *
       900-BGSMEI-READ-EXT.
           EXIT.
      *****************************************************************
      *    社保保険区分編集処理
      *****************************************************************
       9001-SYHAO-HKNKBN-EDIT-SEC      SECTION.
      *
           INITIALIZE                  WRK-SBTKBN-G
      *
           PERFORM 9001-SYAHO-GROUPID-EDIT-SEC
           IF    ( WRK-GROUPID     NOT =   ZERO )
               MOVE    WRK-GROUPID     TO  WRK-SBTGRPID
               IF    ( WRK-GROUPID     =   CONST-SYA-KOH2HEI
                                        OR CONST-SYA-KOH3HEI )
                   MOVE    CONST-SYA-KOHTAN
                                       TO  WRK-SBTGRPID
               END-IF
               PERFORM 90011-SYHAO-HKNKBN-EDIT-SEC
           END-IF
      *
           .
       9001-SYHAO-HKNKBN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    社保保険区分編集処理(保険の種類）
      *****************************************************************
       90011-SYHAO-HKNKBN-EDIT-SEC     SECTION.
      *
           EVALUATE    RECE10-HKNNUM
           WHEN    "001"
           WHEN    "009"
               MOVE    CONST-SYA-H01    TO  WRK-SBTHKN
           WHEN    "002"
               MOVE    CONST-SYA-H02    TO  WRK-SBTHKN
           WHEN    "003"
               MOVE    CONST-SYA-H03    TO  WRK-SBTHKN
           WHEN    "004"
               MOVE    CONST-SYA-H04    TO  WRK-SBTHKN
           WHEN    "031"   THRU    "034"
               MOVE    CONST-SYA-H31    TO  WRK-SBTHKN
           WHEN    "006"
               MOVE    CONST-SYA-H06    TO  WRK-SBTHKN
           WHEN    "007"
               MOVE    CONST-SYA-H07    TO  WRK-SBTHKN
           WHEN    "063"
           WHEN    "072"   THRU    "075"
               MOVE    CONST-SYA-H63    TO  WRK-SBTHKN
           END-EVALUATE
      *
           .
       90011-SYHAO-HKNKBN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    国保保険区分編集処理
      *****************************************************************
       9001-KOKUKO-HKNKBN-EDIT-SEC     SECTION.
      *
           INITIALIZE                  WRK-SBTKBN-G
      *
           PERFORM 9001-KOKUKO-GROUPID-EDIT-SEC
           MOVE    WRK-GROUPID     TO  WRK-SBTGRPID
      *
           .
       9001-KOKUKO-HKNKBN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    後期高齢者保険区分編集処理
      *****************************************************************
       9001-KOUKI-HKNKBN-EDIT-SEC      SECTION.
      *
           INITIALIZE                  WRK-SBTKBN-G
      *
           PERFORM 9001-KOUKI-GROUPID-EDIT-SEC
           MOVE    WRK-GROUPID     TO  WRK-SBTGRPID
      *
           .
       9001-KOUKI-HKNKBN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    労災保険区分編集処理
      *****************************************************************
       9001-ROUSAI-HKNKBN-EDIT-SEC     SECTION.
      *
           INITIALIZE                  WRK-SBTKBN-G
      *
           PERFORM 9001-ROUSAI-GROUPID-EDIT-SEC
           MOVE    WRK-GROUPID     TO  WRK-SBTGRPID
      *
           .
       9001-ROUSAI-HKNKBN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    自賠責保険区分編集処理
      *****************************************************************
       9001-JIBAI-HKNKBN-EDIT-SEC      SECTION.
      *
           INITIALIZE                  WRK-SBTKBN-G
      *
           PERFORM 9001-JIBAI-GROUPID-EDIT-SEC
           MOVE    WRK-GROUPID     TO  WRK-SBTGRPID
      *
           .
       9001-JIBAI-HKNKBN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    公害保険区分編集処理
      *****************************************************************
       9001-KOUGAI-HKNKBN-EDIT-SEC     SECTION.
      *
           INITIALIZE                  WRK-SBTKBN-G
      *
           PERFORM 9001-KOUGAI-GROUPID-EDIT-SEC
           MOVE    WRK-GROUPID     TO  WRK-SBTGRPID
      *
           .
       9001-KOUGAI-HKNKBN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    第三者行為保険区分編集処理
      *****************************************************************
       9001-DAISAN-HKNKBN-EDIT-SEC     SECTION.
      *
           INITIALIZE                  WRK-SBTKBN-G
      *
           PERFORM 9001-DAISAN-GROUPID-EDIT-SEC
           MOVE    WRK-GROUPID     TO  WRK-SBTGRPID
      *
           .
       9001-DAISAN-HKNKBN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    特別療養費保険区分編集処理
      *****************************************************************
       9001-TOKUYO-HKNKBN-EDIT-SEC     SECTION.
      *
           INITIALIZE                  WRK-SBTKBN-G
      *
           PERFORM 9001-TOKUYO-GROUPID-EDIT-SEC
           MOVE    WRK-GROUPID     TO  WRK-SBTGRPID
      *
           .
       9001-TOKUYO-HKNKBN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    社保グループＩＤ編集処理
      *****************************************************************
       9001-SYAHO-GROUPID-EDIT-SEC     SECTION.
      *
           MOVE   ZERO                     TO  WRK-GROUPID
      *
           EVALUATE    RECE10-RECESYUBETU
           WHEN    "1117"
           WHEN    "1118"
               MOVE    CONST-SYA-KOU9TAN   TO  WRK-GROUPID
           WHEN    "1127"
           WHEN    "1128"
           WHEN    "1137"
           WHEN    "1138"
           WHEN    "1147"
           WHEN    "1148"
           WHEN    "1157"
           WHEN    "1158"
               MOVE    CONST-SYA-KOU9HEI   TO  WRK-GROUPID
           WHEN    "1119"
           WHEN    "1110"
               MOVE    CONST-SYA-KOU7TAN   TO  WRK-GROUPID
           WHEN    "1129"
           WHEN    "1120"
           WHEN    "1139"
           WHEN    "1130"
           WHEN    "1149"
           WHEN    "1140"
           WHEN    "1159"
           WHEN    "1150"
               MOVE    CONST-SYA-KOU7HEI   TO  WRK-GROUPID
           WHEN    "1111"
           WHEN    "1112"
               MOVE    CONST-SYA-HONTAN    TO  WRK-GROUPID
           WHEN    "1121"
           WHEN    "1122"
           WHEN    "1131"
           WHEN    "1132"
           WHEN    "1141"
           WHEN    "1142"
           WHEN    "1151"
           WHEN    "1152"
               MOVE    CONST-SYA-HONHEI    TO  WRK-GROUPID
           WHEN    "1115"
           WHEN    "1116"
               MOVE    CONST-SYA-KZKTAN    TO  WRK-GROUPID
           WHEN    "1125"
           WHEN    "1126"
           WHEN    "1135"
           WHEN    "1136"
           WHEN    "1145"
           WHEN    "1146"
           WHEN    "1155"
           WHEN    "1156"
               MOVE    CONST-SYA-KZKHEI    TO  WRK-GROUPID
           WHEN    "1113"
           WHEN    "1114"
               MOVE    CONST-SYA-MISGKTAN  TO  WRK-GROUPID
           WHEN    "1123"
           WHEN    "1124"
           WHEN    "1133"
           WHEN    "1134"
           WHEN    "1143"
           WHEN    "1144"
           WHEN    "1153"
           WHEN    "1154"
               MOVE    CONST-SYA-MISGKHEI  TO  WRK-GROUPID
           WHEN    "1211"
           WHEN    "1212"
               MOVE    CONST-SYA-KOHTAN    TO  WRK-GROUPID
           WHEN    "1221"
           WHEN    "1222"
               MOVE    CONST-SYA-KOH2HEI   TO  WRK-GROUPID
           WHEN    "1231"
           WHEN    "1232"
           WHEN    "1241"
           WHEN    "1242"
               MOVE    CONST-SYA-KOH3HEI   TO  WRK-GROUPID
           WHEN    OTHER
               DISPLAY "RECESBT[" RECE10-RECESYUBETU "]" RECE10-REC
           END-EVALUATE
      *
           .
       9001-SYHAO-GROUPID-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    国保グループＩＤ編集処理
      *****************************************************************
       9001-KOKUKO-GROUPID-EDIT-SEC    SECTION.
      *
           MOVE   ZERO             TO     WRK-GROUPID
      *
           EVALUATE    RECE10-RECESYUBETU
           WHEN    "1117"
           WHEN    "1118"
           WHEN    "1127"
           WHEN    "1128"
           WHEN    "1137"
           WHEN    "1138"
           WHEN    "1147"
           WHEN    "1148"
           WHEN    "1157"
           WHEN    "1158"
               MOVE    CONST-KOK-IPNKOU9   TO  WRK-GROUPID
           WHEN    "1119"
           WHEN    "1110"
           WHEN    "1129"
           WHEN    "1120"
           WHEN    "1139"
           WHEN    "1130"
           WHEN    "1149"
           WHEN    "1140"
           WHEN    "1159"
           WHEN    "1150"
               MOVE    CONST-KOK-IPNKOU7   TO  WRK-GROUPID
           WHEN    "1111"
           WHEN    "1112"
           WHEN    "1121"
           WHEN    "1122"
           WHEN    "1131"
           WHEN    "1132"
           WHEN    "1141"
           WHEN    "1142"
           WHEN    "1151"
           WHEN    "1152"
               MOVE    CONST-KOK-IPN      TO  WRK-GROUPID
           WHEN    "1115"
           WHEN    "1116"
           WHEN    "1125"
           WHEN    "1126"
           WHEN    "1135"
           WHEN    "1136"
           WHEN    "1145"
           WHEN    "1146"
           WHEN    "1155"
           WHEN    "1156"
               MOVE    CONST-KOK-IPN      TO  WRK-GROUPID
           WHEN    "1113"
           WHEN    "1114"
           WHEN    "1123"
           WHEN    "1124"
           WHEN    "1133"
           WHEN    "1134"
           WHEN    "1143"
           WHEN    "1144"
           WHEN    "1153"
           WHEN    "1154"
               MOVE    CONST-KOK-IPNMISGK TO  WRK-GROUPID
           WHEN    "1411"
           WHEN    "1412"
           WHEN    "1421"
           WHEN    "1422"
           WHEN    "1431"
           WHEN    "1432"
           WHEN    "1441"
           WHEN    "1442"
           WHEN    "1451"
           WHEN    "1452"
               MOVE    CONST-KOK-TAI      TO  WRK-GROUPID
           WHEN    "1415"
           WHEN    "1416"
           WHEN    "1425"
           WHEN    "1426"
           WHEN    "1435"
           WHEN    "1436"
           WHEN    "1445"
           WHEN    "1446"
           WHEN    "1455"
           WHEN    "1456"
               MOVE    CONST-KOK-TAIKZK   TO  WRK-GROUPID
           WHEN    "1413"
           WHEN    "1414"
           WHEN    "1423"
           WHEN    "1424"
           WHEN    "1433"
           WHEN    "1434"
           WHEN    "1443"
           WHEN    "1444"
           WHEN    "1453"
           WHEN    "1454"
               MOVE    CONST-KOK-TAIMISGK TO  WRK-GROUPID
           WHEN    OTHER
               DISPLAY "RECESBT[" RECE10-RECESYUBETU "]" RECE10-REC
           END-EVALUATE
      *
           .
      *
       9001-KOKUKO-GROUPID-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    後期高齢者グループＩＤ編集処理
      *****************************************************************
       9001-KOUKI-GROUPID-EDIT-SEC    SECTION.
      *
            MOVE   ZERO                    TO  WRK-GROUPID
      *
           EVALUATE    RECE10-RECESYUBETU
           WHEN    "1317"
           WHEN    "1318"
           WHEN    "1327"
           WHEN    "1328"
           WHEN    "1337"
           WHEN    "1338"
           WHEN    "1347"
           WHEN    "1348"
           WHEN    "1357"
           WHEN    "1358"
               MOVE    CONST-KOU-KOU9      TO  WRK-GROUPID
           WHEN    "1319"
           WHEN    "1310"
           WHEN    "1329"
           WHEN    "1320"
           WHEN    "1339"
           WHEN    "1330"
           WHEN    "1349"
           WHEN    "1340"
           WHEN    "1359"
           WHEN    "1350"
               MOVE    CONST-KOU-KOU7      TO  WRK-GROUPID
           WHEN    OTHER
               DISPLAY "RECESBT[" RECE10-RECESYUBETU "]" RECE10-REC
           END-EVALUATE
      *
           .
      *
       9001-KOUKI-GROUPID-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    労災グループＩＤ編集処理
      *****************************************************************
       9001-ROUSAI-GROUPID-EDIT-SEC    SECTION.
      *
            MOVE   ZERO                    TO  WRK-GROUPID
      *
           EVALUATE    RECE10-RECESYUBETU
           WHEN    "7071"
           WHEN    "7081"
               MOVE    CONST-RSI-TG        TO  WRK-GROUPID
           WHEN    "7072"
           WHEN    "7082"
               MOVE    CONST-RSI-TT        TO  WRK-GROUPID
           WHEN    "7101"
           WHEN    "7091"
               MOVE    CONST-RSI-SG        TO  WRK-GROUPID
           WHEN    "7102"
           WHEN    "7092"
               MOVE    CONST-RSI-ST        TO  WRK-GROUPID
           WHEN    "7021"
               MOVE    CONST-RSI-AC        TO  WRK-GROUPID
           WHEN    "7051"
           WHEN    "7061"
               MOVE    CONST-RSI-KS        TO  WRK-GROUPID
           WHEN    OTHER
               DISPLAY "RECESBT[" RECE10-RECESYUBETU "]" RECE10-REC
           END-EVALUATE
      *
           .
      *
       9001-ROUSAI-GROUPID-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    自賠責グループＩＤ編集処理
      *****************************************************************
       9001-JIBAI-GROUPID-EDIT-SEC     SECTION.
      *
           MOVE   ZERO                    TO  WRK-GROUPID
      *
           EVALUATE    RECE10-RECESYUBETU
           WHEN    "7011"
           WHEN    "7001"
               IF      RECE10-RECEKBN      =   "4"
                   MOVE    CONST-JBI-NEW   TO  WRK-GROUPID
               ELSE
                   MOVE    CONST-JBI-USUAL TO  WRK-GROUPID
               END-IF
           WHEN    OTHER
               DISPLAY "RECESBT[" RECE10-RECESYUBETU "]" RECE10-REC
           END-EVALUATE
      *
           .
      *
       9001-JIBAI-GROUPID-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    公害グループＩＤ編集処理
      *****************************************************************
       9001-KOUGAI-GROUPID-EDIT-SEC        SECTION.
      *
            MOVE   ZERO                    TO  WRK-GROUPID
      *
           EVALUATE    RECE10-RECESYUBETU
           WHEN    "7200"
           WHEN    "7210"
               MOVE    CONST-KGI-GRP       TO  WRK-GROUPID
           WHEN    OTHER
               DISPLAY "RECESBT[" RECE10-RECESYUBETU "]" RECE10-REC
           END-EVALUATE
      *
           .
      *
       9001-KOUGAI-GROUPID-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    第三者行為グループＩＤ編集処理
      *****************************************************************
       9001-DAISAN-GROUPID-EDIT-SEC       SECTION.
      *
           MOVE   ZERO                    TO  WRK-GROUPID
      *
           EVALUATE    RECE10-RECESYUBETU
           WHEN    "7011"
           WHEN    "7001"
                   MOVE    CONST-DSN-GRP  TO  WRK-GROUPID
           WHEN    OTHER
               DISPLAY "RECESBT[" RECE10-RECESYUBETU "]" RECE10-REC
           END-EVALUATE
      *
           .
      *
       9001-DAISAN-GROUPID-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    特別療養費グループＩＤ編集処理
      *****************************************************************
       9001-TOKUYO-GROUPID-EDIT-SEC       SECTION.
      *
           MOVE   ZERO                 TO  WRK-GROUPID
      *
           EVALUATE    RECE10-RECESYUBETU
           WHEN    "6666"
               EVALUATE    RECE10-HKNNUM-S
               WHEN    "068"
                   MOVE    CONST-TKY-H068
                                       TO  WRK-GROUPID
               WHEN    "069"
                   MOVE    CONST-TKY-H069
                                       TO  WRK-GROUPID
               WHEN    "040"
                   MOVE    CONST-TKY-H040
                                       TO  WRK-GROUPID
               END-EVALUATE
           WHEN    OTHER
               DISPLAY "RECESBT[" RECE10-RECESYUBETU "]" RECE10-REC
           END-EVALUATE
      *
           .
      *
       9001-TOKUYO-GROUPID-EDIT-EXT.
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
      *    ＤＢ検索処理（FHETCHを行わない)
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
       910-DBCLOSECURSOR-SEC                 SECTION.
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
