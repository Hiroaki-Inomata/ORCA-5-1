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
       PROGRAM-ID.                 ORCBGS121.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 新保険別請求チェック表 明細表
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
      *    レセプト別請求ファイル
       FD  BGSMEI-FILE.
       01  BGSMEI-REC.
           COPY    "CPBGSF01.INC"  REPLACING  //BGSF01-//
                                   BY         //BGSMEI-//.
      *    エラーファイル
       FD  BGS100ERR-FILE.
       01  BGS100ERR-REC           PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
           COPY  "HCGS121NV02.INC" REPLACING  //HCGS121NV02//
                                   BY         //HCGS121N//.
           COPY  "HCGS121GV02.INC" REPLACING  //HCGS121GV02//
                                   BY         //HCGS121G//.
      *
           COPY  "CPASGNPARA.INC"  REPLACING  //ASGNPARA//
                                   BY         //ASS-BGSMEI//.
           COPY  "CPASGNPARA.INC"  REPLACING  //ASGNPARA//
                                   BY         //ASS-BGS100ERR//.
      *
       01  STS-AREA.
           03  STS-BGSMEI          PIC X(02).
           03  STS-BGS100ERR       PIC X(02).
      *
       01  CNT-AREA.
           03  CNT-NYUIN           PIC 9(05).
           03  CNT-GAIRAI          PIC 9(05).
           03  CNT-PAGE            PIC 9(05).
           03  CNT-LINE            PIC 9(05).
           03  CNT-KENSU           PIC 9(05).
           03  CNT-LEN             PIC 9(05).
           03  CNT-CSV             PIC 9(05).
      *
       01  FLG-AREA.
           03  FLG-BGSMEI          PIC 9(01).
           03  FLG-LOOPEND         PIC 9(01).
           03  FLG-HKNJAINF        PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-PTNUM           PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                PIC 9(05).
           03  IDX1                PIC 9(05).
           03  IDX2                PIC 9(05).
           03  IDX-HKN             PIC 9(05).
           03  IDX-TTL             PIC 9(05).
           03  IDX-BASE            PIC 9(05).
           03  IDXP                PIC 9(05).
           03  IDX-RECEKA          PIC 9(05).
           03  IDXI                PIC 9(05).
           03  IDXL                PIC 9(05).
           03  IDXM                PIC 9(05).
           03  IDXT                PIC 9(05).
      *
       01  WRK-AREA.
           03  WRK-ITEM-SIZE       PIC 9(05).
           03  WRK-ITEM            PIC X(100).
           03  WRK-SYMD            PIC X(08).
           03  WRK-EDTYMD1         PIC X(10).
           03  WRK-EDTYMD3         PIC X(22).
           03  WRK-ERRMSG          PIC X(200).
           03  WRK-Z3              PIC Z(3).
           03  WRK-Z4              PIC Z(3)9.
           03  WRK-Z5              PIC Z(5).
           03  WRK-Z9              PIC Z(9).
           03  WRK-TENZ            PIC Z(8)9.
           03  WRK-M9              PIC -(9).
           03  WRK-HKNNUM          PIC X(3).
           03  WRK-TTLID           PIC 9(5).
           03  WRK-CSV-SRYYM       PIC X(10).
           03  WRK-PRT-SRYYM       PIC X(16).
           03  WRK-PRT-SAKYMD      PIC X(22).
           03  WRK-KANACHK-STR     PIC X(500).
           03  WRK-SRYKANAME1      PIC X(24).
           03  WRK-SKYSBTNAME      PIC X(50).
           03  WRK-MAX             PIC 9(05).
           03  WRK-CSVDATA         PIC X(20000).
           03  WRK-NYUGAIKBN       PIC X(01).
           03  WRK-NYUGAIKBNMEI    PIC X(04).
           03  WRK-PRTKBN.
               05  WRK-PRTKBNZ9    PIC Z9.
           03  WRK-PRTKBNMEI       PIC X(40).
           03  WRK-DATAKBN         PIC X(01).
           03  WRK-DATAKBNMEI      PIC X(50).
           03  WRK-SBTKBN-G.
               05  WRK-SBTGRPID    PIC 9(02).
               05  WRK-SBTRATE     PIC 9(02).
               05  WRK-SBTHKN      PIC 9(02).
           03  WRK-HKNKBNNAME      PIC X(20).
           03  WRK-SYUKA-HKN       PIC 9(01).
           03  WRK-HKNKBN-HKN-G.
               05  WRK-HKNKBN-HKN  PIC 9(01).
           03  WRK-HKNJANUM        PIC X(08).
           03  WRK-HKNJANAME       PIC X(50).
      *
       01  CSV-HEAD-AREA.
           03  CSV-H01-G.
               05  CSV-H01-VAL.
                   07  CSV-H01-PRTKBN          PIC X(50)
                           VALUE   "帳票区分".
                   07  CSV-H01-PRTKBNMEI       PIC X(50)
                           VALUE   "帳票区分名".
                   07  CSV-H01-DATAKBN         PIC X(50)
                           VALUE   "データ区分".
                   07  CSV-H01-DATAKBNMEI      PIC X(50)
                           VALUE   "データ区分名".
                   07  CSV-H01-SRYYM           PIC X(50)
                           VALUE   "診療年月".
                   07  CSV-H01-NYUGAIKBN       PIC X(50)
                           VALUE   "入外区分".
                   07  CSV-H01-NYUGAIKBNMEI    PIC X(50)
                           VALUE   "入外区分名称".
                   07  CSV-H01-SKYSBTKBN       PIC X(50)
                           VALUE   "当月・当月外区分".
                   07  CSV-H01-SKYSBT          PIC X(50)
                           VALUE   "当月・当月外区分名称".
                   07  CSV-H01-SRYKA           PIC X(50)
                           VALUE   "診療科コード".
                   07  CSV-H01-SRYKAMEI        PIC X(50)
                           VALUE   "診療科名称".
                   07  CSV-H01-HKNKBN          PIC X(50)
                           VALUE   "保険区分".
                   07  CSV-H01-PTNUM           PIC X(50)
                           VALUE   "患者番号".
                   07  CSV-H01-NAME            PIC X(50)
                           VALUE   "氏名".
                   07  CSV-H01-HBTNUM          PIC X(50)
                           VALUE   "法別番号".
                   07  CSV-H01-HKNJANUM        PIC X(50)
                           VALUE   "保険者番号".
                   07  CSV-H01-HKNJANAME       PIC X(50)
                           VALUE   "保険者名".
                   07  CSV-H01-KENSU           PIC X(50)
                           VALUE   "件数".
                   07  CSV-H01-NISSU           PIC X(50)
                           VALUE   "日数".
                   07  CSV-H01-TENSU           PIC X(50)
                           VALUE   "点数".
                   07  CSV-H01-FTNMONEY        PIC X(50)
                           VALUE   "患者負担額".
                   07  CSV-H01-HKNMONEY        PIC X(50)
                           VALUE   "保険請求額".
                   07  CSV-H01-KOHMONEY        PIC X(50)
                           VALUE   "公費請求額".
                   07  CSV-H01-TTLMONEY        PIC X(50)
                           VALUE   "保険診療収益（ａ）".
                   07  CSV-H01-SKJKAISU        PIC X(50)
                           VALUE   "食事・生活回数".
                   07  CSV-H01-SKJFTNMONEY     PIC X(50)
                           VALUE   "食事・生活標準負担額".
                   07  CSV-H01-SKJHKNMONEY     PIC X(50)
                           VALUE   "食事・生活保険請求額".
                   07  CSV-H01-SKJKOHMONEY     PIC X(50)
                           VALUE   "食事・生活公費請求額".
                   07  CSV-H01-SKJTTLMONEY     PIC X(50)
                           VALUE   "食事・生活請求円（ｂ）".
                   07  CSV-H01-TTLTTLMONEY     PIC X(50)
                           VALUE   "（ａ）＋（ｂ）".
                   07  CSV-H01-SRYYM2          PIC X(50)
                           VALUE   "診療年月（当月以外）".
               05  CSV-H01-R   REDEFINES   CSV-H01-VAL.
                   07  CSV-H01-ITEM            PIC X(50)
                                               OCCURS  31.
               05  CSV-H01-MAX                 PIC 9(05)
                                               VALUE   31.
           03  CSV-H02-G.
               05  CSV-H02-VAL.
                   07  CSV-H02-PRTKBN          PIC X(50)
                           VALUE   "帳票区分".
                   07  CSV-H02-PRTKBNMEI       PIC X(50)
                           VALUE   "帳票区分名".
                   07  CSV-H02-DATAKBN         PIC X(50)
                           VALUE   "データ区分".
                   07  CSV-H02-DATAKBNMEI      PIC X(50)
                           VALUE   "データ区分名".
                   07  CSV-H02-SRYYM           PIC X(50)
                           VALUE   "診療年月".
                   07  CSV-H02-NYUGAIKBN       PIC X(50)
                           VALUE   "入外区分".
                   07  CSV-H02-NYUGAIKBNMEI    PIC X(50)
                           VALUE   "入外区分名称".
                   07  CSV-H02-SKYSBTKBN       PIC X(50)
                           VALUE   "当月・当月外区分".
                   07  CSV-H02-SKYSBT          PIC X(50)
                           VALUE   "当月・当月外区分名称".
                   07  CSV-H02-SRYKA           PIC X(50)
                           VALUE   "診療科コード".
                   07  CSV-H02-SRYKAMEI        PIC X(50)
                           VALUE   "診療科名称".
                   07  CSV-H02-HKNKBN          PIC X(50)
                           VALUE   "保険区分".
                   07  CSV-H02-PTNUM           PIC X(50)
                           VALUE   "患者番号".
                   07  CSV-H02-NAME            PIC X(50)
                           VALUE   "氏名".
                   07  CSV-H02-HBTNUM          PIC X(50)
                           VALUE   "法別番号".
                   07  CSV-H02-HKNJANUM        PIC X(50)
                           VALUE   "保険者番号".
                   07  CSV-H02-HKNJANAME       PIC X(50)
                           VALUE   "保険者名".
                   07  CSV-H02-KENSU           PIC X(50)
                           VALUE   "件数".
                   07  CSV-H02-NISSU           PIC X(50)
                           VALUE   "日数".
                   07  CSV-H02-TENSU           PIC X(50)
                           VALUE   "点数".
                   07  CSV-H02-FTNMONEY        PIC X(50)
                           VALUE   "患者負担額".
                   07  CSV-H02-HKNMONEY        PIC X(50)
                           VALUE   "保険請求額".
                   07  CSV-H02-KOHMONEY        PIC X(50)
                           VALUE   "公費請求額".
                   07  CSV-H02-TTLMONEY        PIC X(50)
                           VALUE   "保険診療収益（ａ）".
                   07  CSV-H02-SRYYM2          PIC X(50)
                           VALUE   "診療年月（当月以外）".
               05  CSV-H02-R   REDEFINES   CSV-H02-VAL.
                   07  CSV-H02-ITEM            PIC X(50)
                                               OCCURS  25.
               05  CSV-H02-MAX                 PIC 9(05)
                                               VALUE   25.
      *
       01  KEY-AREA.
           03  KEY-RECEKA          PIC X(02).
           03  KEY-NYUGAIKBN       PIC X(01).
           03  KEY-SKYSBT          PIC X(01).
           03  KEY-SBTKBN-G.
               05  KEY-SBTGRPID    PIC 9(02).
               05  KEY-SBTRATE     PIC 9(02).
               05  KEY-SBTHKN      PIC 9(02).
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
           03  CONST-SYA-KOH       PIC 9(02)   VALUE 11.
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
      *
           03  CONST-DSN-GRP        PIC 9(02)   VALUE 1.
      *
           03  CONST-HKNKBN2-TTL   PIC 9(02)   VALUE 99.
      *
           03  CONST-MEI-TTL       PIC 9(03) VALUE 55.
           03  CONST-MEI-MAX       PIC 9(03) VALUE 54.
      *
       01  TOTAL-AREA.
           COPY    "CPBGSF01.INC"  REPLACING  //BGSF01-//
                                   BY         //TOTAL-//.
      *
      *    レセプト別請求データ退避領域
       01  TBGSMEI-AREA.
           COPY    "CPBGSF01.INC"  REPLACING  //BGSF01-//
                                   BY         //TBGSMEI-//.
      *
           COPY  "HCGS121N.INC"    REPLACING  //HCGS121N//
                                   BY         //TMPS121//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1005.INC".
      *
      *    保険者
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *
      *    患者番号
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".
      *
      *    全角半角チェック
           COPY    "CPORCSKANACHK.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    統計ＣＳＶ制御サブ
           COPY    "CPORCSTOUKEICSV.INC".
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
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           PERFORM 990-EXIT-PROGRAM-SEC
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
           MOVE    BGS100P-MEIFNAME    TO  ASS-BGSMEI
           MOVE    BGS100P-ERRFNAME    TO  ASS-BGS100ERR
      *
           MOVE    "STM"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           EVALUATE    BGS100P-HOKEN
           WHEN    CONST-SYAHO
               MOVE    "社保分作成中"
                                       TO  JOB-YOBI
           WHEN    CONST-KOKUHO
               MOVE    "国保分作成中"
                                       TO  JOB-YOBI
           WHEN    CONST-KOUKI
               MOVE    "後期高齢者分作成中"
                                       TO  JOB-YOBI
           WHEN    OTHER
               MOVE    "明細表作成中"
                                       TO  JOB-YOBI
           END-EVALUATE
      *
           PERFORM 800-ORCSJOB-SEC
      *
           MOVE    SPA-SYSYMD          TO  WRK-SYMD
           PERFORM 800-HIZUKE-EDIT-SEC
           MOVE    WRK-EDTYMD3         TO  WRK-PRT-SAKYMD
      *
           MOVE    BGS100P-SRYYMD      TO  WRK-SYMD
           PERFORM 800-HIZUKE-EDIT-SEC
           MOVE    WRK-EDTYMD3(1:16)   TO  WRK-PRT-SRYYM
      *
           MOVE    BGS100P-SRYYM(1:4)  TO  WRK-CSV-SRYYM(1:4)
           MOVE    "/"                 TO  WRK-CSV-SRYYM(5:1)
           MOVE    BGS100P-SRYYM(5:2)  TO  WRK-CSV-SRYYM(6:2)
      *
           MOVE    SPACE               TO  WRK-ITEM
           INSPECT WRK-ITEM  TALLYING  WRK-ITEM-SIZE   FOR ALL " "
      *
           EVALUATE    BGS100P-HOKEN
           WHEN    CONST-SYAHO
               MOVE    BGS100P-SYUKA-SYA
                                       TO  WRK-SYUKA-HKN
           WHEN    CONST-KOKUHO
               MOVE    BGS100P-SYUKA-KOK
                                       TO  WRK-SYUKA-HKN
           WHEN    CONST-KOUKI
               MOVE    BGS100P-SYUKA-KOU
                                       TO  WRK-SYUKA-HKN
           WHEN    OTHER
               MOVE    ZERO            TO  WRK-SYUKA-HKN
           END-EVALUATE
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
           IF    ( BGS100P-EDITKBN =   "1" )
               MOVE    "1"         TO  KEY-SKYSBT
               PERFORM 201-MAIN-SEC
               MOVE    "2"         TO  KEY-SKYSBT
               PERFORM 201-MAIN-SEC
           ELSE
               MOVE    SPACE       TO  KEY-SKYSBT
               PERFORM 201-MAIN-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    主　処理
      *****************************************************************
       201-MAIN-SEC                SECTION.
      *
           IF    ( WRK-SYUKA-HKN   =   1 )
               PERFORM VARYING IDX-RECEKA  FROM    1   BY  1
                       UNTIL ( IDX-RECEKA  >   BGS100P-RECEKA-MAX )
      *
                   MOVE    BGS100P-RECEKA (IDX-RECEKA)
                                   TO  KEY-RECEKA
      *
                   PERFORM 2001-MAIN-SEC
               END-PERFORM
           ELSE
               MOVE    SPACE       TO  KEY-RECEKA
               PERFORM 2001-MAIN-SEC
           END-IF
      *
           .
       201-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    主　処理
      *****************************************************************
       2001-MAIN-SEC                SECTION.
      *
           MOVE    SPACE               TO      WRK-SKYSBTNAME
           IF    ( BGS100P-EDITKBN =   "1" )
               EVALUATE    KEY-SKYSBT
               WHEN    "1"
                   MOVE  "当月診療分"  TO      WRK-SKYSBTNAME
               WHEN    "2"
                   MOVE  "当月診療分以外（月遅れ請求、返戻分再請求）"
                                       TO      WRK-SKYSBTNAME
               END-EVALUATE
           END-IF
      *
           MOVE    SPACE           TO      WRK-SRYKANAME1
           IF    ( WRK-SYUKA-HKN   =   1 )
               EVALUATE    KEY-RECEKA
               WHEN    "00"
                   MOVE    "主科設定なし"
                                       TO  WRK-SRYKANAME1
               WHEN    OTHER
                   INITIALIZE                  SYSKANRI-REC
                   MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
                   MOVE    "1005"          TO  SYS-KANRICD
                   MOVE    KEY-RECEKA      TO  SYS-KBNCD
                   MOVE    BGS100P-SRYYMD  TO  SYS-STYUKYMD
                                               SYS-EDYUKYMD
                   PERFORM 900-SYSKANRI-KEY10-SEL-SEC
                   MOVE    SYSKANRI-REC    TO  SYS-1005-REC
                   MOVE    SYS-1005-SRYKANAME1
                                       TO  WRK-SRYKANAME1
               END-EVALUATE
           END-IF
      *
           PERFORM 2001-MEI-EDIT-SEC
      *
           .
       2001-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細表編集処理
      *****************************************************************
       2001-MEI-EDIT-SEC               SECTION.
      *
           OPEN    INPUT  BGSMEI-FILE
      *
           PERFORM 900-BGSMEI-READ-SEC
      *
           PERFORM UNTIL ( FLG-BGSMEI  NOT =   ZERO )
      *
               MOVE    BGSMEI-NYUGAIKBN    TO  KEY-NYUGAIKBN
               MOVE    BGSMEI-SBTKBN-G     TO  KEY-SBTKBN-G
               MOVE    SPACE               TO  WRK-HKNKBNNAME
               INITIALIZE                      TOTAL-AREA
      *
               PERFORM UNTIL ( FLG-BGSMEI      NOT =   ZERO )
                         OR  ( KEY-NYUGAIKBN   NOT =   BGSMEI-NYUGAIKBN)
      *
                   PERFORM 210-HEAD-SEC
      *
                   PERFORM UNTIL ( FLG-BGSMEI  NOT =   ZERO )
                             OR  ( KEY-NYUGAIKBN
                                               NOT =   BGSMEI-NYUGAIKBN)
                             OR  ( CNT-LINE       >=   CONST-MEI-MAX )
      *
                       PERFORM 220-BODY-SEC
      *
                       PERFORM 900-BGSMEI-READ-SEC
      *
                       IF    ( FLG-BGSMEI      NOT =   ZERO )
                         OR  ( KEY-NYUGAIKBN   NOT =   BGSMEI-NYUGAIKBN)
                           PERFORM 2201-TOTAL-EDIT-SEC
                       END-IF
      *
                   END-PERFORM
      *
                   PERFORM 230-HCGS121-PRINT-SEC
                   PERFORM 2001-HCGS121-CSV-SEC
      *
               END-PERFORM
      *
           END-PERFORM
      *
           CLOSE   BGSMEI-FILE
      *
           .
       2001-MEI-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    明細表ヘッダー処理
      *****************************************************************
       210-HEAD-SEC                    SECTION.
      *
           INITIALIZE                      HCGS121N
                                           TMPS121
      *
           EVALUATE    BGSMEI-HOKEN
           WHEN    CONST-SYAHO
               MOVE    "（社保）"      TO  HCGS121N-HKNNAME
           WHEN    CONST-KOKUHO
               MOVE    "（国保）"      TO  HCGS121N-HKNNAME
           WHEN    CONST-KOUKI
               MOVE    "（後期高齢者）"
                                       TO  HCGS121N-HKNNAME
           WHEN    CONST-TOKUYO
               MOVE    "（特別療養費）" 
                                       TO  HCGS121N-HKNNAME
           WHEN    CONST-ROUSAI
               MOVE    "（労災）"      TO  HCGS121N-HKNNAME
           WHEN    CONST-JIBAI
               MOVE    "（自賠責）"    TO  HCGS121N-HKNNAME
           WHEN    CONST-KOUGAI
               MOVE    "（公害）"      TO  HCGS121N-HKNNAME
           WHEN    CONST-DAISAN
               MOVE    "（自賠責（第三者行為））"
                                       TO  HCGS121N-HKNNAME
           END-EVALUATE
      *
           MOVE    WRK-PRT-SRYYM       TO  HCGS121N-SRYYM
           IF    ( BGSMEI-NYUGAIKBN    =   "1" )
               MOVE    "入院"          TO  HCGS121N-NYUGAI
           ELSE
               MOVE    "外来"          TO  HCGS121N-NYUGAI
           END-IF
           MOVE    WRK-PRT-SAKYMD      TO  HCGS121N-SYSYMD
           MOVE    WRK-SKYSBTNAME      TO  HCGS121N-SKYSBT
      *
           MOVE    WRK-SRYKANAME1      TO  HCGS121N-SRYKANAME
      *
           COMPUTE CNT-PAGE  = CNT-PAGE    +   1
           MOVE    CNT-PAGE            TO  WRK-Z3
           MOVE    WRK-Z3              TO  HCGS121N-PAGE
      *
           MOVE    ZERO                TO  CNT-LINE
      *
           MOVE    BGSMEI-NYUGAIKBN    TO  KEY-NYUGAIKBN
      *
           .
       210-HEAD-EXT.
           EXIT.
      *****************************************************************
      *    明細表明細処理
      *****************************************************************
       220-BODY-SEC                    SECTION.
      *
           COMPUTE CNT-LINE = CNT-LINE +   1
      *
           MOVE    BGSMEI-SBTKBN-G TO  WRK-SBTKBN-G
      *
           IF    ( WRK-SBTHKN      =   CONST-HKNKBN2-TTL )
               MOVE    "　　小　計"
                                   TO  HCGS121N-HKNKBNNAME (CNT-LINE)
                                       TMPS121-HKNKBNNAME (CNT-LINE)
           ELSE
      *
               PERFORM 900-PTNUM-KEY-SEL-SEC
      *
               IF    ( CNT-LINE            =   1 )
                OR   ( KEY-SBTGRPID    NOT =   WRK-SBTGRPID )
                OR   ( KEY-SBTRATE     NOT =   WRK-SBTRATE )
      *
                   MOVE    WRK-SBTGRPID
                                   TO  KEY-SBTGRPID
                   MOVE    WRK-SBTRATE
                                   TO  KEY-SBTRATE
      *
                   PERFORM 2201-HKNKBNNAME-EDIT-SEC
      *
                   EVALUATE    WRK-HKNKBNNAME
                   WHEN "高一公費併用（９）"
                       MOVE    "高一公費併用(9)"
                                   TO  HCGS121N-HKNKBNNAME(CNT-LINE)(3:)
                   WHEN "高一公費併用（８）"
                       MOVE    "高一公費併用(8)"
                                   TO  HCGS121N-HKNKBNNAME(CNT-LINE)(3:)
                   WHEN OTHER
                       MOVE    WRK-HKNKBNNAME
                                   TO  HCGS121N-HKNKBNNAME(CNT-LINE)(3:)
                   END-EVALUATE
      *
               END-IF
      *
               IF   (  BGSMEI-HOKEN        =   CONST-SYAHO
                   AND WRK-SBTGRPID        =   CONST-SYA-KOH )
                 OR (  BGSMEI-HOKEN        =   CONST-ROUSAI
                                            OR CONST-JIBAI
                                            OR CONST-KOUGAI
                                            OR CONST-DAISAN
                                                            )
                   CONTINUE
               ELSE
                   IF    ( BGSMEI-HOKEN    NOT =   CONST-TOKUYO )
                       IF    ( BGSMEI-HKNNUM   =   "009" )
                           MOVE    "01"
                                   TO  HCGS121N-HBTNUM (CNT-LINE)
                       ELSE
                           MOVE    BGSMEI-HKNNUM (2:2)
                                   TO  HCGS121N-HBTNUM (CNT-LINE)
                       END-IF
                   END-IF
      *
                   MOVE    BGSMEI-HKNJANUM
                                   TO  HCGS121N-HKNJANUM (CNT-LINE)
      *
                   PERFORM 900-HKNJAINF-KEY-SEL-SEC
      *
                   IF    ( HKNJA-HKNJANAME NOT =   SPACE )
                       MOVE    HKNJA-HKNJANAME
                                   TO  HCGS121N-HKNJANAME (CNT-LINE)
                   END-IF
               END-IF
      *
               EVALUATE    BGSMEI-SEIKYUFLG
               WHEN   1
                    MOVE    "◯" TO  HCGS121N-HKNKBNNAME(CNT-LINE)(1:2)
               WHEN   2
                    MOVE    "◇" TO  HCGS121N-HKNKBNNAME(CNT-LINE)(1:2)
               END-EVALUATE
      *
               IF    ( PTNUM-PTNUM (11:)   =   SPACE )
                   MOVE    PTNUM-PTNUM
                                   TO  HCGS121N-PTNUM (CNT-LINE)
               ELSE
                   MOVE    PTNUM-PTNUM
                                   TO  HCGS121N-PTNUM2 (CNT-LINE)
               END-IF
      *
               IF    ( BGSMEI-SRYYM    NOT =   BGS100P-SRYYM )
                   MOVE    BGSMEI-SRYYM    TO  WRK-SYMD
                   MOVE    "01"            TO  WRK-SYMD (7:2)
                   PERFORM 800-HIZUKE-EDIT-SEC
                   MOVE    WRK-EDTYMD1(1:6)
                                   TO  HCGS121N-SRYYM2(CNT-LINE)
                   MOVE    BGSMEI-SRYYM(1:4)
                                   TO  TMPS121-SRYYM2(CNT-LINE)(1:4)
                   MOVE    "/"     TO  TMPS121-SRYYM2(CNT-LINE)(5:1)
                   MOVE    BGSMEI-SRYYM(5:2)
                                   TO  TMPS121-SRYYM2(CNT-LINE)(6:2)
               END-IF
      *
               MOVE    BGSMEI-NAME     TO  HCGS121N-NAME (CNT-LINE)
      *
               MOVE    WRK-HKNKBNNAME
                                   TO  TMPS121-HKNKBNNAME (CNT-LINE)
               MOVE    PTNUM-PTNUM
                                   TO  TMPS121-PTNUM2     (CNT-LINE)
      *
           END-IF
      *
           MOVE    BGSMEI-REC      TO  TBGSMEI-AREA
           MOVE    CNT-LINE        TO  IDXM
           PERFORM 2201-BODY-SEC
           IF    ( BGSMEI-SEIKYUFLG    =    2 )
               MOVE    SPACE       TO  HCGS121N-NISSU (CNT-LINE)
                                       HCGS121N-TENSU (CNT-LINE)
           END-IF
      *
           IF    ( WRK-SBTHKN     NOT =   CONST-HKNKBN2-TTL )
               PERFORM 2201-TOTAL-SUM-SEC
           END-IF
      *
           .
       220-BODY-EXT.
           EXIT.
      *****************************************************************
      *    明細表明細処理
      *****************************************************************
       2201-BODY-SEC                   SECTION.
      *
           MOVE    TBGSMEI-KENSU   TO  WRK-Z4
           MOVE    WRK-Z4          TO  HCGS121N-KENSU (IDXM)
      *
           MOVE    TBGSMEI-NISSU   TO  WRK-Z4
           MOVE    WRK-Z4          TO  HCGS121N-NISSU (IDXM)
      *
           MOVE    TBGSMEI-TENSU   TO  WRK-TENZ
           MOVE    WRK-TENZ        TO  HCGS121N-TENSU (IDXM)
      *
           MOVE    TBGSMEI-FTNMONEY
                                   TO  WRK-Z9
           MOVE    WRK-Z9          TO  HCGS121N-FTNMONEY (IDXM)
      *
           MOVE    TBGSMEI-HKNMONEY
                                   TO  WRK-Z9
           MOVE    WRK-Z9          TO  HCGS121N-HKNMONEY (IDXM)
      *
           MOVE    TBGSMEI-KOHMONEY
                                   TO  WRK-Z9
           MOVE    WRK-Z9          TO  HCGS121N-KOHMONEY (IDXM)
      *
           MOVE    TBGSMEI-TTLMONEY
                                   TO  WRK-Z9
           MOVE    WRK-Z9          TO  HCGS121N-TTLMONEY (IDXM)
      *
           MOVE    TBGSMEI-SKJKAISU
                                   TO  WRK-Z5
           MOVE    WRK-Z5          TO  HCGS121N-SKJKAISU (IDXM)
      *
           MOVE    TBGSMEI-SKJTTLMONEY
                                   TO  WRK-Z9
           MOVE    WRK-Z9          TO  HCGS121N-SKJTTLMONEY (IDXM)
      *
           MOVE    TBGSMEI-SKJFTNMONEY
                                   TO  WRK-Z9
           MOVE    WRK-Z9          TO  HCGS121N-SKJFTNMONEY (IDXM)
      *
           MOVE    TBGSMEI-SKJHKNMONEY
                                   TO  WRK-Z9
           MOVE    WRK-Z9          TO  HCGS121N-SKJHKNMONEY (IDXM)
      *
           MOVE    TBGSMEI-SKJKOHMONEY
                                   TO  WRK-Z9
           MOVE    WRK-Z9          TO  HCGS121N-SKJKOHMONEY (IDXM)
      *
           MOVE    TBGSMEI-TTLTTLMONEY
                                   TO  WRK-Z9
           MOVE    WRK-Z9          TO  HCGS121N-TTLTTLMONEY (IDXM)
      *
           .
       2201-BODY-EXT.
           EXIT.
      *****************************************************************
      *    合計集計処理
      *****************************************************************
       2201-TOTAL-SUM-SEC              SECTION.
      *
           COMPUTE TOTAL-KENSU
               =   TOTAL-KENSU + BGSMEI-KENSU
           COMPUTE TOTAL-NISSU
               =   TOTAL-NISSU + BGSMEI-NISSU
           COMPUTE TOTAL-TENSU
               =   TOTAL-TENSU + BGSMEI-TENSU
           COMPUTE TOTAL-FTNMONEY
               =   TOTAL-FTNMONEY     + BGSMEI-FTNMONEY
           COMPUTE TOTAL-HKNMONEY
               =   TOTAL-HKNMONEY     + BGSMEI-HKNMONEY
           COMPUTE TOTAL-KOHMONEY
               =   TOTAL-KOHMONEY     + BGSMEI-KOHMONEY
           COMPUTE TOTAL-TTLMONEY
               =   TOTAL-TTLMONEY     + BGSMEI-TTLMONEY
           COMPUTE TOTAL-SKJKAISU
               =   TOTAL-SKJKAISU     + BGSMEI-SKJKAISU
           COMPUTE TOTAL-SKJTTLMONEY
               =   TOTAL-SKJTTLMONEY  + BGSMEI-SKJTTLMONEY
           COMPUTE TOTAL-SKJFTNMONEY
               =   TOTAL-SKJFTNMONEY  + BGSMEI-SKJFTNMONEY
           COMPUTE TOTAL-SKJHKNMONEY
               =   TOTAL-SKJHKNMONEY  + BGSMEI-SKJHKNMONEY
           COMPUTE TOTAL-SKJKOHMONEY
               =   TOTAL-SKJKOHMONEY  + BGSMEI-SKJKOHMONEY
           COMPUTE TOTAL-TTLTTLMONEY
               =   TOTAL-TTLTTLMONEY  + BGSMEI-TTLTTLMONEY
      *
           .
       2201-TOTAL-SUM-EXT.
           EXIT.
      *****************************************************************
      *    保険区分名称編集処理
      *****************************************************************
       2201-HKNKBNNAME-EDIT-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-HKNKBNNAME
      *
           EVALUATE    BGSMEI-HOKEN
           WHEN    CONST-SYAHO
               EVALUATE    WRK-SBTGRPID
               WHEN    CONST-SYA-KOU9HEI
                   IF    ( BGS100P-SRYYM  >=  CONST-H2605 )
                       IF    ( BGSMEI-SBTRATE  =  1 )
                           MOVE    "高一公費併用（９）"
                                       TO  WRK-HKNKBNNAME
                       ELSE
                           MOVE    "高一公費併用（８）"
                                       TO  WRK-HKNKBNNAME
                       END-IF
                   ELSE
                       MOVE    "高９公費併用"
                                       TO  WRK-HKNKBNNAME
                   END-IF
               WHEN    CONST-SYA-KOU9TAN
                   IF    ( BGS100P-SRYYM  >=  CONST-H2605 )
                       IF    ( BGSMEI-SBTRATE  =  1 )
                           MOVE    "高一単独（９）"
                                       TO  WRK-HKNKBNNAME
                       ELSE
                           MOVE    "高一単独（８）"
                                       TO  WRK-HKNKBNNAME
                       END-IF
                   ELSE
                       MOVE    "高９単独"
                                       TO  WRK-HKNKBNNAME
                   END-IF
               WHEN    CONST-SYA-KOU7HEI
                   MOVE    "高７公費併用"
                                       TO  WRK-HKNKBNNAME
               WHEN    CONST-SYA-KOU7TAN
                   MOVE    "高７単独"
                                       TO  WRK-HKNKBNNAME
               WHEN    CONST-SYA-HONHEI
                   MOVE    "本人公費併用"
                                       TO  WRK-HKNKBNNAME
               WHEN    CONST-SYA-HONTAN
                   MOVE    "本人単独"
                                       TO  WRK-HKNKBNNAME
               WHEN    CONST-SYA-KZKHEI
                   MOVE    "家族公費併用"
                                       TO  WRK-HKNKBNNAME
               WHEN    CONST-SYA-KZKTAN
                   MOVE    "家族単独"
                                       TO  WRK-HKNKBNNAME
               WHEN    CONST-SYA-MISGKHEI
                   MOVE    "６歳公費併用"
                                       TO  WRK-HKNKBNNAME
               WHEN    CONST-SYA-MISGKTAN
                   MOVE    "６歳単独"
                                       TO  WRK-HKNKBNNAME
               WHEN    CONST-SYA-KOH
                   MOVE    "公費単独・併用"
                                       TO  WRK-HKNKBNNAME
               END-EVALUATE
           WHEN    CONST-KOKUHO
               EVALUATE    WRK-SBTGRPID
               WHEN    CONST-KOK-IPNKOU9
                   IF    ( BGS100P-SRYYM  >=  CONST-H2605 )
                       IF    ( BGSMEI-SBTRATE  =  1 )
                           MOVE    "一般高一（９）"
                                       TO  WRK-HKNKBNNAME
                       ELSE
                           MOVE    "一般高一（８）"
                                       TO  WRK-HKNKBNNAME
                       END-IF
                   ELSE
                       MOVE    "一般高９"
                                       TO  WRK-HKNKBNNAME
                   END-IF
               WHEN    CONST-KOK-IPNKOU7
                   MOVE    "一般高７"
                                       TO  WRK-HKNKBNNAME
               WHEN    CONST-KOK-IPN
                   MOVE    "一般"
                                       TO  WRK-HKNKBNNAME
               WHEN    CONST-KOK-IPNMISGK
                   MOVE    "一般６歳"
                                       TO  WRK-HKNKBNNAME
               WHEN    CONST-KOK-TAI
                   MOVE    "退職本人"
                                       TO  WRK-HKNKBNNAME
               WHEN    CONST-KOK-TAIKZK
                   MOVE    "退職家族"
                                       TO  WRK-HKNKBNNAME
               WHEN    CONST-KOK-TAIMISGK
                   MOVE    "退職６歳"
                                       TO  WRK-HKNKBNNAME
               END-EVALUATE
           WHEN    CONST-KOUKI
               EVALUATE    WRK-SBTGRPID
               WHEN    CONST-KOU-KOU9
                   MOVE    "高９"
                                       TO  WRK-HKNKBNNAME
               WHEN    CONST-KOU-KOU7
                   MOVE    "高７"
                                       TO  WRK-HKNKBNNAME
               END-EVALUATE
           WHEN    CONST-TOKUYO
               EVALUATE    WRK-SBTGRPID
               WHEN    CONST-TKY-H068
                   MOVE    "特養費"
                                       TO  WRK-HKNKBNNAME
               WHEN    CONST-TKY-H069
                   MOVE    "退特養費"
                                       TO  WRK-HKNKBNNAME
               WHEN    CONST-TKY-H040
                   MOVE    "後期特養費"
                                       TO  WRK-HKNKBNNAME
               END-EVALUATE
           WHEN    CONST-ROUSAI
               EVALUATE    WRK-SBTGRPID
               WHEN    CONST-RSI-TG
                   MOVE    "短期・業務"
                                       TO  WRK-HKNKBNNAME
               WHEN    CONST-RSI-TT
                   MOVE    "短期・通勤"
                                       TO  WRK-HKNKBNNAME
               WHEN    CONST-RSI-SG
                   MOVE    "傷病・業務"
                                       TO  WRK-HKNKBNNAME
               WHEN    CONST-RSI-ST
                   MOVE    "傷病・通勤"
                                       TO  WRK-HKNKBNNAME
               WHEN    CONST-RSI-AC
                   MOVE    "アフターケア"
                                       TO  WRK-HKNKBNNAME
               WHEN    CONST-RSI-KS
                   MOVE    "公務災害"
                                       TO  WRK-HKNKBNNAME
               END-EVALUATE
           WHEN    CONST-JIBAI
               EVALUATE    WRK-SBTGRPID
               WHEN    CONST-JBI-NEW
                   MOVE    "新様式"
                                       TO  WRK-HKNKBNNAME
               WHEN    CONST-JBI-USUAL
                   MOVE    "従来様式"
                                       TO  WRK-HKNKBNNAME
               END-EVALUATE
           WHEN    CONST-KOUGAI
               EVALUATE    WRK-SBTGRPID
               WHEN    CONST-KGI-GRP
                   MOVE    "公害"
                                       TO  WRK-HKNKBNNAME
               END-EVALUATE
           WHEN    CONST-DAISAN
               EVALUATE    WRK-SBTGRPID
               WHEN    CONST-DSN-GRP
                   MOVE    "第三者行為"
                                       TO  WRK-HKNKBNNAME
               END-EVALUATE
           END-EVALUATE
      *
           .
       2201-HKNKBNNAME-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    合計行編集処理
      *****************************************************************
       2201-TOTAL-EDIT-SEC             SECTION.
      *
           MOVE    CONST-MEI-TTL   TO  IDXM
      *
           MOVE    "　　合　計"    TO  HCGS121N-HKNKBNNAME (IDXM)
                                       TMPS121-HKNKBNNAME  (IDXM)
           MOVE    TOTAL-AREA      TO  TBGSMEI-AREA
           PERFORM 2201-BODY-SEC
      *
           .
       2201-TOTAL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理
      *****************************************************************
       230-HCGS121-PRINT-SEC         SECTION.
      *
           INITIALIZE                  ORCSPRTAREA
      *
           IF    ( KEY-NYUGAIKBN   =   "1" )
               MOVE    "HCGS121NV02.red"  TO  SPRT-PRTID
               MOVE    HCGS121N        TO  SPRT-PRTDATA
           ELSE
               INITIALIZE                      HCGS121G
      *
               MOVE    HCGS121N-PAGE       TO  HCGS121G-PAGE
               MOVE    HCGS121N-SRYYM      TO  HCGS121G-SRYYM
               MOVE    HCGS121N-SYSYMD     TO  HCGS121G-SYSYMD
               MOVE    HCGS121N-NYUGAI     TO  HCGS121G-NYUGAI
               MOVE    HCGS121N-SRYKANAME  TO  HCGS121G-SRYKANAME
               MOVE    HCGS121N-HKNNAME    TO  HCGS121G-HKNNAME
               MOVE    HCGS121N-SKYSBT     TO  HCGS121G-SKYSBT
               PERFORM VARYING IDX0   FROM    1   BY  1
                       UNTIL ( IDX0   >   CONST-MEI-TTL )
                   MOVE    HCGS121N-PTNUM  (IDX0)
                                           TO  HCGS121G-PTNUM (IDX0)
                   MOVE    HCGS121N-PTNUM2 (IDX0)
                                           TO  HCGS121G-PTNUM2(IDX0)
                   MOVE    HCGS121N-SRYYM2 (IDX0)
                                           TO  HCGS121G-SRYYM2(IDX0)
                   MOVE    HCGS121N-NAME   (IDX0)
                                           TO  HCGS121G-NAME  (IDX0)
                   MOVE    HCGS121N-HKNJANUM  (IDX0)
                                           TO  HCGS121G-HKNJANUM  (IDX0)
                   MOVE    HCGS121N-HKNJANAME (IDX0)
                                           TO  HCGS121G-HKNJANAME (IDX0)
                   MOVE    HCGS121N-HKNKBNNAME(IDX0)
                                           TO  HCGS121G-HKNKBNNAME(IDX0)
                   MOVE    HCGS121N-HBTNUM (IDX0)
                                           TO  HCGS121G-HBTNUM (IDX0)
                   MOVE    HCGS121N-KENSU (IDX0)
                                           TO  HCGS121G-KENSU (IDX0)
                   MOVE    HCGS121N-NISSU (IDX0)
                                           TO  HCGS121G-NISSU (IDX0)
                   MOVE    HCGS121N-TENSU (IDX0)
                                           TO  HCGS121G-TENSU (IDX0)
                   MOVE    HCGS121N-FTNMONEY (IDX0)
                                           TO  HCGS121G-FTNMONEY (IDX0)
                   MOVE    HCGS121N-HKNMONEY (IDX0)
                                           TO  HCGS121G-HKNMONEY (IDX0)
                   MOVE    HCGS121N-KOHMONEY (IDX0)
                                           TO  HCGS121G-KOHMONEY (IDX0)
                   MOVE    HCGS121N-TTLMONEY (IDX0)
                                           TO  HCGS121G-TTLMONEY (IDX0)
               END-PERFORM
      *
               MOVE    "HCGS121GV02.red"  TO  SPRT-PRTID
               MOVE    HCGS121G        TO  SPRT-PRTDATA
      *
           END-IF
      *
           PERFORM 230-PRINT-SEC
      *
      *
           .
       230-HCGS121-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理
      *****************************************************************
       230-PRINT-SEC                   SECTION.
      *
           MOVE    "INS"           TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM
                                   TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                   TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                   TO  SPRT-TBL-GROUP
           MOVE    BGS100P-SRYYM   TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID
                                   TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                   TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                   TO  SPRT-PRIORITY
           MOVE    BGS100P-SHELLMSG TO SPRT-TITLE
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID
                                   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM
                                   TO  SPRT-PRTNM
           MOVE    "1"             TO  SPRT-SITEKBN
      *
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF    ( SPRT-RETURN     =   ZERO )
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                   TO  WRK-ERRMSG
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       230-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    明細表編集処理（ＣＳＶ）
      *****************************************************************
       2001-HCGS121-CSV-SEC            SECTION.
      *
           MOVE    BGS100P-HOKEN   TO  WRK-PRTKBNZ9
      *
           MOVE    SPACE           TO  WRK-PRTKBNMEI
           EVALUATE    BGS100P-HOKEN
           WHEN    CONST-SYAHO
               MOVE    "社保"      TO  WRK-PRTKBNMEI
           WHEN    CONST-KOKUHO
               MOVE    "国保"      TO  WRK-PRTKBNMEI
           WHEN    CONST-KOUKI
               MOVE    "後期高齢者"
                                   TO  WRK-PRTKBNMEI
           WHEN    CONST-TOKUYO
               MOVE    "特別療養費" TO WRK-PRTKBNMEI
           WHEN    CONST-ROUSAI
               MOVE    "労災"      TO  WRK-PRTKBNMEI
           WHEN    CONST-JIBAI
               MOVE    "自賠責"    TO  WRK-PRTKBNMEI
           WHEN    CONST-DAISAN
               MOVE    "自賠責（第三者行為）" 
                                   TO  WRK-PRTKBNMEI
           WHEN    CONST-KOUGAI
               MOVE    "公害"      TO  WRK-PRTKBNMEI
           END-EVALUATE
      *
           IF    ( KEY-NYUGAIKBN   =   "1" )
               MOVE    "1"         TO  WRK-NYUGAIKBN
               MOVE    "入院"      TO  WRK-NYUGAIKBNMEI
           ELSE
               MOVE    "2"         TO  WRK-NYUGAIKBN
               MOVE    "外来"      TO  WRK-NYUGAIKBNMEI
           END-IF
      *
           COMPUTE CNT-CSV =   CNT-CSV     +   1
      *
           IF    ( CNT-CSV =   1 )
               IF    ( KEY-NYUGAIKBN   =   "1" )
                   MOVE    SPACE           TO  WRK-CSVDATA
                   MOVE    1               TO  IDXP
                   PERFORM VARYING IDXI    FROM    1   BY  1
                           UNTIL ( IDXI    >   CSV-H01-MAX )
      *
                       MOVE    CSV-H01-ITEM (IDXI)
                                           TO  WRK-ITEM
      *
                       IF    ( IDXI   >=   CSV-H01-MAX    )
                           PERFORM 802-CSVDATA-EDIT-SEC
                       ELSE
                           PERFORM 801-CSVDATA-EDIT-SEC
                       END-IF
      *
                   END-PERFORM
               ELSE
                   MOVE    SPACE           TO  WRK-CSVDATA
                   MOVE    1               TO  IDXP
                   PERFORM VARYING IDXI    FROM    1   BY  1
                           UNTIL ( IDXI    >   CSV-H02-MAX )
      *
                       MOVE    CSV-H02-ITEM (IDXI)
                                           TO  WRK-ITEM
      *
                       IF    ( IDXI   >=   CSV-H02-MAX    )
                           PERFORM 802-CSVDATA-EDIT-SEC
                       ELSE
                           PERFORM 801-CSVDATA-EDIT-SEC
                       END-IF
      *
                   END-PERFORM
               END-IF
      *
               PERFORM 230-TOUKEICSV-SEC
      *
           END-IF
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   CNT-LINE )
      *
               PERFORM 20011-HCGS121-CSV-SEC
      *
           END-PERFORM
      *
           IF    ( FLG-BGSMEI      NOT =   ZERO )
             OR  ( KEY-NYUGAIKBN   NOT =   BGSMEI-NYUGAIKBN)
      *
               MOVE    CONST-MEI-TTL   TO  IDX2
      *
               PERFORM 20011-HCGS121-CSV-SEC
      *
           END-IF
      *
           .
       2001-HCGS121-CSV-EXT.
           EXIT.
      *****************************************************************
      *    明細表編集処理（ＣＳＶ）（入院）
      *****************************************************************
       20011-HCGS121-CSV-SEC           SECTION.
      *
           MOVE  SPACE             TO  WRK-CSVDATA
           MOVE    1               TO  IDXP
      *
           MOVE    "5"             TO  WRK-DATAKBN
           MOVE    "レセプト別"    TO  WRK-DATAKBNMEI
           PERFORM 20011-CSV-KEY-EDIT-SEC
      *
           MOVE    TMPS121-HKNKBNNAME (IDX2)
                                   TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE    TMPS121-PTNUM2 (IDX2)
                                   TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE    HCGS121N-NAME (IDX2)
                                   TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE    HCGS121N-HBTNUM (IDX2)
                                   TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE    HCGS121N-HKNJANUM (IDX2)
                                   TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE    HCGS121N-HKNJANAME (IDX2)
                                   TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE    HCGS121N-KENSU (IDX2)
                                   TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE    HCGS121N-NISSU (IDX2)
                                   TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE    HCGS121N-TENSU (IDX2)
                                   TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE    HCGS121N-FTNMONEY (IDX2)
                                   TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE    HCGS121N-HKNMONEY (IDX2)
                                   TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE    HCGS121N-KOHMONEY (IDX2)
                                   TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE    HCGS121N-TTLMONEY (IDX2)
                                   TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           IF    ( KEY-NYUGAIKBN   =   "1" )
               MOVE    HCGS121N-SKJKAISU (IDX2)
                                   TO  WRK-ITEM
               PERFORM 801-CSVDATA-EDIT-SEC
      *
               MOVE    HCGS121N-SKJFTNMONEY (IDX2)
                                   TO  WRK-ITEM
               PERFORM 801-CSVDATA-EDIT-SEC
      *
               MOVE    HCGS121N-SKJHKNMONEY (IDX2)
                                   TO  WRK-ITEM
               PERFORM 801-CSVDATA-EDIT-SEC
      *
               MOVE    HCGS121N-SKJKOHMONEY (IDX2)
                                   TO  WRK-ITEM
               PERFORM 801-CSVDATA-EDIT-SEC
      *
               MOVE    HCGS121N-SKJTTLMONEY (IDX2)
                                   TO  WRK-ITEM
               PERFORM 801-CSVDATA-EDIT-SEC
      *
               MOVE    HCGS121N-TTLTTLMONEY (IDX2)
                                   TO  WRK-ITEM
               PERFORM 801-CSVDATA-EDIT-SEC
           END-IF
      *
           MOVE    TMPS121-SRYYM2 (IDX2)
                                   TO  WRK-ITEM
           PERFORM 802-CSVDATA-EDIT-SEC
      *
           PERFORM 230-TOUKEICSV-SEC
      *
           .
       20011-HCGS121-CSV-EXT.
           EXIT.
      *****************************************************************
      *    ＣＳＶファイルＫＥＹ項目編集処理
      *****************************************************************
       20011-CSV-KEY-EDIT-SEC         SECTION.
      *
           MOVE  WRK-PRTKBN        TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE  WRK-PRTKBNMEI     TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE  WRK-DATAKBN       TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE  WRK-DATAKBNMEI    TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE  WRK-CSV-SRYYM     TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE  WRK-NYUGAIKBN     TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE  WRK-NYUGAIKBNMEI
                                   TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE  KEY-SKYSBT        TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE  WRK-SKYSBTNAME    TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE  KEY-RECEKA        TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           MOVE  WRK-SRYKANAME1    TO  WRK-ITEM
           PERFORM 801-CSVDATA-EDIT-SEC
      *
           .
       20011-CSV-KEY-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ＣＳＶ出力処理
      *****************************************************************
       230-TOUKEICSV-SEC               SECTION.
      *
           INITIALIZE                  ORCSTOUKEICSVAREA
           MOVE    "INS"           TO  STOUKEICSV-MODE
           MOVE    LNK-PRTKANRI-TBL-KEY
                                   TO  STOUKEICSV-TBL-KEY
           MOVE    LNK-PRTKANRI-RENNUM
                                   TO  STOUKEICSV-RENNUM
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                   TO  STOUKEICSV-TBL-GROUP
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                   TO  STOUKEICSV-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-SHELLID
                                   TO  STOUKEICSV-SHELLID
           MOVE    BGS100P-SRYYM   TO  STOUKEICSV-SRYYM
           MOVE    BGS100P-NYUGAIKBN
                                   TO  STOUKEICSV-NYUGAIKBN
           MOVE    ZERO            TO  STOUKEICSV-PTID
           MOVE    BGS100P-CSVDIR  TO  STOUKEICSV-TO-FOLDER
           MOVE    BGS100P-CSVFNAME TO STOUKEICSV-TO-DATA
           MOVE    "2"             TO  STOUKEICSV-CODE-TYPE
           MOVE    WRK-CSVDATA     TO  STOUKEICSV-CSVDATA
           MOVE    "保険別請求チェック明細表ＣＳＶ作成"
                                   TO  STOUKEICSV-TITLE
      *
           CALL    "ORCSTOUKEICSV"     USING
                                       ORCSTOUKEICSVAREA
                                       SPA-AREA
           IF    ( STOUKEICSV-RETURN   =   ZERO )
               CONTINUE
           ELSE
               MOVE    "統計ＣＳＶＤＢに更新できませんでした"
                                   TO  WRK-ERRMSG
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
      *
       230-TOUKEICSV-EXT.
           EXIT.
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           COMPUTE BGS100P-PAGE
               =   BGS100P-PAGE    +   CNT-PAGE
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
           PERFORM 990-EXIT-PROGRAM-SEC
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    CSVデータ編集処理
      *****************************************************************
       800-CSVDATA-EDIT-SEC            SECTION.
      *
           IF    ( WRK-ITEM        NOT =   SPACE )
               PERFORM 800-GET-ITEM-SIZE-SEC
               STRING  WRK-ITEM(IDXL:CNT-LEN)  DELIMITED   BY  SIZE
               INTO    WRK-CSVDATA
               WITH    POINTER   IDXP
               END-STRING
           END-IF
           .
      *
       800-CSVDATA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    CSVデータ編集処理
      *****************************************************************
       801-CSVDATA-EDIT-SEC            SECTION.
      *
           PERFORM 800-CSVDATA-EDIT-SEC
           PERFORM 800-PUT-DELIMITER-SEC
           .
      *
       801-CSVDATA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    CSVデータ編集処理
      *****************************************************************
       802-CSVDATA-EDIT-SEC            SECTION.
      *
           PERFORM 800-CSVDATA-EDIT-SEC
      *    PERFORM 800-PUT-CR-SEC
           .
      *
       802-CSVDATA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    項目サイズ取得処理
      *****************************************************************
       800-GET-ITEM-SIZE-SEC           SECTION.
      *
           MOVE    ZERO            TO  IDXL
                                       CNT-LEN
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   WRK-ITEM-SIZE )
                    OR   ( WRK-ITEM (IDX0:)    =  SPACE )
      *
               IF    ( IDXL = ZERO )
                   IF    ( WRK-ITEM (IDX0:1)   NOT =  SPACE )
                       MOVE    IDX0 TO IDXL
                       MOVE    1    TO CNT-LEN
                   END-IF
               ELSE
                   COMPUTE CNT-LEN  =  CNT-LEN + 1
               END-IF
      *
           END-PERFORM
      *
           .
       800-GET-ITEM-SIZE-EXT.
           EXIT.
      *****************************************************************
      *    区切り文字編集処理
      *****************************************************************
       800-PUT-DELIMITER-SEC           SECTION.
      *
           STRING    ","   DELIMITED   BY  SIZE
           INTO      WRK-CSVDATA
           WITH      POINTER   IDXP
           END-STRING
      *
           .
       800-PUT-DELIMITER-EXT.
           EXIT.
      *****************************************************************
      *    改行文字編集処理
      *****************************************************************
       800-PUT-CR-SEC                  SECTION.
      *
           STRING    X"0D"   DELIMITED BY  SIZE
           INTO      WRK-CSVDATA
           WITH      POINTER   IDXP
           END-STRING
      *
           .
       800-PUT-CR-EXT.
           EXIT.
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       800-HIZUKE-EDIT-SEC             SECTION.
      *
           MOVE    SPACE           TO  WRK-EDTYMD1
                                       WRK-EDTYMD3
      *
           IF    ( WRK-SYMD            =   SPACE )
               CONTINUE
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"   USING
                                   STS-AREA-DAY
                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-EDTYMD1
               MOVE    LNK-DAY2-EDTYMD3    TO  WRK-EDTYMD3
               INSPECT WRK-EDTYMD3 REPLACING  ALL "  "  BY  "　"
           END-IF
      *
      *
           .
       800-HIZUKE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    全角変換処理
      *****************************************************************
       800-ZENKAKU-SEC                 SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE   "2"              TO  KANACHK-SYORI
           MOVE  500               TO  KANACHK-MAX-CNT
           MOVE  WRK-KANACHK-STR   TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT
                                   TO  WRK-KANACHK-STR
      *
           .
       800-ZENKAKU-EXT.
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
      *    レセプト別請求ファイル読込処理
      *****************************************************************
       900-BGSMEI-READ-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-BGSMEI
                                       FLG-LOOPEND
      *
           PERFORM UNTIL ( FLG-BGSMEI    NOT =   ZERO )
                    OR   ( FLG-LOOPEND   NOT =   ZERO )
      *
               READ    BGSMEI-FILE     NEXT
               AT  END
                   MOVE    1       TO  FLG-BGSMEI
               NOT AT  END
      *
                   MOVE    1       TO  FLG-LOOPEND
      *
                   IF    ( BGS100P-HOKEN   NOT =   BGSMEI-HOKEN )
                           MOVE    ZERO    TO  FLG-LOOPEND
                   END-IF
      *
                   IF    ( BGS100P-NYUGAIKBN   =   "1" OR "2" )
                       IF    ( BGS100P-NYUGAIKBN
                                           NOT =   BGSMEI-NYUGAIKBN )
                           MOVE    ZERO    TO  FLG-LOOPEND
                       END-IF
                   END-IF
      *
                   IF    ( KEY-SKYSBT      NOT =   SPACE )
                       IF   (( KEY-SKYSBT      =   "1" )
                        AND  ( BGSMEI-SKYSBT   =   "1" ))
                         OR (( KEY-SKYSBT      =   "2" )
                        AND  ( BGSMEI-SKYSBT   =   "2" OR "3" ))
                           CONTINUE
                       ELSE
                           MOVE    ZERO    TO  FLG-LOOPEND
                       END-IF
                   END-IF
      *
                   IF    ( KEY-RECEKA      NOT =   SPACE )
                       IF    ( KEY-RECEKA  NOT =   BGSMEI-RECEKA )
                           MOVE    ZERO    TO  FLG-LOOPEND
                       END-IF
                   END-IF
      *
                   IF    ( BGS100P-HKNKBN-HKN
                                           NOT =   SPACE )
      *
                       IF    ( BGS100P-HKNKBN-HKN
                                               =   "1" OR "2" OR "3" )
                           MOVE    BGS100P-HKNKBN-HKN
                                               TO  WRK-HKNKBN-HKN-G
                           IF    ( WRK-HKNKBN-HKN
                                           NOT =   BGSMEI-HOKEN )
                               MOVE    ZERO    TO  FLG-LOOPEND
                           END-IF
                           IF    ( BGS100P-HKNKBN
                                       NOT =   BGSMEI-SBTKBN-G (1:2))
                               MOVE    ZERO    TO  FLG-LOOPEND
                           END-IF
                       ELSE
                           MOVE        ZERO    TO  FLG-LOOPEND
                       END-IF
                   END-IF
      *
                   IF    ( BGS100P-HKNJANUM
                                           NOT =   SPACE )
                       IF    ( BGS100P-HKNJANUM
                                           NOT =   BGSMEI-HKNJANUM )
                               MOVE    ZERO    TO  FLG-LOOPEND
                       END-IF
                   END-IF
      *
               END-READ
      *
           END-PERFORM
      *
           .
      *
       900-BGSMEI-READ-EXT.
           EXIT.
      *****************************************************************
      *    保険者マスタ検索処理
      *****************************************************************
       900-HKNJAINF-KEY-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-HKNJAINF
      *
           INITIALIZE                      HKNJAINF-REC
           MOVE    SPA-HOSPNUM         TO  HKNJA-HOSPNUM
           MOVE    BGSMEI-HKNJANUM     TO  HKNJA-HKNJANUM
           MOVE    HKNJAINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF ( MCP-RC                 =   ZERO )
               MOVE    MCPDATA-REC     TO  HKNJAINF-REC
           ELSE
               MOVE    1               TO  FLG-HKNJAINF
               INITIALIZE                  HKNJAINF-REC
           END-IF
      *
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-HKNJAINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者番号マスタ検索処理
      *****************************************************************
       900-PTNUM-KEY-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNUM
      *
           INITIALIZE                      PTNUM-REC
           MOVE    SPA-HOSPNUM         TO  PTNUM-HOSPNUM
           MOVE    BGSMEI-PTID         TO  PTNUM-PTID
           MOVE    PTNUM-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF ( MCP-RC                 =   ZERO )
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
      *****************************************************************
      *    プログラム終了処理
      *****************************************************************
       990-EXIT-PROGRAM-SEC            SECTION.
      *
           DISPLAY "ORCBGS121 RC [" BGS100P-RC "]"
      *
           EXIT PROGRAM
      *
           .
       990-EXIT-PROGRAM-EXT.
           EXIT.
