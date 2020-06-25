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
       PROGRAM-ID.             ORCR0465.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト（労災・自賠責）
      *  コンポーネント名  : 月次業務　労災自賠責明細書ファイル作成
      *                      金額摘要欄並び替え（労災・自賠責）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC-藤原      新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-門脇    02.10.02  摘要出力項目変更
      *  01.00.02    NACL-門脇    02.11.21  自賠責（ポリネック等の対応）
      *  01.00.03    NACL-門脇    03.09.29  点数マスタを読み込まないよう
      *                                     修正
      *  中止処理追加
      *  01.01.01    NACL-藤原    04/10/13  中止処理の追加
      *  01.01.02    NACL-門脇    05/07/15  自賠責従来様式対応
      *  01.01.03    NACL-藤原    05/08/22  検体検査コメントの追加
      *
      *  03.05.01    NACL-門脇    07/05/25  グループ診療対応
      *  03.05.02    NACL-藤原    07/06/01  realtime preview 処理追加
      *  04.01.01    NACL-門脇    07/10/31  公務災害対応
      *  04.01.02    NACL-門脇    07/11/26  特定器材商品名記載対応
      *
      *  04.03.01    NACL-門脇    08/07/11  診断書明細書料の摘要欄記載対応
      *  04.03.02    NACL-門脇    08/09/25  自賠責０９５９３の集計先対応
      *
      *  04.05.01    NACL-藤原    09/07/14  小数点以下５桁対応
      *
      *  04.06.01    NACL-門脇    13/02/18  労災レセ電対応
      *                                     （新診療行為コード対応）
      *  04.06.02    NACL-門脇    13/09/25  労災マスタ追加対応
      *                                     （電話再診料　等）
      *
      *  04.07.01    NACL-門脇    13/01/21  労災レセ電対応
      *  04.07.02    NACL-門脇    13/10/02  自賠責（第三者行為）対応
      *  04.07.03    NACL-門脇    13/12/03  自賠責（労災準拠）のレセプト様式
      *                                     追加対応
      *  04.07.04    NACL-門脇    14/02/13  労災・自賠責の施術加算対応
      *
      *  04.08.01    NACL-藤原    14/07/07  一時ディレクトリ対応
      *
      *  05.00.01    NACL-門脇    20/03/30  令和２年４月改正対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
           SELECT  RECE41-FILE     ASSIGN  RECE01PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE41-KEY
                                   FILE    STATUS  IS  STS-RECE41.
      *   
           SELECT  RECE44-FILE     ASSIGN  RECE44PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE44-KEY
                                   FILE    STATUS  IS  STS-RECE44.
      *    
           SELECT  RECE45-FILE     ASSIGN  RECE45PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE45-KEY
                                   FILE    STATUS  IS  STS-RECE45.
      *   
           SELECT  RECE46-FILE     ASSIGN  RECE46PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE46-KEY
                                   FILE    STATUS  IS  STS-RECE46.
      *    
           SELECT  RECE46X-FILE     ASSIGN  RECE46PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE46X-KEY
                                   FILE    STATUS  IS  STS-RECE46.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       FD  RECE41-FILE.
       01  RECE41-REC.
           COPY    "CPRCF041.INC".
      *
       FD  RECE44-FILE.
       01  RECE44-REC.
           COPY    "CPRCF044.INC".
      *
       FD  RECE45-FILE.
       01  RECE45-REC.
           COPY    "CPRCF045.INC".
      *
       FD  RECE46-FILE.
       01  RECE46-REC. 
           COPY    "CPRCF0461.INC" REPLACING  //RECE461//
                                   BY         //RECE46//.
      *
       FD  RECE46X-FILE.
       01  RECE46X-REC. 
           COPY    "CPRCF0461.INC" REPLACING  //RECE461//
                                   BY         //RECE46X//.
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE01//.
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE44//.
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE45//.
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE46//.
      *    エラーファイル 名称領域 
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //RECEERR//.
      *
      *    フラグ領域
       01  STS-AREA.
           03  SPA-STS             PIC 9(04).
           03  STS-RECE41          PIC X(02).
           03  STS-RECE44          PIC X(02).
           03  STS-RECE45          PIC X(02).
           03  STS-RECE46          PIC X(02).
           03  STS-RECEERR         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-RECE41          PIC 9(01).
           03  FLG-RECE44          PIC 9(01).
           03  FLG-RECE45          PIC 9(01).
           03  FLG-SP              PIC 9(01).
           03  FLG-NOPRT           PIC 9(01).
           03  FLG-HENSYU          PIC 9(01).
           03  FLG-OK              PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-RECE41          PIC 9(06).
           03  CNT-RECE44          PIC 9(06).
           03  CNT-RECE45          PIC 9(06).
           03  CNT-RECE46          PIC 9(06).
           03  CNT-IN-CANCEL       PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
      *    剤診療行為コード別編集用     
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-SRYYM      PIC X(06).
               05  WRK-PARA-TERMID     PIC X(16).
               05  WRK-PARA-SYSYMD     PIC X(08).
               05  WRK-PARA-RECEKBN    PIC X(01).
               05  WRK-PARA-JOBID      PIC 9(07).
               05  WRK-PARA-SHELLID    PIC X(08).
           03  WRK-PARA-JIBAIPRTKBN    PIC X(01).
           03  WRK-PARA-HOSPNUM        PIC 9(02).
      *
           03  WRK-RECEERR         PIC X(200).
      *
           03  WRK-LINENUM         PIC 9(05).
      *
      *    剤印字行数
           03  WRK-TBLMAX          PIC 9(03).
      *    剤明細総数
           03  WRK-ZAIWMAX         PIC 9(03).
      *     
           03  WRK-WLINETEK                    PIC 9(03).
           03  WRK-SRYSURYOW                   PIC X(24).
           03  WRK-ZAIIDX                      PIC 9(03).
           03  WRK-NAMEKETA                    PIC 9(03).
           03  WRK-YOHAKU                      PIC 9(03).
           03  WRK-WKETA                       PIC 9(03).
      *    剤診療行為コード別編集用     
           03  WRK-WIDX1                       PIC 9(03).
      *     
           03  WRK-SRYKBN                      PIC X(02).
           03  WRK-Z9                          PIC ZZZ9.
      *
           03  WRK-SRYCD1                      PIC X(09).
           03  WRK-GAITENTTLKBN                PIC 9(03).
      *
       01  WRK-HENSYU-AREA.
           03  WRK-HENYMDG         PIC X(22).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HMM         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HDD         PIC Z9.
      *
           03  WRK-HENTIME.
               05  WRK-THH         PIC 99.
               05  FILLER          PIC X(01)   VALUE  ":".
               05  WRK-TMM         PIC 99.
      *          
      *    診療行為別テーブル
       01  TBL-ZAIW-AREA.
           03  TBL-ZAIW-OCC               OCCURS 100.
               05  TBL-ZAIW-SRYACTINF.
      *            診療コード/数量/回数
                   07  TBL-ZAIW-SRYCD           PIC  X(09).
                   07  TBL-ZAIW-SRYSYUKBN-X     PIC  X(03).
                   07  TBL-ZAIW-SRYSURYO        PIC  9(05)V9(05).
                   07  TBL-ZAIW-SRYKAISU        PIC  9(03).
      *            単位コード/単位名称有効桁数/単位名称
                   07  TBL-ZAIW-TANICD          PIC  9(03).
                   07  TBL-ZAIW-TANIMOJISU      PIC  9(01).
                   07  TBL-ZAIW-TANINAME        PIC  X(24).
      *            点数識別区分/点数/明細請求フラグ
                   07  TBL-ZAIW-TENSIKIBETU     PIC  9(01).
                   07  TBL-ZAIW-TEN             PIC  9(09)V9(02).
                   07  TBL-ZAIW-MEISKYFLG       PIC  X(01).
      *            自動算定区分/名称入力番号/点数欄集計先識別
                   07  TBL-ZAIW-AUTOKBN         PIC  X(01).
                   07  TBL-ZAIW-INPUTNUM        PIC  9(03).
                   07  TBL-ZAIW-TENTTLKBN       PIC  9(03).
      *
                   07  TBL-ZAIW-DATAKBN         PIC  9(01).
                   07  TBL-ZAIW-HOUKNSKBN       PIC  9(02).
                   07  TBL-ZAIW-KNSJISKBN       PIC  9(01).
                   07  TBL-ZAIW-KNSJISGRPKBN    PIC  9(02).
                   07  TBL-ZAIW-CDKBNINF.
                     09 TBL-ZAIW-CDKBN-KBN        PIC  X(01).
                     09 TBL-ZAIW-CDKBN-SYO        PIC  9(01).
                     09 TBL-ZAIW-CDKBN-BU         PIC  9(02).
                     09 TBL-ZAIW-CDKBN-KBNNUM     PIC  9(03).
                     09 TBL-ZAIW-CDKBN-KBNNUM-EDA PIC  9(02).
                     09 TBL-ZAIW-CDKBN-KOUBAN     PIC  9(02).
                     09 TBL-ZAIW-KOUHYOJYUNNUM    PIC  9(09).
                   07  TBL-ZAIW-KOKUJISKBKBN1   PIC  9(01).
                   07  TBL-ZAIW-KOKUJISKBKBN2   PIC  9(01).
                   07  TBL-ZAIW-TUSOKUGAIKBN    PIC  9(01).
                   07  TBL-ZAIW-TIMEKSNKBN      PIC  9(01).
                   07  TBL-ZAIW-KZMCOMPSIKIBETU PIC  9(01).
                   07  TBL-ZAIW-CHUKSNINF.
                     09 TBL-ZAIW-CHUKSNCD       PIC  X(04).
                     09 TBL-ZAIW-CHUKSNTUBAN    PIC  X(01).
                   07  TBL-ZAIW-KENTAICOMMENT   PIC  9(01).
                   07  TBL-ZAIW-INPUTCOMENT     PIC  X(140).
               05  TBL-ZAIW-NAMEINF.
      *            漢字有効桁数/漢字名称
                   07  TBL-ZAIW-YUKOKETA        PIC  9(03).
                   07  TBL-ZAIW-NAME            PIC  X(300).
      *          
      *    診療行為別テーブル（出力用）
       01  TBL-ZAI-AREA.
           03  TBL-ZAI-OCC               OCCURS 100.
               05  TBL-ZAI-SRYACTINF.
      *            診療コード/数量/回数
                   07  TBL-ZAI-SRYCD           PIC  X(09).
                   07  TBL-ZAI-SRYSYUKBN-X     PIC  X(03).
                   07  TBL-ZAI-SRYSURYO        PIC  9(05)V9(05).
                   07  TBL-ZAI-SRYKAISU        PIC  9(03).
      *            単位コード/単位名称有効桁数/単位名称
                   07  TBL-ZAI-TANICD          PIC  9(03).
                   07  TBL-ZAI-TANIMOJISU      PIC  9(01).
                   07  TBL-ZAI-TANINAME        PIC  X(24).
      *            点数識別区分/点数/明細請求フラグ
                   07  TBL-ZAI-TENSIKIBETU     PIC  9(01).
                   07  TBL-ZAI-TEN             PIC  9(09)V9(02).
                   07  TBL-ZAI-MEISKYFLG       PIC  X(01).
      *            自動算定区分/名称入力番号/点数欄集計先識別
                   07  TBL-ZAI-AUTOKBN         PIC  X(01).
                   07  TBL-ZAI-INPUTNUM        PIC  9(03).
                   07  TBL-ZAI-TENTTLKBN       PIC  9(03).
      *
                   07  TBL-ZAI-DATAKBN         PIC  9(01).
                   07  TBL-ZAI-HOUKNSKBN       PIC  9(02).
                   07  TBL-ZAI-KNSJISKBN       PIC  9(01).
                   07  TBL-ZAI-KNSJISGRPKBN    PIC  9(02).
                   07  TBL-ZAI-CDKBNINF.
                     09 TBL-ZAI-CDKBN-KBN        PIC  X(01).
                     09 TBL-ZAI-CDKBN-SYO        PIC  9(01).
                     09 TBL-ZAI-CDKBN-BU         PIC  9(02).
                     09 TBL-ZAI-CDKBN-KBNNUM     PIC  9(03).
                     09 TBL-ZAI-CDKBN-KBNNUM-EDA PIC  9(02).
                     09 TBL-ZAI-CDKBN-KOUBAN     PIC  9(02).
                     09 TBL-ZAI-KOUHYOJYUNNUM    PIC  9(09).
                   07  TBL-ZAI-KOKUJISKBKBN1   PIC  9(01).
                   07  TBL-ZAI-KOKUJISKBKBN2   PIC  9(01).
                   07  TBL-ZAI-TUSOKUGAIKBN    PIC  9(01).
                   07  TBL-ZAI-TIMEKSNKBN      PIC  9(01).
                   07  TBL-ZAI-KZMCOMPSIKIBETU PIC  9(01).
                   07  TBL-ZAI-CHUKSNINF.
                     09 TBL-ZAI-CHUKSNCD       PIC  X(04).
                     09 TBL-ZAI-CHUKSNTUBAN    PIC  X(01).
                   07  TBL-ZAI-KENTAICOMMENT   PIC  9(01).
                   07  TBL-ZAI-INPUTCOMENT     PIC  X(140).
               05  TBL-ZAI-NAMEINF.
      *            漢字有効桁数/漢字名称
                   07  TBL-ZAI-YUKOKETA        PIC  9(03).
                   07  TBL-ZAI-NAME            PIC  X(300).
      *
      *    固定値
       01  CONST-AREA.
      *    初診
           03  CONS-SYOSHIN    PIC X(09)   VALUE   "101110010".
      *     
      *    再診（２０１３年６月まで）
           03  CONS-SAISHIN-G.
               05  FILLER      PIC X(09)   VALUE   "101120040".
               05  FILLER      PIC X(09)   VALUE   "101120050".
           03  CONS-SAISHIN-R              REDEFINES   CONS-SAISHIN-G.
               05  CONS-SAISHIN            PIC X(09)   OCCURS  2. 
      *    再診（２０１３年７月から）
           03  CONS-SAISHIN2-G.
               05  FILLER      PIC X(09)   VALUE   "101120010".
               05  FILLER      PIC X(09)   VALUE   "101120050".
               05  FILLER      PIC X(09)   VALUE   "101120060".
               05  FILLER      PIC X(09)   VALUE   "101120070".
           03  CONS-SAISHIN2-R             REDEFINES   CONS-SAISHIN2-G.
               05  CONS-SAISHIN2           PIC X(09)   OCCURS  4. 
      *
           COPY    "MCPAREA".
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *     
      *    数値変換サブ
           COPY    "CPORCSNUM.INC".
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(256).
      ****************************************************************
       PROCEDURE           DIVISION
               USING
           COMMAND-PARAM.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL       SPA-STS     =   2
      *
           PERFORM 300-END-SEC
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               STOP    RUN
           ELSE
               EXIT    PROGRAM
           END-IF 
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  SPA-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  TBL-ZAI-AREA
           INITIALIZE                  TBL-ZAIW-AREA
           INITIALIZE                  RECEERR
      *
      *    ファイルＯＰＥＮ処理
      *
           PERFORM 110-FILE-OPEN-SEC
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               PERFORM 120-DBOPEN-SEC
      *
      *        ステップ管理開始処理
               MOVE    "STS"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    "ORCR0465"      TO  JOB-PGID
               MOVE    "金額摘要欄並び替え（労災・自賠責）"
                                       TO  JOB-SHELLMSG
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *        処理中止設定処理
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
      *
           PERFORM 990-RECE41-READ-SEC
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイルＯＰＥＮ処理
      *****************************************************************
       110-FILE-OPEN-SEC          SECTION.
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA
                                               WRK-PARA-JIBAIPRTKBN
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           MOVE    SPACE               TO  RECE01PARA
           INITIALIZE                      RECE01PARA
           MOVE    "RECE410"           TO  RECE01PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE01PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE01PARA-HOSPNUM
      *
           MOVE    SPACE               TO  RECE44PARA
           INITIALIZE                      RECE44PARA
           MOVE    "RECE44B"           TO  RECE44PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE44PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE44PARA-HOSPNUM
      *
           MOVE    SPACE               TO  RECE45PARA
           INITIALIZE                      RECE45PARA
           MOVE    "RECE45B"           TO  RECE45PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE45PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE45PARA-HOSPNUM
      *
           MOVE    SPACE               TO  RECE46PARA
           INITIALIZE                      RECE46PARA
           MOVE    "RECE461"           TO  RECE46PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE46PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE46PARA-HOSPNUM
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES  (1)
           MOVE    RECE01PARA-BASENAME TO  SGETTEMP-BASENAMES  (2)
           MOVE    RECE44PARA-BASENAME TO  SGETTEMP-BASENAMES  (3)
           MOVE    RECE45PARA-BASENAME TO  SGETTEMP-BASENAMES  (4)
           MOVE    RECE46PARA-BASENAME TO  SGETTEMP-BASENAMES  (5)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  RECE01PARA
           MOVE    SGETTEMP-FULLNAMES (3)
                                       TO  RECE44PARA
           MOVE    SGETTEMP-FULLNAMES (4)
                                       TO  RECE45PARA
           MOVE    SGETTEMP-FULLNAMES (5)
                                       TO  RECE46PARA
      *
           OPEN    INPUT               RECE41-FILE
      *
           OPEN    INPUT               RECE44-FILE
      *
           OPEN    INPUT               RECE45-FILE
      *
           OPEN    OUTPUT              RECE46X-FILE
           CLOSE                       RECE46X-FILE
      *
           OPEN    I-O                 RECE46-FILE
      *
           .
       110-FILE-OPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    行カウンタ初期化
           MOVE    ZERO                TO  WRK-LINENUM
           INITIALIZE                  TBL-ZAI-AREA
           MOVE    ZERO                TO  WRK-TBLMAX
      *
      *    パラメータ０４ファイルより編集
           INITIALIZE                      RECE44-REC
           MOVE    RECE41-HOSPNUM      TO  RECE44-HOSPNUM
           MOVE    RECE41-SRYYM        TO  RECE44-SRYYM
           MOVE    RECE41-NYUGAIKBN    TO  RECE44-NYUGAIKBN
           MOVE    RECE41-PTID         TO  RECE44-PTID
           MOVE    RECE41-RECEKA       TO  RECE44-RECEKA
           MOVE    RECE41-HKNID        TO  RECE44-HKNID
           MOVE    RECE41-TEKSTYMD     TO  RECE44-TEKSTYMD
           MOVE    ZERO                TO  RECE44-ZAINUM
      *
           START   RECE44-FILE
               KEY IS    >=   RECE44-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE44
               NOT INVALID
                   PERFORM 990-RECE44-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( SPA-STS              =   2                )
               OR     ( FLG-RECE44           =   1                )
               OR     ( RECE41-HOSPNUM   NOT =   RECE44-HOSPNUM   )
               OR     ( RECE41-SRYYM     NOT =   RECE44-SRYYM     )
               OR     ( RECE41-NYUGAIKBN NOT =   RECE44-NYUGAIKBN )
               OR     ( RECE41-PTID      NOT =   RECE44-PTID      )
               OR     ( RECE41-RECEKA    NOT =   RECE44-RECEKA    )
               OR     ( RECE41-HKNID     NOT =   RECE44-HKNID     )
               OR     ( RECE41-TEKSTYMD  NOT =   RECE44-TEKSTYMD  )
      *
               PERFORM 2002-TBLZAIW-TAIHI-SEC
      *
               IF      SPA-STS          =   ZERO
      *            レセプト０４ファイル編集処理
                   PERFORM 2003-SRYKBN11-HENSYU-SEC
      *
                   ADD     1               TO  CNT-RECE44
      *
                   PERFORM 990-RECE44-NEXT-SEC
      *
      *            処理中止設定処理
                   IF      CNT-IN-CANCEL   =   50
                       MOVE    ZERO            TO  CNT-IN-CANCEL
                       PERFORM 500-CANCEL-HENSYU-SEC
                   END-IF
               END-IF     
           END-PERFORM
      *
           IF      SPA-STS         =   ZERO
               PERFORM 990-RECE41-READ-SEC
      *
      *        処理中止設定処理
               IF      CNT-IN-CANCEL   =   50
                   MOVE    ZERO            TO  CNT-IN-CANCEL
                   PERFORM 500-CANCEL-HENSYU-SEC
               END-IF
           END-IF    
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細テーブル退避処理
      *****************************************************************
       2002-TBLZAIW-TAIHI-SEC    SECTION.
      *
           INITIALIZE                      TBL-ZAIW-AREA
           MOVE    ZERO                TO  WRK-ZAIWMAX
           MOVE    ZERO                TO  WRK-TBLMAX
           MOVE    ZERO                TO  IDY                           
      *
      *    パラメータ０５ファイルより編集
           INITIALIZE                      RECE45-REC
           MOVE    RECE44-HOSPNUM      TO  RECE45-HOSPNUM
           MOVE    RECE44-SRYYM        TO  RECE45-SRYYM
           MOVE    RECE44-NYUGAIKBN    TO  RECE45-NYUGAIKBN
           MOVE    RECE44-PTID         TO  RECE45-PTID
           MOVE    RECE44-RECEKA       TO  RECE45-RECEKA
           MOVE    RECE44-HKNID        TO  RECE45-HKNID
           MOVE    RECE44-TEKSTYMD     TO  RECE45-TEKSTYMD
           MOVE    RECE44-ZAINUM       TO  RECE45-ZAINUM
           START   RECE45-FILE
               KEY IS    >=   RECE45-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE45
               NOT INVALID
                   PERFORM 990-RECE45-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( SPA-STS              =   2                   )
               OR     ( FLG-RECE45           =   1                   )
               OR     ( RECE44-HOSPNUM   NOT =   RECE45-HOSPNUM      )
               OR     ( RECE44-SRYYM     NOT =   RECE45-SRYYM        )
               OR     ( RECE44-NYUGAIKBN NOT =   RECE45-NYUGAIKBN    )
               OR     ( RECE44-PTID      NOT =   RECE45-PTID         )
               OR     ( RECE44-RECEKA    NOT =   RECE45-RECEKA       )
               OR     ( RECE44-HKNID     NOT =   RECE45-HKNID        )
               OR     ( RECE44-TEKSTYMD  NOT =   RECE45-TEKSTYMD     )
               OR     ( RECE44-ZAINUM    NOT =   RECE45-ZAINUM       )
      *
               PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX               >   5     )
                         OR      ( RECE45-SRYCD(IDX) =   SPACE )
                         OR      ( SPA-STS           =   2     ) 
                   IF      RECE45-SIKFLG (IDX)         =   "2"        
      *
                       ADD     1                 TO  WRK-ZAIWMAX 
                       IF      WRK-ZAIWMAX       >   100
                           MOVE    "テーブルがオーバーしました"
                                                     TO  WRK-RECEERR
                           PERFORM 500-ERR-HENSYU-SEC
                           MOVE    2                 TO  SPA-STS
                           GO  TO  2002-TBLZAIW-TAIHI-EXT
                       ELSE
                           MOVE    WRK-ZAIWMAX       TO  IDZ
                           MOVE    RECE45-SRYACTINF (IDX)
                                             TO  TBL-ZAIW-SRYACTINF(IDZ)
                           MOVE    RECE45-NAMEINF   (IDX)
                                             TO  TBL-ZAIW-NAMEINF  (IDZ)
      *
                       END-IF
                   END-IF  
               END-PERFORM
               ADD     1                 TO  CNT-RECE45
      *
               PERFORM 990-RECE45-NEXT-SEC
           END-PERFORM
      *
           .
       2002-TBLZAIW-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    編集処理
      *****************************************************************
       2003-SRYKBN11-HENSYU-SEC    SECTION.
      *
      *    剤　明細テーブルチェック
           INITIALIZE                    TBL-ZAI-AREA
           MOVE    ZERO              TO  WRK-TBLMAX
           MOVE    ZERO              TO  FLG-OK
           MOVE    SPACE             TO  WRK-SRYCD1
           MOVE    ZERO              TO  WRK-GAITENTTLKBN
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       WRK-ZAIWMAX   
               MOVE    IDX1          TO  WRK-WIDX1
               PERFORM 20031-SRYKBN-CHECK-SEC
               IF      FLG-NOPRT         =   ZERO
      *            レセプト０５剤編集処理
                   PERFORM 430-TBLZAI-HEN-SEC
               END-IF
           END-PERFORM    
      *
           IF       WRK-TBLMAX           >   ZERO
      *        剤・診療行為テーブル出力処理
               PERFORM 450-RECE46-OUT-SEC
           END-IF
      *
           .
       2003-SRYKBN11-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細印字チェック 診療区分（１１）（１２） 処理
      *****************************************************************
       20031-SRYKBN-CHECK-SEC        SECTION.
      * 
           MOVE    ZERO          TO  FLG-NOPRT
      *
           IF     (WRK-PARA-RECEKBN       =  "5")
           OR     (RECE41-HKNKBN          =  "5")
           OR     (RECE41-HKNKBN          =  "6")
             CONTINUE
           ELSE
             EVALUATE    RECE44-SRYKBN
               WHEN    "11"
                   IF  TBL-ZAIW-SRYCD (WRK-WIDX1)
                                             =   CONS-SYOSHIN
                       MOVE    1                 TO  FLG-NOPRT
                   END-IF
               WHEN    "12"
                   IF  RECE41-SRYYM   >=  "201307"
                     PERFORM VARYING IDY FROM    1   BY  1
                               UNTIL   IDY         >   4
                               OR      FLG-NOPRT   =   1
                       IF  TBL-ZAIW-SRYCD (WRK-WIDX1)
                                         =   CONS-SAISHIN2(IDY)
                           MOVE    1           TO  FLG-NOPRT
                           IF   (WRK-PARA-RECEKBN      =  "4")
                           AND  (WRK-PARA-JIBAIPRTKBN  =  "2")
                              IF  TBL-ZAIW-SRYCD (WRK-WIDX1) =
                                  "101120050" OR "101120060"
                                              OR "101120070"
                                MOVE   ZERO    TO  FLG-NOPRT
                              END-IF
                           END-IF
                       END-IF
                     END-PERFORM
                   ELSE
                     PERFORM VARYING IDY FROM    1   BY  1
                               UNTIL   IDY         >   2
                               OR      FLG-NOPRT   =   1  
                       IF  TBL-ZAIW-SRYCD (WRK-WIDX1)
                                         =   CONS-SAISHIN (IDY)
                           MOVE    1           TO  FLG-NOPRT
                       END-IF
                       IF      RECE41-SAISHIN-PRTKBN2  =   "1"  
                           MOVE    ZERO        TO  FLG-NOPRT
                       END-IF
                     END-PERFORM
                   END-IF
             END-EVALUATE
      *
           END-IF
      *
           .
       20031-SRYKBN-CHECK-EXT.
           EXIT.
      *     
      *****************************************************************
      *    剤テーブル編集処理
      *****************************************************************
       430-TBLZAI-HEN-SEC     SECTION.
      *
           ADD     1                TO  WRK-TBLMAX
           IF  WRK-TBLMAX           >   100
               DISPLAY "*** ORCR0465 TBL-ZAI INDEX[100] OVER  ***"
           ELSE
      *        先頭の診療行為コード編集
               IF      FLG-OK       =   ZERO
                  IF  ( TBL-ZAIW-SRYCD(IDX1)(1:1)  =  "8"   )
                  OR  ( TBL-ZAIW-SRYCD(IDX1)(1:3)  =  "008" )
                    CONTINUE
                  ELSE
                    MOVE   TBL-ZAIW-SRYCD(IDX1)  TO  WRK-SRYCD1
                    MOVE   TBL-ZAIW-TENTTLKBN(IDX1)
                                                 TO  WRK-GAITENTTLKBN
                    MOVE      1                  TO  FLG-OK
                  END-IF
               END-IF
      *
               MOVE    TBL-ZAIW-SRYACTINF (IDX1)
                                       TO  TBL-ZAI-SRYACTINF(WRK-TBLMAX)
               MOVE    TBL-ZAIW-NAMEINF   (IDX1)
                                       TO  TBL-ZAI-NAMEINF  (WRK-TBLMAX)
           END-IF    
           .
       430-TBLZAI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト０６出力処理
      *****************************************************************
       450-RECE46-OUT-SEC    SECTION.
      *               
           PERFORM   VARYING   IDY   FROM    1   BY  1
                     UNTIL     IDY   >   WRK-TBLMAX
               ADD     1                 TO  WRK-LINENUM
               MOVE    SPACE             TO  RECE46-REC
               INITIALIZE                    RECE46-REC
      *
               MOVE  TBL-ZAI-SRYCD(IDY)       TO  RECE46-SRYCD
               MOVE  TBL-ZAI-SRYSURYO(IDY)    TO  RECE46-SRYSURYO
               MOVE  TBL-ZAI-SRYKAISU(IDY)    TO  RECE46-SRYKAISU
               MOVE  TBL-ZAI-TANICD(IDY)      TO  RECE46-TANICD
               MOVE  TBL-ZAI-TANIMOJISU(IDY)  TO  RECE46-TANIMOJISU
               MOVE  TBL-ZAI-TANINAME(IDY)    TO  RECE46-TANINAME
               MOVE  TBL-ZAI-TENSIKIBETU(IDY) TO  RECE46-TENSIKIBETU
               MOVE  TBL-ZAI-TEN(IDY)         TO  RECE46-TEN
               MOVE  TBL-ZAI-MEISKYFLG(IDY)   TO  RECE46-MEISKYFLG
               MOVE  TBL-ZAI-AUTOKBN(IDY)     TO  RECE46-AUTOKBN
               MOVE  TBL-ZAI-INPUTNUM(IDY)    TO  RECE46-INPUTNUM
               MOVE  TBL-ZAI-TENTTLKBN(IDY)   TO  RECE46-TENTTLKBN
      *
               MOVE  TBL-ZAI-YUKOKETA(IDY)    TO  RECE46-YUKOKETA
               MOVE  TBL-ZAI-NAME(IDY)        TO  RECE46-NAME
      *
               PERFORM 4501-RECE46-HENSYU-SEC
           END-PERFORM
      *     
           INITIALIZE                    TBL-ZAI-AREA
           MOVE    ZERO                  TO  WRK-TBLMAX
      *
           .
       450-RECE46-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト０６編集処理
      *****************************************************************
       4501-RECE46-HENSYU-SEC    SECTION.
      *
           MOVE    RECE41-KEY        TO  RECE46-KEY
           MOVE    RECE44-PTNUM      TO  RECE46-PTNUM
           MOVE    RECE44-SRYKA      TO  RECE46-SRYKA
           MOVE    RECE44-SRYKBN     TO  RECE46-SRYKBN 
           MOVE    RECE44-ZAIKAISU   TO  RECE46-ZAIKAISU 
      **** MOVE    TBL-ZAI-SRYCD (1) TO  RECE46-SRYCD1
           MOVE    WRK-SRYCD1        TO  RECE46-SRYCD1
           MOVE    RECE44-ZAINUM     TO  RECE46-ZAINUM
           MOVE    WRK-LINENUM       TO  RECE46-LINENUM
           IF    ((RECE46-SRYCD1   =  "101110030") AND
                  (RECE41-SRYYM   <=  "201306"   ))
           OR    ((RECE46-SRYCD1   =  "101800890") AND
                  (RECE41-SRYYM   >=  "201307"   ))
                   MOVE    777          TO  RECE46-GAITENTTLKBN
           ELSE
             IF    RECE46-SRYCD1(1:5)   =  "09591" OR "09592" OR "09593"
               EVALUATE  RECE46-SRYCD1(1:5)
                 WHEN     "09591"
                 WHEN     "09592"
                     MOVE     999       TO  RECE46-GAITENTTLKBN
                 WHEN     "09593"
                   EVALUATE  RECE44-JIBAISYUKEIKBN
                     WHEN   0
                     WHEN   5
                       MOVE   998       TO  RECE46-GAITENTTLKBN
                     WHEN   1
                       MOVE   111       TO  RECE46-GAITENTTLKBN
                     WHEN   2
                       MOVE   121       TO  RECE46-GAITENTTLKBN
                     WHEN   3
                       MOVE   131       TO  RECE46-GAITENTTLKBN
                     WHEN   4
                       MOVE   778       TO  RECE46-GAITENTTLKBN
                   END-EVALUATE
               END-EVALUATE
             ELSE
               IF   (RECE46-SRYCD1 >= "101801010" AND <= "101801020")
               AND  (RECE41-SRYYM  >= "201307"                      )
                    MOVE   997               TO  RECE46-GAITENTTLKBN
               ELSE
                    MOVE   WRK-GAITENTTLKBN  TO  RECE46-GAITENTTLKBN
               END-IF
             END-IF
           END-IF
      *
           IF      IDY   =   WRK-TBLMAX
                MOVE    1      TO     RECE46-ZAILAST
           END-IF
      *
           WRITE   RECE46-REC
           IF      STS-RECE46          NOT =   ZERO
               DISPLAY "*** ORCR0465 RECE46 WRITE ERR  ***"
               MOVE   WRK-TBLMAX     TO  IDY
               MOVE    2             TO  SPA-STS                       
           ELSE
               ADD     1             TO  CNT-RECE46
           END-IF    
      *
           .
       4501-RECE46-HENSYU-EXT.
           EXIT.
      *     
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF      STS-RECEERR         =   ZERO
               CLOSE   RECEERR-FILE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR         TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *         
               IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *            ジョブ管理開始処理
                   MOVE    "JBE"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    WRK-RECEERR     TO  JOB-YOBI
                   MOVE    "9999"          TO  JOB-ERRCD
grpsys             PERFORM 900-CALL-ORCSJOB-SEC
               END-IF
           END-IF
      *                             
           MOVE    1                   TO  FLG-END
           MOVE    2                   TO  SPA-STS
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理中止処理
      *****************************************************************
       500-CANCEL-HENSYU-SEC                SECTION.
      *
           IF      WRK-PARA-SHELLID    =   "RECEPTX"
               CONTINUE
           ELSE
               MOVE    "CHK"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
               IF    ( SJOBKANRI-RETURN
                                       =   ZERO )
               AND   ( JOB-STOPFLG     =   1    )
                   MOVE    "処理が中止されました"
                                           TO  WRK-RECEERR
      *
                   OPEN    INPUT   RECEERR-FILE
                   IF      STS-RECEERR         =   ZERO
                       CLOSE   RECEERR-FILE
                   ELSE
                       OPEN    OUTPUT              RECEERR-FILE
      *    
                       MOVE    WRK-RECEERR         TO  RECEERR-REC
                       WRITE   RECEERR-REC
                       CLOSE   RECEERR-FILE
      *         
      *                ジョブ管理終了処理
                       MOVE    "CAN"           TO  SJOBKANRI-MODE
                       INITIALIZE                  JOBKANRI-REC
                       MOVE    WRK-RECEERR     TO  JOB-YOBI
                       MOVE    "8888"          TO  JOB-ERRCD
                       MOVE    2               TO  JOB-STOPFLG
grpsys                 PERFORM 900-CALL-ORCSJOB-SEC
                   END-IF
      *                             
                   MOVE    1                   TO  FLG-END
               END-IF
           END-IF
           .
       500-CANCEL-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           CLOSE                       RECE41-FILE
           CLOSE                       RECE44-FILE
           CLOSE                       RECE45-FILE
           CLOSE                       RECE46-FILE
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *    ステップ管理終了処理
               MOVE    "STE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *
               PERFORM 900-DBDISCONNECT-SEC
           END-IF
      *
           DISPLAY "RECE41  IN  " CNT-RECE41
           DISPLAY "RECE46  CNT " CNT-RECE46
           DISPLAY "*** ORCR0465 END ***"
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト０２データ順検索処理
      *****************************************************************
       990-RECE41-READ-SEC           SECTION.
      *
           READ    RECE41-FILE         NEXT
               AT  END
                   MOVE    1           TO  FLG-END
                   MOVE    2           TO  SPA-STS
               NOT AT  END
                   ADD     1           TO  CNT-RECE41
                   ADD     1           TO  CNT-IN-CANCEL
      *
                   DISPLAY "RECE41  PTNUM=" RECE41-PTNUM
      *
           END-READ
      *
           .
       990-RECE41-READ-EXT.
           EXIT.
      *
      ****************************************************************
      *    レセプト０４データ読み
      ****************************************************************
       990-RECE44-NEXT-SEC           SECTION.
      *
           READ    RECE44-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE44
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE44
                   ADD     1         TO  CNT-IN-CANCEL
                   IF     (RECE44-MEISAISU    =   ZERO        )
                   OR     (RECE44-SRYKBN      =   "95" OR "96")
                       GO  TO  990-RECE44-NEXT-SEC
                   END-IF       
           END-READ
      *
           .
       990-RECE44-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    レセプト０５データ読み
      ****************************************************************
       990-RECE45-NEXT-SEC           SECTION.
      *
           READ    RECE45-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE45
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE45
           END-READ
      *
           .
       990-RECE45-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理ＤＢ制御処理
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
grpsys     MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       120-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       120-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.
