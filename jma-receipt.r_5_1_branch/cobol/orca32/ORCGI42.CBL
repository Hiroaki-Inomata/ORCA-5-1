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
       PROGRAM-ID.                 ORCGI42.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院会計照会
      *  コンポーネント名  : チェック（Ｉ４２）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02.09.19    ひさなが      新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.00    NACL-多々納  04/05/06  会計照会と同じ機能とする
      *  01.00.01    NACL-多々納  06/05/19  MONFUNC対応,I43,I45削除
      *  03.05.00    NACL-多々納  07/05/09  グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    診療行為コード表名称
           SELECT  TENSUKBN-FILE   ASSIGN  FILE-NAME
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-TENSUKBN.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    診療行為コード表名称
       FD  TENSUKBN-FILE.
       01  TENSUKBN-REC.
           03  TENSU-DATA-REC.
               05  TENSU-KNO.
                   07  TENSU-KNOKBN     PIC X(01).
                   07  TENSU-KNONO      PIC 9(03).
                   07  TENSU-KNONO2     PIC 9(02).
               05  TENSU-KNOMEI         PIC X(50).
               05  TENSU-STYYMM         PIC X(06).
               05  TENSU-EDYYMM         PIC X(06).
      *
       WORKING-STORAGE             SECTION.
      *
       01  FILE-NAME               PIC X(512).
      *    ファイル名
       01  DATA-FILE.
           03  FILLER          PIC X(05)   VALUE   "data/".
           03  FILLER          PIC X(16)   VALUE   "TENSUKBNNAME.TXT".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    会計カード共通パラメタ
           COPY    "I4COMMON-SPA".
      *
      *    画面用ＳＰＡ
       01  SPA-I42.
           03  SPA-I42-AREA.
               05  SPA-GMN-PAGE                  PIC 9(02).
               05  SPA-GMN-MAX                   PIC 9(04).
               05  SPA-GMN-CUR                   PIC 9(02).
      *
           03  SPA-GMN-AREA.
               05  SPA-GMN-SYORI                 PIC 9(04).
      *
               05  SPA-GMN-NYUGAIKBN-G.
                   07  SPA-GMN-NYUGAIKBN           PIC X(01).
                   07  SPA-GMN-NYUH                PIC X(01).
                   07  SPA-GMN-NYUGAIKBNMEI        PIC X(06).
      *
               05  SPA-GMN-SRYKA-G.
                   07  SPA-GMN-SRYKA               PIC X(02).
                   07  SPA-GMN-SRYKAH              PIC X(01).
                   07  SPA-GMN-SRYKAMEI            PIC X(16).
      *
               05  SPA-GMN-AREA2.
                   07  SPA-GMN-TBL.
                       09  SPA-GMN-TBLOCC      OCCURS  9.
                           11  SPA-GMN-TBLREC1     OCCURS  100.
                               13  SPA-GMN-TSRYYMD     PIC X(09).
                               13  SPA-GMN-TNAIYOU1    PIC X(50).
                               13  SPA-GMN-TKAISU1     PIC 9(04).
                       09  SPA-GMN-TBLOCC2     OCCURS  9.
                           11  SPA-GMN-TBLREC2     OCCURS  100.
                               13  SPA-GMN-TSRYYYMM    PIC X(06).
                               13  SPA-GMN-TNAIYOU2    PIC X(50).
                               13  SPA-GMN-TKAISU2     PIC 9(04).
                           11  SPA-GMN-TMAX            PIC 9(04).
      *
                   07  SPA-GMN-TENSU           PIC S9(07).
                   07  SPA-GMN-NYUTENSU        PIC S9(07).
                   07  SPA-GMN-RSITENSU        PIC 9(07).
                   07  SPA-GMN-JIHIKIN         PIC 9(08).
      *
                   07  SPA-GMN-SRYYM           PIC X(06).
      *
               05  SPA-NAI-SRYYM.
                   07  SPA-NAI-SRYYY       PIC 9(04).
                   07  SPA-NAI-SRYMM       PIC 9(02).
      *
      *        入外区分
               05  SPA-GMN-NYUGAIKBN-COMB.
                   07  SPA-GMN-NYUGAIKBN-LST     PIC X(10)
                                                     OCCURS  2.
               05  SPA-NYUGAI-MAX                PIC 9(04).
      *
      *
      *        保険組合せ
               05  SPA-GMN-HHKNCOMBI-G.
                   07  SPA-GMN-HHKNCOMBI          PIC X(04).
                   07  SPA-GMN-HHKNCOMBIH         PIC X(01).
                   07  SPA-GMN-HHKNCOMBIMEI       PIC X(50).
      *  保険組合せ一覧
               05  SPA-HKNCOMI-AREA.
                   07  SPA-GMN-HKNCOMBI-LISTG.
                       09  SPA-GMN-HKN-LIST    OCCURS  20.
                           11  SPA-GMN-THKNCOMBINUM-G.
                               13  SPA-GMN-THKNCOMBINUM    PIC X(04).
                               13  SPA-GMN-THKNCOMBY1      PIC X(01).
                               13  SPA-GMN-THKNCOMBIMEI    PIC X(50).
      *
                           11  SPA-NAI-TFTNRATE     PIC  9(03).
                           11  SPA-NAI-THKNNUM      PIC  X(03).
                           11  SPA-NAI-TKOH1HKNNUM  PIC  X(03).
                           11  SPA-NAI-TKOH2HKNNUM  PIC  X(03).
                           11  SPA-NAI-TKOH3HKNNUM  PIC  X(03).
                           11  SPA-NAI-TKOH4HKNNUM  PIC  X(03).
                       09  SPA-HKN-MAX             PIC  9(04).
      *
               05  SPA-I42NAI-AREA.
                   07  SPA-NAI-TBL.
                       09  SPA-NAI-TBLOCC      OCCURS  9.
                           11  SPA-NAI-TBLREC1     OCCURS  100.
                               13  SPA-NAI-SRYYMD     PIC X(08).
                               13  SPA-NAI-SRYCD1     PIC X(09).
                       09  SPA-NAI-TBLOCC2     OCCURS  9.
                           11  SPA-NAI-TBLREC2     OCCURS  100.
                               13  SPA-NAI-SRYYYMM    PIC X(06).
                               13  SPA-NAI-SRYCD2     PIC X(09).
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-TENSUKBN        PIC X(02).
      *
010900*    フラグ領域
011000 01  FLG-AREA.
011100     03  FLG-END             PIC 9(01).
011200     03  FLG-SRYACT          PIC 9(01).
011300     03  FLG-HKNMST          PIC 9(01).
011400     03  FLG-HKNKUMI         PIC 9(01).
011500     03  FLG-SRYACCT         PIC 9(01).
011600     03  FLG-TENSU           PIC 9(01).
011700     03  FLG-TENSUKBN        PIC 9(01).
           03  FLG-NYUINACT        PIC 9(01).
           03  FLG-NYUINACCT       PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-SRYACCTPLUS     PIC 9(01).
      *
           03  FLG-CHK             PIC 9(01).
      *
           03  FLG-HKNKBN          PIC 9(01).
           03  FLG-OK              PIC 9(01).
      *
           03  FLG-GENZAN          PIC 9(01).
012400*
012500*    カウント領域
012600 01  CNT-AREA.
012700     03  CNT-ERR             PIC 9(06).
012900*
013000*    システム領域
013100 01  SYS-AREA.
013200     03  SYS-YMD.
013300         05  SYS-YY          PIC 9(02).
013400         05  SYS-MM          PIC 9(02).
013500         05  SYS-DD          PIC 9(02).
013600*
013700*    添字領域
013800 01  IDX-AREA.
013900     03  IDX                 PIC 9(04).
014000     03  IDY                 PIC 9(04).
014100     03  IDZ                 PIC 9(04).
014200     03  IDX1                PIC 9(04).
014300     03  IDX2                PIC 9(04).
014400     03  IDX3                PIC 9(04).
014400     03  IDX-D               PIC 9(04).
           03  IDX-MAX             PIC 9(04).
           03  IDH                 PIC 9(04).
           03  IDX-N               PIC 9(04).
           03  IDX-N2              PIC 9(04).
014500*
014600*    一時領域
014700 01  WRK-AREA.
014800     03  WRK-SYSYMD.
014900         05  WRK-SYSYY       PIC 9(04).
015000         05  WRK-SYSMM       PIC 9(02).
015100         05  WRK-SYSDD       PIC 9(02).
015200     03  WRK-SYMD.
015300         05  WRK-SYY         PIC 9(04).
015400         05  WRK-SMM         PIC 9(02).
015500         05  WRK-SDD         PIC 9(02).
015600     03  WRK-WYMD.
015700         05  WRK-WGO         PIC X(01).
015800         05  WRK-WYY         PIC 9(02).
015900         05  WRK-WMM         PIC 9(02).
016000         05  WRK-WDD         PIC 9(02).
      *    診療日付
           03  WRK-SRYYM.
               05  WRK-SRYYY       PIC 9(04).
               05  WRK-SRYMM       PIC 9(02).
      *    点数集計用
           03  WRK-TENSU           PIC 9(07).
           03  WRK-SUM-TEN         PIC 9(07).
           03  WRK-RSI-TEN         PIC 9(07).
           03  WRK-GOK-JIHIKIN     PIC 9(08).
           03  WRK-GOK-RSITENSU    PIC 9(08).
           03  WRK-GOK-TENSU       PIC S9(07).
           03  WRK-GOK-NYUTEN      PIC S9(07).
           03  WRK-GOK-NYUTEN2     PIC S9(07).
      *
           03  WRK-ZZZ              PIC ZZZ.
           03  WRK-Z8               PIC ZZ,ZZZ,ZZZ.
           03  WRK-Z82              PIC --,---,---.
      *
           03  WRK-ERRMSG          PIC X(40).
      *
           03  WRK-HKNCOMBI        PIC 9(04).
      *
           03  WRK-RSI-TENSU        PIC 9(08).
      *
      *    保険テーブル
       01  TBL-HKNCOMBI-AREA.
           03  TBL-HKNCOMBI-OCC        OCCURS   20.
               05  TBL-HKNCOMBI            PIC X(04).
               05  TBL-HKNKBN              PIC 9(01).
      *
      *    入院点数計算用
       01  TBL-NYUIN-TENSU-AREA.
           03  TBL-NYUIN-TENSU-G.
               05  TBL-NYUIN-TENSU-OCC     OCCURS   100.
                   07  TBL-ZAITEN          PIC S9(07).
                   07  TBL-DAY-G.
                       09  TBL-DAY         PIC 9(03)
                                               OCCURS   31.
      *    外泊
           03  TBL-GAIHAKU-SRYYYMM         PIC X(06).
           03  TBL-GAIHAKU-G.
               05  TBL-GAIHAKU             PIC 9(03)
                                           OCCURS   31.
      *    保険組合せ
           03  TBL-HKNCOMBI-NYU-G.
               05  TBL-HKNCOMBI-NYU        PIC 9(03)
                                           OCCURS   31.
      *
      *
      *    編集用テーブル
       01  WRK-TBL-AREA.
           03  WRK-TBL-OCCURS          OCCURS  9.
               05  WRK-TBL-OCC2        OCCURS  200.
                   07  WRK-TBL-SRYCD           PIC X(09).
                   07  WRK-TBL-SRYYYMM         PIC X(06).
                   07  WRK-TBL-MEISYO          PIC X(50).
                   07  WRK-TBL-NYUGAI          PIC X(01).
017500*        コード表記 
017600             07  WRK-TBL-KNO.
017700                 09  WRK-TBL-KNOKBN          PIC  X(01).
017800                 09  WRK-TBL-KNONO           PIC  X(03).
017900                 09  WRK-TBL-KNONO2          PIC  X(02).
018000             07  WRK-TBL-OCC3            OCCURS   31.
018100                 09  WRK-TBL-KAISU           PIC 9(02).
018200             07  WRK-TBL-TUKIKAISU           PIC 9(04).
016900 01  WRK-TBL-AREA2.
018300     03  TBL-IDX1                PIC 9(04).
018400     03  TBL-IDX2                PIC 9(04).
018500     03  TBL-IDX3                PIC 9(02).
018500     03  TBL-IDX4                PIC 9(02).
      *
      *    ソート編集用テーブル
       01  WRK-STBL-AREA.
           03  WRK-STBL-OCCURS          OCCURS  9.
               05  WRK-STBL-OCC2        OCCURS  200.
                   07  WRK-STBL-SRYCD           PIC X(09).
                   07  WRK-STBL-SRYYYMM         PIC X(06).
                   07  WRK-STBL-MEISYO          PIC X(50).
                   07  WRK-STBL-NYUGAI          PIC X(01).
019400*            コード表記 
019500             07  WRK-STBL-KNO.
019600                 09  WRK-STBL-KNOKBN      PIC  X(01).
019700                 09  WRK-STBL-KNONO       PIC  X(03).
019800                 09  WRK-STBL-KNONO2      PIC  X(02).
019900             07  WRK-STBL-OCC3            OCCURS   31.
020000                 09  WRK-STBL-KAISU       PIC 9(02).
020100             07  WRK-STBL-TUKIKAISU       PIC 9(04).
020200*
018800 01  WRK-STBL-AREA2.
020300     03  WRK-SRYYYMM             PIC X(06).
020400     03  WRK-SRYCD               PIC X(09).
020500     03  WRK-KNO                 PIC X(06).
020600     03  WRK-SRYYMD              PIC X(08).
           03  WRK-ZAISKBKBN           PIC X(01).
      *
           03  WRK-ORC-DBPATH          PIC X(64).
020700*
020800*    画面用ソート
020900 01  WRK-GMN-AREA.
021000     05  WRK-GMN-TBLREC1     OCCURS  100.
021100         07  WRK-GMN-TSRYYMD     PIC X(09).
021200         07  WRK-GMN-TNAIYOU1    PIC X(50).
021300         07  WRK-GMN-TKAISU1     PIC 9(04).
021400*
021500     05  WRK-NAI-TBLREC1     OCCURS  100.
021600         07  WRK-NAI-SRYYMD     PIC X(08).
021700         07  WRK-NAI-SRYCD      PIC X(09).
021800*
021900 01  WRK-HEN-AREA.
022000     03  WRK-HENYMDG             PIC X(09).
022100     03  WRK-HENYMD.
022200         05  WRK-HGO         PIC X(01).
022300         05  WRK-HYY         PIC 9(02).
022400         05  FILLER          PIC X(01)   VALUE   ".".
022500         05  WRK-HMM         PIC 9(02).
022600         05  FILLER          PIC X(01)   VALUE   ".".
022700         05  WRK-HDD         PIC 9(02).
      *
023800*    コード表名称テーブル
023900*    COPY    "CMKNOMEI.INC".
024000*   コード表名称テーブル
       01  MKNO-DATA-REC.
           03  MKNO-TBL-OCC        OCCURS    500   INDEXED   MKNO-IDX.
               05  MKNO-KNO.
                   07  MKNO-KNOKBN     PIC X(01).
                   07  MKNO-KNONO      PIC 9(03).
                   07  MKNO-KNONO2     PIC 9(02).
               05  MKNO-KNOMEI         PIC X(50).
               05  MKNO-STYYMM         PIC X(06).
               05  MKNO-EDYYMM         PIC X(06).
022800*
022900**************************************************************
023000*    ファイルレイアウト
023100**************************************************************
023200*
023300*    システム管理マスタ
023400     COPY    "CPSYSKANRI.INC".
023500*
023600     COPY    "CPSK1005.INC".
023700*
024100*    点数マスタ−
024200*01  TNS-TENSU-REC.
024300     COPY    "CPTENSU.INC".
024400*
024500*    診療行為マスタ−
024600 01  SRYACT-REC.
024700     COPY    "CPSRYACT.INC".
024800*
024900*    保険組み合わせ
025000 01  HKNCOMBI-REC.
025100     COPY    "CPHKNCOMBI.INC".
025200*
025300*    診療会計マスタ−
025400 01  SRYACCT-REC.
025500     COPY    "CPSRYACCT.INC".
025600*
025700*    患者コメント
025800 01  PTCOM-REC.
025900     COPY    "CPPTCOM.INC".
      *
      *    入院会計マスタ−
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    入院行為マスタ−
       01  NYUINACT-REC.
           COPY    "CPNYUINACT.INC".
      *
      *    診療会計附加情報マスタ−
       01  SRYACCTPLUS-REC.
           COPY    "CPSRYACCTPLUS.INC".
      *
      *****************************************************************
      *    サブプロ用連絡領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *   数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    保険組合せセット領域
           COPY    "CPORCSHKNSET.INC".
      *
      *    フィアルデイレクトリ位置指定サブ
           COPY  "CPMKPASS.INC".
      *
      *   ＤＢ検索
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      **** COPY    "CPORCMCP.INC".
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
           COPY    "ORCA32SCRAREA.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      **************************************************************
      *    主　　処理
      **************************************************************
       200-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-I42
           MOVE    SPA-WORK        TO  SPA-I4KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
031200     EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
031300         WHEN    "LINK"          ALSO    ANY
031400             PERFORM 100-INIT-SEC
031500*    画面遷移
031600         WHEN     "PUTG"           ALSO   "CLICKED"
031700             PERFORM 200-GMNSENI
031800*    入力
031900         WHEN      OTHER
032000             PERFORM 200-ENTRY
032100     END-EVALUATE.
032200*
032300*    画面遷移がない時、画面表示へ
032400     IF      FLG-END             =   ZERO
032500         PERFORM 500-SET-SCREEN
032600     END-IF
032700*
032800     MOVE    SPA-I4KYOUTU    TO  SPA-WORK
032900     MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-I42         TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      **************************************************************
      *    初期　処理
      **************************************************************
       100-INIT-SEC                SECTION.
      *
      *    DISPLAY "*** ORCGI42    START  ***"
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
      *    診療行為コード表名称セット
           PERFORM 1001-TENSUKBN-SET-SEC
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "I4ERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *
      *        初期画面編集
               PERFORM 300-SCREEN-SEC
      *
      *        画面編集
               PERFORM 500-GMNHEN-SEC
           END-IF
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "I42"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
          .
       100-INIT-EXT.
           EXIT.
      *
      **************************************************************
      *    診療行為コード表名称セット
      **************************************************************
       1001-TENSUKBN-SET-SEC          SECTION.
      *
      *    フィアルデイレクトリ位置指定
           INITIALIZE                  ORCSMKPASSAREA
           MOVE    DATA-FILE           TO  MKPASS-IN-01
           CALL    "ORCSMKPASS"        USING   ORCSMKPASSAREA
           MOVE    MKPASS-OT-01        TO  FILE-NAME
      *
      *    診療行為コード表名称セット
           INITIALIZE                  MKNO-DATA-REC
      *
           OPEN    INPUT               TENSUKBN-FILE
           IF      STS-TENSUKBN    NOT =   ZERO
               DISPLAY "TENSUKBN-FILE OPEN ERR:" STS-TENSUKBN
               GO     TO      1001-TENSUKBN-SET-EXT
           END-IF
      *
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  FLG-TENSUKBN
           PERFORM UNTIL    FLG-TENSUKBN     =   1
               READ                    TENSUKBN-FILE
                   AT  END
                       MOVE    1           TO  FLG-TENSUKBN
                   NOT AT  END
                      ADD     1           TO  IDX
                      IF      IDX         >   500
                           CONTINUE
                       ELSE
                           MOVE    TENSUKBN-REC      TO  MKNO-TBL-OCC
                                                       (IDX)
                       END-IF
               END-READ
           END-PERFORM
      *
           CLOSE                   TENSUKBN-FILE
      *
           .  
       1001-TENSUKBN-SET-EXT.
           EXIT.
      *
      **************************************************************
      *    初期画面処理
      **************************************************************
       300-SCREEN-SEC              SECTION.
      *
           INITIALIZE              SPA-I42
      *
           PERFORM 310-SPA-SET-SEC
           MOVE    1                   TO  SPA-GMN-SYORI
      *
           MOVE    1                   TO  SPA-GMN-CUR
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       310-SPA-SET-SEC                 SECTION.
      *
           MOVE    SPA-I40-SRYYMD(1:6) TO  SPA-NAI-SRYYM
      *    診療科
           MOVE    SPA-I40-SRYKA       TO  SPA-GMN-SRYKA
           PERFORM 3101-SRYKA-HEN-SEC
      *    入外区分
           INITIALIZE                  SPA-GMN-NYUGAIKBN-COMB
           IF      SPA-BEDFLG          >   ZERO
               MOVE    "1 入院"            TO  SPA-GMN-NYUGAIKBN-LST (1)
               MOVE    "2 外来"            TO  SPA-GMN-NYUGAIKBN-LST (2)
               MOVE    2                   TO  SPA-NYUGAI-MAX
           ELSE
               MOVE    "2 外来"            TO  SPA-GMN-NYUGAIKBN-LST (1)
               MOVE    1                   TO  SPA-NYUGAI-MAX
           END-IF
           MOVE    SPA-NYUGAIKBN       TO  SPA-GMN-NYUGAIKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-NYUGAI-MAX
               IF      SPA-GMN-NYUGAIKBN   =   SPA-GMN-NYUGAIKBN-LST
                                                           (IDX)(1:1)
                   MOVE    SPA-GMN-NYUGAIKBN-LST (IDX)
                                           TO  SPA-GMN-NYUGAIKBN-G
               END-IF
           END-PERFORM
      *
      *    保険一覧編集
           PERFORM 3008-HKNCOMBI-HEN-SEC
      *
           PERFORM 3100-GMNSET-SYORI-SEC
      *
           .
       310-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   診療科内容編集処理
      *****************************************************************
       3101-SRYKA-HEN-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SRYKA-MAX-41
               IF      SPA-GMN-SRYKA       =   SPA-GMN-SRYKA-L-41 (IDX)
                   MOVE    SPA-GMN-SRYKA-LIST(IDX)
                                               TO  SPA-GMN-SRYKA-G
                   MOVE    1                   TO  FLG-CHK
                   MOVE    SPA-GMN-SRYKA-MAX-41    TO  IDX
               END-IF
           END-PERFORM
      *
      *
           .
       3101-SRYKA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   保険一覧編集処理
      *****************************************************************
       3008-HKNCOMBI-HEN-SEC             SECTION.
      *
      *
           INITIALIZE                  SPA-GMN-HKNCOMBI-LISTG
      *
           MOVE    SPA-GMN-BIRTHDAY    TO  SPA-BIRTHDAY
           MOVE    SPA-NAI-SRYYM       TO  SPA-SRYYMD(1:6)
           MOVE    "00"                TO  SPA-SRYYMD(7:2)
           MOVE    SPA-GMN-PTID        TO  SPA-PTID
      *
           INITIALIZE                  ORCSHKNSETAREA
           MOVE    "1"                 TO  ORCSHKN-KBN
           MOVE    "1"                 TO  ORCSHKN-KBN2
           CALL    "ORCSHKNSET"        USING
                                       SPA-AREA
                                       ORCSHKNSETAREA
      *
           MOVE    ORCSHKN-HKN-MAX     TO  SPA-HKN-MAX
           IF      SPA-HKN-MAX         =   ZERO
               GO      TO      3008-HKNCOMBI-HEN-EXT
           END-IF
      *
      *    全体を追加
           MOVE    "0000"              TO  SPA-GMN-THKNCOMBINUM (01)
           MOVE    "全保険（包括分以外）"
                                       TO  SPA-GMN-THKNCOMBIMEI (01)
      *
           MOVE    1                   TO  IDH
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >   ORCSHKN-HKN-MAX
               ADD     1                   TO  IDH
               MOVE    ORCSHKN-GMN-HKNCOMBINUM (IDX)
                                       TO  SPA-GMN-THKNCOMBINUM (IDH)
               MOVE    ORCSHKN-GMN-HKNCOMBIMEI (IDX)
                                       TO  SPA-GMN-THKNCOMBIMEI (IDH)
      *
               MOVE    ORCSHKN-NAI-HKNNUM (IDX)
                                       TO  SPA-NAI-THKNNUM  (IDH)
               MOVE    ORCSHKN-NAI-KOH1HKNNUM (IDX)
                                       TO  SPA-NAI-TKOH1HKNNUM (IDH)
               MOVE    ORCSHKN-NAI-KOH2HKNNUM (IDX)
                                       TO  SPA-NAI-TKOH2HKNNUM (IDH)
               MOVE    ORCSHKN-NAI-KOH3HKNNUM (IDX)
                                       TO  SPA-NAI-TKOH3HKNNUM (IDH)
               MOVE    ORCSHKN-NAI-KOH4HKNNUM (IDX)
                                       TO  SPA-NAI-TKOH4HKNNUM (IDH)
      *
               MOVE    ORCSHKN-NAI-FTNRATE (IDX)
                                       TO  SPA-NAI-TFTNRATE (IDH)
      *
               MOVE    IDH             TO  SPA-HKN-MAX
           END-PERFORM
      *    最終行が包括まとめ以外の時、全保険名称を修正
           IF      SPA-GMN-THKNCOMBINUM (SPA-HKN-MAX)  =   "9999"
               CONTINUE
           ELSE
               MOVE    "全保険"        TO  SPA-GMN-THKNCOMBIMEI (01)
           END-IF
      *
           MOVE    SPA-GMN-THKNCOMBINUM-G (01)
                                       TO  SPA-GMN-HHKNCOMBI-G
      *
           .
       3008-HKNCOMBI-HEN-EXT.
           EXIT.
      *
      **************************************************************
      *    画面編集用内容編集処理
      **************************************************************
       3100-GMNSET-SYORI-SEC            SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-MAX
           INITIALIZE                      WRK-TBL-AREA
           INITIALIZE                      WRK-TBL-AREA2
           INITIALIZE                      SPA-GMN-AREA2
           INITIALIZE                      SPA-I42NAI-AREA
      *
           MOVE    SPA-NAI-SRYYM       TO  WRK-SYMD(1:6)
           MOVE    "01"                TO  WRK-SYMD(7:2)
           PERFORM 41012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:6)    TO  SPA-GMN-SRYYM
      *    当月分
           MOVE    SPA-NAI-SRYYM       TO  WRK-SRYYM
           PERFORM 3101-ACCT-SYORI-SEC
      *    点数合計計算
           PERFORM 3109-TENSU-KEISAN-SEC
      *
      *    前月分
           COMPUTE WRK-SRYMM           =   WRK-SRYMM     -   1
           IF      WRK-SRYMM           =   ZERO
               MOVE    12                  TO  WRK-SRYMM
               COMPUTE WRK-SRYYY           =   WRK-SRYYY      -   1
           END-IF
           PERFORM 3101-ACCT-SYORI-SEC
      *
      *    診療年月・診療コード順にＳＯＲＴ
           PERFORM 310121-SORT-01-SEC
      *
      *    編集領域からＳＰＡへ
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   9
               PERFORM 31012-SPA-HEN-SEC
           END-PERFORM
      *
      *    入院基本料を診察料の後
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   100  )  OR
                              (SPA-GMN-TNAIYOU2 (1 IDX)
                                           =   SPACE  )
               IF      SPA-GMN-TNAIYOU2 (1 IDX)    NOT =   SPACE
                   MOVE    IDX             TO  IDX-MAX
               END-IF
           END-PERFORM
           MOVE    IDX-MAX             TO  IDX2
           PERFORM VARYING     IDX     FROM    1    BY  1
                   UNTIL      (IDX     >   100  )  OR
                              (IDX2    >=  100  )  OR
                              (SPA-GMN-TNAIYOU2 (9 IDX)
                                           =   SPACE  )
               IF      SPA-GMN-TNAIYOU2 (9 IDX)    NOT =   SPACE
                   ADD     1               TO  IDX2
                   MOVE    SPA-GMN-TBLREC2 (9 IDX)
                                           TO  SPA-GMN-TBLREC2
                                                       (1 IDX2)
               END-IF
           END-PERFORM
      *
047200     .
047300 3100-GMNSET-SYORI-EXT.
047400     EXIT.
047500*
      *
      *****************************************************************
039800*    診療会計マスターから編集処理
039900*****************************************************************
040000 3101-ACCT-SYORI-SEC                SECTION.
040800*
041200*    診療会計マスターから編集
041300     INITIALIZE                      SRYACCT-REC
041400     MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
041500     MOVE    SPA-GMN-NYUGAIKBN   TO  ACCT-NYUGAIKBN
041600     MOVE    SPA-GMN-PTID        TO  ACCT-PTID
041700     MOVE    SPA-GMN-SRYKA       TO  ACCT-SRYKA
           MOVE    WRK-SRYYM           TO  ACCT-SRYYM
      *
           IF      SPA-GMN-SRYKA       =   ZERO
               MOVE    "key7"          TO  WRK-ORC-DBPATH
           ELSE
               MOVE    "key2"          TO  WRK-ORC-DBPATH
           END-IF
      *
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    WRK-ORC-DBPATH      TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    WRK-ORC-DBPATH      TO  MCP-PATHNAME
               PERFORM 900-SRYACCT-NEXT-SEC
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1               TO  FLG-SRYACCT
           END-IF
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL      (FLG-SRYACCT     =   1   )
      *        診療区分により、処理を決定する
               MOVE    ZERO                TO  IDX
               EVALUATE    ACCT-SRYKBN
      *                診療
                   WHEN    "11"
                   WHEN    "12"
                       MOVE    1               TO  IDX
      *                処方
                   WHEN    "21"
                   WHEN    "22"
                   WHEN    "23"
                       MOVE    2               TO  IDX
      *                注射
                   WHEN    "31"
                   WHEN    "32"
                   WHEN    "33"
                       MOVE    3               TO  IDX
      *                処置
                   WHEN    "40"
                       MOVE    4               TO  IDX
      *                手術・麻酔
                   WHEN    "50"
                   WHEN    "54"
                       MOVE    5               TO  IDX
      *                検査
                   WHEN    "60"
                       MOVE    6               TO  IDX
      *                Ｘ線
                   WHEN    "70"
                       MOVE    7               TO  IDX
      *                リハビリ
                   WHEN    "80"
                       MOVE    8               TO  IDX
               END-EVALUATE
      *
               IF      SPA-GMN-HHKNCOMBI   =   ZERO
      *            包括まとめ以外
                   IF      ACCT-HKNCOMBI       =   "9999"
                       MOVE    ZERO            TO  IDX
                   END-IF
               ELSE
                   IF      SPA-GMN-HHKNCOMBI   =   ACCT-HKNCOMBI
                       CONTINUE
                   ELSE
                       MOVE    ZERO            TO  IDX
                   END-IF
               END-IF
      *
               IF      IDX             NOT =   ZERO
                   PERFORM 3101-SRYACCT-SPA-SEC
               END-IF
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    WRK-ORC-DBPATH      TO  MCP-PATHNAME
               PERFORM 900-SRYACCT-NEXT-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    WRK-ORC-DBPATH      TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    入院の時、入院料の編集
           IF      SPA-GMN-NYUGAIKBN   =   "1"
               PERFORM 3102-NYUACCT-SYORI-SEC
           END-IF
      *
           .
       3101-ACCT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *   診療会計マスターより画面内容編集処理
      *****************************************************************
       3101-SRYACCT-SPA-SEC             SECTION.
      *
      *    診療行為マスター読み込み
           INITIALIZE                      SRYACT-REC
           MOVE    ACCT-HOSPNUM        TO  SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID           TO  SRY-PTID
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
      *    点数検索用日付編集
           MOVE    ACCT-SRYYM          TO  WRK-SYSYMD(1:6)
           MOVE    01                  TO  WRK-SYSDD
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL       IDX-D   >   31
               IF      ACCT-DAY (1 IDX-D)  NOT =   ZERO
                   MOVE    IDX-D           TO  WRK-SYSDD
                   MOVE    31              TO  IDX-D
               END-IF
           END-PERFORM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-SINKOUI-RAED-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACT
           END-IF
      *
           PERFORM UNTIL       FLG-SRYACT      =   1       
      *        剤明細編集
               PERFORM VARYING     IDZ     FROM    1   BY  1
                       UNTIL      (IDZ         >   5   )  OR
                                  (SRY-SRYCD (IDZ) =   SPACE )
      *            コメント、薬剤投与、粉砕、自動発生分は対象外
                   IF     (SRY-SRYCD (IDZ)(1:1)    =   "8"  )  OR
                          (SRY-SRYCD (IDZ)(1:3)    =   "001")  OR
                          (SRY-SRYCD (IDZ)(1:3)    =   "008")  OR
                          (SRY-SRYCD (IDZ)         =   "099209901") OR
                          (SRY-SRYCD (IDZ)         =   "099309901") OR
                          (SRY-AUTOKBN(IDZ)        =   "2"
                                                   OR  "3" )
                       CONTINUE
                   ELSE
      *                点数マスタを取得する
                       MOVE    SRY-SRYCD (IDZ)     TO  TNS-SRYCD
                       PERFORM 910-TENSU-READ-SEC
      *                加算とフィルムは対象外とする
                       IF     (FLG-TENSU       NOT =   ZERO  )  OR
                              (TNS-DATAKBN         =   "2"  OR  "3")
                           CONTINUE
                       ELSE
                           PERFORM 31011-SINKOUI-SPA-SEC
                       END-IF
                   END-IF
               END-PERFORM
      *
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-SINKOUI-RAED-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
046200*
052700     .
052800 3101-SRYACCT-SPA-EXT.
052900     EXIT.
053000*
053100*****************************************************************
053200*   診療行為ごとの編集処理
053300*****************************************************************
053400 31011-SINKOUI-SPA-SEC           SECTION.
053500*
053600     IF      IDX             =   1   OR  2  OR  3
      *        手術追加
                                   OR  5
053700*        診療行為別編集
053800         PERFORM 310111-SINKOUI-01-SEC 
053900     ELSE
054000*        コード表別編集
054100         PERFORM 310112-SINKOUI-02-SEC 
054200     END-IF
054300*
054400     .
054500 3101-SRYACCT-SPA-EXT.
054600     EXIT.
054700*
054800*****************************************************************
054900*   診療行為ごとの編集処理
055000*****************************************************************
055100 310111-SINKOUI-01-SEC           SECTION.
055200*
      *    手術の時手技料のみ
           IF      IDX                 =   5
               IF     (TNS-SRYCD(1:1)      =   "1" )  AND
                      (TNS-DATAKBN         =   "1" )
                   CONTINUE
               ELSE
                   GO      TO      310111-SINKOUI-01-EXT
               END-IF
           END-IF
055300     MOVE    ZERO                TO  TBL-IDX2
055400*    診療行為より、対象チェック
055500     PERFORM VARYING     TBL-IDX1    FROM    1   BY  1
055600             UNTIL       TBL-IDX1       >    200
055700         EVALUATE    TRUE
055800             WHEN    WRK-TBL-SRYCD (IDX TBL-IDX1)    =   SPACE
055900                 MOVE    SRY-SRYCD (IDZ)     TO  WRK-TBL-SRYCD
056000                                                  (IDX TBL-IDX1)
056100                 MOVE    ACCT-SRYYM          TO  WRK-TBL-SRYYYMM
056200                                                  (IDX TBL-IDX1)
056300                 MOVE    TNS-NAME            TO  WRK-TBL-MEISYO
056400                                                  (IDX TBL-IDX1)
056500                 MOVE    TNS-CDKBN-KBN       TO  WRK-TBL-KNOKBN
056600                                                  (IDX TBL-IDX1)
056700                 MOVE    TNS-CDKBN-KBNNUM    TO  WRK-TBL-KNONO
056800                                                  (IDX TBL-IDX1)
056900                 MOVE    TNS-CDKBN-KBNNUM-EDA  TO  WRK-TBL-KNONO2
057000                                                  (IDX TBL-IDX1)
057100                 MOVE    TBL-IDX1            TO  TBL-IDX2
057200                 MOVE    200                 TO  TBL-IDX1
057300             WHEN    (SRY-SRYCD (IDZ)    =   WRK-TBL-SRYCD
057400                                                 (IDX TBL-IDX1))
057500                AND  (ACCT-SRYYM         =   WRK-TBL-SRYYYMM
057600                                                 (IDX TBL-IDX1))
057700                 MOVE    TBL-IDX1            TO  TBL-IDX2
057800                 MOVE    200                 TO  TBL-IDX1
057900         END-EVALUATE
058000     END-PERFORM
058100*
058200     IF      TBL-IDX2            =   ZERO
058300         MOVE    "9999"              TO  SPA-ERRCD
058500     END-IF
058600*
058700*    診療行為回数計算
           PERFORM VARYING     TBL-IDX3    FROM    1   BY  1
058900             UNTIL      (TBL-IDX3    >   31  )   OR
                              (SPA-ERRCD   NOT =   SPACE )
059000*
               IF     (ACCT-SRYKBN         =   "21" )  AND
                      (SPA-GMN-NYUGAIKBN   =   "1"  )
                   ADD     ACCT-DAY(1 TBL-IDX3)    TO  WRK-TBL-KAISU
                                             (IDX TBL-IDX2 TBL-IDX3)
               ELSE
059100             IF      ACCT-DAY(1 TBL-IDX3)
                                          NOT =   ZERO
059200                 ADD     1              TO  WRK-TBL-KAISU
059300                                       (IDX TBL-IDX2 TBL-IDX3)
                   END-IF
               END-IF
059500     END-PERFORM
059600*
059700     .
059800 310111-SINKOUI-01-EXT.
059900     EXIT.
060000*
      *
      *****************************************************************
      *   コード表ごとの編集処理
      *****************************************************************
       310112-SINKOUI-02-SEC           SECTION.
      *
           MOVE    ZERO                TO  TBL-IDX2
      *
      *    コード表がない時、対象外
           IF     (TNS-CDKBN-KBN       =   SPACE )  AND
                  (TNS-CDKBN-KBNNUM    =   ZERO  )  AND
                  (TNS-CDKBN-KBNNUM-EDA    =   ZERO  )
               GO      TO      310112-SINKOUI-02-EXT
           END-IF
      *    療養担当手当は対象とする
           IF     (TNS-CDKBN-KBN       =   "*"   )
               IF     (TNS-CDKBN-KBNNUM    =   ZERO  )  AND
                      (TNS-CDKBN-KBNNUM-EDA    =   1 )  AND
                      (TNS-SRYKBN          =   "80"  )
                   CONTINUE
               ELSE
                   GO      TO      310112-SINKOUI-02-EXT
               END-IF
           END-IF
      *    手技料のみ
           IF      SRY-SRYCD (IDZ)(1:1)    NOT =   "1"
               GO      TO      310112-SINKOUI-02-EXT
           END-IF
      *    検体検査コメント以外
           IF     (TNS-KENTAICOMMENT   NOT =   ZERO )  AND
                  (ACCT-SRYKBN             =   "60" )
               GO      TO      310112-SINKOUI-02-EXT
           END-IF
061400*
061500*    診療行為より、対象チェック
061600     PERFORM VARYING     TBL-IDX1    FROM    1   BY  1
061700             UNTIL       TBL-IDX1       >    200
061800         EVALUATE    TRUE
061900             WHEN    WRK-TBL-KNO   (IDX TBL-IDX1)    =   SPACE
062000                 MOVE    SRY-SRYCD (IDZ)     TO  WRK-TBL-SRYCD
062100                                                 (IDX TBL-IDX1)
062200                 MOVE    ACCT-SRYYM          TO  WRK-TBL-SRYYYMM
062300                                                 (IDX TBL-IDX1)
062400                 MOVE    TNS-CDKBN-KBN       TO  WRK-TBL-KNOKBN
062500                                                 (IDX TBL-IDX1)
062600                 MOVE    TNS-CDKBN-KBNNUM    TO  WRK-TBL-KNONO
062700                                                 (IDX TBL-IDX1)
062800                 MOVE    TNS-CDKBN-KBNNUM-EDA  TO  WRK-TBL-KNONO2
062900                                                 (IDX TBL-IDX1)
063000*
063100*                名称編集
063200                 PERFORM 3101121-KNO-MEI-SEC
063300*
063400                 MOVE    TBL-IDX1            TO  TBL-IDX2
063500                 MOVE    200                 TO  TBL-IDX1
063600             WHEN    (TNS-CDKBN-KBN     
063700                            =   WRK-TBL-KNOKBN(IDX TBL-IDX1))
063800                AND  (TNS-CDKBN-KBNNUM   
063900                            =   WRK-TBL-KNONO(IDX TBL-IDX1))
064000                AND  (TNS-CDKBN-KBNNUM-EDA   
064100                            =   WRK-TBL-KNONO2(IDX TBL-IDX1))
064200                AND  (ACCT-SRYYM         
064300                            =   WRK-TBL-SRYYYMM(IDX TBL-IDX1))
064400                 MOVE    TBL-IDX1            TO  TBL-IDX2
064500                 MOVE    200                 TO  TBL-IDX1
064600         END-EVALUATE
064700     END-PERFORM
064800*
064900     IF      TBL-IDX2            =   ZERO
065000         MOVE    "9999"              TO  SPA-ERRCD
065100         GO      TO      310112-SINKOUI-02-EXT
065200     END-IF
065300*
065400*    診療行為回数計算
058800     PERFORM VARYING     TBL-IDX3    FROM    1   BY  1
058900             UNTIL       TBL-IDX3    >   31
059000*
059100         IF      ACCT-DAY(1  TBL-IDX3)
                                          NOT =   ZERO
059200             ADD     1              TO  WRK-TBL-KAISU
059300                                       (IDX TBL-IDX2 TBL-IDX3)
059400         END-IF
059500     END-PERFORM
066300*
066400     .
066500 310112-SINKOUI-02-EXT.
066600     EXIT.
      *
      *****************************************************************
      *   コード表示名称編集処理
      *****************************************************************
       3101121-KNO-MEI-SEC              SECTION.
      *
           SET     MKNO-IDX            TO  1
           SEARCH      MKNO-TBL-OCC    VARYING   MKNO-IDX
                   AT   END
                       MOVE    WRK-TBL-KNO   (IDX TBL-IDX1)
                               TO  WRK-TBL-MEISYO(IDX TBL-IDX1)
                   WHEN    WRK-TBL-KNO   (IDX TBL-IDX1)
                                     =   MKNO-KNO    (MKNO-IDX)
                       AND (SRY-SRYYM    >=   MKNO-STYYMM (MKNO-IDX))
                       AND (SRY-SRYYM    <=   MKNO-EDYYMM (MKNO-IDX))
                       MOVE    MKNO-KNOMEI (MKNO-IDX)
                               TO  WRK-TBL-MEISYO(IDX TBL-IDX1)
           END-SEARCH
      *
           .
       3101121-KNO-MEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院診療会計編集処理
      *****************************************************************
       3102-NYUACCT-SYORI-SEC             SECTION.
      *
      *    包括まとめは対象外
           IF      SPA-GMN-HHKNCOMBI   =   "9999"
               GO      TO      3102-NYUACCT-SYORI-EXT
           END-IF
           INITIALIZE                      TBL-GAIHAKU-G
      *
           INITIALIZE                      NYUINACCT-REC
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    SPA-GMN-NYUGAIKBN   TO  NACCT-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  NACCT-PTID
      *    診療科（変更時のみ）
           IF      SPA-I40-SRYKA       =   SPA-GMN-SRYKA
               MOVE    "00"                TO  NACCT-SRYKA
           ELSE
               MOVE    SPA-GMN-SRYKA       TO  NACCT-SRYKA
           END-IF
           MOVE    WRK-SRYYM           TO  NACCT-SRYYM
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-NYUINACCT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-NYUINACCT
           END-IF
           PERFORM
               UNTIL   FLG-NYUINACCT       =   1
      *
      *        診療区分により、処理を決定する
               MOVE    ZERO                TO  IDX
               EVALUATE    NACCT-SRYKBN
      *            入院
                   WHEN    "90"
                   WHEN    "92"
                   WHEN    "97"
                       MOVE    9               TO  IDX
               END-EVALUATE
               IF      IDX             >   ZERO
                   PERFORM 31021-NYUINACCT-SPA-SEC
               END-IF
               EVALUATE    NACCT-ZAISKBKBN
                   WHEN    "2"
      *            外泊
                       MOVE    NACCT-DAY-AREA  TO  TBL-GAIHAKU-G
                       MOVE    NACCT-SRYYM     TO  TBL-GAIHAKU-SRYYYMM
               END-EVALUATE
      *
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-NYUINACCT-READ-SEC
           END-PERFORM
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    外泊の再編集
           PERFORM 310212-GAIHAKU-SEC
      *
           .
       3102-NYUACCT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *   診療会計マスターより画面内容編集処理
      *****************************************************************
       31021-NYUINACCT-SPA-SEC             SECTION.
      *
      *    診療行為マスター読み込み
           INITIALIZE                      NYUINACT-REC
           MOVE    NACCT-HOSPNUM       TO  NSRY-HOSPNUM
           MOVE    NACCT-NYUGAIKBN     TO  NSRY-NYUGAIKBN
           MOVE    NACCT-PTID          TO  NSRY-PTID
           MOVE    NACCT-SRYKA         TO  NSRY-SRYKA
           MOVE    NACCT-SRYYM         TO  NSRY-SRYYM
           MOVE    NACCT-ZAINUM        TO  NSRY-ZAINUM
      *    点数検索用日付編集
           MOVE    NACCT-SRYYM         TO  WRK-SYSYMD(1:6)
           MOVE    01                  TO  WRK-SYSDD
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL       IDX-D   >   31
               IF      NACCT-DAY (IDX-D)   NOT =   ZERO
                   MOVE    IDX-D           TO  WRK-SYSDD
                   MOVE    31              TO  IDX-D
               END-IF
           END-PERFORM
      *
           MOVE    NYUINACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_nyuinact"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 940-NYUINACT-RAED-SEC
           ELSE
               MOVE    1                   TO  FLG-NYUINACT
           END-IF
      *
           PERFORM UNTIL       FLG-NYUINACT      =   1
      *        剤明細編集
               PERFORM VARYING     IDZ     FROM    1   BY  1
                       UNTIL      (IDZ         >   5   )  OR
                                  (NSRY-SRYCD (IDZ) =   SPACE )
      *            コメントは対象外
                   IF     NSRY-SRYCD (IDZ)(1:1)    =   "8"
                       CONTINUE
                   ELSE
      *                点数マスタを取得する
                       MOVE    NSRY-SRYCD (IDZ)     TO  TNS-SRYCD
                       PERFORM 910-TENSU-READ-SEC
                       IF      FLG-TENSU           =   ZERO
                           PERFORM 31011-NYUINACT-SPA-SEC
                       END-IF
                   END-IF
               END-PERFORM
      *
               MOVE    "tbl_nyuinact"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 940-NYUINACT-RAED-SEC
           END-PERFORM
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       31021-NYUINACCT-SPA-EXT.
           EXIT.
      *
      *****************************************************************
      *   入院診療行為ごとの編集処理
      *****************************************************************
       31011-NYUINACT-SPA-SEC           SECTION.
      *
           MOVE    ZERO                TO  TBL-IDX2
      *    診療行為より、対象チェック
           PERFORM VARYING     TBL-IDX1    FROM    1   BY  1
                   UNTIL       TBL-IDX1       >    200
               EVALUATE    TRUE
                   WHEN    WRK-TBL-SRYCD (IDX TBL-IDX1)    =   SPACE
                       MOVE    NSRY-SRYCD (IDZ)    TO  WRK-TBL-SRYCD
                                                       (IDX TBL-IDX1)
                       MOVE    NACCT-SRYYM         TO  WRK-TBL-SRYYYMM
                                                        (IDX TBL-IDX1)
                       MOVE    TNS-NAME            TO  WRK-TBL-MEISYO
                                                        (IDX TBL-IDX1)
      *                剤識別番号
                       MOVE    NACCT-ZAISKBKBN     TO  WRK-TBL-KNOKBN
                                                        (IDX TBL-IDX1)
      *************    MOVE    TNS-CDKBN-KBNNUM    TO  WRK-TBL-KNONO
      *                                                 (IDX TBL-IDX1)
      *                MOVE    TNS-CDKBN-KBNNUM-EDA  TO  WRK-TBL-KNONO2
      *************                                     (IDX TBL-IDX1)
      *                入院料
                       MOVE    "1"                 TO  WRK-TBL-NYUGAI
                                                        (IDX TBL-IDX1)
                       MOVE    TBL-IDX1            TO  TBL-IDX2
                       MOVE    200                 TO  TBL-IDX1
                   WHEN    (NSRY-SRYCD (IDZ)   =   WRK-TBL-SRYCD
                                                       (IDX TBL-IDX1))
                      AND  (NACCT-SRYYM        =   WRK-TBL-SRYYYMM
                                                       (IDX TBL-IDX1))
                       MOVE    TBL-IDX1            TO  TBL-IDX2
                       MOVE    200                 TO  TBL-IDX1
               END-EVALUATE
           END-PERFORM
      *
           IF      TBL-IDX2            =   ZERO
               MOVE    "9999"              TO  SPA-ERRCD
               GO      TO      31011-NYUINACT-SPA-EXT
           END-IF
      *
      *    診療行為回数計算
           PERFORM VARYING     TBL-IDX3    FROM    1   BY  1
                   UNTIL       TBL-IDX3    >   31
      *
               IF      NACCT-DAY(TBL-IDX3)
                                          NOT =   ZERO
                       ADD     1              TO  WRK-TBL-KAISU
                                             (IDX TBL-IDX2 TBL-IDX3)
               END-IF
           END-PERFORM
      *
           .
       31011-NYUINACT-SPA-EXT.
           EXIT.
      *    
      *****************************************************************
      *   外泊の再編集処理
      *****************************************************************
       310212-GAIHAKU-SEC           SECTION.
      *
      *    食事・加算を削除
      *    診療行為より、対象チェック
           PERFORM VARYING     TBL-IDX1    FROM    1   BY  1
                   UNTIL       TBL-IDX1       >    200
               IF      WRK-TBL-KNOKBN (9 TBL-IDX1) =   "4"
                                                  OR   "6"
      *            外泊日の加算を表示しない
                   PERFORM VARYING     TBL-IDX3    FROM    1   BY  1
                           UNTIL       TBL-IDX3    >   31
                     IF      TBL-GAIHAKU-SRYYYMM  = WRK-TBL-SRYYYMM
                                                        (9 TBL-IDX1)
                       IF      TBL-GAIHAKU (TBL-IDX3)  =   1
                                                       OR  2
                                                       OR  3
                           MOVE    ZERO            TO  WRK-TBL-KAISU
                                                   (9 TBL-IDX1 TBL-IDX3)
                       END-IF
                     END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
           .
       310212-GAIHAKU-EXT.
           EXIT.
068800*****************************************************************
068900*   編集領域からＳＰＡへ  編集処理
069000*****************************************************************
069100 31012-SPA-HEN-SEC               SECTION.
069200*
069300     MOVE    ZERO                TO  IDX1
069400     MOVE    ZERO                TO  IDX2
           INITIALIZE                  SPA-I42NAI-AREA
069500*
069600     PERFORM VARYING     TBL-IDX2    FROM    1   BY  1
069700             UNTIL      (TBL-IDX2       >    200     )   OR
                              (IDX1           =    100     )   OR
                              (IDX2           =    100     )   OR
069800                        (WRK-TBL-SRYCD (IDX TBL-IDX2)
                                              =   SPACE )
069900         ADD     1               TO  IDX1
070000         MOVE    WRK-TBL-SRYYYMM (IDX TBL-IDX2)
070100                                 TO  SPA-NAI-SRYYYMM (IDX IDX1)
070200         MOVE    WRK-TBL-SRYCD   (IDX TBL-IDX2)
070300                                 TO  SPA-NAI-SRYCD2 (IDX IDX1)
070400*
070500         MOVE    WRK-TBL-SRYYYMM (IDX TBL-IDX2)
070600                                     TO  WRK-SYMD(1:6)
070700         MOVE    01                  TO  WRK-SDD
070800         PERFORM 41012-SEIWA-HEN-SEC
070900         MOVE    WRK-HENYMDG    TO  SPA-GMN-TSRYYYMM (IDX IDX1)
071000*
071100         MOVE    WRK-TBL-MEISYO  (IDX TBL-IDX2)
071200                                TO  SPA-GMN-TNAIYOU2(IDX IDX1)
071300         PERFORM VARYING     TBL-IDX3    FROM    1   BY  1
071400                 UNTIL      (TBL-IDX3       >    31      )  OR
                                  (IDX2           =    200     )
                   IF      WRK-TBL-KAISU (IDX TBL-IDX2 TBL-IDX3)
                                                  >    ZERO 
                     IF      WRK-TBL-NYUGAI(IDX TBL-IDX2)  
                                                  NOT =   "1"
071700                 ADD     1                  TO  IDX2
071800                 MOVE    SPA-NAI-SRYYYMM (IDX IDX1)
071900                                            TO  SPA-NAI-SRYYMD
072000                                                (IDX IDX2)(1:6)
072100                 MOVE    TBL-IDX3           TO  SPA-NAI-SRYYMD
072200                                                (IDX IDX2)(7:2)
072300*
072400                 MOVE    SPA-NAI-SRYYMD (IDX IDX2)
072500                                            TO  WRK-SYMD
072600                 PERFORM 41012-SEIWA-HEN-SEC
072700                 MOVE    WRK-HENYMDG        TO  SPA-GMN-TSRYYMD
072800                                                     (IDX IDX2)
072900*
073000                 MOVE    WRK-TBL-MEISYO  (IDX TBL-IDX2)
073100                                           TO  SPA-GMN-TNAIYOU1
073200                                                      (IDX IDX2)
073300                 MOVE    WRK-TBL-SRYCD   (IDX TBL-IDX2)
073400                                            TO  SPA-NAI-SRYCD1
073500                                                      (IDX IDX2)
073600                 ADD     1                  TO  SPA-GMN-TKAISU1
073700                                                      (IDX IDX2)
                     END-IF
      *
      *              投薬、入院の時
                     IF      (IDX                =   2  )  AND
                             (SPA-GMN-NYUGAIKBN  =   "1" )
                         ADD     WRK-TBL-KAISU (IDX TBL-IDX2 TBL-IDX3)
                                                 TO  SPA-GMN-TKAISU2
                                                            (IDX IDX1)
                     ELSE
073900                   ADD     1                TO  SPA-GMN-TKAISU2
074000                                                      (IDX IDX1)
                     END-IF
074100             END-IF
074200         END-PERFORM
074300      END-PERFORM
074400*
074500*    診療年月日・診療コード順にＳＯＲＴ
074600     PERFORM 310122-SORT-02-SEC
074700*
074800     .
074900 31012-SPA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   診療年月・診療コード順にＳＯＲＴ  編集処理
      *****************************************************************
       310121-SORT-01-SEC               SECTION.
      *
      *    診療年月・診療コード順にＳＯＲＴ
           INITIALIZE                      WRK-STBL-AREA
           INITIALIZE                      WRK-STBL-AREA2
           MOVE    WRK-TBL-AREA        TO  WRK-STBL-AREA
           INITIALIZE                      WRK-TBL-AREA2
           INITIALIZE                      WRK-TBL-AREA
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   9
      *        診療行為別集計ＳＯＲＴ
               PERFORM 3101211-SORT-011-SEC 
           END-PERFORM 
      *
           .
       310121-SORT-01-EXT.
           EXIT.
      *
      *****************************************************************
      *   診療行為別集計ＳＯＲＴ  編集処理
      *****************************************************************
       3101211-SORT-011-SEC               SECTION.
      *
           IF      IDX             =   9
      *        入院料
               PERFORM 31012110-NYUIN-SORT-SEC
               GO      TO  3101211-SORT-011-EXT
           END-IF
      *
           PERFORM VARYING     IDX1        FROM    1   BY  1
                   UNTIL       IDX1        >   200
               MOVE    LOW-VALUE           TO  WRK-SRYYYMM
               MOVE    HIGH-VALUE          TO  WRK-SRYCD
               MOVE    ZERO                TO  IDX3
               PERFORM VARYING     IDX2        FROM    1   BY  1
                       UNTIL       IDX2        >   200
                   IF      WRK-STBL-SRYCD (IDX IDX2)    =   SPACE
                       CONTINUE
                   ELSE
                       IF      WRK-STBL-SRYYYMM(IDX IDX2)
                                                   >   WRK-SRYYYMM
                           MOVE    WRK-STBL-SRYCD  (IDX IDX2)  
                                                     TO  WRK-SRYCD
                           MOVE    WRK-STBL-SRYYYMM(IDX IDX2)  
                                                     TO  WRK-SRYYYMM
                           MOVE    IDX2             TO  IDX3
                       ELSE
                          IF     (WRK-STBL-SRYYYMM(IDX IDX2)
                                               =   WRK-SRYYYMM) AND
                                 (WRK-STBL-SRYCD(IDX IDX2)
                                               <   WRK-SRYCD  )
                               MOVE    WRK-STBL-SRYCD  (IDX IDX2)
                                                      TO  WRK-SRYCD
                               MOVE    WRK-STBL-SRYYYMM(IDX IDX2)
                                                      TO  WRK-SRYYYMM
                               MOVE    IDX2           TO  IDX3
                          END-IF
                      END-IF
                   END-IF
               END-PERFORM
      *
               IF      IDX3                >   ZERO
                   MOVE    WRK-STBL-OCC2 (IDX IDX3)
                                          TO  WRK-TBL-OCC2 (IDX IDX1)
                   MOVE    SPACE          TO  WRK-STBL-OCC2(IDX IDX3)
               END-IF
           END-PERFORM
      *
           .
       3101211-SORT-011-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *   入院料ＳＯＲＴ処理
      *****************************************************************
       31012110-NYUIN-SORT-SEC               SECTION.
      *
      *
           PERFORM VARYING     IDX1        FROM    1   BY  1
                   UNTIL       IDX1        >   200
               MOVE    LOW-VALUE           TO  WRK-SRYYYMM
               MOVE    HIGH-VALUE          TO  WRK-ZAISKBKBN
               MOVE    HIGH-VALUE          TO  WRK-SRYCD
               MOVE    ZERO                TO  IDX3
               PERFORM VARYING     IDX2        FROM    1   BY  1
                       UNTIL       IDX2        >   200
                   IF      WRK-STBL-SRYCD (IDX IDX2)    =   SPACE
                       CONTINUE
                   ELSE
                       EVALUATE    TRUE
                       WHEN    WRK-STBL-SRYYYMM(IDX IDX2)
                                               >   WRK-SRYYYMM
                           MOVE    WRK-STBL-SRYCD  (IDX IDX2)  
                                                     TO  WRK-SRYCD
                           MOVE    WRK-STBL-KNOKBN (IDX IDX2)  
                                                     TO  WRK-ZAISKBKBN
                           MOVE    WRK-STBL-SRYYYMM(IDX IDX2)  
                                                     TO  WRK-SRYYYMM
                           MOVE    IDX2             TO  IDX3
                       WHEN   (WRK-STBL-SRYYYMM(IDX IDX2)
                                               =   WRK-SRYYYMM) AND
                              (WRK-STBL-KNOKBN(IDX IDX2)
                                               <   WRK-ZAISKBKBN ) 
                           MOVE    WRK-STBL-SRYCD  (IDX IDX2)  
                                                    TO  WRK-SRYCD
                           MOVE    WRK-STBL-KNOKBN (IDX IDX2)  
                                                    TO  WRK-ZAISKBKBN
                           MOVE    WRK-STBL-SRYYYMM(IDX IDX2)  
                                                    TO  WRK-SRYYYMM
                           MOVE    IDX2             TO  IDX3
                       WHEN    (WRK-STBL-SRYYYMM(IDX IDX2)
                                               =   WRK-SRYYYMM) AND
                               (WRK-STBL-KNOKBN(IDX IDX2)
                                               <   WRK-ZAISKBKBN )  AND
                               (WRK-STBL-SRYCD(IDX IDX2)
                                               <   WRK-SRYCD  )
                           MOVE    WRK-STBL-SRYCD  (IDX IDX2)
                                                     TO  WRK-SRYCD
                           MOVE    WRK-STBL-KNOKBN (IDX IDX2)  
                                                     TO  WRK-ZAISKBKBN
                           MOVE    WRK-STBL-SRYYYMM(IDX IDX2)
                                                     TO  WRK-SRYYYMM
                           MOVE    IDX2              TO  IDX3
                       END-EVALUATE
                   END-IF
               END-PERFORM
      *
               IF      IDX3                >   ZERO
                   MOVE    WRK-STBL-OCC2 (IDX IDX3)
                                          TO  WRK-TBL-OCC2 (IDX IDX1)
                   MOVE    SPACE          TO  WRK-STBL-OCC2(IDX IDX3)
               END-IF
           END-PERFORM
      *
      *
           .
       31012110-NYUIN-SORT-EXT.
           EXIT.
      *
      *****************************************************************
      *   診療年月日・診療コード順にＳＯＲＴ処理
      *****************************************************************
       310122-SORT-02-SEC               SECTION.
      *    ワークへ編集
           INITIALIZE                      WRK-GMN-AREA
           MOVE    ZERO                TO  IDX-MAX
082600     PERFORM VARYING     IDX1    FROM    1   BY  1
082700             UNTIL       IDX1    >    100
082800         MOVE    SPA-GMN-TBLREC1 (IDX IDX1)
082900                                      TO  WRK-GMN-TBLREC1 (IDX1)
083000         MOVE    SPA-NAI-TBLREC1 (IDX IDX1)
083100                                      TO  WRK-NAI-TBLREC1 (IDX1)
083200*        INITIALIZE                   SPA-GMN-TBLREC1 (IDX IDX1)
083300*        INITIALIZE                   SPA-NAI-TBLREC1 (IDX IDX1)
               MOVE    SPACE           TO   SPA-GMN-TBLREC1 (IDX IDX1)
083300         MOVE    SPACE           TO   SPA-NAI-TBLREC1 (IDX IDX1)
083400     END-PERFORM
083500*
083600     PERFORM VARYING     IDX1        FROM    1   BY  1
083700             UNTIL       IDX1        >   100
083800         MOVE    LOW-VALUE           TO  WRK-SRYYMD
083900         MOVE    HIGH-VALUE          TO  WRK-SRYCD
084000         MOVE    ZERO                TO  IDX3
084100         PERFORM VARYING     IDX2        FROM    1   BY  1
084200                 UNTIL       IDX2        >   100
084300         IF      WRK-NAI-SRYCD (IDX2)    =   SPACE
084400             CONTINUE
084500         ELSE
084600             EVALUATE    TRUE
084700*
084800                 WHEN    WRK-NAI-SRYYMD (IDX2)   >   WRK-SRYYMD
084900                     MOVE    WRK-NAI-SRYCD  (IDX2)   
085000                                                 TO  WRK-SRYCD
085100                     MOVE    WRK-NAI-SRYYMD (IDX2)   
085200                                                 TO  WRK-SRYYMD
085300                     MOVE    IDX2                TO  IDX3
085400*
085500                 WHEN   (WRK-NAI-SRYYMD (IDX2)   
085600                                          =   WRK-SRYYMD )  AND
085700                        (WRK-NAI-SRYCD  (IDX2)   <   WRK-SRYCD  )
085800                     MOVE    WRK-NAI-SRYCD  (IDX2)   
085900                                          TO  WRK-SRYCD
086000                     MOVE    WRK-NAI-SRYYMD (IDX2)   
086100                                          TO  WRK-SRYYMD
086200                     MOVE    IDX2         TO  IDX3
086300*                同日に同じ行為の時、削除とする
086400                 WHEN   (WRK-NAI-SRYYMD (IDX2)   
086500                                        =   WRK-SRYYMD )  AND
086600                        (WRK-NAI-SRYCD  (IDX2)   
086700                                        =   WRK-SRYCD  )
086800                     MOVE    SPACE  TO  WRK-GMN-TBLREC1 (IDX2)
086900                     MOVE    SPACE  TO  WRK-NAI-TBLREC1 (IDX2)
087000             END-EVALUATE
      *
               END-IF
087100         END-PERFORM
087200*
087300         IF      IDX3                >   ZERO
087400             MOVE    WRK-GMN-TBLREC1 (IDX3)
087500                                TO  SPA-GMN-TBLREC1 (IDX IDX1)
087600             MOVE    WRK-NAI-TBLREC1 (IDX3)
087700                                TO  SPA-NAI-TBLREC1 (IDX IDX1)
087800             MOVE    SPACE      TO  WRK-GMN-TBLREC1 (IDX3)
087900             MOVE    SPACE      TO  WRK-NAI-TBLREC1 (IDX3)
088000         END-IF
088100     END-PERFORM
088200     .
088300 310122-SORT-02-EXT.
088400     EXIT.
088500*
      *
      *****************************************************************
      *    当月点数合計計算処理
      *****************************************************************
       3109-TENSU-KEISAN-SEC                SECTION.
      *
           INITIALIZE                      TBL-HKNCOMBI-AREA
           MOVE    ZERO                TO  WRK-GOK-TENSU
           MOVE    ZERO                TO  WRK-GOK-RSITENSU
           MOVE    ZERO                TO  WRK-GOK-JIHIKIN
           MOVE    ZERO                TO  WRK-GOK-NYUTEN2
      *    診療会計マスターから編集
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SPA-GMN-NYUGAIKBN   TO  ACCT-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  ACCT-PTID
           MOVE    WRK-SRYYM           TO  ACCT-SRYYM
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 900-SRYACCT-NEXT-SEC
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1               TO  FLG-SRYACCT
           END-IF
           PERFORM
               UNTIL      (FLG-SRYACCT     =   1   )
               MOVE    ZERO                TO  FLG-OK
               IF      SPA-GMN-HHKNCOMBI   =   ZERO
      *            包括まとめ以外
                   IF      ACCT-HKNCOMBI       =   "9999"
                       CONTINUE
                   ELSE
                       MOVE    1               TO  FLG-OK
                   END-IF
               ELSE
                   IF      SPA-GMN-HHKNCOMBI   =   ACCT-HKNCOMBI
                       MOVE    1               TO  FLG-OK
                   END-IF
               END-IF
               IF     (FLG-OK              =   1   )  AND
                      (ACCT-ZAIKAISU       >   ZERO)
      *            保険チェック
                   PERFORM 31091-HKNCOMBI-CHK-SEC
      *            自費と労災の時、診療行為検索へ
                   IF     (ACCT-SRYKBN         =   "96"  OR  "95" ) OR
                         ((FLG-HKNKBN          =   1         )  AND
                          (ACCT-SRYKBN         =   "11"  OR   "12"
                                               OR  "13"  OR   "80" ))
      *!!!!!!!!!!!!!!  PERFORM 31092-SRYACCT-HEN-SEC
                       IF      ACCT-ZAIREQFLG      =   "1"
      *                    付加情報の労災金額判定
                           PERFORM 950-SRYACCTPLUS-READ-SEC
                           IF      ACCTP-RSIKINGAKU    >   ZERO
                               PERFORM 310921-RSIKINGAKU-HEN-SEC
                           ELSE
                               PERFORM 31092-SRYACCT-HEN-SEC
                           END-IF
                       END-IF
                   ELSE
                       MOVE    ZERO                TO  FLG-GENZAN
      *                減額判定
                       IF      ACCT-ZAIREQFLG      =   "1"
                           PERFORM 950-SRYACCTPLUS-READ-SEC
                           IF      ACCTP-PLUSKBN       =   1
                               MOVE    1               TO  FLG-GENZAN
                           END-IF
                       ELSE
      *
      *!診療会計に減額フラグを追加するまで
      *                内服の逓減かどうかを判定する
                       IF      ACCT-SRYKBN         =   "21"
                                                   OR  "90"
                                                   OR  "92"
                                                   OR  "97"
                           PERFORM 31092-SRYACCT-HEN-SEC
                       END-IF
      *!
                       END-IF
      *!
                       COMPUTE WRK-TENSU       =   ACCT-ZAITEN
                                               *   ACCT-ZAIKAISU
                       IF      ACCT-SRYKBN         =   "90"
                                                   OR  "92"
                                                   OR  "97"
                           IF      FLG-GENZAN          =   ZERO
                               COMPUTE WRK-GOK-NYUTEN2 =
                                                       WRK-GOK-NYUTEN2
                                                       +   WRK-TENSU
                           ELSE
                               COMPUTE WRK-GOK-NYUTEN2 =
                                                       WRK-GOK-NYUTEN2
                                                       -   WRK-TENSU
                           END-IF
                       ELSE
                           IF      FLG-GENZAN          =   ZERO
                               COMPUTE WRK-GOK-TENSU   =   WRK-GOK-TENSU
                                                       +   WRK-TENSU
                           ELSE
                               COMPUTE WRK-GOK-TENSU   =   WRK-GOK-TENSU
                                                       -   WRK-TENSU
                           END-IF
                       END-IF
                   END-IF
               END-IF
      *
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 900-SRYACCT-NEXT-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    WRK-GOK-TENSU       TO  SPA-GMN-TENSU
           MOVE    WRK-GOK-RSITENSU    TO  SPA-GMN-RSITENSU
           MOVE    WRK-GOK-JIHIKIN     TO  SPA-GMN-JIHIKIN
      *
      *    入院の時、入院料の点数合計
           IF      SPA-GMN-NYUGAIKBN   =   "1"
               PERFORM 31091-NYUACCT-TENSU-SEC
               ADD     WRK-GOK-NYUTEN2     TO  SPA-GMN-NYUTENSU
           END-IF
      *
           .
       3109-TENSU-KEISAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院料の点数合計処理
      *****************************************************************
       31091-NYUACCT-TENSU-SEC                SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-NYUTENSU
           MOVE    ZERO                TO  WRK-GOK-NYUTEN
           INITIALIZE                      TBL-NYUIN-TENSU-AREA
      *
           INITIALIZE                      NYUINACCT-REC
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    SPA-GMN-NYUGAIKBN   TO  NACCT-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  NACCT-PTID
           MOVE    "00"                TO  NACCT-SRYKA
           MOVE    WRK-SRYYM           TO  NACCT-SRYYM
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-NYUINACCT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-NYUINACCT
           END-IF
           MOVE    ZERO                TO  IDX-N
           PERFORM
               UNTIL  (FLG-NYUINACCT       =   1  )  OR
                      (IDX-N               >   100 )
               EVALUATE    NACCT-ZAISKBKBN
                   WHEN    "1"
                       ADD     1               TO  IDX-N
                       MOVE    NACCT-ZAITEN    TO  TBL-ZAITEN (IDX-N)
                       MOVE    NACCT-DAY-AREA  TO  TBL-DAY-G  (IDX-N)
                   WHEN    "2"
      *            外泊
                       MOVE    NACCT-SRYYM     TO  TBL-GAIHAKU-SRYYYMM
                       MOVE    NACCT-DAY-AREA  TO  TBL-GAIHAKU-G
      *            食事関係
                   WHEN    "4"
                       CONTINUE
                   WHEN    "5"
      *            保険
                       MOVE    NACCT-DAY-AREA  TO  TBL-HKNCOMBI-NYU-G
      *            以外
                   WHEN    OTHER
                       IF      NACCT-ZAITEN    >   ZERO
                           ADD     1               TO  IDX-N
      *                    栄養管理体制減算に該当する場合はマイナス
                           IF     (NACCT-MEISAISU   =    1)   AND
                                  (NACCT-SRYCDTOTAL =    190182590  OR
                                                         190182690)
                               COMPUTE  TBL-ZAITEN(IDX-N)  =
                                        NACCT-ZAITEN    *   -1
                           ELSE
                               MOVE  NACCT-ZAITEN  TO  TBL-ZAITEN(IDX-N)
                           END-IF
                           MOVE    NACCT-DAY-AREA  TO  TBL-DAY-G (IDX-N)
                       END-IF
               END-EVALUATE
      *
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-NYUINACCT-READ-SEC
           END-PERFORM
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    保険毎の計算
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL       IDX-D   >   31
               MOVE    TBL-HKNCOMBI-NYU (IDX-D)    TO  WRK-HKNCOMBI
               IF     (SPA-GMN-HHKNCOMBI   =   ZERO  )  OR
                      (SPA-GMN-HHKNCOMBI   =   WRK-HKNCOMBI)
                   PERFORM VARYING     IDX-N2  FROM    1   BY  1
                           UNTIL       IDX-N2  >   IDX-N
                       IF      TBL-DAY (IDX-N2 IDX-D)  >   ZERO
                           ADD     TBL-ZAITEN(IDX-N2)
                                                   TO  WRK-GOK-NYUTEN
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
           MOVE    WRK-GOK-NYUTEN      TO  SPA-GMN-NYUTENSU
      *
           .
       31091-NYUACCT-TENSU-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せチェック処理
      *****************************************************************
       31091-HKNCOMBI-CHK-SEC                SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   20
               IF      TBL-HKNCOMBI (IDX)  =   SPACE
                   MOVE    ACCT-HKNCOMBI       TO  TBL-HKNCOMBI (IDX)
                   PERFORM 310911-HKNCOMBI-HEN-SEC
                   MOVE    TBL-HKNKBN (IDX)    TO  FLG-HKNKBN
                   MOVE    20                  TO  IDX
               ELSE
                   IF      TBL-HKNCOMBI (IDX)  =   ACCT-HKNCOMBI
                       MOVE    TBL-HKNKBN (IDX)    TO  FLG-HKNKBN
                       MOVE    20              TO  IDX
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       31091-HKNCOMBI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ編集処理
      *****************************************************************
       310911-HKNCOMBI-HEN-SEC                SECTION.
      *
           INITIALIZE                  HKNCOMBI-REC
           MOVE    ACCT-HOSPNUM        TO  COMB-HOSPNUM
           MOVE    ACCT-PTID           TO  COMB-PTID
           MOVE    ACCT-HKNCOMBI       TO  COMB-HKNCOMBINUM
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
      *
      *    労災・自賠責チェック
           IF      COMB-HKNNUM         =   "971"  OR  "973"
               MOVE    1               TO  TBL-HKNKBN (IDX)
           ELSE
               MOVE    ZERO            TO  TBL-HKNKBN (IDX)
           END-IF
      *    第三者行為は、自賠責とする
           IF      COMB-KOH1HKNNUM     =   "970"
               MOVE    1               TO  TBL-HKNKBN (IDX)
           END-IF
           .
       310911-HKNCOMBI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   診療会計マスターより画面内容編集処理
      *****************************************************************
       31092-SRYACCT-HEN-SEC             SECTION.
      *
      *    診療行為マスター読み込み
           INITIALIZE                      SRYACT-REC
           MOVE    ACCT-HOSPNUM        TO  SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID           TO  SRY-PTID
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
      *    点数検索用日付編集
           MOVE    ACCT-SRYYM          TO  WRK-SYSYMD(1:6)
           MOVE    01                  TO  WRK-SYSDD
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL       IDX-D   >   31
               IF      ACCT-DAY (1 IDX-D)  NOT =   ZERO
                   MOVE    IDX-D           TO  WRK-SYSDD
                   MOVE    31              TO  IDX-D
               END-IF
           END-PERFORM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-SINKOUI-RAED-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACT
           END-IF
           MOVE    ZERO                TO  WRK-SUM-TEN
           MOVE    ZERO                TO  WRK-RSI-TEN
      *
           MOVE    ZERO                TO  FLG-GENZAN
           PERFORM UNTIL       FLG-SRYACT      =   1
      *!診療会計に減額フラグを追加するまで
      *        薬剤料逓減　を減算
               IF     (ACCT-SRYKBN         =   "21" )  AND
                      (SRY-SRYCD (01)      =   "630010002")
                   MOVE    1               TO  FLG-GENZAN
                   MOVE    1               TO  FLG-SRYACT
               END-IF
      *        自費の時、自費合計を集計
               IF      ACCT-SRYKBN         =   "96"  OR  "95"
                   COMPUTE WRK-GOK-JIHIKIN     =   WRK-GOK-JIHIKIN
                                               +  (SRY-JIHIMONEYTOTAL
                                               *   ACCT-ZAIKAISU )
                   MOVE    1                   TO  FLG-SRYACT
               ELSE
      *            剤明細編集
                   PERFORM VARYING     IDZ     FROM    1   BY  1
                           UNTIL      (IDZ         >   5   )  OR
                                      (SRY-SRYCD (IDZ) =   SPACE )  OR
                                      (FLG-SRYACT      =   1     )
      *!診療会計に減額フラグを追加するまで
      *                入院の減額
                       IF      ACCT-SRYKBN         =   "90" OR  "92"
                                                   OR  "97"
                           MOVE    SRY-SRYCD (IDZ)     TO  TNS-SRYCD
                           PERFORM 910-TENSU-READ-SEC
                           IF      TNS-TENSIKIBETU     =   7
                                                       OR  8
                               IF      TNS-TEN         =   ACCT-ZAITEN
                                    MOVE    1          TO  FLG-GENZAN
                                    MOVE    1          TO  FLG-SRYACT
                               END-IF
                           END-IF
                       END-IF
      *
      *                労災コードの時、点数取得
                       IF      (SRY-SRYCD (IDZ)(1:3)   =   "101"  )  OR
                               (SRY-SRYCD (IDZ)(1:5)   =   "09591" OR
                                                           "09592" OR
                                                           "09593" OR
                                                           "09594" )
                           MOVE    SRY-SRYCD (IDZ)     TO  TNS-SRYCD
                           PERFORM 910-TENSU-READ-SEC
      *                    労災の金額算定分の時、点数をゼロとする
                           IF     (TNS-TENSIKIBETU     =   1  )  OR
                                  (TNS-SRYCD (1:5)     =   "09591" OR
                                                           "09592" OR
                                                           "09593" OR
                                                           "09594" )
                               COMPUTE WRK-SUM-TEN     =   WRK-SUM-TEN
                                                       +  (TNS-TEN
                                                       /   10  )
                               ADD     TNS-TEN         TO  WRK-RSI-TEN
                           END-IF
                       END-IF
                   END-PERFORM
      *
                   MOVE    "tbl_sryact"        TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 930-SINKOUI-RAED-SEC
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    自費の時、自費合計を集計
           IF      ACCT-SRYKBN         =   "96"  OR  "95"
                                       OR  "21"
                                       OR  "90"  OR  "92"
                                       OR  "97"
               CONTINUE
           ELSE
      *
      *        労災の点数集計
               IF      WRK-SUM-TEN         <=  ACCT-ZAITEN
                   COMPUTE WRK-TENSU       =  (ACCT-ZAITEN
                                           -   WRK-SUM-TEN )
                                           *   ACCT-ZAIKAISU
                   COMPUTE WRK-GOK-TENSU   =   WRK-GOK-TENSU
                                           +   WRK-TENSU
               END-IF
               COMPUTE WRK-GOK-RSITENSU    =   WRK-GOK-RSITENSU
                                           +  (WRK-RSI-TEN
                                           *   ACCT-ZAIKAISU )
           END-IF
      *
           .
       31092-SRYACCT-HEN-EXT.
           EXIT.
      *
      *!!!
      *****************************************************************
      *   診療会計付加マスター労災金額編集処理
      *****************************************************************
       310921-RSIKINGAKU-HEN-SEC             SECTION.
      *
      *    労災金額集計
           COMPUTE WRK-GOK-RSITENSU    =   WRK-GOK-RSITENSU
                                       +  (ACCTP-RSIKINGAKU
                                       *   ACCT-ZAIKAISU )
      *    点数混在の時
           COMPUTE WRK-RSI-TENSU       =   ACCTP-RSIKINGAKU
                                       /   10
           IF      WRK-RSI-TENSU       <   ACCT-ZAITEN
               COMPUTE WRK-TENSU       =  (ACCT-ZAITEN
                                       -   WRK-RSI-TENSU )
                                       *   ACCT-ZAIKAISU
               COMPUTE WRK-GOK-TENSU   =   WRK-GOK-TENSU
                                       +   WRK-TENSU
           END-IF
           .
       310921-RSIKINGAKU-HEN-EXT.
           EXIT.
088600*****************************************************************
088700*    西暦和暦編集処理
088800*****************************************************************
088900 41012-SEIWA-HEN-SEC             SECTION.
089000*
089100     IF      WRK-SYMD            =   ALL "9"  OR   SPACE
089200         MOVE    SPACE               TO  WRK-HGO
089300         MOVE    WRK-SYMD(3:2)       TO  WRK-HYY
089400         MOVE    WRK-SYMD(5:2)       TO  WRK-HMM
089500         MOVE    WRK-SYMD(7:2)       TO  WRK-HDD
089600         MOVE    WRK-HENYMD          TO  WRK-HENYMDG
089700     ELSE
089800         INITIALIZE                      STS-AREA-DAY
089900         INITIALIZE                      LNK-DAY2-AREA
090000         MOVE    "21"                TO  LNK-DAY2-IRAI
090100         MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
090200         CALL    "ORCSDAY"           USING   STS-AREA-DAY
090300                                             LNK-DAY2-AREA
090400         MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
090500     END-IF
090600     .
090700 41012-SEIWA-HEN-EXT.
090800     EXIT.
090900*
091000*****************************************************************
091100*    画面編集処理
091200*****************************************************************
091300 500-GMNHEN-SEC                  SECTION.
      *
091400     MOVE    SPACE               TO  I42
091500*
091400     INITIALIZE                      I42
091600     MOVE    SPA-GMN-PTNUM       TO  I42-PTNUM
091700     MOVE    SPA-GMN-KANANAME    TO  I42-KANANAME
091800     MOVE    SPA-GMN-NAME        TO  I42-NAME
091900     MOVE    SPA-GMN-SEX         TO  I42-SEX
092000     MOVE    SPA-GMN-BIRTHDAYW   TO  I42-BIRTHDAY
           MOVE    SPA-GMN-SRYKA-G     TO  I42-SRYKA
      *    診療科
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX         >   SPA-GMN-SRYKA-MAX-41
               MOVE    SPA-GMN-SRYKA-LIST (IDX)
                                          TO  I42-SRYKA-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-SRYKA-MAX-41   TO  I42-SRYKA-COUNT
      *    入外区分
           MOVE    SPA-GMN-NYUGAIKBN-G TO  I42-NYUGAIKBN
           PERFORM VARYING   IDX     FROM    1   BY  1
                   UNTIL     IDX         >   SPA-NYUGAI-MAX
               MOVE    SPA-GMN-NYUGAIKBN-LST (IDX)
                                          TO  I42-NYUGAIKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-NYUGAI-MAX      TO  I42-NYUGAIKBN-COUNT
      *
      *    保険組合せ
           MOVE    SPA-GMN-HHKNCOMBI-G TO  I42-HHKNCOMBI
           PERFORM VARYING   IDX       FROM    1   BY  1
                   UNTIL     IDX       >   SPA-HKN-MAX
               MOVE    SPA-GMN-THKNCOMBINUM-G (IDX)
                                           TO  I42-HHKNCOMBI-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-HKN-MAX         TO  I42-HHKNCOMBI-COUNT
      *
           IF      SPA-GMN-SYORI       =   ZERO
               MOVE   1                TO  SPA-GMN-SYORI
           END-IF
      *
           MOVE    SPA-GMN-SRYYM       TO  I42-SRYYM
           MOVE    SPA-GMN-TENSU       TO  WRK-Z8
           MOVE    WRK-Z8              TO  I42-TENSU
           MOVE    SPA-GMN-RSITENSU    TO  WRK-Z8
           MOVE    WRK-Z8              TO  I42-RSITENSU
           MOVE    SPA-GMN-JIHIKIN     TO  WRK-Z8
           MOVE    WRK-Z8              TO  I42-JIHIKIN
      *
           IF      SPA-GMN-NYUGAIKBN   =   "1"
               MOVE    "入院点数"          TO  I42-MIDASIN
           END-IF
           IF      SPA-GMN-NYUTENSU    <   ZERO
               MOVE    SPA-GMN-NYUTENSU    TO  WRK-Z82
               MOVE    WRK-Z82             TO  I42-NYUTENSU
           ELSE
               MOVE    SPA-GMN-NYUTENSU    TO  WRK-Z8
               MOVE    WRK-Z8              TO  I42-NYUTENSU
           END-IF
      *
           EVALUATE    SPA-GMN-SYORI
               WHEN    1
                   IF      SPA-GMN-NYUGAIKBN   =   "2"
                       MOVE    "＜診察＞"           TO  I42-MIDASI-VALUE
                   ELSE
                       MOVE    "＜診察・入院料＞"   TO  I42-MIDASI-VALUE
                   END-IF
               WHEN    2
                   MOVE    "＜処方＞"           TO  I42-MIDASI-VALUE
               WHEN    3
                   MOVE    "＜注射＞"           TO  I42-MIDASI-VALUE
               WHEN    4
                   MOVE    "＜処置＞"           TO  I42-MIDASI-VALUE
               WHEN    5
                   MOVE    "＜手術・麻酔＞"     TO  I42-MIDASI-VALUE
               WHEN    6
                   MOVE    "＜検査＞"           TO  I42-MIDASI-VALUE
               WHEN    7
                   MOVE    "＜画像診断＞"       TO  I42-MIDASI-VALUE
               WHEN    8
                   MOVE    "＜リハ・他＞"       TO  I42-MIDASI-VALUE
           END-EVALUATE
           MOVE    "red"               TO  I42-MIDASI-STYLE
      *
092300     MOVE    SPA-GMN-SYORI       TO  IDX
           MOVE    ZERO                TO  I42-SRYACTYMD-COUNT
092400     PERFORM   VARYING   IDY     FROM    1   BY  1
092500               UNTIL     IDY         >   100
092600         IF     (IDY             =   1   )   OR
092700                (SPA-GMN-TSRYYMD (IDX IDY)
092800                      NOT =   SPA-GMN-TSRYYMD (IDX IDY - 1) )
092900             MOVE    SPA-GMN-TSRYYMD (IDX IDY)   
093000                                 TO  I42-SRYACTYMD-YMD (IDY)
093100         END-IF
093200         MOVE    SPA-GMN-TNAIYOU1(IDX IDY)
093300                                 TO  I42-SRYACTYMD-NAME(IDY)
               IF      SPA-GMN-TNAIYOU1 (IDX IDY) NOT =   SPACE
                   MOVE    IDY         TO  I42-SRYACTYMD-COUNT
               END-IF
           END-PERFORM
      *
           MOVE    ZERO                TO  I42-SRYACTCNT-COUNT
           PERFORM   VARYING   IDY     FROM    1   BY  1
                     UNTIL     IDY         >   100
               IF     (IDY             =   1   )   OR
                      (SPA-GMN-TSRYYYMM (IDX IDY)
                                   NOT =   SPA-GMN-TSRYYYMM
                                                   (IDX IDY - 1) )
                   MOVE    SPA-GMN-TSRYYYMM(IDX IDY)   
                                       TO  I42-SRYACTCNT-YM(IDY)
               END-IF
               MOVE    SPA-GMN-TNAIYOU2(IDX IDY)   
                                       TO  I42-SRYACTCNT-NAME(IDY)
               MOVE    SPA-GMN-TKAISU2 (IDX IDY)
                                       TO  WRK-ZZZ
               MOVE    WRK-ZZZ         TO  I42-SRYACTCNT-CNT (IDY)
               IF      SPA-GMN-TNAIYOU2(IDX IDY) NOT =  SPACE
                   MOVE    IDY         TO  I42-SRYACTCNT-COUNT
               END-IF   
           END-PERFORM
      *
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
      *    カーソルセット
           EVALUATE    SPA-GMN-CUR
               WHEN    01
                   MOVE    "B01"           TO  MCP-WIDGET
               WHEN    02
                   MOVE    "SRYKA"         TO  MCP-WIDGET
               WHEN    03
                   MOVE    "NYUGAIKBN"     TO  MCP-WIDGET
               WHEN    04
                   MOVE    "HHKNCOMBI"     TO  MCP-WIDGET
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
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *
      *    前月
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 450-MAETUKI-SEC
      *    次月
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 480-ATOTUKI-SEC
      *    ←
               WHEN    "CLICKED"       ALSO    "B05"
                   PERFORM 450-MAE-SYORI-SEC
      *    →
               WHEN    "CLICKED"       ALSO    "B08"
                   PERFORM 470-ATO-SYORI-SEC
      *        （診察）
               WHEN    "CLICKED"       ALSO    "B001"
                   PERFORM 220-SINS
      *        （処方）
               WHEN    "CLICKED"       ALSO    "B002"
                   PERFORM 230-SYOH
      *        （注射）
               WHEN    "CLICKED"       ALSO    "B007"
                   PERFORM 220-CHUSHA-SEC
      *        （処置）
               WHEN    "CLICKED"       ALSO    "B003"
                   PERFORM 240-SYOC
      *        （手術・麻酔）
               WHEN    "CLICKED"       ALSO    "B008"
                   PERFORM 240-SYUJU-SEC
      *        （検査）
               WHEN    "CLICKED"       ALSO    "B004"
                   PERFORM 250-KENS
      *        （Ｘ線）
               WHEN    "CLICKED"       ALSO    "B005"
                   PERFORM 260-XSEN
      *        （リハビリ）
               WHEN    "CLICKED"       ALSO    "B006"
                   PERFORM 270-RIHA
           END-EVALUATE
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        診療科入力
               WHEN    "ACTIVATE"      ALSO    "SRYKA"
                   PERFORM 41013-SRYKAI-SYORI-SEC
      *        入外区分
               WHEN    "ACTIVATE"      ALSO    "NYUGAIKBN"
                   PERFORM 41017-NYUGAIKBN-CHK-SEC
      *        保険組合せ
               WHEN    "ACTIVATE"      ALSO    "HHKNCOMBI"
                   PERFORM 41018-HKNCOMBI-CHK-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科入力処理
      *****************************************************************
       41013-SRYKAI-SYORI-SEC         SECTION.
      *
           MOVE    2                   TO  SPA-GMN-CUR
      *
           MOVE    I42-SRYKA           TO  SPA-GMN-SRYKA-G
      *    診療科選択
           PERFORM 3101-SRYKA-HEN-SEC
      *    診療科選択
           IF      FLG-CHK             =   ZERO
               MOVE    "0001"          TO  SPA-ERRCD
               GO      TO      41013-SRYKAI-SYORI-EXT
           END-IF
      *
      *    画面編集用内容編集処理
           PERFORM 3100-GMNSET-SYORI-SEC
      *
           MOVE    1                   TO  SPA-GMN-SYORI
           MOVE    1                   TO  SPA-GMN-CUR
           .
       41013-SRYKAI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入外区分処理
      *****************************************************************
       41017-NYUGAIKBN-CHK-SEC             SECTION.
      *
           MOVE    I42-NYUGAIKBN       TO  SPA-GMN-NYUGAIKBN-G
      *
           MOVE    ZERO                TO  FLG-CHK
      *
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >   SPA-NYUGAI-MAX
              IF      SPA-GMN-NYUGAIKBN-LST (IDX)(1:1)
                                           =   SPA-GMN-NYUGAIKBN
                   MOVE    SPA-GMN-NYUGAIKBN-LST (IDX)
                                               TO  SPA-GMN-NYUGAIKBN-G
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
      *
           IF      FLG-CHK             =   ZERO
               MOVE    3                   TO  SPA-GMN-CUR
               MOVE    "0002"              TO  SPA-ERRCD
               GO      TO      41017-NYUGAIKBN-CHK-EXT
           END-IF
      *
      *    画面編集用内容編集処理
           PERFORM 3100-GMNSET-SYORI-SEC
      *
           MOVE    1                   TO  SPA-GMN-SYORI
           MOVE    1                   TO  SPA-GMN-CUR
      *
           .
       41017-NYUGAIKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ処理
      *****************************************************************
       41018-HKNCOMBI-CHK-SEC             SECTION.
      *
           MOVE    I42-HHKNCOMBI       TO  SPA-GMN-HHKNCOMBI-G
      *
           MOVE    ZERO                TO  FLG-CHK
      *
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >   SPA-HKN-MAX
              IF      SPA-GMN-THKNCOMBINUM(IDX)
                                           =   SPA-GMN-HHKNCOMBI
                   MOVE    SPA-GMN-THKNCOMBINUM-G (IDX)
                                               TO  SPA-GMN-HHKNCOMBI-G
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
      *
           IF      FLG-CHK             =   ZERO
               MOVE    4                   TO  SPA-GMN-CUR
               MOVE    "0003"              TO  SPA-ERRCD
               GO      TO      41018-HKNCOMBI-CHK-EXT
           END-IF
      *
      *    画面編集用内容編集処理
           PERFORM 3100-GMNSET-SYORI-SEC
      *
           MOVE    1                   TO  SPA-GMN-SYORI
           MOVE    1                   TO  SPA-GMN-CUR
      *
           .
       41018-HKNCOMBI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "I41"               TO  SPA-SAKIPG
           MOVE    "I42"               TO  SPA-MOTOPG
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
100200*
100300     MOVE    1                   TO  FLG-END
100400*
100500     .
100600 210-BACK-EXT.
100700     EXIT.
100800*****************************************************************
100900*    診察　処理
101000*****************************************************************
101100 220-SINS                    SECTION.
101200*
101300     MOVE    1             TO  SPA-GMN-SYORI
102300     .
102400 220-SINS-EXT.
102500     EXIT.
100800*****************************************************************
100900*    注射　処理
101000*****************************************************************
101100 220-CHUSHA-SEC              SECTION.
101200*
101300     MOVE    3             TO  SPA-GMN-SYORI
101400*
102300     .
102400 220-CHUSHA-EXT.
102500     EXIT.
102600*****************************************************************
102700*    処方　処理
102800*****************************************************************
102900 230-SYOH                    SECTION.
103000*
103100     MOVE    2             TO  SPA-GMN-SYORI
103200*
104100     .
104200 230-SYOH-EXT.
104300     EXIT.
      *****************************************************************
      *    処置　処理
      *****************************************************************
       240-SYOC                    SECTION.
      *
           MOVE    4             TO  SPA-GMN-SYORI
      *
           .
       240-SYOC-EXT.
           EXIT.
      *****************************************************************
      *    手術・麻酔処理
      *****************************************************************
       240-SYUJU-SEC                  SECTION.
      *
           MOVE    5             TO  SPA-GMN-SYORI
      *
           .
       240-SYUJU-EXT.
           EXIT.
106200*****************************************************************
106300*    検査　処理
106400*****************************************************************
106500 250-KENS                    SECTION.
106600*
106700     MOVE    6             TO  SPA-GMN-SYORI
106800*
107700     .
107800 250-KENS-EXT.
107900     EXIT.
108000*****************************************************************
108100*    Ｘ線　処理
108200*****************************************************************
108300 260-XSEN                    SECTION.
108400*
108500     MOVE    7             TO  SPA-GMN-SYORI
108600*
109500     .
109600 260-XSEN-EXT.
109700     EXIT.
109800*****************************************************************
109900*    リハビリ　処理
110000*****************************************************************
110100 270-RIHA                    SECTION.
110200*
110300     MOVE    8             TO  SPA-GMN-SYORI
110400*
111300     .
111400 270-RIHA-EXT.
111500     EXIT.
      *
      *****************************************************************
      *    前月選択　処理
      *****************************************************************
       450-MAETUKI-SEC             SECTION.
      *
           COMPUTE SPA-NAI-SRYMM       =   SPA-NAI-SRYMM
                                       -   1
           IF      SPA-NAI-SRYMM       =   ZERO
               MOVE    12                  TO  SPA-NAI-SRYMM
               COMPUTE SPA-NAI-SRYYY       =   SPA-NAI-SRYYY    -   1
           END-IF
      *
      *    保険一覧編集
           PERFORM 3008-HKNCOMBI-HEN-SEC
      *    画面編集用内容編集処理
           PERFORM 3100-GMNSET-SYORI-SEC
      *
           MOVE    1                   TO  SPA-GMN-SYORI
           MOVE    1                   TO  SPA-GMN-CUR
           .
       450-MAETUKI-EXT.
           EXIT.
      *****************************************************************
      *    次月選択　処理
      *****************************************************************
       480-ATOTUKI-SEC             SECTION.
      *
           COMPUTE SPA-NAI-SRYMM       =   SPA-NAI-SRYMM
                                       +   1
           IF      SPA-NAI-SRYMM       >   12
               MOVE    1                   TO  SPA-NAI-SRYMM
               COMPUTE SPA-NAI-SRYYY       =   SPA-NAI-SRYYY    +   1
           END-IF
      *
      *    保険一覧編集
           PERFORM 3008-HKNCOMBI-HEN-SEC
      *    画面編集用内容編集処理
           PERFORM 3100-GMNSET-SYORI-SEC
      *
           MOVE    1                   TO  SPA-GMN-SYORI
           MOVE    1                   TO  SPA-GMN-CUR
           .
       480-ATOTUKI-EXT.
           EXIT.
      *****************************************************************
      *    前選択　処理
      *****************************************************************
       450-MAE-SYORI-SEC             SECTION.
      *
           COMPUTE SPA-GMN-SYORI       =   SPA-GMN-SYORI
                                       -   1
           IF      SPA-GMN-SYORI       =   ZERO
               MOVE    8                   TO  SPA-GMN-SYORI
           END-IF
      *
           .
       450-MAE-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    次選択　処理
      *****************************************************************
       470-ATO-SYORI-SEC             SECTION.
      *
           COMPUTE SPA-GMN-SYORI       =   SPA-GMN-SYORI
                                       +   1
           IF      SPA-GMN-SYORI       >   8
               MOVE    1                   TO  SPA-GMN-SYORI
           END-IF
      *
           .
       470-ATO-SYORI-EXT.
           EXIT.
111600*****************************************************************
111700*    自画面編集処理
111800*****************************************************************
111900 500-SET-SCREEN              SECTION.
112000*
112100     PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
112400*
112500     MOVE    "CURRENT"           TO  MCP-PUTTYPE.
112600     MOVE    "I42    "           TO  MCP-WINDOW.
112700*
112800     PERFORM 900-PUT-WINDOW.
112900     .
113000 500-SET-SCREEN-EXT.
113100     EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
           MOVE    SPACE               TO  WRK-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "診療科入力エラー"
                                       TO  WRK-ERRMSG
               WHEN    "0002"
                   MOVE    "入外区分入力エラー"
                                       TO  WRK-ERRMSG
               WHEN    "0003"
                   MOVE    "保険組合せエラー"
                                       TO  WRK-ERRMSG
               WHEN     OTHER 
                   MOVE    SPA-ERRCD
                                       TO  WRK-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  I4ERR
           MOVE    SPA-ERRCD           TO  I4ERR-ERRCODE
           MOVE    WRK-ERRMSG          TO  I4ERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "I42"               TO  SPA-MOTOPG
           MOVE    "I4ERR"             TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "I4ERR"             TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    WRK-SYSYMD          TO  TNS-YUKOSTYMD
           MOVE    WRK-SYSYMD          TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       910-TENSU-READ-EXT.
           EXIT.
      *
119100*****************************************************************
119200*    診療会計マスタＦＥＴＣＨ読込
119300*****************************************************************
119400 900-SRYACCT-NEXT-SEC         SECTION.
119500*
           PERFORM 920-DBFETCH-SEC
120000     IF      MCP-RC              =   ZERO
120100         MOVE    MCPDATA-REC         TO  SRYACCT-REC
120200         MOVE    ZERO                TO  FLG-SRYACCT
120300     ELSE
120400         INITIALIZE                      SRYACCT-REC
120500         MOVE    1                   TO  FLG-SRYACCT
120600     END-IF
120700*
120800     .
120900 900-SRYACCT-NEXT-EXT.
121000     EXIT.
121100*****************************************************************
121200*    診療行為マスター読込
121300*****************************************************************
121400 930-SINKOUI-RAED-SEC        SECTION.
121500*
           PERFORM 920-DBFETCH-SEC
122000     IF      MCP-RC              =   ZERO
122100         MOVE    MCPDATA-REC         TO  SRYACT-REC
122200         MOVE    ZERO                TO  FLG-SRYACT
122300     ELSE
122400         INITIALIZE                      SRYACT-REC
122500         MOVE    1                   TO  FLG-SRYACT
122600     END-IF
122700*
122800     .
122900 930-SINKOUI-RAED-EXT.
123000     EXIT.
      *
      *****************************************************************
      *    入院行為マスター読込
      *****************************************************************
       930-NYUINACCT-READ-SEC        SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  NYUINACCT-REC
               MOVE    ZERO                TO  FLG-NYUINACCT
           ELSE
               INITIALIZE                      NYUINACCT-REC
               MOVE    1                   TO  FLG-NYUINACCT
           END-IF
           .
      *
       930-NYUINACCT-READ-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    入院行為マスター読込
      *****************************************************************
       940-NYUINACT-RAED-SEC        SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  NYUINACT-REC
               MOVE    ZERO                TO  FLG-NYUINACT
           ELSE
               INITIALIZE                      NYUINACT-REC
               MOVE    1                   TO  FLG-NYUINACT
           END-IF.
      *
       940-NYUINACT-RAED-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せマスター読み込み
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
               MOVE    ZERO            TO  FLG-HKNCOMBI
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスター読込（主キー）
      *****************************************************************
       950-SRYACCTPLUS-READ-SEC           SECTION.
      *
           MOVE    SPACE               TO  SRYACCTPLUS-REC
           INITIALIZE                  SRYACCTPLUS-REC
           MOVE    ACCT-HOSPNUM        TO  ACCTP-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  ACCTP-NYUGAIKBN
           MOVE    ACCT-PTID           TO  ACCTP-PTID
           MOVE    ACCT-SRYKA          TO  ACCTP-SRYKA
           MOVE    ACCT-SRYYM          TO  ACCTP-SRYYM
           MOVE    ACCT-ZAINUM         TO  ACCTP-ZAINUM
           MOVE    1                   TO  ACCTP-RENNUM
      *
           MOVE    SRYACCTPLUS-REC     TO  MCPDATA-REC
           MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacctplus"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  SRYACCTPLUS-REC
                   MOVE    ZERO                TO  FLG-SRYACCTPLUS
               ELSE
                   MOVE    1                   TO  FLG-SRYACCTPLUS
                   INITIALIZE                  SRYACCTPLUS-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SRYACCTPLUS
               INITIALIZE                  SRYACCTPLUS-REC
           END-IF
      *
           MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       950-SRYACCTPLUS-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *
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
      *    ＤＢクローズ処理
      *****************************************************************
       990-DBCLOSE-SEC                SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       990-DBCLOSE-EXT.
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
      *
