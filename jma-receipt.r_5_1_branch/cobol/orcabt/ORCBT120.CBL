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
       PROGRAM-ID.             ORCBT120.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 日次統計
      *  コンポーネント名  : 日次統計データ用基本情報作成（入院）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/03/31    NACL−小豆沢  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *
      *  04.05.01    NACL-藤原    12/07/02  住所欄に番地方書を追加
      *
      *  04.08.01    NACL-藤原    14/07/07  一時ディレクトリ対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    日次統計データ
           SELECT  TOUKEI04-FILE   ASSIGN  TOUKEI04PARA
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   FILE    STATUS  IS  STS-TOUKEI04.
      *
      *    エラーファイル
           SELECT  TOUKEIERR-FILE  ASSIGN  TOUKEIERR
                                   FILE    STATUS  IS  STS-TOUKEIERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    日次統計データ
       FD  TOUKEI04-FILE.
       01  TOUKEI04-REC. 
           COPY    "CPTCF004.INC".
      *
      *    エラーファイル
       FD  TOUKEIERR-FILE.
       01  TOUKEIERR-REC           PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *
           COPY    "CPCOMMONDAT5.INC"
                                   REPLACING  //TEIKI01PARA//
                                   BY         //TOUKEI04PARA//.
      *
           COPY    "CPRECEERR.INC" REPLACING  //RECEERR//
                                   BY         //TOUKEIERR//.
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-TOUKEI04                    PIC X(02).
           03  STS-TOUKEIERR                   PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                         PIC 9(01).
           03  FLG-SYSKANRI                    PIC 9(01).
           03  FLG-HKNCOMBI                    PIC 9(01). 
           03  FLG-HKNNUM                      PIC 9(01). 
           03  FLG-TOUKEI04                    PIC 9(01).
           03  FLG-PTHKNINF                    PIC 9(01).
           03  FLG-PTKOHINF                    PIC 9(01).
           03  FLG-PTINF                       PIC 9(01).
           03  FLG-KYUSEIRRK                   PIC 9(01).
           03  FLG-MONTHLYNUM                  PIC 9(01).
           03  FLG-SRYACT                      PIC 9(01).
           03  FLG-TENSU                       PIC 9(01).
           03  FLG-SYUNOU                      PIC 9(01).
           03  FLG-PTRSIINF                    PIC 9(01).
           03  FLG-ADRS                        PIC 9(01).
           03  FLG-JYURRK                      PIC 9(01).
           03  FLG-PTNYUINRRK                  PIC 9(01).
      * 
           03  FLG-SYORI                       PIC 9(01).
           03  FLG-CHK-END                     PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                          PIC 9(06).
           03  CNT-TOUKEI04                    PIC 9(06).
      *
       01  INDEX-AREA.
           03  IDX                             PIC 9(03). 
           03  IDX1                            PIC 9(03). 
           03  IDX2                            PIC 9(03). 
           03  IDX3                            PIC 9(03). 
           03  IDX4                            PIC 9(03). 
           03  IDX5                            PIC 9(03). 
           03  IDY                             PIC 9(03). 
           03  IDZ                             PIC 9(03).
           03  IDXX                            PIC 9(03).
           03  KOH-IDX                         PIC 9(03).
      *
      *
      *    一時領域
      *
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-TERMID             PIC X(32).
               05  WRK-PARA-HOSPID             PIC X(24).
               05  WRK-PARA-PREFNUM            PIC X(02). 
               05  WRK-PARA-SRYYMD             PIC X(08).
               05  WRK-PARA-NYUGAIKBN          PIC X(01).
               05  WRK-PARA-DATAKBN            PIC X(01).
               05  WRK-PARA-HOUKATUKBN         PIC X(01).
               05  WRK-PARA-JOBID              PIC 9(07).
               05  WRK-PARA-SHELLID            PIC X(08).
               05  WRK-PARA-HOSPNUM            PIC 9(02).
      *
           03  WRK-TOUKEIERR               PIC X(200). 
      *
           03  WRK-MCP-TABLE               PIC X(64).
           03  WRK-MCP-PATHNAME            PIC X(64).
      *
           03  WRK-ERR-AREA.
               05  WRK-UPDKBN              PIC X(01).
               05  WRK-TEISEIKBN           PIC X(01).
      * 
           03  WRK-YMD.
               05  WRK-YY              PIC 9(04).
               05  WRK-MM              PIC 9(02).
               05  WRK-DD              PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
      *    患者毎
           03  WRK-HENSYU-AREA.
               05  WRK-KIHONKBN            PIC 9(01).
               05  WRK-JIKANKBN            PIC 9(01).
               05  WRK-SYOKAIKBN           PIC 9(01).
      *
           03  WRK-OLD-PTID                PIC 9(10).
           03  WRK-NEW-PTID                PIC 9(10).
      *
           COPY    "MCPAREA".
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
      *    医療機関情報
           COPY  "CPSK1001.INC".
      *
      *    患者番号構成管理情報
           COPY    "CPSK1009.INC".
      *     
      *    県内扱い保険者情報
           COPY    "CPSK1033.INC".
      *
      *    診療科情報
           COPY    "CPSK1005.INC".
      *     
           COPY    "CPSK2005.INC".
      *
      *    患者登録機能制御情報
           COPY  "CPSK1017.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    診療行為
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者保険情報
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    患者公費情報
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    保険情報
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    収納情報
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    点数
           COPY    "CPTENSU.INC".
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *    住所マスタ
       01  ADRS-REC.
           COPY    "CPADRS.INC".
      *
      *    入院会計
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    入院会計編集サブ
           COPY    "CPORCHCM34S01.INC".
      *
      *    公費付加情報取得サブ
           COPY    "CPORCSKOHPLUS.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *   保険算定用年齢・割合計算サブ
           COPY    "CPORCSAGECHK.INC".
      *    日次統計用入院情報取得サブ
           COPY    "CPORCBT120SUB.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "COMMON-SPA".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(1000).
      ****************************************************************
       PROCEDURE           DIVISION
               USING
           COMMAND-PARAM.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC    UNTIL   FLG-END =   1 
      *
           PERFORM 300-TERM-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
                                       SPA-AREA
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA-TERMID
                                               WRK-PARA-HOSPID
                                               WRK-PARA-PREFNUM 
                                               WRK-PARA-SRYYMD
                                               WRK-PARA-NYUGAIKBN
                                               WRK-PARA-DATAKBN
                                               WRK-PARA-HOUKATUKBN 
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-HOSPNUM
                                               TOUKEIERR
           END-UNSTRING 
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *     
           MOVE    "TOKEI110"          TO  TOUKEI04PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  TOUKEI04PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  TOUKEI04PARA-HOSPNUM
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    TOUKEIERR           TO  SGETTEMP-BASENAMES  (1)
           MOVE    TOUKEI04PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (2)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SPACE               TO  TOUKEIERR
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  TOUKEIERR
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  TOUKEI04PARA-FULLNAME
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    "ORCBT120"      TO  JOB-PGID
           MOVE    "日次統計データ作成（入院）"
                                   TO  JOB-SHELLMSG
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
      *
      *    診療年月編集
           MOVE    WRK-PARA-SRYYMD     TO  WRK-SYMD
      *
      *    医療機関ＩＤ編集
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    WRK-SYMD            TO  SYS-1001-STYUKYMD
                                           SYS-1001-EDYUKYMD 
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
           ELSE
               MOVE    "医療機関情報が取得できませんでした。"
                                           TO  WRK-TOUKEIERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
      *    患者番号構成管理情報編集
           INITIALIZE                  SYS-1009-REC
           MOVE    "1009"              TO  SYS-1009-KANRICD
           MOVE    "*"                 TO  SYS-1009-KBNCD
           MOVE    WRK-SYMD            TO  SYS-1009-STYUKYMD
                                           SYS-1009-EDYUKYMD 
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-1009-HOSPNUM
           MOVE    SYS-1009-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1009-REC
               MOVE    SYS-1009-PTNUMKSIKBN
                                       TO  SPA-1009-PTNUMKSIKBN
               MOVE    SYS-1009-HJNKSIKBN  
                                       TO  SPA-1009-HJNKSIKBN
               MOVE    SYS-1009-HJNKSINENKBN 
                                       TO  SPA-1009-HJNKSINENKBN
               MOVE    SYS-1009-HJNKSIRENNUMKBN
                                       TO  SPA-1009-HJNKSIRENNUMKBN
               MOVE    SYS-1009-HJNKSIRENNUMKETA
                                       TO  SPA-1009-HJNKSIRENNUMKETA
           ELSE
               MOVE    "患者番号構成管理情報が取得できませんでした"
                                           TO  WRK-TOUKEIERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           OPEN    OUTPUT              TOUKEI04-FILE
           CLOSE                       TOUKEI04-FILE
      *
           OPEN    OUTPUT              TOUKEI04-FILE
           .
       100-INIT-EXT.
           EXIT.
      * 
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    入院履歴読み込み
           INITIALIZE                           PTNYUINRRK-REC
           MOVE    WRK-PARA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    WRK-PARA-SRYYMD          TO  PTNYUINRRK-TENNYUYMD
                                                PTNYUINRRK-TENSTUYMD
           MOVE    PTNYUINRRK-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"         TO  MCP-TABLE
           MOVE    "key40"                  TO  MCP-PATHNAME
           PERFORM 900-PTNYUINRRK-SELECT-SEC
           IF      FLG-PTNYUINRRK            =   ZERO
               MOVE    "tbl_ptnyuinrrk"         TO  MCP-TABLE
               MOVE    "key40"                  TO  MCP-PATHNAME
               PERFORM   900-PTNYUINRRK-READ-SEC
               MOVE      PTNYUINRRK-PTID    TO  WRK-NEW-PTID
               PERFORM   UNTIL   FLG-PTNYUINRRK    =   1
                   IF     PTNYUINRRK-RRKEDANUM    =   1
                          PERFORM   2001-HENSYU-SEC
                   END-IF
                   MOVE    "tbl_ptnyuinrrk"         TO  MCP-TABLE
                   MOVE    "key40"                  TO  MCP-PATHNAME
                   PERFORM   900-PTNYUINRRK-READ-SEC
                   MOVE      PTNYUINRRK-PTID    TO  WRK-NEW-PTID
               END-PERFORM
           END-IF
           MOVE    "tbl_ptnyuinrrk"         TO  MCP-TABLE
           MOVE    "key40"                  TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           MOVE    1                   TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT. 
      *
      *****************************************************************
      *    編集処理
      *****************************************************************
       2001-HENSYU-SEC              SECTION.
      *
      *    テスト患者のときは読み飛ばす
           INITIALIZE                      PTINF-REC
           MOVE    PTNYUINRRK-HOSPNUM  TO  PTINF-HOSPNUM
           MOVE    PTNYUINRRK-PTID     TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
      *
           DISPLAY "PTNYUINRRK PTID=" PTNYUINRRK-PTID 
                   " TSTPTNUMKBN=" PTINF-TSTPTNUMKBN
      *
      *    テスト患者の時読み飛ばし
           IF     FLG-PTINF           =   ZERO
               IF   PTINF-TSTPTNUMKBN   =   "1"
                   CONTINUE
               ELSE
                  INITIALIZE              ORCBT120SUB-AREA
                 MOVE  PTNYUINRRK-HOSPNUM
                                     TO  LNK-ORCBT120SUB-HOSPNUM
                  MOVE  PTNYUINRRK-PTID
                                     TO  LNK-ORCBT120SUB-PTID
                  MOVE  WRK-PARA-SRYYMD
                                     TO  LNK-ORCBT120SUB-SANTEIYMD
                  MOVE    "00"       TO  LNK-ORCBT120SUB-SRYKA
                  MOVE  PTNYUINRRK-RRKNUM
                                     TO  LNK-ORCBT120SUB-RRKNUM
                  CALL    "ORCBT120SUB"  USING
                                         ORCBT120SUB-AREA
                                         SPA-AREA
                  PERFORM   VARYING   IDX   FROM  1  BY  1
                   UNTIL    (IDX   >  10)   OR
                            (LNK-ORCBT120SUB-HKNCOMBINUM(IDX) = ZERO)
                       PERFORM   20011-MEISAI-HENSYU-SEC
                       PERFORM   2002-OUT-SYORI-SEC
                  END-PERFORM
               END-IF
           END-IF
      *
           .
       2001-HENSYU-EXT.
           EXIT. 
      *
      *****************************************************************
      *    統計データの編集処理
      *****************************************************************
       20011-MEISAI-HENSYU-SEC              SECTION.
      *
           INITIALIZE                      TOUKEI04-REC
      *    医療機関ＩＤ
           MOVE    WRK-PARA-HOSPID     TO  TOUKEI04-HOSPID
      *    医療機関コード
           MOVE    SYS-1001-HOSPCD     TO  TOUKEI04-HOSPCD
      *    診療年月日
           MOVE    WRK-PARA-SRYYMD     TO  TOUKEI04-SRYYMD
      *    レコード識別
           IF     IDX      =      1
               MOVE   1                TO  TOUKEI04-RECSKB
           ELSE
               MOVE   2                TO  TOUKEI04-RECSKB
           END-IF
      *    レセプト種別
           MOVE    LNK-ORCBT120SUB-RECESYUBETU(IDX)
                                       TO  TOUKEI04-RECESYUBETU
      *    入外区分
           MOVE       1                TO  TOUKEI04-NYUGAIKBN
      *    診療科
           MOVE       LNK-ORCBT120SUB-NYUINKA
                                       TO  TOUKEI04-SRYKA
      *    レセ電診療科
           MOVE       LNK-ORCBT120SUB-RECENYUINKA
                                       TO  TOUKEI04-RECEKA
      *    保険組合せ番号
           MOVE       LNK-ORCBT120SUB-HKNCOMBINUM(IDX)
                                       TO  TOUKEI04-HKNCOMBINUM
      *    患者番号
           INITIALIZE                      ORCSPTIDAREA
           MOVE    PTNYUINRRK-HOSPNUM  TO  SPTID-HOSPNUM
           MOVE    PTNYUINRRK-PTID     TO  SPTID-PTID
           CALL    "ORCSPTID"      USING   ORCSPTIDAREA
                                           SPA-AREA
           MOVE    SPTID-PTNUM         TO  TOUKEI04-PTNUM
           MOVE    PTNYUINRRK-PTID     TO  TOUKEI04-PTID
      *
      *患者情報
           MOVE    PTINF-KANANAME      TO  TOUKEI04-KANANAME
           MOVE    PTINF-NAME          TO  TOUKEI04-NAME
           MOVE    PTINF-SEX           TO  TOUKEI04-SEX
           MOVE    PTINF-BIRTHDAY      TO  TOUKEI04-BIRTHDAY
           MOVE    PTINF-HOME-POST     TO  TOUKEI04-HOME-POST
           STRING  PTINF-HOME-ADRS     DELIMITED   BY  SPACE
                   PTINF-HOME-BANTI    DELIMITED   BY  SPACE
                                       INTO        TOUKEI04-HOME-ADRS
           END-STRING
           MOVE    PTINF-HOME-TEL1     TO  TOUKEI04-HOME-TEL
           INITIALIZE                  ADRS-REC
           MOVE    PTINF-HOME-POST TO  ADRS-POST
grpsys     MOVE    SPA-HOSPNUM     TO  ADRS-HOSPNUM
           MOVE    ADRS-REC        TO  MCPDATA-REC
           PERFORM 900-ADRS-INV-SEC
           MOVE    ADRS-LPUBCD         TO  TOUKEI04-LPUBCD
      *
      *    医師コード
           MOVE    LNK-ORCBT120SUB-DRCD
                                       TO  TOUKEI04-DRCD
      *入院情報
      *    入退院区分
           MOVE    LNK-ORCBT120SUB-NYUIN-KBN
                                       TO  TOUKEI04-NYUIN-KBN
      *    病棟番号
           MOVE    LNK-ORCBT120SUB-BTUNUM
                                       TO  TOUKEI04-BTUNUM
      *    病室番号
           MOVE    LNK-ORCBT120SUB-BRMNUM
                                       TO  TOUKEI04-BRMNUM
      *    外泊区分
           MOVE    LNK-ORCBT120SUB-GAIHAKU-KBN
                                       TO  TOUKEI04-GAIHAKU-KBN
      *    食事区分
           MOVE    LNK-ORCBT120SUB-SKJ-KBN
                                       TO  TOUKEI04-SKJ-KBN
      *    退院事由
           MOVE    LNK-ORCBT120SUB-TAIINCD
                                       TO  TOUKEI04-TAIINCD
      *    前日診療科
           MOVE       LNK-ORCBT120SUB-MAE-NYUINKA
                                       TO  TOUKEI04-MAE-SRYKA
      *    前日レセ電診療科
           MOVE       LNK-ORCBT120SUB-MAE-RECENYUINKA
                                       TO  TOUKEI04-MAE-RECEKA
      *    前日病棟番号
           MOVE    LNK-ORCBT120SUB-MAE-BTUNUM
                                       TO  TOUKEI04-MAE-BTUNUM
      *    前日病室番号
           MOVE    LNK-ORCBT120SUB-MAE-BRMNUM
                                       TO  TOUKEI04-MAE-BRMNUM
      *収納情報
      *    伝票番号
           MOVE    LNK-ORCBT120SUB-DENPNUM(IDX)
                                       TO  TOUKEI04-DENPNUM
      *    伝票状態区分
           MOVE    LNK-ORCBT120SUB-DENPJTIKBN(IDX)
                                       TO  TOUKEI04-DENPJTIKBN
      *    伝票作成区分
           MOVE    LNK-ORCBT120SUB-CREATEKBN(IDX)
                                       TO  TOUKEI04-CREATEKBN
      *    保険点数
           MOVE    LNK-ORCBT120SUB-HKNTEN(IDX)
                                       TO  TOUKEI04-HKNTEN
      *    保険負担金額
           MOVE    LNK-ORCBT120SUB-HKNMONEY(IDX)
                                       TO  TOUKEI04-HKNMONEY
      *    保険適用外金額
           MOVE    LNK-ORCBT120SUB-TGMONEY(IDX)         
                                       TO  TOUKEI04-TGMONEY
      *    保険適用外金税額
           MOVE    LNK-ORCBT120SUB-TGMONEY-TAX(IDX)
                                       TO  TOUKEI04-TGMONEY-TAX
      *    自費金額
           MOVE    LNK-ORCBT120SUB-JIHI(IDX)
                                       TO  TOUKEI04-JIHI
      *    自費税額
           MOVE    LNK-ORCBT120SUB-JIHI-TAX(IDX)
                                       TO  TOUKEI04-JIHI-TAX
      *    食事負担金額
           MOVE    LNK-ORCBT120SUB-SKJ(IDX)
                                       TO  TOUKEI04-SKJ
      *    消費税額
           MOVE    LNK-ORCBT120SUB-SKJ-TAX(IDX)
                                       TO  TOUKEI04-SKJ-TAX
      *    室料差額
           MOVE    LNK-ORCBT120SUB-RMSAGAKU(IDX)
                                       TO  TOUKEI04-RMSAGAKU
      *    室料差額税額
           MOVE    LNK-ORCBT120SUB-RMSAGAKU-TAX(IDX)
                                       TO  TOUKEI04-RMSAGAKU-TAX
      *    請求金額
           MOVE    LNK-ORCBT120SUB-SKYMONEY(IDX)
                                       TO  TOUKEI04-SKYMONEY
      *保険情報
      *    保険番号
           MOVE    LNK-ORCBT120SUB-HKNNUM(IDX)
                                       TO  TOUKEI04-HKNNUM
      *    保険ＩＤ
           MOVE    LNK-ORCBT120SUB-HKNID(IDX)
                                       TO  TOUKEI04-HKNID
      *    保険者番号
           MOVE    LNK-ORCBT120SUB-HKNJANUM(IDX)
                                       TO  TOUKEI04-HKNJANUM
      *    本人家族区分
           MOVE    LNK-ORCBT120SUB-HONKZKKBN(IDX)
                                       TO  TOUKEI04-HONKZKKBN
      *    補助区分
           MOVE    LNK-ORCBT120SUB-HOJOKBN(IDX)
                                       TO  TOUKEI04-HOJOKBN
      *    継続区分
           MOVE    LNK-ORCBT120SUB-CONTKBN(IDX)
                                       TO  TOUKEI04-CONTKBN
      *公費情報
           PERFORM    VARYING   KOH-IDX   FROM  1  BY  1
             UNTIL    (KOH-IDX   >   4)       OR
                      (LNK-ORCBT120SUB-KOH-HKNNUM(IDX KOH-IDX) = SPACE)
      *        公費−公費番号
               MOVE    LNK-ORCBT120SUB-KOH-HKNNUM(IDX KOH-IDX)
                                       TO  TOUKEI04-KOH-HKNNUM(KOH-IDX)
      *        公費−公費ＩＤ
               MOVE    LNK-ORCBT120SUB-KOH-ID(IDX KOH-IDX)
                                       TO  TOUKEI04-KOH-ID(KOH-IDX)
      *        公費−支払区分
               MOVE    LNK-ORCBT120SUB-KOH-PAYKBN(IDX KOH-IDX)
                                       TO  TOUKEI04-KOH-PAYKBN(KOH-IDX)
           END-PERFORM
      *労災保険区分
           MOVE    LNK-ORCBT120SUB-RSIHKNKBN(IDX)
                                       TO  TOUKEI04-RSIHKNKBN
      *自賠責請求
           MOVE    LNK-ORCBT120SUB-JIBAIKBN(IDX)
                                       TO  TOUKEI04-JIBAISEIKBN
      *
           .
       20011-MEISAI-HENSYU-EXT.
           EXIT. 
      *
      *****************************************************************
      *    出力処理
      *****************************************************************
       2002-OUT-SYORI-SEC              SECTION.
      *
           WRITE   TOUKEI04-REC
           ADD     1                   TO  CNT-TOUKEI04
           .
       2002-OUT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   TOUKEIERR-FILE
           IF      STS-TOUKEIERR         =   ZERO
               CLOSE   TOUKEIERR-FILE
           ELSE
               OPEN    OUTPUT              TOUKEIERR-FILE
      *
               MOVE    WRK-TOUKEIERR         TO  TOUKEIERR-REC
               WRITE   TOUKEIERR-REC
               CLOSE   TOUKEIERR-FILE
      *         
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-TOUKEIERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
           END-IF
      *                             
           MOVE    1                   TO  FLG-END
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           CLOSE   TOUKEI04-FILE
      *
           IF      CNT-TOUKEI04           =   ZERO  
               MOVE    "処理対象のデータがありませんでした"
                                           TO  WRK-TOUKEIERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *     
           DISPLAY "TOUKEI04  CNT-TOUKEI04:" CNT-TOUKEI04
           DISPLAY "*** ORCBT120 END ***"
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ　ＲＥＡＤ
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       910-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       910-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込（主キー）
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTINF
                   MOVE    MCPDATA-REC         TO  PTINF-REC
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    住所マスタ検索
      *****************************************************************
       900-ADRS-INV-SEC            SECTION.
      *
           MOVE    "tbl_adrs"          TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_adrs"          TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  ADRS-REC
                   MOVE    ZERO                TO  FLG-ADRS
               ELSE
                   INITIALIZE                      ADRS-REC
                   MOVE    1                   TO  FLG-ADRS
               END-IF
           ELSE
               INITIALIZE                  ADRS-REC
               MOVE    1               TO  FLG-ADRS
           END-IF
      *
           MOVE    "tbl_adrs"          TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者入院履歴マスタ読込
      *****************************************************************
       900-PTNYUINRRK-SELECT-SEC         SECTION.
      *
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC           =   ZERO
               MOVE    ZERO         TO  FLG-PTNYUINRRK
           ELSE
               MOVE    1            TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       900-PTNYUINRRK-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者入院履歴マスター読込
      *****************************************************************
       900-PTNYUINRRK-READ-SEC        SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
               MOVE    ZERO            TO  FLG-PTNYUINRRK
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       900-PTNYUINRRK-READ-EXT.
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
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               DISPLAY "SELECT ERR table=" MCP-TABLE
                       " pathname="        MCP-PATHNAME
           END-IF
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＦＥＴＣＨ処理
      *****************************************************************
       900-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-CLOSE-EXT.
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
      *    ＤＢオープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
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
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
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
