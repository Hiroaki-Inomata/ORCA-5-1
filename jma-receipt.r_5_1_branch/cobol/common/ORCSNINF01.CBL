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
       PROGRAM-ID.             ORCSNINF01.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 共通
      *  コンポーネント名  : 入院情報取得
      *  管理者            : 
      *  作成日付    作業者        記述
      *  14/04/07    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       INPUT-OUTPUT        SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
       01  FLG-AREA.
           03  FLG-PTINF               PIC 9(01).
           03  FLG-PTNYUINRRK          PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-TENSU               PIC 9(01).
           03  FLG-BUNBEN              PIC 9(01).
           03  FLG-LOOPEND             PIC 9(01).
           03  FLG-NYUINACT            PIC 9(01).
           03  FLG-NYUINACCT           PIC 9(01).
           03  FLG-SENTEICDCHG         PIC 9(01).
           03  FLG-SENTEI              PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
           03  IDX1                    PIC 9(05).
           03  IDX2                    PIC 9(05).
           03  IDXA                    PIC 9(05).
           03  IDXB                    PIC 9(05).
           03  IDXK                    PIC 9(05).
           03  IDXZ                    PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-ERRCD               PIC X(02).
           03  WRK-ERRMSG              PIC X(100).
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-EDTYMD1             PIC X(09).
           03  WRK-DATE.
               05  WRK-DATE-YY         PIC 9(04).
               05  WRK-DATE-FL1        PIC X(01).
               05  WRK-DATE-MM         PIC 9(02).
               05  WRK-DATE-FL2        PIC X(01).
               05  WRK-DATE-DD         PIC 9(02).
           03  WRK-HMS.
               05  WRK-HMS-HH          PIC 9(02).
               05  WRK-HMS-MM          PIC 9(02).
               05  WRK-HMS-SS          PIC 9(02).
           03  WRK-TIME.
               05  WRK-TIME-HH         PIC 9(02).
               05  WRK-TIME-FL1        PIC X(01).
               05  WRK-TIME-MM         PIC 9(02).
               05  WRK-TIME-FL2        PIC X(01).
               05  WRK-TIME-SS         PIC 9(02).
           03  WRK-SRYYMD.
               05  WRK-SRYYMD-YM       PIC X(06).
               05  WRK-SRYYMD-DD       PIC 9(02).
           03  WRK-DRCD                PIC X(05)
                                       OCCURS 3.
           03  WRK-NUM7Z               PIC ZZZZZZ9.
           03  WRK-KJNYMD              PIC X(08).
      *
           03  WRK-NENREI              PIC 9(03).
           03  WRK-YMDS1.
               05  WRK-YYS1            PIC 9(04).
               05  WRK-MMS1            PIC 9(02).
               05  WRK-DDS1            PIC 9(02).
           03  WRK-YMDS2.
               05  WRK-YYS2            PIC 9(04).
               05  WRK-MMS2            PIC 9(02).
               05  WRK-DDS2            PIC 9(02).
           03  WRK-NYUINRYO-G.
               05  WRK-NYUINRYO-CD     PIC X(09).
               05  WRK-NYUINRYO-NAME   PIC X(100).
           03  WRK-TOKTEI-G.
               05  WRK-TOKTEI-CD       PIC X(09).
               05  WRK-TOKTEI-NAME     PIC X(100).
           03  WRK-KSNSRYCD-G.
               05  WRK-KSNSRYCD-MAX        PIC 9(03).
               05  WRK-KSNSRYCD-OCC        OCCURS 3.
                   07  WRK-KSNSRYCD        PIC X(09).
                   07  WRK-KSNSRYCD-NAME   PIC X(100).
           03  WRK-SKJKBN-G.
               05  WRK-SKJKBN              PIC X(01).
               05  WRK-SKJKBN-NAME         PIC X(30).
      *
       01  KEY-AREA.
           03  KEY-RRKNUM              PIC 9(03).
           03  KEY-SHONUM              PIC 9(03).
           03  KEY-SRYCD               PIC X(09).
      *
      *
       01  CONST-AREA.
           03  CONST-H180401           PIC X(08)   VALUE "20060401".
           03  CONST-H211001           PIC X(08)   VALUE "20091001".
           03  CONST-RRK-MAX           PIC 9(03)   VALUE 100.
      *
           COPY    "COMMON-SPA"        REPLACING   //SPA-//
                                       BY          //TSPA-//.
      *
       01  TRRK-REC.
           COPY    "CPPTNYUINRRK.INC"  REPLACING   //PTNYUINRRK-//
                                       BY          //TRRK-//.
      *
       01  BRRK-REC.
           COPY    "CPPTNYUINRRK.INC"  REPLACING   //PTNYUINRRK-//
                                       BY          //BRRK-//.
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *    特定入院料コード取得パラメタ
           COPY    "CPORCSTOKNYUIN.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
           COPY    "CPORCSKANACHK.INC".
      *
      *    入退院日取得サブパラメタ
           COPY    "CPORCSNTYMD.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    保険組合せセット領域
           COPY    "CPORCSAPIHKN.INC".
      *
      *    分娩情報編集サブ
           COPY    "CPORCSI02.INC".
       01  SNRRK-AREA.
           COPY    "CPPTNYUINRRK.INC"  REPLACING  //PTNYUINRRK-//
                                       BY         //SNRRK-//.
      *
      *    入院料加算算定可否チェック
           COPY    "CPORCSI01.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理
           COPY    "CPSYSKANRI.INC".
      *    職員情報
           COPY    "CPSK1010.INC".
      *    病棟
           COPY    "CPSK5001.INC".
      *    病室
           COPY    "CPSK5002.INC".
      *    診療科情報
           COPY    "CPSK1005.INC".
      *    室料差額
           COPY    "CPSK5113.INC".
      *    退院事由
           COPY    "CPSK5013.INC".
      *    特定入院料等（病棟）
           COPY    "CPSK5110.INC".
      *    特定入院料等（病室）
           COPY    "CPSK5111.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    入院会計
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    入院診療行為
       01  NYUINACT-REC.
           COPY    "CPNYUINACT.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
       01  BUNBEN-REC.
           COPY    "CPBUNBEN.INC".
      *
      *    選定入院料コード置換
       01  SENTEICDCHG-REC.
           COPY    "CPSENTEICDCHG.INC".
      *
      *
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                 SECTION.
           COPY    "CPORCSNINF01.INC".
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           SNINF01-AREA
           SPA-AREA.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-MAIN-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  KEY-AREA
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *
      *    主処理
           IF    ( WRK-ERRCD          =   SPACE )
               PERFORM 200-MAIN-SEC
           END-IF
      *
      *    返却領域編集
           PERFORM 300-END-SEC
      *
           .
       000-MAIN-EXIT.
           EXIT    PROGRAM.
      *
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           MOVE    SNINF01-IN-RRKNUM     TO  KEY-RRKNUM
           MOVE    SNINF01-IN-SHONUM     TO  KEY-SHONUM
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
           PERFORM 2001-PTINF-EDIT-SEC
      *
           PERFORM 2001-NYUINRRK-EDIT-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者報編集処理
      *****************************************************************
       2001-PTINF-EDIT-SEC                 SECTION.
      *
           PERFORM 900-PTINF-KEY-SEL-SEC
      *
           MOVE    SPA-PTNUM           TO  SNINF01-PTNUM
           MOVE    PTINF-NAME          TO  SNINF01-NAME
           MOVE    PTINF-KANANAME      TO  SNINF01-KANANAME
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE            TO  SNINF01-BIRTHDAY
           MOVE    PTINF-SEX           TO  SNINF01-SEX
      *
          .
       2001-PTINF-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴情報編集処理
      *****************************************************************
       2001-NYUINRRK-EDIT-SEC          SECTION.
      *
           MOVE    ZERO                TO  IDXA
      *
           PERFORM 900-PTNYUINRRK-KEY59-SEL-SEC
      *
           PERFORM UNTIL ( FLG-PTNYUINRRK  NOT =   ZERO )
                    OR   ( IDXA               >=   CONST-RRK-MAX )
      *
               IF    ( PTNYUINRRK-JTIKBN       =   "5" OR "6" )
                   IF   ( PTNYUINRRK-JTIKBN       =   "6" )
                    AND ( PTNYUINRRK-NYUINCHUKBN  =   "1" )
                       IF    ( IDXA        >=  1 )
                           MOVE    PTNYUINRRK-NYUINYMD
                                               TO  WRK-SYMD
                           PERFORM 800-DATE-SEC
                           MOVE    WRK-DATE    TO  SNINF01-NYUINYMD
                                                              (IDXA)
                       END-IF
                   ELSE
                       PERFORM 20012-NYUINRRK-EDIT-SEC
                   END-IF
               ELSE
                   PERFORM 20011-NYUINRRK-EDIT-SEC
               END-IF
      *
               PERFORM 900-PTNYUINRRK-KEY59-FET-SEC
      *
           END-PERFORM
      *
           MOVE    IDXA                TO  SNINF01-NYUINRRK-INF-MAX
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key59"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
          .
       2001-NYUINRRK-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入院歴情報編集処理（通常登録分）
      *****************************************************************
       20011-NYUINRRK-EDIT-SEC                SECTION.
      *
           COMPUTE IDXA    =   IDXA    +   1
      *
           MOVE    PTNYUINRRK-REC      TO  BRRK-REC
           IF    ( PTNYUINRRK-RRKEDANUM    >  1 )
               PERFORM 900-PTNYUINRRK-KEY-SEL-SEC
           END-IF
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "5001"              TO  SYS-KANRICD
           MOVE    PTNYUINRRK-BTUNUM   TO  SYS-KBNCD
           MOVE    PTNYUINRRK-TENNYUYMD
                                       TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-5001-REC
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "5002"              TO  SYS-KANRICD
           MOVE    PTNYUINRRK-BRMNUM   TO  SYS-KBNCD
           MOVE    PTNYUINRRK-TENNYUYMD
                                       TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-5002-REC
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "1005"              TO  SYS-KANRICD
           MOVE    PTNYUINRRK-NYUINKA  TO  SYS-KBNCD
           MOVE    PTNYUINRRK-TENNYUYMD
                                       TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-1005-REC
      *
           INITIALIZE                  SI02-AREA
           MOVE    PTNYUINRRK-REC  TO  SNRRK-AREA
           MOVE    "1"             TO  SI02-REQID
           CALL    "ORCSI02"           USING
                                       SI02-AREA
                                       SNRRK-AREA
                                       MCPAREA
                                       SPA-AREA
      *
           MOVE    PTNYUINRRK-RRKNUM
                               TO  SNINF01-RRKNUM (IDXA)
      *
           MOVE    "入院歴作成区分"
                               TO  SNINF01-CREATEKBN-LBL (IDXA)
           MOVE    "0"         TO  SNINF01-CREATEKBN-DATA(IDXA)
           MOVE    "通常登録"  TO  SNINF01-CREATEKBN-NAME(IDXA)
      *
           MOVE    PTNYUINRRK-NYUINYMD TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE    TO  SNINF01-NYUINYMD (IDXA)
      *
           MOVE    "入院日の食事開始時間"
                               TO  SNINF01-TIMEKBN-IN-LBL(IDXA)
           MOVE    PTNYUINRRK-TIMEKBN-IN
                               TO  SNINF01-TIMEKBN-IN-DATA(IDXA)
           EVALUATE    PTNYUINRRK-TIMEKBN-IN
           WHEN    "0"
               MOVE    "食事なし"  TO  SNINF01-TIMEKBN-IN-NAME(IDXA)
           WHEN    "1"
               MOVE    "朝食から"  TO  SNINF01-TIMEKBN-IN-NAME(IDXA)
           WHEN    "2"
               MOVE    "昼食から"  TO  SNINF01-TIMEKBN-IN-NAME(IDXA)
           WHEN    "3"
               MOVE    "夕食から"  TO  SNINF01-TIMEKBN-IN-NAME(IDXA)
           END-EVALUATE
      *
           IF    ( PTNYUINRRK-RRKEDANUM    NOT =   1 )
               MOVE    PTNYUINRRK-TENNYUYMD
                                   TO  WRK-SYMD
               PERFORM 800-DATE-SEC
               MOVE    WRK-DATE    TO  SNINF01-IDOYMD (IDXA)
           END-IF
      *
           IF    ( SNINF01-IN-KJNYMD       NOT =   SPACE )
               MOVE    SNINF01-IN-KJNYMD
                                   TO  WRK-SYMD
               PERFORM 800-DATE-SEC
               MOVE    WRK-DATE    TO  SNINF01-IDOYMD (IDXA)
           END-IF
      *
           MOVE    PTNYUINRRK-TAIINYMD TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE    TO  SNINF01-TAIINYMD (IDXA)
      *
           IF    ( PTNYUINRRK-TAIINCD  NOT =   SPACE )
               MOVE    "退院事由"      TO  SNINF01-TAIINCD-LBL (IDXA)
               MOVE    PTNYUINRRK-TAIINCD
                                       TO  SNINF01-TAIINCD-DATA(IDXA)
      *
               INITIALIZE                          SYSKANRI-REC
               MOVE    SPA-HOSPNUM             TO  SYS-HOSPNUM
               MOVE    "5013"                  TO  SYS-KANRICD
               MOVE    PTNYUINRRK-TAIINCD      TO  SYS-KBNCD
               MOVE    PTNYUINRRK-TENNYUYMD    TO  SYS-STYUKYMD
                                                   SYS-EDYUKYMD
               PERFORM 900-SYSKANRI-KEY10-SEL-SEC
               MOVE    SYSKANRI-REC    TO  SYS-5013-REC
               MOVE    SYS-5013-NAME   TO  SNINF01-TAIINCD-NAME(IDXA)
           END-IF
      *
           MOVE    "病棟番号"          TO  SNINF01-BTUNUM-LBL (IDXA)
           MOVE    PTNYUINRRK-BTUNUM   TO  SNINF01-BTUNUM-DATA(IDXA)
      *
           MOVE    "病棟名"            TO  SNINF01-BTUNAME-LBL(IDXA)
           MOVE    SYS-5001-BTU-NAME
                                   TO  SNINF01-BTUNAME-DATA (IDXA)
      *
           MOVE    "病室番号"      TO  SNINF01-BRMNUM-LBL   (IDXA)
           MOVE    SYS-5002-BRM-NUM
                                   TO  SNINF01-BRMNUM-DATA(IDXA)
      *
           MOVE    "診療科"        TO  SNINF01-SRYKA-LBL  (IDXA)
           MOVE    PTNYUINRRK-NYUINKA
                                   TO  SNINF01-SRYKA-DATA (IDXA)
           MOVE    SYS-1005-SRYKANAME
                                   TO  SNINF01-SRYKA-NAME (IDXA)
      *
           MOVE    PTNYUINRRK-DRCD1    TO  WRK-DRCD (1)
           MOVE    PTNYUINRRK-DRCD2    TO  WRK-DRCD (2)
           MOVE    PTNYUINRRK-DRCD3    TO  WRK-DRCD (3)
      *
           MOVE    ZERO            TO  IDXB
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   3 )
      *
               IF    ( WRK-DRCD (IDX0) NOT =   SPACE )
                   COMPUTE IDXB    =   IDXB    +   1
                   MOVE    "担当医"    TO  SNINF01-DR-LBL (IDXA IDXB)
                   MOVE    WRK-DRCD (IDX0) 
                                       TO  SNINF01-DR-DATA(IDXA IDXB)
                   INITIALIZE                  SYSKANRI-REC
                   MOVE    SPA-HOSPNUM TO  SYS-HOSPNUM
                   MOVE    "1010"      TO  SYS-KANRICD
                   MOVE    WRK-DRCD (IDX0)
                                       TO  SYS-KBNCD
                   MOVE    PTNYUINRRK-TENNYUYMD
                                       TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
                   PERFORM 900-SYSKANRI-KEY10-SEL-SEC
                   MOVE    SYSKANRI-REC
                                       TO  SYS-1010-REC
                   MOVE    SYS-1010-NAME
                                       TO  SNINF01-DR-NAME(IDXA IDXB)
               END-IF
      *
           END-PERFORM
      *
      *    保険組合せ
           MOVE    PTNYUINRRK-HKNCOMBINUM
                                       TO  SNINF01-HKNCOMBI (IDXA)
      *
           MOVE    SPA-AREA            TO  TSPA-AREA
           MOVE    PTNYUINRRK-TENNYUYMD
                                       TO  SPA-SYSYMD
           INITIALIZE                  ORCSAPIHKNAREA
           MOVE    "2"             TO  ORCSAPIHKN-KBN2
           CALL    "ORCSAPIHKN"        USING
                                       SPA-AREA
                                       ORCSAPIHKNAREA
           MOVE    TSPA-AREA   TO      SPA-AREA
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   20 )
                     OR  ( ORCSAPIHKN-THKNCOMBI (IDX1)
                                       =   SPACE  )
               IF    ( ORCSAPIHKN-THKNCOMBI (IDX1)
                                       =   PTNYUINRRK-HKNCOMBINUM )
                   PERFORM 200111-HKN-EDIT-SEC
                   MOVE    20      TO  IDX1
               END-IF
      *
           END-PERFORM
      *
           PERFORM 200111-SHOKAIYMD-EDIT-SEC
      *
           MOVE    "介護からの異動"
                                   TO  SNINF01-KAIGOKBN-LBL (IDXA)
           MOVE    PTNYUINRRK-KAIGOKBN
                                   TO  SNINF01-KAIGOKBN-DATA(IDXA)
           EVALUATE    PTNYUINRRK-KAIGOKBN
           WHEN    "1"
               MOVE    "対象外"
                                   TO  SNINF01-KAIGOKBN-NAME(IDXA)
           WHEN    "2"
               MOVE    "急性増悪により"
                                   TO  SNINF01-KAIGOKBN-NAME(IDXA)
           END-EVALUATE
      *
           IF    ( PTNYUINRRK-RM-SAGAKU    NOT =   SPACE )
               INITIALIZE              SYSKANRI-REC
               MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
               MOVE    "5113"          TO  SYS-KANRICD
               MOVE    PTNYUINRRK-RM-SAGAKU
                                       TO  SYS-KBNCD
               MOVE    PTNYUINRRK-TENNYUYMD
                                       TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
               PERFORM 900-SYSKANRI-KEY10-SEL-SEC
               MOVE    SYSKANRI-REC    TO  SYS-5113-REC
               IF    ( FLG-SYSKANRI    =   ZERO )
                   MOVE    "室料差額"  TO  SNINF01-SAGAKU-LBL  (IDXA)
                   MOVE    "円"        TO  SNINF01-SAGAKU-NAME (IDXA)
                   MOVE    SYS-5113-BRM-SAGAKU-NM
                                       TO  WRK-NUM7Z
                   MOVE    WRK-NUM7Z   TO  SNINF01-SAGAKU-DATA (IDXA)
               END-IF
           END-IF
      *
           MOVE    "選定入院"  TO  SNINF01-SENTEIKBN-LBL (IDXA)
           MOVE    PTNYUINRRK-SENTEIKBN
                               TO  SNINF01-SENTEIKBN-DATA(IDXA)
           EVALUATE    PTNYUINRRK-SENTEIKBN
           WHEN    "1"
               MOVE    "選定対象"
                               TO  SNINF01-SENTEIKBN-NAME(IDXA)
           WHEN    "2"
               MOVE    "選定対象外"
                               TO  SNINF01-SENTEIKBN-NAME(IDXA)
           END-EVALUATE
      *
           MOVE    PTNYUINRRK-NYUINYMD     TO  WRK-SRYYMD
           PERFORM 200111-NYUINRYO-EDIT-SEC
           IF    ( WRK-NYUINRYO-CD NOT =   SPACE )
               MOVE    "入院日の入院料"
                               TO  SNINF01-NYUINRYO-LBL (IDXA)
               MOVE    WRK-NYUINRYO-CD 
                               TO  SNINF01-NYUINRYO-DATA(IDXA)
               MOVE    WRK-NYUINRYO-NAME
                               TO  SNINF01-NYUINRYO-NAME(IDXA)
      *
               MOVE    WRK-KSNSRYCD-MAX
                               TO  SNINF01-KSNSRYCD-MAX (IDXA)
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   WRK-KSNSRYCD-MAX )
                   MOVE    "入院日の入院加算"
                               TO  SNINF01-KSNSRYCD-LBL (IDXA IDX0)
                   MOVE    WRK-KSNSRYCD (IDX0)
                               TO  SNINF01-KSNSRYCD-DATA (IDXA IDX0)
                   MOVE    WRK-KSNSRYCD-NAME (IDX0)
                               TO  SNINF01-KSNSRYCD-NAME (IDXA IDX0)
               END-PERFORM
      *
           END-IF
      *
           PERFORM 200111-TOKSNT-EDIT-SEC
      *
           IF    ( PTNYUINRRK-RRKEDANUM    NOT =   1 )
            OR   ( SNINF01-IN-KJNYMD       NOT =   SPACE )
               IF    ( PTNYUINRRK-RRKEDANUM    NOT =   1 )
                   MOVE    PTNYUINRRK-TENNYUYMD    TO  WRK-SRYYMD
               END-IF
               IF    ( SNINF01-IN-KJNYMD   NOT =   SPACE )
                   MOVE    SNINF01-IN-KJNYMD       TO  WRK-SRYYMD
               END-IF
               PERFORM 200111-NYUINRYO-EDIT-SEC
               IF    ( WRK-NYUINRYO-CD NOT =   SPACE )
                   MOVE    "前回異動日の入院料"
                               TO  SNINF01-IDONYUINRYO-LBL (IDXA)
                   MOVE    WRK-NYUINRYO-CD 
                               TO  SNINF01-IDONYUINRYO-DATA(IDXA)
                   MOVE    WRK-NYUINRYO-NAME
                               TO  SNINF01-IDONYUINRYO-NAME(IDXA)
      *
                   MOVE    WRK-KSNSRYCD-MAX
                               TO  SNINF01-IDOKSNSRYCD-MAX (IDXA)
                   PERFORM VARYING IDX0    FROM    1   BY  1
                           UNTIL ( IDX0    >   WRK-KSNSRYCD-MAX )
                       MOVE    "前回異動日の入院加算"
                               TO  SNINF01-IDOKSNSRYCD-LBL (IDXA IDX0)
                       MOVE    WRK-KSNSRYCD (IDX0)
                               TO  SNINF01-IDOKSNSRYCD-DATA (IDXA IDX0)
                       MOVE    WRK-KSNSRYCD-NAME (IDX0)
                               TO  SNINF01-IDOKSNSRYCD-NAME (IDXA IDX0)
                   END-PERFORM
              END-IF
           END-IF
      *
           MOVE    "入院会計"      TO  SNINF01-NACCTKBN-LBL (IDXA)
           MOVE    PTNYUINRRK-NYUCALKBN
                                   TO  SNINF01-NACCTKBN-DATA(IDXA)
           EVALUATE    PTNYUINRRK-NYUCALKBN
           WHEN    "1"
               MOVE    "入院料を算定しない"
                                   TO  SNINF01-NACCTKBN-NAME(IDXA)
           WHEN    "2"
               MOVE    "入院料を算定する"
                                   TO  SNINF01-NACCTKBN-NAME(IDXA)
           END-EVALUATE
      *
           IF    ( SI02-BUNBENCHK  NOT =   ZERO )
      *
               PERFORM 900-BUNBEN-KEY3-SEL-SEC
      *
               MOVE    "分娩区分"  TO  SNINF01-BUNBENKBN-LBL(IDXA)
               MOVE    BUNBEN-BUNBENKBN
                                   TO  SNINF01-BUNBENKBN-DATA(IDXA)
               EVALUATE    BUNBEN-BUNBENKBN
               WHEN    "0"
                   MOVE    "分娩入院でない"
                                   TO  SNINF01-BUNBENKBN-NAME(IDXA)
               WHEN    "1"
                   MOVE    "正常分娩"
                                   TO  SNINF01-BUNBENKBN-NAME(IDXA)
               WHEN    "2"
                   MOVE    "異常分娩"
                                   TO  SNINF01-BUNBENKBN-NAME(IDXA)
               END-EVALUATE
      *
               IF    ( BUNBEN-BUNBENKBN    =   "1" OR "2" )
                AND((( PTNYUINRRK-TAIINYMD NOT =   "99999999" )
                AND  ( PTNYUINRRK-TAIINYMD    >=   CONST-H211001 ))
                 OR  ( PTNYUINRRK-NYUINYMD    >=   CONST-H211001 ))
                   MOVE    "直接支払制度"
                                   TO  SNINF01-RIYOUKBN-LBL (IDXA)
                   MOVE    BUNBEN-RIYOUKBN
                                   TO  SNINF01-RIYOUKBN-DATA(IDXA)
                   EVALUATE    BUNBEN-RIYOUKBN
                   WHEN    "0"
                       MOVE    "利用しない"
                                   TO  SNINF01-RIYOUKBN-NAME(IDXA)
                   WHEN    "1"
                       MOVE    "利用する"
                                   TO  SNINF01-RIYOUKBN-NAME(IDXA)
                   END-EVALUATE
               END-IF
           END-IF
      *
           MOVE    "定期請求"      TO  SNINF01-TEIKIKBN-LBL (IDXA)
           MOVE    PTNYUINRRK-TEIKI-SEIKYUKBN
                                   TO  SNINF01-TEIKIKBN-DATA(IDXA)
           EVALUATE    PTNYUINRRK-TEIKI-SEIKYUKBN
           WHEN    "1"
               MOVE    "医療機関での設定"
                                   TO  SNINF01-TEIKIKBN-NAME(IDXA)
           WHEN    "2"
               MOVE    "月末時のみ請求"
                                   TO  SNINF01-TEIKIKBN-NAME(IDXA)
           WHEN    "3"
               MOVE    "定期請求しない"
                                   TO  SNINF01-TEIKIKBN-NAME(IDXA)
           END-EVALUATE
      *
           MOVE    "検索時患者表示"
                                   TO  SNINF01-DISPKBN-LBL (IDXA)
           MOVE    PTNYUINRRK-KENSAKU-DISPKBN
                                   TO  SNINF01-DISPKBN-DATA(IDXA)
           EVALUATE    PTNYUINRRK-KENSAKU-DISPKBN
           WHEN    "1"
               MOVE    "表示可"
                                   TO  SNINF01-DISPKBN-NAME(IDXA)
           WHEN    "2"
               MOVE    "表示不可"
                                   TO  SNINF01-DISPKBN-NAME(IDXA)
           END-EVALUATE
      *
           MOVE    "食事の種類"    TO  SNINF01-SKJKBN-LBL (IDXA)
           MOVE    PTNYUINRRK-SKJKBN
                                   TO  SNINF01-SKJKBN-DATA (IDXA)
           MOVE    PTNYUINRRK-SKJKBN
                                   TO  WRK-SKJKBN
           PERFORM 20011-SKJKBN-SEC
           MOVE    WRK-SKJKBN-NAME TO  SNINF01-SKJKBN-NAME (IDXA)
      *
           IF    ( PTNYUINRRK-SKJKBN   =   "B" OR "C" )
      *
               MOVE    "食事の種類（朝）"
                                   TO  SNINF01-TSKJKBN-LBL (IDXA 1)
               MOVE    PTNYUINRRK-SKJKBN-ASA
                                   TO  SNINF01-TSKJKBN-DATA(IDXA 1)
               MOVE    PTNYUINRRK-SKJKBN-ASA
                                   TO  WRK-SKJKBN
               PERFORM 20011-SKJKBN-SEC
               MOVE    WRK-SKJKBN-NAME TO SNINF01-TSKJKBN-NAME(IDXA 1)
      *
               MOVE    "食事の種類（昼）"
                                   TO  SNINF01-TSKJKBN-LBL (IDXA 2)
               MOVE    PTNYUINRRK-SKJKBN-HIRU
                                   TO  SNINF01-TSKJKBN-DATA(IDXA 2)
               MOVE    PTNYUINRRK-SKJKBN-HIRU
                                   TO  WRK-SKJKBN
               PERFORM 20011-SKJKBN-SEC
               MOVE    WRK-SKJKBN-NAME TO SNINF01-TSKJKBN-NAME(IDXA 2)
      *
               MOVE    "食事の種類（夕）"
                                   TO  SNINF01-TSKJKBN-LBL (IDXA 3)
               MOVE    PTNYUINRRK-SKJKBN-YU
                                   TO  SNINF01-TSKJKBN-DATA(IDXA 3)
               MOVE    PTNYUINRRK-SKJKBN-YU
                                   TO  WRK-SKJKBN
               PERFORM 20011-SKJKBN-SEC
               MOVE    WRK-SKJKBN-NAME TO SNINF01-TSKJKBN-NAME(IDXA 3)
      *
           END-IF
      *
          .
       20011-NYUINRRK-EXT.
           EXIT.
      *****************************************************************
      *    保険情報編集処理
      *****************************************************************
       200111-HKN-EDIT-SEC              SECTION.
      *
      *    保険の種類
           MOVE    ORCSAPIHKN-HKNNUM (IDX1)
                                   TO  SNINF01-HKNNUM   (IDXA)
      *
      *    保険者番号
           MOVE    ORCSAPIHKN-HKNJANUM (IDX1)
                                   TO  SNINF01-HKNJANUM (IDXA)
      *
      *    保険の名称
           MOVE    ORCSAPIHKN-HKNNUM-NAME (IDX1)
                                   TO  SNINF01-HKNJANAME(IDXA)
      *
      *    記号
           MOVE    ORCSAPIHKN-KIGO  (IDX1)
                                   TO  SNINF01-HKIGO (IDXA)
      *
      *    番号
           MOVE    ORCSAPIHKN-NUM  (IDX1)
                                   TO  SNINF01-HNUM  (IDXA)
      *
      *    公費情報
           PERFORM VARYING IDXK    FROM    1   BY  1
                   UNTIL ( IDXK    >   4 )
      *
      *        公費の種類
               MOVE    ORCSAPIHKN-KOHNUM (IDX1 IDXK)
                                   TO  SNINF01-KOHNUM  (IDXA IDXK)
      *        公費の種類名称
               MOVE    ORCSAPIHKN-KOHNUM-NAME (IDX1 IDXK)
                                   TO  SNINF01-KOHNAME (IDXA IDXK)
      *        負担者番号
               MOVE    ORCSAPIHKN-FTNJANUM (IDX1 IDXK)
                                   TO  SNINF01-FTNJANUM(IDXA IDXK)
      *        受給者番号
               MOVE    ORCSAPIHKN-JKYSNUM (IDX1 IDXK)
                                   TO  SNINF01-JKYSNUM (IDXA IDXK)
           END-PERFORM
      *
          .
       200111-HKN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    初回入院日編集処理
      *****************************************************************
       200111-SHOKAIYMD-EDIT-SEC       SECTION.
      *
           IF    ( PTNYUINRRK-SHOKAIKBN    =   "1" )
               MOVE    PTNYUINRRK-NYUINYMD
                                       TO  WRK-SYMD
               PERFORM 800-DATE-SEC
               MOVE    WRK-DATE        TO  SNINF01-SHOKAIYMD (IDXA)
           ELSE
               MOVE    PTNYUINRRK-REC  TO  TRRK-REC
               PERFORM 900-PTNYUINRRK-KEY30-SEL-SEC
               MOVE    TRRK-NYUINYMD   TO  WRK-SYMD
               PERFORM 800-DATE-SEC
               MOVE    WRK-DATE        TO  SNINF01-SHOKAIYMD (IDXA)
           END-IF
      *
          .
       200111-SHOKAIYMD-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入院料編集処理
      *****************************************************************
       200111-NYUINRYO-EDIT-SEC            SECTION.
      *
           MOVE    ZERO                    TO  FLG-SENTEI
      *
           INITIALIZE                      SI01-AREA
           MOVE    PTNYUINRRK-PTID     TO  SI01-PTID
           MOVE    WRK-SRYYMD          TO  SI01-SRYYMD
           MOVE    PTNYUINRRK-NYUINYMD TO  SI01-NYUINYMD
           MOVE    PTNYUINRRK-BTUNUM   TO  SI01-BTUNUM
           CALL    "ORCSI01"           USING
                                       SI01-AREA
                                       SPA-AREA
           INITIALIZE                      WRK-NYUINRYO-G
           INITIALIZE                      WRK-KSNSRYCD-G
      *
           PERFORM 900-NYUINACCT-KEY7-SEL-SEC
      *
           PERFORM UNTIL ( FLG-NYUINACCT   NOT =   ZERO )
      *
               IF    ( NACCT-ZAISKBKBN     =   "1" )
                AND  ( NACCT-DAY (WRK-SRYYMD-DD)
                                           >   ZERO )
                   IF    ( NACCT-SRYCDTOTAL    =    099999916 )
                       MOVE    1           TO  FLG-SENTEI
                   ELSE
                       PERFORM 2001111-NYUINRYO-EDIT-SEC
                   END-IF
               END-IF
      *
               IF    ( NACCT-ZAISKBKBN     =   "6" )
                AND  ( NACCT-DAY (WRK-SRYYMD-DD)
                                           >   ZERO )
                AND  ( WRK-KSNSRYCD-MAX    <   3 )
                   PERFORM 2001111-KSNSRYCD-EDIT-SEC
               END-IF
      *
               PERFORM 900-NYUINACCT-KEY7-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key7"                  TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( WRK-NYUINRYO-CD NOT =   SPACE )
               IF    ( FLG-SENTEI      =   1 )
                   MOVE    WRK-NYUINRYO-CD 
                                       TO  KEY-SRYCD
                   PERFORM 900-SENTEICDCHG-KEY2-SEL-SEC
                   IF    ( FLG-SENTEICDCHG =   ZERO )
                       MOVE    SENCHG-SENTEISRYCD
                                           TO  KEY-SRYCD
                       MOVE    WRK-SRYYMD  TO  WRK-KJNYMD
                       PERFORM 900-TENSU-KEY-SEL-SEC
                       MOVE    TNS-SRYCD   TO  WRK-NYUINRYO-CD
                       MOVE    TNS-NAME    TO  WRK-NYUINRYO-NAME
                   END-IF
               END-IF
           END-IF
      *
          .
       200111-NYUINRYO-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入院料編集処理
      *****************************************************************
       2001111-NYUINRYO-EDIT-SEC            SECTION.
      *
           PERFORM 900-NYUINACT-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-NYUINACT    NOT =   ZERO )
      *
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   5 )
                        OR   ( WRK-NYUINRYO-CD NOT =   SPACE )
      *
                   IF    ( NSRY-SRYCD (IDX0)   NOT =   SPACE )
                       MOVE    NSRY-SRYCD (IDX0)
                                           TO  KEY-SRYCD
                       MOVE    WRK-SRYYMD  TO  WRK-KJNYMD
                       PERFORM 900-TENSU-KEY-SEL-SEC
                       IF    ( TNS-DATAKBN =   1 )
                           MOVE    TNS-SRYCD
                                           TO  WRK-NYUINRYO-CD
                           MOVE    TNS-NAME
                                           TO  WRK-NYUINRYO-NAME
                       END-IF
                   END-IF
      *
               END-PERFORM
      *
               PERFORM 900-NYUINACT-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_nyuinact"          TO  MCP-TABLE
           MOVE    "key2"                  TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
          .
       2001111-NYUINRYO-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    特定入院料算定要件非該当編集処理
      *****************************************************************
       200111-TOKSNT-EDIT-SEC              SECTION.
      *
           MOVE    SPACE   TO  WRK-TOKTEI-G
           EVALUATE    BRRK-TOKUTEI-KBN
           WHEN    "2"
               EVALUATE    BRRK-TOKUTEI-NYUIN
               WHEN    "10"
                   IF    ( BRRK-NYUINYMD    >= CONST-H180401 )
                       MOVE    "190121010"     TO  WRK-TOKTEI-CD
                   END-IF
               WHEN    "11"
                   IF    ( BRRK-NYUINYMD    >= CONST-H180401 )
                       MOVE    "190121110"     TO  WRK-TOKTEI-CD
                   END-IF
               WHEN    "12"
                   IF    ( BRRK-NYUINYMD    >= CONST-H180401 )
                       MOVE    "190121210"     TO  WRK-TOKTEI-CD
                   END-IF
               END-EVALUATE
               IF    ( WRK-TOKTEI-CD   =   SPACE )
                   INITIALIZE              SYSKANRI-REC
                   MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
                   MOVE    "5110"          TO  SYS-KANRICD
                   MOVE    BRRK-TOKUTEI-NYUIN
                                           TO  SYS-KBNCD
                   MOVE    BRRK-NYUINYMD
                                           TO  SYS-STYUKYMD
                                               SYS-EDYUKYMD
                   PERFORM 900-SYSKANRI-KEY10-SEL-SEC
                   MOVE    SYSKANRI-REC    TO  SYS-5110-REC
                   MOVE    SYS-5110-SRYCD  TO  WRK-TOKTEI-CD
               END-IF
           WHEN    "3"
               INITIALIZE                  SYSKANRI-REC
               MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
               MOVE    "5111"              TO  SYS-KANRICD
               MOVE    BRRK-TOKUTEI-NYUIN
                                           TO  SYS-KBNCD
               MOVE    BRRK-NYUINYMD TO  SYS-STYUKYMD
                                               SYS-EDYUKYMD
               PERFORM 900-SYSKANRI-KEY10-SEL-SEC
               MOVE    SYSKANRI-REC        TO  SYS-5111-REC
               MOVE    SYS-5111-SRYCD      TO  WRK-TOKTEI-CD
           END-EVALUATE
      *
           IF    ( WRK-TOKTEI-CD           NOT =   SPACE )
               MOVE    BRRK-NYUINYMD TO  WRK-KJNYMD
               MOVE    WRK-TOKTEI-CD TO  KEY-SRYCD
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF    ( TNS-CDKBN-KBN  =   "A" )
                AND  ( TNS-CDKBN-KBNNUM  >= 300 )
                AND  ( TNS-CDKBN-KBNNUM  <= 399 )
                   MOVE    "算定要件非該当特定入院料"
                                       TO  SNINF01-TOKSNTCD-LBL(IDXA)
                   IF    ( BRRK-TOKU-SANTEIYOUKENKBN
                                           =   "1" )
                       MOVE    "1"     TO  SNINF01-TOKSNTKBN-DATA(IDXA)
                       MOVE    "算定要件に該当しない患者"
                                       TO  SNINF01-TOKSNTKBN-NAME(IDXA)
      *
                       MOVE    "算定要件非該当特定入院料"
                                       TO  SNINF01-TOKSNTCD-LBL(IDXA)
                       MOVE    WRK-TOKTEI-CD
                                       TO  SNINF01-TOKSNTCD-DATA(IDXA)
                       MOVE    TNS-NAME
                                       TO  SNINF01-TOKSNTCD-NAME(IDXA)
      *
                   ELSE
                       MOVE    "0"     TO  SNINF01-TOKSNTKBN-DATA(IDXA)
                       MOVE    "算定要件に該当する患者"
                                       TO  SNINF01-TOKSNTKBN-NAME(IDXA)
                   END-IF
               END-IF
           END-IF
      *
          .
       200111-TOKSNT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入院料編集処理
      *****************************************************************
       2001111-KSNSRYCD-EDIT-SEC           SECTION.
      *
           MOVE    ZERO                TO  IDXZ
      *
           PERFORM 900-NYUINACT-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-NYUINACT    NOT =   ZERO )
                    OR   ( IDXZ    >   ZERO )
      *
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   5 )
                        OR   ( IDXZ    >   ZERO )
      *
                   IF    ( NSRY-SRYCD (IDX0)   NOT =   SPACE )
                       PERFORM VARYING IDX2    FROM    1   BY  1
                               UNTIL ( IDX2    >   SI01-KSNLST-MAX )
                                OR   ( IDXZ    >   ZERO )
                           IF    ( SI01-KSNLST-SRYCD(IDX2)
                                               =   NSRY-SRYCD (IDX0))
                               MOVE    IDX2    TO  IDXZ
                           END-IF
                       END-PERFORM
                   END-IF
      *
               END-PERFORM
      *
               PERFORM 900-NYUINACT-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_nyuinact"          TO  MCP-TABLE
           MOVE    "key2"                  TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( IDXZ    >   ZERO )
               COMPUTE WRK-KSNSRYCD-MAX
                   =   WRK-KSNSRYCD-MAX    +   1
               MOVE    SI01-KSNLST-SRYCD(IDXZ)
                               TO  WRK-KSNSRYCD (WRK-KSNSRYCD-MAX)
               MOVE    SI01-KSNLST-NAME (IDXZ)
                               TO  WRK-KSNSRYCD-NAME (WRK-KSNSRYCD-MAX)
           END-IF
      *
          .
       2001111-KSNSRYCD-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入院歴情報編集処理（歴作成分）
      *****************************************************************
       20012-NYUINRRK-EDIT-SEC                SECTION.
      *
           COMPUTE IDXA    =   IDXA    +   1
      *
           MOVE    PTNYUINRRK-RRKNUM
                                   TO  SNINF01-RRKNUM (IDXA)
      *
           MOVE    "入院歴作成区分"
                                   TO  SNINF01-CREATEKBN-LBL (IDXA)
           IF    ( PTNYUINRRK-JTIKBN   =   "5" )
               IF    ( PTNYUINRRK-TAINRELKBN   =   "1" )
                   MOVE    "3"     TO  SNINF01-CREATEKBN-DATA(IDXA)
                   MOVE    "他院歴（特別な関係にある医療機関）"
                                   TO  SNINF01-CREATEKBN-NAME(IDXA)
               ELSE
                   MOVE    "2"     TO  SNINF01-CREATEKBN-DATA(IDXA)
                   MOVE    "他院歴"
                                   TO  SNINF01-CREATEKBN-NAME(IDXA)
               END-IF
           ELSE
                   MOVE    "1"     TO  SNINF01-CREATEKBN-DATA(IDXA)
                   MOVE    "自院歴"
                                   TO  SNINF01-CREATEKBN-NAME(IDXA)
           END-IF
      *
           MOVE    PTNYUINRRK-NYUINYMD TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE        TO  SNINF01-NYUINYMD (IDXA)
      *
           MOVE    PTNYUINRRK-TAIINYMD TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE        TO  SNINF01-TAIINYMD (IDXA)
      *
           MOVE    "病棟名"        TO  SNINF01-BTUNAME-LBL (IDXA)
           MOVE    PTNYUINRRK-BTUNAME
                                   TO  SNINF01-BTUNAME-DATA(IDXA)
      *
           PERFORM 200111-SHOKAIYMD-EDIT-SEC
      *
          .
       20012-NYUINRRK-EXT.
           EXIT.
      *****************************************************************
      *    食事区分編集処理
      *****************************************************************
       20011-SKJKBN-SEC                SECTION.
      *
           MOVE    SPACE           TO  WRK-SKJKBN-NAME
      *
           EVALUATE    WRK-SKJKBN
           WHEN    "0"
               MOVE    "食事なし"  TO  WRK-SKJKBN-NAME
           WHEN    "1"
               MOVE    "食事あり"  TO  WRK-SKJKBN-NAME
           WHEN    "2"
               MOVE    "食事あり（特別食）"
                                   TO  WRK-SKJKBN-NAME
           WHEN    "3"
               MOVE    "食事あり（流動食）"
                                   TO  WRK-SKJKBN-NAME
           WHEN    "A"
               MOVE    "変更なし"
                                   TO  WRK-SKJKBN-NAME
           WHEN    "B"
               MOVE    "変更なし（個別指示）"
                                   TO  WRK-SKJKBN-NAME
           WHEN    "C"
               MOVE    "個別指示"
                                   TO  WRK-SKJKBN-NAME
           END-EVALUATE
      *
          .
       20011-SKJKBN-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       800-DATE-SEC                    SECTION.
      *
           MOVE    SPACE               TO  WRK-DATE
      *
           IF    ( WRK-SYMD        NOT  =   SPACE )
               INITIALIZE                  WRK-DATE
               MOVE    WRK-SYY             TO  WRK-DATE-YY
               MOVE    WRK-SMM             TO  WRK-DATE-MM
               MOVE    WRK-SDD             TO  WRK-DATE-DD
               MOVE    "-"                 TO  WRK-DATE-FL1
               MOVE    "-"                 TO  WRK-DATE-FL2
      *
               EVALUATE    WRK-SYMD
               WHEN    "00000000"
                   MOVE    01              TO  WRK-DATE-MM
                   MOVE    01              TO  WRK-DATE-DD
               WHEN    "99999999"
                   MOVE    12              TO  WRK-DATE-MM
                   MOVE    31              TO  WRK-DATE-DD
               END-EVALUATE
      *
           END-IF
      *
           .
       800-DATE-EXT.
           EXIT.
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       800-SYMD-SEC                    SECTION.
      *
           MOVE    SPACE               TO  WRK-SYMD
      *
           IF    ( WRK-DATE        NOT  =   SPACE )
               MOVE    WRK-DATE-YY     TO  WRK-SYY
               MOVE    WRK-DATE-MM     TO  WRK-SMM
               MOVE    WRK-DATE-DD     TO  WRK-SDD
           END-IF
      *
           .
       800-SYMD-EXT.
           EXIT.
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       800-HIZUKE-SEC                      SECTION.
      *
           MOVE    SPACE           TO  WRK-EDTYMD1
      *
           IF  ( WRK-SYMD          =   ALL "9"  OR   ZERO )
               MOVE    WRK-SYMD (1:8)
                                   TO  WRK-EDTYMD1
           ELSE
               INITIALIZE              STS-AREA-DAY
               INITIALIZE              LNK-DAY2-AREA
               MOVE    "21"        TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD    TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"       USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
               IF    ( STS-DAY-RC1     =   ZERO )
                   MOVE    LNK-DAY2-EDTYMD1
                                   TO  WRK-EDTYMD1
               END-IF
           END-IF
      *
           .
       800-HIZUKE-EXT.
           EXIT.
      *****************************************************************
      *    時刻編集処理
      *****************************************************************
       800-TIME-SEC                    SECTION.
      *
           INITIALIZE                      WRK-TIME
           MOVE    WRK-HMS-HH          TO  WRK-TIME-HH
           MOVE    WRK-HMS-MM          TO  WRK-TIME-MM
           MOVE    WRK-HMS-SS          TO  WRK-TIME-SS
           MOVE    ":"                 TO  WRK-TIME-FL1
           MOVE    ":"                 TO  WRK-TIME-FL2
      *
           .
       800-TIME-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
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
      *    患者情報取得処理
      *****************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTINF
      *
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTINF-PTID
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
      *    入院履歴検索処理
      *****************************************************************
       900-PTNYUINRRK-KEY30-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           MOVE    TRRK-REC            TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key30"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  TRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
               MOVE    "00000000"      TO  TRRK-NYUINYMD
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key30"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNYUINRRK-KEY30-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理
      *****************************************************************
       900-PTNYUINRRK-KEY-SEL-SEC      SECTION.
      *
           MOVE    1                   TO  BRRK-RRKEDANUM
           MOVE    BRRK-REC            TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  BRRK-REC
           ELSE
               INITIALIZE                  BRRK-REC
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNYUINRRK-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理
      *****************************************************************
       900-PTNYUINRRK-KEY59-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
           INITIALIZE                      PTNYUINRRK-REC
      *
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-PTID            TO  PTNYUINRRK-PTID
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key59"             TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               PERFORM 900-PTNYUINRRK-KEY59-FET-SEC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
      *
           .
       900-PTNYUINRRK-KEY59-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴FETCH処理
      *****************************************************************
       900-PTNYUINRRK-KEY59-FET-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
           MOVE    ZERO                TO  FLG-LOOPEND
      *
           PERFORM UNTIL ( FLG-PTNYUINRRK  NOT =   ZERO )
                    OR   ( FLG-LOOPEND     NOT =   ZERO )
      *
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key59"             TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
      *
                   MOVE    1               TO  FLG-LOOPEND
                   IF    ( KEY-SHONUM  NOT =   ZERO )
                       IF    ( PTNYUINRRK-SHONUM
                                           =   KEY-SHONUM )
                           CONTINUE
                       ELSE
                           MOVE    ZERO    TO  FLG-LOOPEND
                       END-IF
                   END-IF
      *
                   IF    ( KEY-RRKNUM  NOT =   ZERO )
                       IF    ( PTNYUINRRK-RRKNUM
                                           =   KEY-RRKNUM )
                           CONTINUE
                       ELSE
                           MOVE    ZERO    TO  FLG-LOOPEND
                       END-IF
                   END-IF
      *
               ELSE
                   MOVE    1               TO  FLG-PTNYUINRRK
                   INITIALIZE                  PTNYUINRRK-REC
               END-IF
      *
           END-PERFORM
      *
           .
       900-PTNYUINRRK-KEY59-FET-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ検索処理
      *****************************************************************
       900-TENSU-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
      *
           INITIALIZE  TNS-TENSU-REC
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    KEY-SRYCD           TO  TNS-SRYCD
           MOVE    WRK-KJNYMD          TO  TNS-YUKOSTYMD
                                           TNS-YUKOEDYMD
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC      =   ZERO )
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
           ELSE
               MOVE    1               TO  FLG-TENSU
               INITIALIZE                  TNS-TENSU-REC
           END-IF
      *
      *    点数マスタクローズ
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-TENSU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院会計検索処理
      *****************************************************************
       900-NYUINACCT-KEY7-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
           INITIALIZE                      NYUINACCT-REC
      *
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    "1"                 TO  NACCT-NYUGAIKBN
           MOVE    SPA-PTID            TO  NACCT-PTID
           MOVE    WRK-SRYYMD-YM       TO  NACCT-SRYYM
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           ELSE
               MOVE    1               TO  FLG-NYUINACCT
               INITIALIZE                  NYUINACCT-REC
           END-IF
      *
           .
       900-NYUINACCT-KEY7-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院会計FETCH処理
      *****************************************************************
       900-NYUINACCT-KEY7-FET-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           ELSE
               MOVE    1               TO  FLG-NYUINACCT
               INITIALIZE                  NYUINACCT-REC
           END-IF
      *
           .
       900-NYUINACCT-KEY7-FET-EXT.
           EXIT.
      *****************************************************************
      *    入院診療行為検索処理
      *****************************************************************
       900-NYUINACT-KEY2-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACT
           INITIALIZE                      NYUINACT-REC
      *
           MOVE    NACCT-HOSPNUM       TO  NSRY-HOSPNUM
           MOVE    NACCT-NYUGAIKBN     TO  NSRY-NYUGAIKBN
           MOVE    NACCT-PTID          TO  NSRY-PTID
           MOVE    NACCT-SRYKA         TO  NSRY-SRYKA
           MOVE    NACCT-SRYYM         TO  NSRY-SRYYM
           MOVE    NACCT-ZAINUM        TO  NSRY-ZAINUM
           MOVE    NYUINACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF   (  MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACT-REC
           ELSE
               INITIALIZE                  NYUINACT-REC
               MOVE    1               TO  FLG-NYUINACT
           END-IF
      *
           .
       900-NYUINACT-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院診療行為検索処理
      *****************************************************************
       900-NYUINACT-KEY2-FET-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACT
      *
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF   (  MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACT-REC
           ELSE
               INITIALIZE                  NYUINACT-REC
               MOVE    1               TO  FLG-NYUINACT
           END-IF
      *
           .
       900-NYUINACT-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    選定療養コード置換検索処理
      *****************************************************************
       900-SENTEICDCHG-KEY2-SEL-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-SENTEICDCHG
      *
           INITIALIZE                      SENTEICDCHG-REC
           MOVE    KEY-SRYCD           TO  SENCHG-IPNSRYCD
           MOVE    SENTEICDCHG-REC     TO  MCPDATA-REC
           MOVE    "tbl_senteicdchg"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SENTEICDCHG-REC
           ELSE
               MOVE    1               TO  FLG-SENTEICDCHG
               INITIALIZE                  SENTEICDCHG-REC
           END-IF
      *
           MOVE    "tbl_senteicdchg"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SENTEICDCHG-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    分娩管理検索処理
      *****************************************************************
       900-BUNBEN-KEY3-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-BUNBEN
           INITIALIZE                  BUNBEN-REC
      *
           MOVE    SPA-HOSPNUM     TO  BUNBEN-HOSPNUM
           MOVE    SPA-PTID
                                   TO  BUNBEN-PTID
           MOVE    "1"             TO  BUNBEN-NYUGAIKBN
           MOVE    PTNYUINRRK-RRKNUM
                                   TO  BUNBEN-RRKNUM
           MOVE    BUNBEN-REC      TO  MCPDATA-REC
      *
           MOVE    "tbl_bunben"    TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  BUNBEN-REC
           ELSE
               INITIALIZE              BUNBEN-REC
               MOVE    1           TO  FLG-BUNBEN
           END-IF
      *
           MOVE    "tbl_bunben"    TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-BUNBEN-KEY3-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
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
      *    ＤＢ検索処理（FETCHはしない）
      *****************************************************************
       911-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
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
           MOVE    "DBFETCH"       TO  MCP-FUNC
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
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
