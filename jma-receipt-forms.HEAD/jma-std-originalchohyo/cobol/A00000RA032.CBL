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
       PROGRAM-ID.             A00000RA032.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為
      *  コンポーネント名  : お薬手帳印刷用パラメタ作成
      *  管理者            : 
      *  作成日付    作業者        記述
      *  08/03/05    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *  作成日付    作業者    記述
      *  16/08/08    萩野      薬袋新規作成
      *  16/09/26    萩野      院内分、院外分の印字をプログラムオプションで
      *                        選択できるようにする
      *  20/03/25    萩野      コメント桁数変更
      *****************************************************************
      *
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
      *FILE-CONTROL.
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PARA            PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-WKSRYACT        PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-PTCOM           PIC 9(01).
           03  FLG-PTCONF          PIC 9(01).
      *
           03  FLG-KAISU           PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-ZAITAKU         PIC 9(01).
           03  FLG-PRINT           PIC 9(01).
           03  FLG-SYORI           PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-TAIGAI          PIC 9(01).
           03  FLG-OK2             PIC 9(01).
           03  FLG-ZAITAKU-ARI     PIC 9(01).
           03  FLG-HENSYU          PIC 9(01).
           03  FLG-YKJOKUKBN       PIC 9(01).
      *H26.10
           03  FLG-NO              PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX-SRY             PIC 9(04).
           03  IDXA                PIC 9(04).
           03  IDXB                PIC 9(04).
           03  IDX-B               PIC 9(04).
           03  IDX-S               PIC 9(04).
           03  IDX-T               PIC 9(04).
           03  IDX-T1              PIC 9(04).
           03  IDX-T2              PIC 9(04).
           03  IDX-T3              PIC 9(04).
           03  IDX-T4              PIC 9(04).
           03  IDX-L               PIC 9(04).
           03  IDX-L1              PIC 9(04).
           03  IDX-L2              PIC 9(04).
      *
           03  IDX-ACT             PIC 9(02).
           03  IDX-REN             PIC 9(04).
           03  IDX-JYU             PIC 9(04).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME            PIC 9(08).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RENNUM          PIC 9(01).
           03  WRK-ZAINUM          PIC 9(08).
      *
           03  WRK-SRYKBN              PIC X(02).
           03  WRK-SRYSYUKBN           PIC X(03).
      *    剤内容
           03  WRK-KAISU           PIC 9(05).
           03  WRK-ZAITENKEI       PIC 9(07).
           03  WRK-YKZTEN          PIC 9(07).
           03  WRK-KIZAITEN        PIC 9(07).
      *
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WYY         PIC X(03).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-HENYMDG         PIC X(22).
           03  WRK-SRYYMD          PIC X(08).
      *
           03  WRK-HKNCOMBINUM     PIC  X(04).
           03  WRK-SRYKA           PIC  X(02).
      *
      *    プログラム名
           03  WRK-OKUSURI-PG       PIC X(12).
      *
           03  WRK-DENPNUM         PIC 9(07).
           03  WRK-JYU-DENPNUM     PIC 9(07).
      *
           03  WRK-DRCD            PIC X(05).
      *
           03  WRK-CNT             PIC 9(03).
           03  WRK-TMPCNT          PIC 9(03).
      *
      *    用法テーブル
           03  WRK-YOHOU-G.
               05  WRK-YOHOU-TBL       OCCURS  10.
                   07  WRK-YOHOU           PIC X(09).
                   07  WRK-YOHOU-KBN       PIC X(01).
           03  WRK-YSRYCD              PIC X(09).
           03  IDXY                    PIC 9(04).
           03  FLG-YSET                PIC 9(01).
      *
      *ver.4.7
           03  WRK-SHO-SRYCD           PIC X(09).
      *
      *    ＱＲ設定区分
           03  WRK-QRKBN               PIC X(01).
           03  WRK-CSVKBN              PIC X(01).
           03  WRK-PRTKBN              PIC X(01).
      *
       01  TBL-DATA-AREA.
      *    ドクター毎
           03  TBL-DATA-OCC            OCCURS   10.
               05  TBL-DRCD            PIC X(05).
               05  TBL-DENPNUM         PIC 9(07)
                                       OCCURS   10.
               05  TBL-MAX             PIC 9(03).
      *        処方
               05  TBL-DRTBL-OCC       OCCURS   100.
      *            剤回数
                   07  TBL-ZAIKAIKEI       PIC 9(03).
      *            診療区分
                   07  TBL-SRYKBN          PIC X(02).
      *            診療種別
                   07  TBL-SRYSYUKBN       PIC X(03).
      *            剤番号
                   07  TBL-ZAINUM          PIC  9(08).
      *            剤内容
                   07  TBL-NAI-TBL         OCCURS   50.
      *                コード
                       09  TBL-SRYCD           PIC  X(09).
      *                商品コード
                       09  TBL-SHO-SRYCD       PIC  X(09).
      *                数量
                       09  TBL-SRYSURYO        PIC  9(05)V9(05).
      *                換算値数量
                       09  TBL-KANSURYO        PIC  9(05)V9(05).
      *                回数（外用薬）
                       09  TBL-SRYKAISU        PIC  9(03).
      *                名称入力番号
                       09  TBL-INPUTCOMENT     PIC  X(140).
      *                用量（0992000)
                       09  TBL-YOURYOU-G.
                           11  TBL-YOURYOU-1   PIC 9(05)V9(05).
                           11  TBL-YOURYOU-2   PIC 9(05)V9(05).
                           11  TBL-YOURYOU-3   PIC 9(05)V9(05).
                           11  TBL-YOURYOU-4   PIC 9(05)V9(05).
                           11  TBL-YOURYOU-5   PIC 9(05)V9(05).
      *            用法コード
                   07  TBL-YSRYCD              PIC X(09).
      *        処方コメント
               05  TBL-COM-MAX             PIC 9(03).
               05  TBL-COMMENT-TBL         OCCURS   10.
      *            剤番号
                   07  TBL-COM-ZAINUM          PIC  9(08).
      *            剤内容
                   07  TBL-COM-NAI-TBL         OCCURS   50.
      *                コード
                       09  TBL-COM-SRYCD        PIC  X(09).
      *                名称入力番号
                       09  TBL-COM-INPUTCOMENT  PIC  X(140).
      *    剤内容テーブル
       01  WRK-SRYACT-GRP-AREA.
           03  WRK-TBL-DRCD            PIC X(05).
           03  WRK-SRYACT-TBL          OCCURS   50.
      *診療コード
               05  WRK-SRY-SRYCD           PIC  X(09).
      *数量
               05  WRK-SRY-SRYSURYO        PIC  9(05)V9(05).
      *回数
               05  WRK-SRY-SRYKAISU        PIC  9(03).
      *明細請求区分
               05  WRK-SRY-MEISKYFLG       PIC  X(01).
      *自動算定区分
               05  WRK-SRY-AUTOKBN         PIC  X(01).
      *名称入力番号
               05  WRK-SRY-INPUTNUM        PIC  9(03).
      *換算値数量
               05  WRK-SRY-KANSURYO        PIC  9(05)V9(05).
      *自費金額
               05  WRK-SRY-JIHIMONEY       PIC  9(07).
      *名称
               05  WRK-SRY-COM-NAME        PIC X(140).
      *
           03  WRK-SRY-MAX                 PIC 9(03).
      *    診療行為マスタワーク
       01  TBL-SRYACT-AREA.
           02  TBL-SRYACT-REC         OCCURS   10.
           COPY    "CPSRYACT.INC"     REPLACING
                                      //SRY-// BY //TBL-SRY-//.
      *
      *    診療行為マスタワーク
       01  LNK-WKSRYACT-AREA.
           02  LNK-WKSRYACT-REC      OCCURS   100.
           COPY    "CPWKSRYACT.INC"  REPLACING
                                     //WKSRY-// BY //LNK-WKSRY-//.
           03  LNK-WKSRY-SYORI         PIC X(01).
      *
       01  LNK-WKSRYACT-MAX           PIC 9(04).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    帳票ＰＧ
           COPY    "CPSK1032.INC".
      *    診療行為管理情報 
           COPY    "CPSK1038.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    診療行為マスタ−
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    ワーク診療行為マスタ−
       01  WKSRYACT-REC.
           COPY    "CPWKSRYACT.INC".
      *
      *    患者コメント
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *
      *    患者個別設定
       01  PTCONF-REC.
           COPY    "CPPTCONF.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    お薬情報出力パラメタ
      *     COPY    "CPORCHC62.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    パラメタ
           COPY    "CPORCSCH62.INC".
           COPY    "CPORCHC62.INC".
      *
       01  H016-DR-AREA.
           03  H016-DRCD             PIC X(05).
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCSCH62AREA
           ORCHC62AREA
           H016-DR-AREA
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
           INITIALIZE                  TBL-DATA-AREA
      *
           MOVE    ORCHC62-CNT         TO  WRK-TMPCNT
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *
           EVALUATE    ORCSCH62-KBN
               WHEN    "1"
               WHEN    "2"
      *            更新時発行
                   PERFORM 100-SRYACT-SYORI-SEC
               WHEN    "3"
               WHEN    "4"
      *            中途終了時発行
                   PERFORM 200-WKSRYACT-SYORI-SEC
               WHEN    "5"
      *            内容変更発行
                   PERFORM 300-ORCSCH62-SYORI-SEC
      *--- 2016/09/26 START ------------------------------------------
               WHEN    "6"
      *            更新時発行（院内院外）
                   PERFORM 100-SRYACT-SYORI-SEC
               WHEN    "7"
      *            中途終了時発行（院内院外）
                   PERFORM 200-WKSRYACT-SYORI-SEC
      *--- 2016/09/26 END   ------------------------------------------
           END-EVALUATE
      *
      *    印刷処理へ
           PERFORM 300-HC62-SYORI-SEC
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                   SECTION.
      *
      *    お薬手帳出力
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    "00000009"          TO  ORCSPRTNM-KBNCD
           MOVE    SPA-SRYYMD          TO  ORCSPRTNM-SRYYMD
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYS-1032-REC
           IF      ORCSPRTNM-RC        =   ZERO
               MOVE    ORCSPRTNM-PRTPG     TO  WRK-OKUSURI-PG
           ELSE
               MOVE    1                   TO  FLG-ERR
           END-IF
      *
           MOVE    ZERO                TO  FLG-YKJOKUKBN
      *    システム管理検索（診療行為一括入金管理情報）
           MOVE    SPACE               TO  SYS-1038-REC
           INITIALIZE                  SYS-1038-REC
           MOVE    "1038"              TO  SYS-1038-KANRICD
           MOVE    "*"                 TO  SYS-1038-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-1038-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1038-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1038-HOSPNUM
           MOVE    SYS-1038-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  SYS-1038-REC
      *            包括保険を対象とする
                   IF      SYS-1038-YKJOKUKBN  =   "1"
                       MOVE    1               TO  FLG-YKJOKUKBN
                   END-IF
               END-IF
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *ver.4.7
      *    ＱＲ出力設定
      *     MOVE    SPACE               TO  PTCONF-REC
      *     INITIALIZE                  PTCONF-REC
      *     MOVE    SPA-HOSPNUM         TO  PTCONF-HOSPNUM
      *     MOVE    SPA-PTID            TO  PTCONF-PTID
      *     MOVE    "MNOTEQR"           TO  PTCONF-CKEY
      *     PERFORM 970-PTCONF-READ-SEC
      *     IF      FLG-PTCONF          =   ZERO
      *         MOVE    PTCONF-CDATA(1:1)   TO  WRK-QRKBN
      *     END-IF
      *    データフォーマット区分設定
      *     MOVE    SPACE               TO  PTCONF-REC
      *     INITIALIZE                  PTCONF-REC
      *     MOVE    SPA-HOSPNUM         TO  PTCONF-HOSPNUM
      *     MOVE    SPA-PTID            TO  PTCONF-PTID
      *     MOVE    "MNOTEVER"          TO  PTCONF-CKEY
      *     PERFORM 970-PTCONF-READ-SEC
      *     IF      FLG-PTCONF          =   ZERO
      *         MOVE    PTCONF-CDATA(1:1)   TO  WRK-CSVKBN
      *     END-IF
      *    ＣＳＶ出力のみの設定
      *     IF     (ORCSCH62-KBN2       =   "1"   )
      *        AND (WRK-CSVKBN      NOT =   SPACE )
      *         MOVE    "1"                 TO  WRK-PRTKBN
      *     END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理
      *****************************************************************
       300-HC62-SYORI-SEC             SECTION.
      *
           MOVE    ZERO                TO  WRK-CNT
           EVALUATE  ORCSCH62-KBN
             WHEN  "3"
      *--- 2016/09/26 START ------------------------------------------
             WHEN  "7"
      *--- 2016/09/26 END   ------------------------------------------
               PERFORM VARYING  IDX-T1  FROM  1  BY  1
                       UNTIL   (IDX-T1  >  10 )
                 IF  H016-DRCD  =  TBL-DRCD (IDX-T1)
                   PERFORM 3001-ORCHC60-HENSYU-SEC
                   MOVE    10                  TO  IDX-T1
                 END-IF
               END-PERFORM
             WHEN  OTHER
               PERFORM VARYING     IDX-T1  FROM    1   BY  1
                       UNTIL      (IDX-T1  >   10   )
                   IF      TBL-MAX (IDX-T1)    NOT =   ZERO
                     IF      IDX-T1    =   WRK-TMPCNT
                       PERFORM 3001-ORCHC60-HENSYU-SEC
                     END-IF
                   END-IF
               END-PERFORM
           END-EVALUATE
      *
           .
       300-HC62-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理
      *****************************************************************
       3001-ORCHC60-HENSYU-SEC             SECTION.
      *
           MOVE    ZERO                TO  IDX-T2
           MOVE    ZERO                TO  IDX-T3
           INITIALIZE                  ORCHC62AREA
      *    診療日
           MOVE    SPA-SRYYMD          TO  ORCHC62-SRYYMD
           ADD     1                   TO  WRK-CNT
           MOVE    WRK-CNT             TO  ORCHC62-CNT
      *ver.4.7
      *    ＱＲ区分など
      *     MOVE    WRK-QRKBN           TO  ORCHC62-QRKBN
      *     MOVE    WRK-CSVKBN          TO  ORCHC62-CSVKBN
      *     MOVE    WRK-PRTKBN          TO  ORCHC62-PRTKBN
      *     MOVE    ORCSCH62-PGKBN      TO  ORCHC62-PGKBN
      *    ドクター
           MOVE    TBL-DRCD (IDX-T1)   TO  ORCHC62-DRCD
      *    伝票番号
           IF     (ORCSCH62-KBN        =   "1"
      *--- 2016/09/26 START ------------------------------------------
      *                                 OR  "2" )
                                       OR  "2"
                                       OR  "6" )
      *--- 2016/09/26 END   ------------------------------------------
              AND (ORCSCH62-DENPNUM    NOT =   ZERO )
               MOVE    ORCSCH62-DENPNUM    TO  ORCHC62-DENPNUM
               PERFORM VARYING     IDX-T2  FROM    1   BY  1
                       UNTIL      (IDX-T2      >   10 )
                   MOVE    TBL-DENPNUM (IDX-T1 IDX-T2)
                                       TO  ORCHC62-GRP-DENPNUM(IDX-T2)
               END-PERFORM
           END-IF
      *
      *    投薬内容
           PERFORM VARYING     IDX-T2  FROM    1   BY  1
                   UNTIL      (IDX-T2      >   100 )
                          OR  (IDX-T2      >   TBL-MAX (IDX-T1))
               ADD     1               TO  ORCHC62-ZAI-MAX
               MOVE    TBL-ZAIKAIKEI (IDX-T1 IDX-T2)
                                       TO  ORCHC62-ZAIKAIKEI (IDX-T2)
               MOVE    TBL-SRYKBN (IDX-T1 IDX-T2)
                                       TO  ORCHC62-SRYKBN    (IDX-T2)
               MOVE    TBL-SRYSYUKBN (IDX-T1 IDX-T2)
                                       TO  ORCHC62-SRYSYUKBN (IDX-T2)
               MOVE    TBL-ZAINUM (IDX-T1 IDX-T2)
                                       TO  ORCHC62-ZAINUM   (IDX-T2)
               PERFORM VARYING     IDX-T3  FROM    1   BY  1
                       UNTIL      (IDX-T3      >   50  )
                              OR  (TBL-SRYCD (IDX-T1 IDX-T2 IDX-T3)
                                               =   SPACE  )
      *            診療明細
                   MOVE    TBL-SRYCD (IDX-T1 IDX-T2 IDX-T3)
                                           TO  ORCHC62-SRYCD
                                                   (IDX-T2 IDX-T3)
      *H24.9       商品コード
                   MOVE    TBL-SHO-SRYCD (IDX-T1 IDX-T2 IDX-T3)
                                           TO  ORCHC62-SHO-SRYCD
                                                   (IDX-T2 IDX-T3)
                   MOVE    TBL-SRYSURYO (IDX-T1 IDX-T2 IDX-T3)
                                           TO  ORCHC62-SRYSURYO
                                                   (IDX-T2 IDX-T3)
                   MOVE    TBL-KANSURYO (IDX-T1 IDX-T2 IDX-T3)
                                           TO  ORCHC62-KANSURYO
                                                   (IDX-T2 IDX-T3)
                   MOVE    TBL-SRYKAISU (IDX-T1 IDX-T2 IDX-T3)
                                           TO  ORCHC62-SRYKAISU
                                                   (IDX-T2 IDX-T3)
                   MOVE    TBL-INPUTCOMENT (IDX-T1 IDX-T2 IDX-T3)
                                           TO  ORCHC62-INPUTCOMENT
                                                   (IDX-T2 IDX-T3)
      *            用量
      *             MOVE    TBL-YOURYOU-G (IDX-T1 IDX-T2 IDX-T3)
      *                                     TO  ORCHC62-YOURYOU-TBL
      *                                         (IDX-T2 IDX-T3)
               END-PERFORM
      *        用法コード
      *         MOVE    TBL-YSRYCD (IDX-T1 IDX-T2)
      *                                 TO  ORCHC62-YSRYCD   (IDX-T2)
           END-PERFORM
      *    処方備考コメント
           PERFORM VARYING     IDX-T2  FROM    1   BY  1
                   UNTIL      (IDX-T2      >   100 )
                          OR  (IDX-T2      >   TBL-COM-MAX (IDX-T1))
               ADD     1               TO  ORCHC62-ZAI-MAX
               MOVE    ORCHC62-ZAI-MAX TO  IDX-B
               MOVE    1               TO  ORCHC62-ZAIKAIKEI (IDX-B)
               MOVE    "98"            TO  ORCHC62-SRYKBN    (IDX-B)
               MOVE    "980"           TO  ORCHC62-SRYSYUKBN (IDX-B)
               MOVE    TBL-COM-ZAINUM (IDX-T1 IDX-T2)
                                       TO  ORCHC62-ZAINUM   (IDX-B)
               PERFORM VARYING     IDX-T3  FROM    1   BY  1
                       UNTIL      (IDX-T3      >   50  )
                              OR  (TBL-COM-SRYCD (IDX-T1 IDX-T2 IDX-T3)
                                               =   SPACE  )
      *            診療明細
                   MOVE    TBL-COM-SRYCD (IDX-T1 IDX-T2 IDX-T3)
                                           TO  ORCHC62-SRYCD
                                                   (IDX-B IDX-T3)
                   MOVE    TBL-COM-INPUTCOMENT (IDX-T1 IDX-T2 IDX-T3)
                                           TO  ORCHC62-INPUTCOMENT
                                                   (IDX-B IDX-T3)
               END-PERFORM
           END-PERFORM
      *
      *    印刷処理
      *     PERFORM 300-HC62-PRINT-SEC
           .
       3001-ORCHC60-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷処理
      *****************************************************************
      * 300-HC62-PRINT-SEC             SECTION.
      *
      *     CALL    WRK-OKUSURI-PG          USING
      *                                     SPA-AREA
      *                                     ORCHC62AREA
      *
      *     .
      * 300-HC62-PRINT-EXT.
      *     EXIT.
      *
      *****************************************************************
      *    更新時発行処理
      *****************************************************************
       100-SRYACT-SYORI-SEC             SECTION.
      *
           IF      ORCSCH62-DENPNUM    =   ZERO
               MOVE    ZERO                TO  WRK-RENNUM
               PERFORM 1001-RENNUM-SET-SEC
           ELSE
               MOVE    SPA-SRYYMD          TO  WRK-SRYYMD
               MOVE    SPA-HKNCOMBINUM     TO  WRK-HKNCOMBINUM
               MOVE    ORCSCH62-DENPNUM    TO  WRK-DENPNUM
      *        編集
               PERFORM 1002-SYOHOU-SYORI-SEC
           END-IF
      *
           .
       100-SRYACT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴の連番確定処理
      *****************************************************************
       1001-RENNUM-SET-SEC                SECTION.
      *
           MOVE    ZERO                TO  FLG-PRINT
      *    受診履歴検索
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                      JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    SPA-SRYKA           TO  JYURRK-SRYKA
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 970-JYURRK-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-JYURRK
           END-IF
           PERFORM
                   UNTIL       FLG-JYURRK      =   1
               MOVE    JYURRK-RENNUM       TO  WRK-RENNUM
               MOVE    JYURRK-SRYYMD       TO  WRK-SRYYMD
               MOVE    ZERO                TO  FLG-SYORI
               IF      SPA-HKNCOMBINUM     =   SPACE
                   MOVE    JYURRK-HKNCOMBINUM  TO  WRK-HKNCOMBINUM
                   MOVE    1                   TO  FLG-SYORI
               ELSE
                   IF      SPA-HKNCOMBINUM     =   JYURRK-HKNCOMBINUM
                       MOVE    1               TO  FLG-SYORI
                   END-IF
               END-IF
      *        包括保険対象外
               IF     (WRK-HKNCOMBINUM     =   "9999")
               AND    (FLG-YKJOKUKBN       =   ZERO  )
                   MOVE    ZERO                TO  FLG-SYORI
               END-IF
      *        編集
               IF      FLG-SYORI           =   1
                   MOVE    JYURRK-DENPNUM      TO  WRK-DENPNUM
                   PERFORM 1002-SYOHOU-SYORI-SEC
               END-IF
               IF      FLG-PRINT           =   1
                   MOVE    1               TO  FLG-JYURRK
               ELSE
                   MOVE    ZERO            TO  FLG-JYURRK
                   MOVE    "key7"          TO  MCP-PATHNAME
                   PERFORM 970-JYURRK-NEXT-SEC
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       1001-RENNUM-SET-EXT.
           EXIT.
      *****************************************************************
      *    処方内容編集処理
      *****************************************************************
       1002-SYOHOU-SYORI-SEC          SECTION.
      *
      *    受診履歴検索
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                      JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    WRK-DENPNUM         TO  JYURRK-DENPNUM
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 970-JYURRK-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-JYURRK
               INITIALIZE                      JYURRK-REC
           END-IF
           IF      JYURRK-GRP-DENPNUM  =   ZERO
      *         単独分
                PERFORM 10021-ONLY-SYORI-SEC
           ELSE
      *         複数分
                PERFORM 10022-FUKU-SYORI-SEC
           END-IF
      *
           .
       1002-SYOHOU-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    単独分編集処理
      *****************************************************************
       10021-ONLY-SYORI-SEC          SECTION.
      *
           PERFORM
                   UNTIL       FLG-JYURRK      =   1
      *        包括保険対象外
               IF     (JYURRK-HKNCOMBINUM      =   "9999")
               AND    (FLG-YKJOKUKBN           =   ZERO  )
                   CONTINUE
               ELSE
                   PERFORM 100211-JYURRK-CHK-SEC
               END-IF
      *
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 970-JYURRK-NEXT-SEC
           END-PERFORM
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       10021-ONLY-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    複数分編集処理
      *****************************************************************
       10022-FUKU-SYORI-SEC          SECTION.
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    受診履歴検索
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key26"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-JYURRK
               INITIALIZE                      JYURRK-REC
           END-IF
      *
           PERFORM
                   UNTIL       FLG-JYURRK      =   1
      *
      *        包括保険対象外
               IF     (JYURRK-HKNCOMBINUM      =   "9999")
               AND    (FLG-YKJOKUKBN           =   ZERO  )
                   CONTINUE
               ELSE
                   PERFORM 100211-JYURRK-CHK-SEC
               END-IF
      *
               MOVE    "key26"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-NEXT-SEC
           END-PERFORM
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       10022-FUKU-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    処方内容編集処理
      *****************************************************************
       100211-JYURRK-CHK-SEC          SECTION.
      *
           MOVE    JYURRK-DENPNUM      TO  WRK-JYU-DENPNUM
      *
           PERFORM VARYING     IDX-JYU     FROM    1   BY  1
                   UNTIL      (IDX-JYU         >   15  )   OR
                              (JYURRK-ZAINUM (IDX-JYU)
                                               =   ZERO )
               MOVE    JYURRK-ZAINUM (IDX-JYU) TO  WRK-ZAINUM
               COMPUTE IDX-REN         =   JYURRK-RENNUM
                                       +   1
      *ver.4.8
      ***      IF      IDX-REN         >   4
      *            MOVE    4               TO  IDX-REN
      ****     END-IF
               PERFORM 2002-SRYACT-SYORI-SEC
           END-PERFORM
      *
           .
       100211-JYURRK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *        診療行為より編集処理
      *****************************************************************
       2002-SRYACT-SYORI-SEC      SECTION.
      *
           INITIALIZE                  SRYACT-REC
           MOVE    SPA-HOSPNUM         TO  SRY-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  SRY-NYUGAIKBN
           MOVE    SPA-PTID            TO  SRY-PTID
           MOVE    JYURRK-SRYKA        TO  SRY-SRYKA
           MOVE    WRK-SRYYMD(1:6)     TO  SRY-SRYYM
           MOVE    WRK-ZAINUM          TO  SRY-ZAINUM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SRYACT-READ-SEC
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           MOVE    ZERO                TO  WRK-KAISU
           MOVE    SPACE               TO  WRK-SRYKBN
           MOVE    SPACE               TO  WRK-SRYSYUKBN
           INITIALIZE                  TBL-SRYACT-AREA
           MOVE    ZERO                TO  IDX-SRY
      *
           PERFORM
                   UNTIL   FLG-SRYACT      =   1
      *        在宅・投薬
      *        注射を対象に追加
               IF     (SRY-SRYKBN          =   "14"
                                           OR  "21"
                                           OR  "22"
                                           OR  "23"
                                           OR  "31"
                                           OR  "32"
                                           OR  "33"
                                           OR  "98" )
                   ADD     1               TO  IDX-SRY
                   MOVE    SRYACT-REC      TO  TBL-SRYACT-REC (IDX-SRY)
               END-IF
      *
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SRYACT-READ-SEC
           END-PERFORM
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      IDX-SRY             >   ZERO
               PERFORM 20021-SRYACT-CHK-SEC
           END-IF
      *
           .
       2002-SRYACT-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    剤毎の対象有無チェック処理
      *****************************************************************
       20021-SRYACT-CHK-SEC         SECTION.
      *
           MOVE    TBL-SRYACT-REC (1)  TO  SRYACT-REC
           MOVE    SRY-SRYSYUKBN       TO  WRK-SRYSYUKBN
           MOVE    SRY-SRYKBN          TO  WRK-SRYKBN
           MOVE    SRY-ZAINUM          TO  WRK-ZAINUM
      *
           MOVE    ZERO                TO  FLG-TAIGAI
           MOVE    ZERO                TO  FLG-ZAITAKU-ARI
      *--- 2016/09/26 START ------------------------------------------
      *    院内の在宅判定
           IF     (ORCSCH62-KBN        =   "1"  )
             OR   (ORCSCH62-KBN        =   "6"  )
             IF   (WRK-SRYKBN          =   "14" )
      *        薬剤の有無
               PERFORM VARYING     IDXB    FROM    1   BY  1
                       UNTIL      (IDXB    >   IDX-SRY   )
                               OR (FLG-ZAITAKU-ARI =   1 )
                   PERFORM VARYING     IDXA    FROM    1   BY  1
                           UNTIL      (IDXA    >   5   )   OR
                                      (TBL-SRY-SRYCD (IDXB IDXA)
                                                       =   SPACE)
                                   OR (FLG-ZAITAKU-ARI =   1 )
                       IF     (TBL-SRY-SRYCD (IDXB IDXA)(1:1)
                                                       =   "6"  OR
                                                           "7" )  OR
                              (TBL-SRY-SRYCD (IDXB IDXA)(1:3)
                                                       =   "059" )
                           MOVE    1               TO  FLG-ZAITAKU-ARI
                       END-IF
                   END-PERFORM
               END-PERFORM
             END-IF
           END-IF
      *
      *    対象外の剤判定
           EVALUATE   ORCSCH62-KBN
      *        院外処方
               WHEN    "2"
      *            院内分は対象外
                   IF      (WRK-SRYKBN         =   "14"  )
                       IF     (WRK-SRYSYUKBN      =   "148"
                                                  OR  "149" )
                           CONTINUE
                       ELSE
                           MOVE    1               TO  FLG-TAIGAI
                       END-IF
                   END-IF
                   IF     (WRK-SRYSYUKBN       =   "213"  OR
                                                   "223"  OR
                                                   "233"  OR
                                                   "211"  OR
                                                   "221"  OR
                                                   "231"  OR
                                                   "291"  OR
                                                   "297"  OR
                                                   "294" )
      *                院内処方種別
                       MOVE    1               TO  FLG-TAIGAI
                   END-IF
      *        院内処方
               WHEN    "1"
      *            院外は対象外
                   IF      (WRK-SRYKBN         =   "14"  )
                       IF     (WRK-SRYSYUKBN       =   "148"
                                                   OR  "149" )
                          OR  (FLG-ZAITAKU-ARI     =   ZERO  )
                           MOVE    1               TO  FLG-TAIGAI
                       END-IF
      *                院内種別であれば対象
                       IF     (WRK-SRYSYUKBN       =   "141"
                                                   OR  "142" )
                           MOVE    ZERO            TO  FLG-TAIGAI
                       END-IF
                   END-IF
                   IF     (WRK-SRYSYUKBN       =   "212"  OR
                                                   "222"  OR
                                                   "232"  OR
                                                   "292"  OR
                                                   "298"  OR
                                                   "295")
                       MOVE    1               TO  FLG-TAIGAI
                   END-IF
      *--- 2016/09/26 START ------------------------------------------
               WHEN    "6"
      *            院内と院外で印字される両方の印字分を対象とする
                   IF      (WRK-SRYKBN         =   "14"  )
                       IF     (WRK-SRYSYUKBN      =   "148"
                                                  OR  "149" )
                           CONTINUE
                       ELSE
                           IF     (FLG-ZAITAKU-ARI     =   ZERO  )
                               MOVE    1               TO  FLG-TAIGAI
                           END-IF
                       END-IF
                   END-IF
      *--- 2016/09/26 END   ------------------------------------------
           END-EVALUATE
      *--- 2016/09/26 END   ------------------------------------------
      *--- 2016/09/26 START ------------------------------------------
      *     IF   ( ( WRK-SRYKBN    NOT  =  "21"
      *                                OR  "22"
      *                                OR  "23"
      *                                OR  "98" )
      *       AND  ( WRK-SRYKBN         =  "14"  AND
      *              WRK-SRYSYUKBN  NOT =  "141" AND
      *                                    "142" AND
      *                                    "148" AND
      *                                    "149" ) ) 
      *       MOVE    1               TO  FLG-TAIGAI
      *     END-IF
      *--- 2016/09/26 END   ------------------------------------------
      *
           IF      FLG-TAIGAI          =   ZERO
      *        診療会計より、点数チェック
               PERFORM 20021-SRYACCT-SYORI-SEC
           END-IF
      *--- 2016/09/26 START ------------------------------------------
           IF      FLG-TAIGAI          =   ZERO
      *        点数がゼロのものを院外処方分とする
               IF     (WRK-SRYSYUKBN       =   "210"
                                           OR  "220"
                                           OR  "230" 
                                           OR  "290" 
                                           OR  "293" 
                                           OR  "296" 
                                           OR  SPACE )
                   IF     (ORCSCH62-KBN        =   "1"  )
      *                剤点数ゼロで院内処方印刷
                       IF     (ACCT-ZAITEN         =   ZERO )
                           MOVE    1               TO  FLG-TAIGAI
                       END-IF
                   ELSE
      *--- 2016/09/26 START ------------------------------------------
                     IF     (ORCSCH62-KBN        =   "2"  )
      *--- 2016/09/26 END   ------------------------------------------
                       IF     (ACCT-ZAITEN     NOT =   ZERO )
      *                    院外処方印刷で点数あり
                           MOVE    1               TO  FLG-TAIGAI
                       END-IF
      *--- 2016/09/26 START ------------------------------------------
                     END-IF
      *--- 2016/09/26 END   ------------------------------------------
                   END-IF
      *--- 2016/09/26 END   ------------------------------------------
      ****         院内包括は対象
      *            IF      WRK-SRYSYUKBN       =   "213"
      *                                        OR  "223"
      *                                        OR  "233"
      *                MOVE    ZERO            TO  FLG-TAIGAI
      *****        END-IF
      *--- 2016/09/26 START ------------------------------------------
                END-IF
           END-IF
      *--- 2016/09/26 END   ------------------------------------------
      *
      *    処方せん印刷処理
           MOVE    TBL-SRYACT-REC (1)  TO  SRYACT-REC
           INITIALIZE                  WRK-SRYACT-GRP-AREA
           IF      FLG-TAIGAI          =   ZERO
               PERFORM VARYING     IDXB    FROM    1   BY  1
                       UNTIL      (IDXB    >   IDX-SRY   )
                   PERFORM VARYING     IDXA    FROM    1   BY  1
                           UNTIL      (IDXA    >   5   )
                       IF      TBL-SRY-SRYCD (IDXB IDXA)   NOT =   SPACE
                           ADD     1               TO  WRK-SRY-MAX
                           MOVE    TBL-SRY-SRYCD (IDXB IDXA)
                                                   TO  WRK-SRY-SRYCD
                                                           (WRK-SRY-MAX)
                           MOVE    TBL-SRY-SRYSURYO (IDXB IDXA)
                                                   TO  WRK-SRY-SRYSURYO
                                                           (WRK-SRY-MAX)
                           MOVE    TBL-SRY-SRYKAISU (IDXB IDXA)
                                                   TO  WRK-SRY-SRYKAISU
                                                           (WRK-SRY-MAX)
                           MOVE    TBL-SRY-MEISKYFLG (IDXB IDXA)
                                                   TO  WRK-SRY-MEISKYFLG
                                                           (WRK-SRY-MAX)
                           MOVE    TBL-SRY-KANSURYO (IDXB IDXA)
                                                   TO  WRK-SRY-KANSURYO
                                                           (WRK-SRY-MAX)
                           MOVE    TBL-SRY-AUTOKBN (IDXB IDXA)
                                                   TO  WRK-SRY-AUTOKBN
                                                           (WRK-SRY-MAX)
                           MOVE    TBL-SRY-INPUTNUM (IDXB IDXA)
                                                   TO  WRK-SRY-INPUTNUM
                                                           (WRK-SRY-MAX)
                           MOVE    TBL-SRY-JIHIMONEY (IDXB IDXA)
                                                   TO  WRK-SRY-JIHIMONEY
                                                           (WRK-SRY-MAX)
                       END-IF
                   END-PERFORM
               END-PERFORM
           END-IF
           IF      FLG-TAIGAI          =   ZERO
               MOVE    JYURRK-DRCD         TO  WRK-DRCD
               PERFORM 2005-TBL-DRCD-SEC
      *        内容編集処理
               IF      WRK-SRYKBN          =   "98"
                   PERFORM 20023-TBL-HENSYU98-SEC
               ELSE
                   PERFORM 20022-TBL-HENSYU-SEC
               END-IF
           END-IF
           .
       20021-SRYACT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ドクターよりテーブル位置編集処理
      *****************************************************************
       2005-TBL-DRCD-SEC         SECTION.
      *
           MOVE    ZERO                TO  IDX-T1
           PERFORM VARYING     IDXB    FROM    1   BY  1
                   UNTIL      (IDXB    >   10     )  OR
                              (IDX-T1  NOT =   ZERO )
               IF      TBL-DRCD (IDXB)     =   WRK-DRCD
                   MOVE    IDXB            TO  IDX-T1
               ELSE
                   IF      TBL-DRCD(IDXB)      =   SPACE
                       MOVE    WRK-DRCD        TO  TBL-DRCD(IDXB)
                       MOVE    IDXB            TO  IDX-T1
                   END-IF
               END-IF
           END-PERFORM
      *    伝票番号
           IF      WRK-JYU-DENPNUM NOT =   ZERO
               PERFORM VARYING     IDXB    FROM    1   BY  1
                       UNTIL      (IDXB    >   10     )
                   IF     (TBL-DENPNUM (IDX-T1 IDXB)   =   ZERO )
                   OR     (WRK-JYU-DENPNUM     =   TBL-DENPNUM
                                                      (IDX-T1 IDXB))
                       MOVE    WRK-JYU-DENPNUM TO  TBL-DENPNUM
                                                       (IDX-T1 IDXB)
                       MOVE    10              TO  IDXB
                    END-IF
               END-PERFORM
           END-IF
           .
       2005-TBL-DRCD-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル編集処理
      *****************************************************************
       20022-TBL-HENSYU-SEC         SECTION.
      *
           ADD     1                   TO  TBL-MAX    (IDX-T1)
           MOVE    TBL-MAX (IDX-T1)    TO  IDX-T2
           MOVE    WRK-SRYKBN          TO  TBL-SRYKBN (IDX-T1 IDX-T2)
           MOVE    WRK-SRYSYUKBN       TO  TBL-SRYSYUKBN (IDX-T1 IDX-T2)
           MOVE    WRK-KAISU           TO  TBL-ZAIKAIKEI (IDX-T1 IDX-T2)
           MOVE    WRK-ZAINUM          TO  TBL-ZAINUM    (IDX-T1 IDX-T2)
           MOVE    ZERO                TO  IDX-T3
           MOVE    ZERO                TO  IDX-T4
           MOVE    ZERO                TO  IDXY
           INITIALIZE                      WRK-YOHOU-G
      *ver.4.7
           MOVE    SPACE               TO  WRK-SHO-SRYCD
           PERFORM VARYING     IDXA    FROM    1   BY  1
                   UNTIL      (IDXA    >   WRK-SRY-MAX   )   OR
                              (WRK-SRY-SRYCD (IDXA)    =   SPACE)
      *ver.4.7
      *        商品名
               IF     (WRK-SRY-SRYCD (IDXA)(1:3)   =   "058" )
                   MOVE    WRK-SRY-SRYCD(IDXA)     TO  WRK-SHO-SRYCD
               END-IF
      *
      *H26.10
               MOVE    ZERO            TO  FLG-NO
               IF     (WRK-SRY-SRYCD (IDXA)(1:1) =   "6" )
      *            点数マスタを検索
                   MOVE    WRK-SRY-SRYCD (IDXA)    TO  TNS-SRYCD
                   PERFORM 910-TENSU-READ-SEC
                   IF      TNS-TENSIKIBETU     =   7
      *                減点は対象外
                       MOVE    1               TO  FLG-NO
                   END-IF
               END-IF
      *        薬剤・器材・用法・コメントのみ
               IF     (WRK-SRY-SRYCD (IDXA)(1:1)   =   "6"
                                                   OR  "7"
                                                   OR  "8" )
                  OR  (WRK-SRY-SRYCD (IDXA)(1:3)   =   "059"
                                                   OR  "001"
                                                   OR  "008")
                   IF     (WRK-SRY-SRYCD (IDXA)    =   "630010002" )
                     OR   (WRK-SRY-SRYCD (IDXA)    =   "630010004" )
      *               OR   (WRK-SRY-SRYCD (IDXA)(1:4)   =   "0086"  )
      *H26.10        （精減）
                     OR   (WRK-SRY-SRYCD (IDXA)    =   "820000166" )
                     OR   (FLG-NO                  =   1   )
                       CONTINUE
                   ELSE
                       ADD     1               TO  IDX-T3
      *
                       IF     (WRK-SRY-SRYCD (IDXA)(1:1)   =   "6")
                            MOVE    IDX-T3         TO  IDX-T4
                       END-IF
      *
                       MOVE    WRK-SRY-SRYCD (IDXA)
                                               TO  TBL-SRYCD
                                                  (IDX-T1 IDX-T2 IDX-T3)
                       MOVE    WRK-SRY-SRYSURYO (IDXA)
                                               TO  TBL-SRYSURYO
                                                  (IDX-T1 IDX-T2 IDX-T3)
                       MOVE    WRK-SRY-KANSURYO (IDXA)
                                               TO  TBL-KANSURYO
                                                  (IDX-T1 IDX-T2 IDX-T3)
                       MOVE    WRK-SRY-SRYKAISU (IDXA)
                                               TO  TBL-SRYKAISU
                                                  (IDX-T1 IDX-T2 IDX-T3)
      *ver.4.7
      *                商品名
                       IF     (WRK-SRY-SRYCD (IDXA)(1:1)   =   "7" )
                           MOVE    WRK-SHO-SRYCD   TO  TBL-SHO-SRYCD
                                                  (IDX-T1 IDX-T2 IDX-T3)
                           MOVE    SPACE           TO  WRK-SHO-SRYCD
                       END-IF
      *
                       MOVE    WRK-SRY-COM-NAME (IDXA)
                                               TO  TBL-INPUTCOMENT
                                                  (IDX-T1 IDX-T2 IDX-T3)
                       IF     (WRK-SRY-INPUTNUM (IDXA)
                                                NOT =   ZERO )
                           PERFORM 200221-PTCOM-HEN-SEC
                           MOVE    PTCOM-INPUTCOMENT
                                               TO  TBL-INPUTCOMENT
                                                  (IDX-T1 IDX-T2 IDX-T3)
                       END-IF
                   END-IF
               END-IF
      *        用量
               IF      (WRK-SRY-SRYCD (IDXA)(1:7)   =   "0992000" )
                  AND  (IDX-T4             >   ZERO   )
                  EVALUATE    WRK-SRY-SRYCD (IDXA)
      *            朝
                   WHEN    "099200011"
                       MOVE    WRK-SRY-SRYSURYO (IDXA) TO  TBL-YOURYOU-1
                                                 (IDX-T1 IDX-T2 IDX-T4)
      *            昼
                   WHEN    "099200012"
                       MOVE    WRK-SRY-SRYSURYO (IDXA) TO  TBL-YOURYOU-2
                                                (IDX-T1 IDX-T2 IDX-T4)
      *            夜
                   WHEN    "099200013"
                       MOVE    WRK-SRY-SRYSURYO (IDXA) TO  TBL-YOURYOU-3
                                                (IDX-T1 IDX-T2 IDX-T4)
      *            寝
                   WHEN    "099200014"
                       MOVE    WRK-SRY-SRYSURYO (IDXA) TO  TBL-YOURYOU-4
                                                (IDX-T1 IDX-T2 IDX-T4)
      *            起床
                   WHEN    "099200015"
                       MOVE    WRK-SRY-SRYSURYO (IDXA) TO  TBL-YOURYOU-5
                                                (IDX-T1 IDX-T2 IDX-T4)
                   END-EVALUATE
               END-IF
      *
      *        用法編集
               IF      WRK-SRY-SRYCD (IDXA)(1:3)   =   "001"
                   MOVE    ZERO                TO  FLG-YSET
      *            点数マスタを検索して、用法時点を判定する
                   MOVE    WRK-SRY-SRYCD(IDXA) TO  TNS-SRYCD
                   PERFORM 910-TENSU-READ-SEC
                   IF     (FLG-TENSU           =   ZERO      )
      *            コメントは対象外
                   AND    (TNS-KENTAICOMMENT   =   0  OR  1  )
                       IF    ((TNS-SSTKIJUNCD(1)   =   ZERO )  AND
                              (TNS-SSTKIJUNCD(2)   =   ZERO )  AND
                              (TNS-SSTKIJUNCD(3)   =   ZERO )  AND
                              (TNS-SSTKIJUNCD(4)   =   ZERO )  AND
                              (TNS-SSTKIJUNCD(5)   =   ZERO ))
                           MOVE    ZERO                TO  FLG-YSET
                       ELSE
                           MOVE    1                   TO  FLG-YSET
                       END-IF
                       ADD     1                   TO  IDXY
                       IF      IDXY                <=  10
                           MOVE    WRK-SRY-SRYCD(IDXA) TO  WRK-YOHOU
                                                               (IDXY)
                           MOVE    FLG-YSET            TO  WRK-YOHOU-KBN
                                                               (IDXY)
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
      *    用法は最後の用法コードとする
           MOVE    SPACE               TO  WRK-YSRYCD
           PERFORM VARYING     IDXY    FROM    10  BY  -1
                   UNTIL      (IDXY        <   1      )
               IF     (WRK-YOHOU(IDXY)     NOT =   SPACE )
               AND    (WRK-YOHOU-KBN(IDXY)     =   "1"   )
                   MOVE    WRK-YOHOU(IDXY)     TO  WRK-YSRYCD
                   MOVE    1                   TO  IDXY
               ELSE
                   IF     (WRK-YOHOU(IDXY) NOT =   SPACE )
                   AND    (WRK-YSRYCD          =   SPACE )
                       MOVE    WRK-YOHOU(IDXY)     TO  WRK-YSRYCD
                   END-IF
               END-IF
           END-PERFORM
           MOVE    WRK-YSRYCD          TO  TBL-YSRYCD  (IDX-T1 IDX-T2)
      *
      *    対象がない時
           IF      IDX-T3              =   ZERO
               COMPUTE TBL-MAX (IDX-T1)     =   TBL-MAX (IDX-T1)  -  1
           END-IF
           .
       20022-TBL-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    備考コメントテーブル編集処理
      *****************************************************************
       200221-PTCOM-HEN-SEC         SECTION.
      *
           MOVE    SRY-HOSPNUM         TO  PTCOM-HOSPNUM
           MOVE    SRY-PTID            TO  PTCOM-PTID
           MOVE    SRY-ZAINUM          TO  PTCOM-ZAINUM
           MOVE    WRK-SRY-SRYCD    (IDXA)
                                       TO  PTCOM-SRYCD
           MOVE    WRK-SRY-INPUTNUM (IDXA)
                                       TO  PTCOM-RENNUM
      *    主キーで検索
           PERFORM 950-PTCOM-READ-SEC
           .
       200221-PTCOM-HEN-EXT.
           EXIT.
      *****************************************************************
      *    備考コメントテーブル編集処理
      *****************************************************************
       20023-TBL-HENSYU98-SEC         SECTION.
      *
           ADD     1                   TO  TBL-COM-MAX (IDX-T1)
           MOVE    TBL-COM-MAX (IDX-T1)    TO  IDX-T2
           MOVE    SRY-ZAINUM          TO  TBL-COM-ZAINUM
                                                      (IDX-T1 IDX-T2)
           MOVE    ZERO                TO  IDX-T3
           PERFORM VARYING     IDXA    FROM    1   BY  1
                   UNTIL      (IDXA    >   WRK-SRY-MAX   )   OR
                              (WRK-SRY-SRYCD (IDXA)    =   SPACE)
      *        薬剤・器材・用法・コメントのみ
               IF     (WRK-SRY-SRYCD (IDXA)(1:1)   =   "6"
                                                   OR  "7"
                                                   OR  "8" )
                  OR  (WRK-SRY-SRYCD (IDXA)(1:3)   =   "059"
                                                   OR  "001")
                  OR  (WRK-SRY-SRYCD (IDXA)(1:4)   =   "0082"
                                                   OR  "0083"
                                                   OR  "0084"
                                                   OR  "0085"
                                                   OR  "0086")
                   ADD     1               TO  IDX-T3
                   MOVE    WRK-SRY-SRYCD (IDXA)
                                           TO  TBL-COM-SRYCD
                                                  (IDX-T1 IDX-T2 IDX-T3)
                   MOVE    WRK-SRY-COM-NAME (IDXA)
                                           TO  TBL-COM-INPUTCOMENT
                                                  (IDX-T1 IDX-T2 IDX-T3)
                   IF     (WRK-SRY-INPUTNUM (IDXA)
                                               NOT =   ZERO )
                       PERFORM 200221-PTCOM-HEN-SEC
                       MOVE    PTCOM-INPUTCOMENT
                                               TO  TBL-COM-INPUTCOMENT
                                                  (IDX-T1 IDX-T2 IDX-T3)
                   END-IF
               END-IF
           END-PERFORM
      *    対象がない時
           IF      IDX-T3              =   ZERO
               COMPUTE TBL-COM-MAX (IDX-T1)    =   TBL-COM-MAX (IDX-T1)
                                               -  1
           END-IF
           .
       20022-TBL-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    診療会計より回数編集処理
      *****************************************************************
       20021-SRYACCT-SYORI-SEC         SECTION.
      *
      *    診療会計マスター読み込み
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SPA-PTID            TO  ACCT-PTID
           MOVE    SRY-SRYKA           TO  ACCT-SRYKA
           MOVE    SRY-SRYYM           TO  ACCT-SRYYM
           MOVE    SRY-SRYKBN          TO  ACCT-SRYKBN
           MOVE    SRY-ZAINUM          TO  ACCT-ZAINUM
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 930-SRYACCT-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      ACCT-ZAIKBN         =   1
      *        薬評分外
               MOVE    1                   TO  FLG-TAIGAI
           ELSE
               MOVE    SPA-SRYYMD(7:2)     TO  IDX-ACT
               MOVE    ACCT-DAY (IDX-REN IDX-ACT)
                                           TO  WRK-KAISU
           END-IF
      *
           .
       20021-SRYACCT-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    中途終了時の発行処理
      *****************************************************************
       200-WKSRYACT-SYORI-SEC         SECTION.
      *
           INITIALIZE                  LNK-WKSRYACT-AREA
           MOVE    ZERO                TO  LNK-WKSRYACT-MAX
      *
           MOVE    SPACE               TO  WKSRYACT-REC
           INITIALIZE                  WKSRYACT-REC
           MOVE    SPA-HOSPNUM         TO  WKSRY-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  WKSRY-NYUGAIKBN
           MOVE    SPA-PTID            TO  WKSRY-PTID
           MOVE    SPA-SRYKA           TO  WKSRY-SRYKA
           MOVE    SPA-SRYYMD          TO  WKSRY-SRYYMD
           MOVE    SPA-HKNCOMBINUM     TO  WKSRY-HKNCOMBI
      *
           MOVE    WKSRYACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 940-WKSRYACT-READ-SEC
           ELSE
               INITIALIZE                  WKSRYACT-REC
               MOVE    1               TO  FLG-WKSRYACT
           END-IF
      *
           MOVE    ZERO                TO  WRK-KAISU
           MOVE    ZERO                TO  IDX-S
           MOVE    SPACE               TO  WRK-SRYKBN
           MOVE    SPACE               TO  WRK-SRYSYUKBN
           MOVE    SPA-DRCD            TO  WRK-DRCD
           PERFORM
               UNTIL   FLG-WKSRYACT    =   1
      *        ドクターの再編集
               IF      WKSRY-INPUTCD(1)(1:1)   =   "$"
                   IF      WKSRY-INPUTCD(1)(5:4)   NUMERIC
                       MOVE    "1"                 TO  WRK-DRCD(1:1)
                       MOVE    WKSRY-INPUTCD(1)(5:4)
                                                   TO  WRK-DRCD(2:4)
                   END-IF
               END-IF
               IF     (WKSRY-INPUTCD(1)(1:1)   =   "#" )
                   MOVE    WKSRY-INPUTCD(1)(2:4)   TO  WRK-HKNCOMBINUM
               END-IF
      *        投薬と在宅のみ対象とする
      *        注射を対象に追加
               IF     (WKSRY-SRYKBN        =   "21"
                                           OR  "22"
                                           OR  "23"
                                           OR  "14"
                                           OR  "31"
                                           OR  "32"
                                           OR  "33"
                                           OR  "98"  )
      *           包括保険分は対象外
                   IF     ((WRK-HKNCOMBINUM    =   "9999" )
                   AND     (FLG-YKJOKUKBN      =   ZERO   ) )
      *                薬評分対象外
                   OR      (WKSRY-ZAIKBN       =   1      )
      *                減点外
                   OR      (WKSRY-ZAIKBN       =   2      )
                       CONTINUE
                   ELSE
                       ADD     1                   TO  LNK-WKSRYACT-MAX
                       MOVE    WKSRYACT-REC        TO  LNK-WKSRYACT-REC
                                                  (LNK-WKSRYACT-MAX)
                       MOVE    WRK-DRCD            TO  LNK-WKSRY-DRCD
                                                  (LNK-WKSRYACT-MAX)
                   END-IF
               END-IF
      *
               MOVE    "tbl_wksryact"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 940-WKSRYACT-READ-SEC
           END-PERFORM
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    診療科・保険組合せ毎に処方せんを印刷する
           MOVE    SPACE               TO  WRK-DRCD
           MOVE    ZERO                TO  FLG-HENSYU
           MOVE    ZERO                TO  FLG-OK2
           PERFORM VARYING     IDX-L   FROM    1   BY  1
                   UNTIL       IDX-L   >   LNK-WKSRYACT-MAX
      *        対象チェック・編集
               MOVE    LNK-WKSRYACT-REC (IDX-L)
                                           TO  WKSRYACT-REC
      *        対象チェック・編集
               PERFORM 20032-WKSRYACT-CHK-SEC
           END-PERFORM
           IF      FLG-HENSYU          =   1
               PERFORM 20032-PRINT-HENSYU-SEC
               IF      FLG-OK2             =   1
      *            前の剤の処方せん処理
                   MOVE    WRK-TBL-DRCD         TO  WRK-DRCD
                   PERFORM 20033-WRKSRY-HENSYU-SEC
               END-IF
           END-IF
           .
       200-WKSRYACT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    中途終了データ編集処理
      *****************************************************************
       20032-WKSRYACT-CHK-SEC         SECTION.
      *
           IF      WKSRY-RENNUM        =   1
               IF      FLG-HENSYU          =   1
                   PERFORM 20032-PRINT-HENSYU-SEC
                   IF      FLG-OK2             =   1
      *                前の剤の処方せん処理
                       MOVE    WRK-TBL-DRCD         TO  WRK-DRCD
                       PERFORM 20033-WRKSRY-HENSYU-SEC
                   END-IF
                   MOVE    ZERO                TO  FLG-HENSYU
               END-IF
               INITIALIZE                  WRK-SRYACT-GRP-AREA
               MOVE    1                   TO  FLG-HENSYU
      *        回数
               MOVE    WKSRY-ZAIKAIKEI     TO  WRK-KAISU
               MOVE    WKSRY-SRYSYUKBN     TO  WRK-SRYSYUKBN
               MOVE    WKSRY-SRYKBN        TO  WRK-SRYKBN
               MOVE    ZERO                TO  WRK-ZAINUM
               MOVE    WKSRY-DRCD          TO  WRK-DRCD
           END-IF
      *
      *    在宅自己注射はすべてで対象とする
           IF     (WKSRY-SRYKBN        =   "14"    )
               MOVE    WKSRY-YKZTEN        TO  WRK-YKZTEN
               MOVE    WKSRY-KIZAITEN      TO  WRK-KIZAITEN
           END-IF
      *    投薬対象
           IF      WKSRY-SRYKBN        =   "21"  OR
                                           "22"  OR
                                           "23"
               MOVE    WKSRY-ZAITENKEI     TO  WRK-ZAITENKEI
           END-IF
      *
           PERFORM VARYING     IDXA    FROM    1   BY  1
                   UNTIL      (IDXA    >   5   )
               IF     (WKSRY-SRYCD (IDXA)(1:1) =   "6"
                                               OR  "7"
                                               OR  "8"  )
                 OR   (WKSRY-SRYCD (IDXA)(1:3) =   "001"
                                               OR  "058"
                                               OR  "059" )
                 OR   (WKSRY-SRYCD (IDXA)(1:4) =   "0082"
                                               OR  "0083"
                                               OR  "0084"
                                               OR  "0085"
                                               OR  "0086")
                 OR   (WKSRY-SRYCD (IDXA)(1:7) =   "0992000" )
                   MOVE    WKSRY-DRCD          TO  WRK-TBL-DRCD
      *
                   ADD     1                   TO  WRK-SRY-MAX
                   MOVE    WKSRY-SRYCD (IDXA)  TO  WRK-SRY-SRYCD
                                                           (WRK-SRY-MAX)
                   MOVE    WKSRY-SRYSURYO (IDXA)
                                               TO  WRK-SRY-SRYSURYO
                                                         (WRK-SRY-MAX)
                   MOVE    WKSRY-SRYKAISU (IDXA)
                                               TO  WRK-SRY-SRYKAISU
                                                           (WRK-SRY-MAX)
                   MOVE    WKSRY-AUTOKBN (IDXA)
                                               TO  WRK-SRY-AUTOKBN
                                                           (WRK-SRY-MAX)
                   MOVE    WKSRY-KANSURYO (IDXA)
                                               TO  WRK-SRY-KANSURYO
                                                         (WRK-SRY-MAX)
                   MOVE    ZERO                TO  WRK-SRY-INPUTNUM
                                                           (WRK-SRY-MAX)
                   MOVE    WKSRY-JIHIMONEY (IDXA)
                                               TO  WRK-SRY-JIHIMONEY
                                                           (WRK-SRY-MAX)
      *            名称
                   IF    ((WKSRY-SRYCD (IDXA)(1:1) =    "8")   OR
                          (WKSRY-SRYCD (IDXA)(1:3) =    "008") OR
                          (WKSRY-SRYCD (IDXA)(1:3) =    "001")) AND
                          (WKSRY-INPUTCOMENT (IDXA)
                                               NOT =   SPACE)
                       MOVE    WKSRY-INPUTCOMENT (IDXA)
                                               TO  WRK-SRY-COM-NAME
                                                           (WRK-SRY-MAX)
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       20032-WKSRYACT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    中途終了剤毎の編集処理
      *****************************************************************
       20032-PRINT-HENSYU-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-OK2
      *
      *--- 2016/09/26 START ------------------------------------------
      *    投薬で、点数がゼロのもを対象（院外）
           IF      ORCSCH62-KBN        =   "4" 
               IF     (WRK-SRYKBN          =   "21"  OR
                                               "22"  OR
                                               "23"    )  AND
                      (WRK-ZAITENKEI       =   ZERO   )
                   MOVE    1               TO  FLG-OK2
               END-IF
      *        院外の診療種別
               IF      WRK-SRYSYUKBN       =   "212"
                                           OR  "222"
                                           OR  "232"
                                           OR  "292"
                                           OR  "295"
                                           OR  "298"
                   MOVE    1               TO  FLG-OK2
               END-IF
      *        院内の診療種別
               IF      WRK-SRYSYUKBN       =   "211"
                                           OR  "221"
                                           OR  "231"
                                           OR  "291"
                                           OR  "294"
                                           OR  "297"
                                           OR  "213"
                                           OR  "223"
                                           OR  "233"
                   MOVE    ZERO            TO  FLG-OK2
               END-IF
           END-IF
      *    投薬で、点数がゼロのもを対象（院内）
           IF      ORCSCH62-KBN        =   "3"
               IF     (WRK-SRYKBN          =   "21"  OR
                                               "22"  OR
                                               "23"    )  AND
                      (WRK-ZAITENKEI       >   ZERO    )
                   MOVE    1               TO  FLG-OK2
               END-IF
      *        院内の診療種別
               IF      WRK-SRYSYUKBN       =   "211"
                                           OR  "221"
                                           OR  "231"
                                           OR  "291"
                                           OR  "294"
                                           OR  "297"
                                           OR  "213"
                                           OR  "223"
                                           OR  "233"
                   MOVE    1               TO  FLG-OK2
               END-IF
           END-IF
      *--- 2016/09/26 END   ------------------------------------------
      *--- 2016/09/26 START ------------------------------------------
           IF      ORCSCH62-KBN        =   "7"
      *--- 2016/09/26 END   ------------------------------------------
               IF     (WRK-SRYKBN          =   "21"
                                            OR "22"
                                            OR "23" )
                   MOVE    1               TO  FLG-OK2
               END-IF
      *--- 2016/09/26 START ------------------------------------------
           END-IF
      *--- 2016/09/26 END   ------------------------------------------
      *
      *--- 2016/09/26 START ------------------------------------------
      *    在宅で薬剤・器材がある時、
           IF      WRK-SRYKBN          =   "14"
               IF     (WRK-SRYSYUKBN       =   "148"  OR
                                               "149" )  AND
                      (ORCSCH62-KBN        =   "4"   )
                   MOVE    1               TO  FLG-OK2
               END-IF
               IF     (WRK-SRYSYUKBN   NOT =   "148"  AND
                                               "149" )  AND
                      (ORCSCH62-KBN         =   "3"  )
      *            在宅で薬剤・器材がある時、
                   IF     (WRK-YKZTEN      >   ZERO )  OR
                          (WRK-KIZAITEN    >   ZERO )
                    OR    (WRK-SRYSYUKBN   =   "141"
                                           OR  "142"  )
                       MOVE    1               TO  FLG-OK2
                   END-IF
               END-IF
               IF      ORCSCH62-KBN        =   "7"
                   IF     (WRK-SRYSYUKBN   =   "148"
                                           OR  "149" )
                     MOVE    1               TO  FLG-OK2
                   END-IF
                   IF     (WRK-SRYSYUKBN   NOT =   "148"
                                               AND "149" )
                       IF     (WRK-YKZTEN      >   ZERO )
                        OR    (WRK-KIZAITEN    >   ZERO )
                        OR    (WRK-SRYSYUKBN   =   "141"
                                               OR  "142"  )
                           MOVE    1               TO  FLG-OK2
                       END-IF
                   END-IF
               END-IF
           END-IF
      *--- 2016/09/26 END   ------------------------------------------
      *
      *    処方のみで、院内処方印刷の時
      *    IF      WRK-SRYKBN          =   "21"  OR  "22" OR "23"
      *        IF     (WRK-SRYSYUKBN       =   "213"  OR
      *                                        "223"  OR
      *                                        "233")   AND
      *               (ORCSCH62-KBN        =   "3"   )
      *            MOVE    1               TO  FLG-OK2
      *        END-IF
      *****END-IF
      *
      *    注射
           IF     (WRK-SRYKBN          =   "31"
                                        OR "32"
                                        OR "33")
               MOVE    1               TO  FLG-OK2
           END-IF
      *
      *    処方コメント
           IF      WRK-SRYKBN          =   "98"
               MOVE    1               TO  FLG-OK2
           END-IF
           .
       20032-PRINT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    中途終了データ編集処理
      *****************************************************************
       20033-WRKSRY-HENSYU-SEC         SECTION.
      *
           PERFORM 2005-TBL-DRCD-SEC
      *    内容編集処理
           IF      WRK-SRYKBN          =   "98"
               PERFORM 20023-TBL-HENSYU98-SEC
           ELSE
               PERFORM 20022-TBL-HENSYU-SEC
           END-IF
           .
       20033-WRKSRY-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    内容変更発行処理
      *****************************************************************
       300-ORCSCH62-SYORI-SEC         SECTION.
      *
           PERFORM VARYING     IDX-L   FROM    1   BY  1
                   UNTIL      (IDX-L   >   ORCSCH62-ZAI-MAX)
               INITIALIZE                  WRK-SRYACT-GRP-AREA
               MOVE    ORCSCH62-ZAIKAIKEI(IDX-L)   TO  WRK-KAISU
               MOVE    ORCSCH62-SRYSYUKBN(IDX-L)   TO  WRK-SRYSYUKBN
               MOVE    ORCSCH62-SRYKBN   (IDX-L)   TO  WRK-SRYKBN
               MOVE    ZERO                        TO  WRK-ZAINUM
               PERFORM VARYING     IDX-L2  FROM    1   BY  1
                       UNTIL      (IDX-L2  >   50  )   OR
                                  (ORCSCH62-SRYCD (IDX-L IDX-L2)
                                               =   SPACE )
                   IF      ORCSCH62-SRYCD (IDX-L IDX-L2)(1:4)
                                               =   "0086"
                       CONTINUE
                   ELSE
                       PERFORM 3001-ORCSCH62-TBLSET-SEC
                       MOVE    1                   TO  FLG-HENSYU
                   END-IF
               END-PERFORM
      *
               IF      FLG-HENSYU          =   1
      *            前の剤の処方せん処理
                   MOVE    SPA-DRCD            TO  WRK-DRCD
                   PERFORM 20033-WRKSRY-HENSYU-SEC
               END-IF
           END-PERFORM
      *
           .
       200-ORCSCH62-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    診療会計よりテーブル編集処理
      *****************************************************************
       3001-ORCSCH62-TBLSET-SEC         SECTION.
      *
           ADD     1                   TO  WRK-SRY-MAX
           MOVE    ORCSCH62-SRYCD (IDX-L IDX-L2)
                                       TO  WRK-SRY-SRYCD  (WRK-SRY-MAX)
           MOVE    ORCSCH62-SRYSURYO (IDX-L IDX-L2)
                                       TO  WRK-SRY-SRYSURYO
                                                          (WRK-SRY-MAX)
           MOVE    ORCSCH62-SRYKAISU (IDX-L IDX-L2)
                                       TO  WRK-SRY-SRYKAISU
                                                          (WRK-SRY-MAX)
           MOVE    ORCSCH62-KANSURYO (IDX-L IDX-L2)
                                       TO  WRK-SRY-KANSURYO
                                                          (WRK-SRY-MAX)
           IF    ((ORCSCH62-SRYCD (IDX-L IDX-L2)(1:1)
                                       =    "8")   OR
                  (ORCSCH62-SRYCD (IDX-L IDX-L2)(1:3)
                                       =    "008")  OR
                  (ORCSCH62-SRYCD (IDX-L IDX-L2)(1:3)
                                       =    "001")) AND
                  (ORCSCH62-INPUTCOMENT (IDX-L IDX-L2)
                                   NOT =   SPACE)
               MOVE    ORCSCH62-INPUTCOMENT (IDX-L IDX-L2)
                                           TO  WRK-SRY-COM-NAME
                                                         (WRK-SRY-MAX)
           END-IF
      *
           .
       3001-ORCSCH62-TBLSET-EXT.
           EXIT.
      *****************************************************************
      *    患者コメントマスタ読み込み
      *****************************************************************
       950-PTCOM-READ-SEC           SECTION.
      *
           MOVE    PTCOM-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptcom"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTCOM-REC
                   MOVE    ZERO                TO  FLG-PTCOM
               ELSE
                   MOVE    1                   TO  FLG-PTCOM
                   INITIALIZE                  PTCOM-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTCOM
               INITIALIZE                  PTCOM-REC
           END-IF
      *
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       950-PTCOM-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    SPA-SRYYMD          TO  TNS-YUKOSTYMD
           MOVE    SPA-SRYYMD          TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
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
      *****************************************************************
      *    診療行為マスター読込
      *****************************************************************
       900-SRYACT-READ-SEC         SECTION.
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACT-REC
               MOVE    ZERO                TO  FLG-SRYACT
           ELSE
               INITIALIZE                      SRYACT-REC
               MOVE    1                   TO  FLG-SRYACT
           END-IF
      *
           .
       900-SRYACT-READ-EXT.
           EXIT.
      *****************************************************************
      *    診療会計マスター読込
      *****************************************************************
       930-SRYACCT-NEXT-SEC         SECTION.
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
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
       930-SRYACCT-NEXT-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴順読み
      *****************************************************************
       970-JYURRK-NEXT-SEC           SECTION.
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  JYURRK-REC 
               MOVE    ZERO                TO  FLG-JYURRK
           ELSE
               MOVE    1                   TO  FLG-JYURRK
           END-IF
      *
           .
       970-JYURRK-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ワーク診療行為データ読込
      *****************************************************************
       940-WKSRYACT-READ-SEC           SECTION.
      *
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  WKSRYACT-REC
               MOVE    ZERO            TO  FLG-WKSRYACT
           ELSE
               INITIALIZE                  WKSRYACT-REC
               MOVE    1               TO  FLG-WKSRYACT
           END-IF
      *
           .
       940-WKSRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       960-KANRIMST-READ-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SYSKANRI
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           .
       960-KANRIMST-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者個別設定テーブル読込処理
      *****************************************************************
       970-PTCONF-READ-SEC         SECTION.
      *
           MOVE    PTCONF-REC          TO  MCPDATA-REC
           MOVE    "tbl_ptconf"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTCONF-REC
                   MOVE    ZERO                TO  FLG-PTCONF
               ELSE
                   INITIALIZE                  PTCONF-REC
                   MOVE    1                   TO  FLG-PTCONF
               END-IF
           ELSE
               INITIALIZE                  PTCONF-REC
               MOVE    1                   TO  FLG-PTCONF
           END-IF
      *
           MOVE    "tbl_ptconf"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       970-PTCONF-READ-EXT.
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
