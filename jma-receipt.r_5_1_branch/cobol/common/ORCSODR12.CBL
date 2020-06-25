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
       IDENTIFICATION                  DIVISION.
       PROGRAM-ID.                     ORCSODR12.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : オーダー編集（入院登録訂正）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  04/11/11    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT                     DIVISION.
       CONFIGURATION                   SECTION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
       WORKING-STORAGE                 SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-PTNYUINRRK          PIC 9(01).
           03  FLG-BUNBEN              PIC 9(01).
           03  FLG-ORDER               PIC 9(01).
           03  FLG-ODRITEM             PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-END                 PIC 9(01).
           03  FLG-HIZUKE-ERR          PIC 9(01).
           03  FLG-HKNCOMBI            PIC 9(01).
           03  FLG-NYUINACCT           PIC 9(01).
           03  FLG-BUNBENCHK           PIC 9(01).
      *
      *    カウント領域
      *01  CNT-AREA.
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
           03  IDX1                    PIC 9(05).
           03  IDX2                    PIC 9(05).
           03  IDX3                    PIC 9(05).
           03  IDX4                    PIC 9(05).
           03  IDXK                    PIC 9(05).
           03  IDX-ODR                 PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RRKNUM              PIC 9(03).
           03  WRK-YMD                 PIC X(10).
           03  WRK-ZOGENPTN            PIC X(01).
           03  WRK-ZOGEN               PIC S9(05).
           03  WRK-SYMD                PIC X(08).
           03  WRK-KEISANYMD           PIC X(08).
           03  WRK-HENYMDG             PIC X(10).
           03  WRK-COMBI-HKNCOMBI      PIC 9(04).
           03  WRK-STYMD               PIC X(08).
           03  WRK-EDYMD               PIC X(08).
           03  WRK-NISSU-G.
               05  WRK-NISSU1          PIC 9(05).
               05  WRK-NISSU2          PIC 9(05).
           03  WRK-NACCT-SRYYM         PIC X(06).
           03  WRK-NYUKBN              PIC X(02).
           03  WRK-MCP-WINDOW          PIC X(64).
      *
       01  ITEM-AREA.
           03  ITEM-NYUKBN             PIC X(02).
           03  ITEM-BTUNUM             PIC X(02).
           03  ITEM-BRMNUM             PIC X(08).
           03  ITEM-BTUSBT             PIC X(02).
           03  ITEM-SHOKAI             PIC X(02).
           03  ITEM-LASTNYUINYMD       PIC X(08).
           03  ITEM-SHONUM             PIC 9(03).
           03  ITEM-SAGAKUCD           PIC X(02).
           03  ITEM-SENTEIKBN          PIC X(01).
           03  ITEM-KAIGOKBN           PIC X(01).
           03  ITEM-AUTOSET            PIC X(01).
           03  ITEM-TOKNYUIN-G.
               05  ITEM-TOKNYUIN-KBN   PIC X(01).
               05  ITEM-TOKNYUIN-NYUIN PIC X(02).
           03  ITEM-TOKNYUIN-SRYCD     PIC X(09).
           03  ITEM-SANTEICD           PIC X(09).
           03  ITEM-TOKHIGAITOKBN      PIC X(01).
           03  ITEM-DRCD-G.
               05  ITEM-DRCD-MAX       PIC 9(02).
               05  ITEM-DRCD           PIC X(05)   OCCURS  3.
           03  ITEM-NYUINKA            PIC X(02).
           03  ITEM-TEMPSET            PIC X(01).
           03  ITEM-HKNCOMBI           PIC 9(04).
           03  ITEM-NACCTKBN           PIC X(01).
           03  ITEM-BUNBENKBN          PIC X(01).
           03  ITEM-RIYOUKBN           PIC X(01).
           03  ITEM-KSNCD-G.
               05  ITEM-KSNCD-MAX      PIC 9(02).
               05  ITEM-KSNCD          PIC X(09)   OCCURS  3.
           03  ITEM-TEIKIKBN           PIC X(01).
           03  ITEM-DISPKBN            PIC X(01).
      *
      *
       01  ERRCD-AREA.
           03  ERRCD-ITEM-G.
               05  ERRCD-ITEM          PIC X(04).
               05  ERRCD-ORDER         PIC X(04).
           03  ERRCD-ERRCD             PIC X(04).
      *
       01  CONST-AREA.
           03  CONST-ITEMNUM-MAX       PIC 9(05)   VALUE 30.
           03  CONST-API               PIC 9(01)   VALUE 2.
           03  CONST-TRUE              PIC X(05)   VALUE "TRUE".
           03  CONST-FALSE             PIC X(05)   VALUE "FALSE".
      *
      *    入院CLAIM項目IDコピー句
           COPY    "CPNCLAIMID.INC".
      *
      *    ORDERIDコピー句
           COPY    "CPNORDERID.INC".
      *
      *    患者入院履歴退避
       01  TRRK-REC.
           COPY    "CPPTNYUINRRK.INC"  REPLACING  //PTNYUINRRK-//
                                       BY         //TRRK-//.
      *
      *    患者入院履歴ワーク
       01  WKPTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC"  REPLACING  //PTNYUINRRK-//
                                       BY         //WKPTNYUINRRK-//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1005.INC".
           COPY    "CPSK1010.INC".
           COPY    "CPSK5001.INC".
           COPY    "CPSK5002.INC".
           COPY    "CPSK5111.INC".
           COPY    "CPSK5113.INC".
      *
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    入院会計
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    オーダーテーブル
       01  ORDER-REC.
           COPY    "CPORDER.INC".
      *
      *    オーダ項目テーブル
       01  ODRITEM-REC.
           COPY    "CPODRITEM.INC".
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    出産育児一時金
       01  BUNBEN-REC.
           COPY    "CPBUNBEN.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    オーダー共通サブ
           COPY    "CPORCSODRS01.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
           COPY    "CPORCSGDAY.INC".
      *
      *    カナチェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    病室名称編集
           COPY    "CPORCSBRMNUM.INC".
      *
      *    入院会計作成サブ
           COPY    "CPORCSODRNACCT.INC".
       01  LNK-PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC"  REPLACING  //PTNYUINRRK-//
                                       BY         //LNK-PTNYUINRRK-//.
      *
      *    分娩情報編集サブ
           COPY    "CPORCSI02.INC".
       01  SNRRK-AREA.
           COPY    "CPPTNYUINRRK.INC"  REPLACING  //PTNYUINRRK-//
                                       BY         //SNRRK-//.
      *
      *    入退院登録PUSH通知
           COPY    "CPORCSI03.INC".
      *
      *
      *    入院料取得サブ
           COPY "CPORCSNYUINRYO.INC".
      *
      *    特定入院料コード取得サブ
           COPY  "CPORCSTOKNYUIN.INC".
      *
      *    排他制御
           COPY    "CPORCSLOCK.INC".
      *
      *    入院料加算算定可否チェックサブ
           COPY    "CPORCSI01.INC".
      *
      *    ユーザプログラム起動サブパラメタ
           COPY    "CPORCSUSERCHK.INC".
      *
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                         SECTION.
      *
      *    入院異動チェックサブ
           COPY    "CPORCSODR.INC".
       01  LNK-ODRITEM-REC.
           02  LNK-ODRITEM-MAX         PIC 9(03).
           02  LNK-ODRITEM-OCC         OCCURS   30.
           COPY    "CPODRITEM.INC"     REPLACING  //ODRITEM-//
                                       BY         //LNK-ODRITEM-//.
           COPY    "MCPAREA".
           COPY    "COMMON-SPA".
      *
      ******************************************************************
      *
       PROCEDURE                       DIVISION
                                       USING
                                               SODR-AREA
                                               LNK-ODRITEM-REC
                                               MCPAREA
                                               SPA-AREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
      D    DISPLAY "ORCSODR12 START"
           PERFORM 100-INIT-SEC
      *
           IF    ( FLG-END     =   ZERO )
               PERFORM 200-MAIN-SEC
           END-IF
      *
           PERFORM 300-END-SEC
      D    DISPLAY "ORCSODR12 END"
      *
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
      *    INITIALIZE                  CNT-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  ERRCD-AREA
           INITIALIZE                  ITEM-AREA
           INITIALIZE                  TRRK-REC
           INITIALIZE                  SODR-OT
      *
           IF    ( SODR-SRCKBN     =   "1" OR "2" )
               PERFORM 990-LOCK-CHK-SEC
           END-IF
      *
           IF    ( ERRCD-ITEM-G  =   SPACE )
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    SODR-HOSPNUM    TO  PTNYUINRRK-HOSPNUM
               MOVE    SODR-PTID       TO  PTNYUINRRK-PTID
               PERFORM 900-PTNYUINRRK-KEY3-SEL-SEC
               IF    ( FLG-PTNYUINRRK  =   ZERO )
                   MOVE    PTNYUINRRK-REC
                                       TO  TRRK-REC
                   IF    ( PTNYUINRRK-RRKEDANUM    >   1 )
                       MOVE    "0204"  TO  ERRCD-ORDER
                   END-IF
                   IF    ( ERRCD-ORDER =   SPACE )
      *                入院中であること
                       IF    ( TRRK-TAIINYMD NOT =   "99999999" )
                           MOVE    "0052"  TO  ERRCD-ORDER
                       END-IF
                   END-IF
                   IF    ( ERRCD-ORDER =   SPACE )
                       PERFORM 900-PTNYUINRRK-KEY75-SEL-SEC
                       IF    ( FLG-PTNYUINRRK  =   ZERO )
                           MOVE    "0205"  TO  ERRCD-ORDER
                       END-IF
                   END-IF
               ELSE
                   MOVE    "0051"      TO  ERRCD-ORDER
               END-IF
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           IF    ( ERRCD-ORDER  =   SPACE )
            AND  ( ERRCD-ITEM   =   SPACE )
               PERFORM 2001-INPUT-CHK-SEC
           END-IF
      *
           IF    ( ERRCD-ORDER  =   SPACE )
            AND  ( ERRCD-ITEM   =   SPACE )
               PERFORM 2002-KANREN-CHK-SEC
           END-IF
      *
           IF    ( ERRCD-ORDER  =   SPACE )
            AND  ( ERRCD-ITEM   =   SPACE )
      *
               IF    ( FLG-END  =   ZERO )
                   PERFORM 2101-PTNYUINRRK-UPDATE-SEC
               END-IF
      *
               IF    ( FLG-END  =   ZERO )
                  PERFORM 2101-ORCSODRNACCT-CALL-SEC
               END-IF
      *
               IF    ( FLG-END  =   ZERO )
                   IF    ( SODR-SRCKBN =   "1" OR "2" )
                       PERFORM 2101-ORCSI02-CALL-SEC
                   END-IF
               END-IF
      *
               IF    ( FLG-END  =   ZERO )
                  PERFORM 2101-PTINF-UPDATE-SEC
               END-IF
      *
               IF    ( FLG-END  =   ZERO )
                   IF    ( SODR-PROCKBN    =   "1" )
                       PERFORM 2101-ORDER-RECONF-SEC
                   END-IF
               END-IF
      *
               IF    ( FLG-END  =   ZERO )
                   IF    ( SODR-PROCKBN    =   "2" )
                       CONTINUE
                   ELSE
                       PERFORM 2101-ORDER-INSERT-SEC
                   END-IF
               END-IF
      *
               IF    ( FLG-END  =   ZERO )
                   IF    ( SODR-KEYGENERATE
                                           =   CONST-API )
                       PERFORM 2101-USERPG-CALL-SEC
      *
                       PERFORM 2101-PUSHEVENT-SEC
                   END-IF
               END-IF
      *
           ELSE
               IF    ( SODR-PROCKBN    =   "1" OR "2" )
                       CONTINUE
               ELSE
                   IF    ( FLG-END  =   ZERO )
                       PERFORM 2101-ORDER-INSERT-SEC
                   END-IF
               END-IF
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目チェック処理
      *****************************************************************
       2001-INPUT-CHK-SEC              SECTION.
      *
           PERFORM VARYING IDX-ODR    FROM    1   BY  1
                   UNTIL ( IDX-ODR    >   LNK-ODRITEM-MAX )
      *
               IF    ( LNK-ODRITEM-ITEM (IDX-ODR)  =   SPACE )
                AND  ( LNK-ODRITEM-MEMO (IDX-ODR)  =   SPACE )
                   CONTINUE
               ELSE
                   EVALUATE    LNK-ODRITEM-ITEMID (IDX-ODR)
                   WHEN    NCLAIMID-SHOKAIJMA
                       PERFORM 20011-SHOKAI-CHK-SEC
                   WHEN    NCLAIMID-BTUNUMJMA
                       PERFORM 20011-BTUNUM-CHK-SEC
                   WHEN    NCLAIMID-BRMNUMJMA
                       PERFORM 20011-BRMNUM-CHK-SEC
                   WHEN    NCLAIMID-SAGAKUJMA
                       PERFORM 20011-SAGAKU-CHK-SEC
                   WHEN    NCLAIMID-SENTEIKBNJMA
                       PERFORM 20011-SENTEIKBN-CHK-SEC
                   WHEN    NCLAIMID-NYUINKAJMA
                       PERFORM 20011-NYUINKA-CHK-SEC
                   WHEN    NCLAIMID-KAIGOKBNJMA
                       PERFORM 20011-KAIGOKBN-CHK-SEC
                   WHEN    NCLAIMID-DRJMA
                       PERFORM 20011-DR-CHK-SEC
                   WHEN    NCLAIMID-AUTOSETJMA
                       PERFORM 20011-AUTOSET-CHK-SEC
                   WHEN    NCLAIMID-TOKNYUINJMA
                       PERFORM 20011-TOKNYUIN-CHK-SEC
                   WHEN    NCLAIMID-TOKHIGAITOKBNJMA
                       PERFORM 20011-TOKHIGAITOKBN-CHK-SEC
                   WHEN    NCLAIMID-TEMPSETJMA
                       PERFORM 20011-TEMPSET-CHK-SEC
                   WHEN    NCLAIMID-HKNCOMBIJMA
                       PERFORM 20011-HKNCOMBI-CHK-SEC
                   WHEN    NCLAIMID-NACCTKBNJMA
                       PERFORM 20011-NACCTKBN-CHK-SEC
                   WHEN    NCLAIMID-KSNCDJMA
                       PERFORM 20011-KSNCD-CHK-SEC
                   WHEN    NCLAIMID-TEIKIKBNJMA
                       PERFORM 20011-TEIKIKBN-CHK-SEC
                   WHEN    NCLAIMID-DISPKBNJMA
                       PERFORM 20011-DISPKBN-CHK-SEC
                   END-EVALUATE
               END-IF
      *
           END-PERFORM
      *
           IF    ( SODR-SRCKBN     =   "1" OR "2" )
      *
               INITIALIZE                  SI02-AREA
                                           SNRRK-AREA
               MOVE    "1"             TO  SI02-REQID
               MOVE    TRRK-REC        TO  SNRRK-AREA
               MOVE    ITEM-NYUINKA    TO  SNRRK-NYUINKA
               DISPLAY "SNRRK-NYUINKA=" SNRRK-NYUINKA
               DISPLAY "SNRRK-NYUINYMD=" SNRRK-NYUINYMD
               CALL    "ORCSI02"       USING
                                       SI02-AREA
                                       SNRRK-AREA
                                       MCPAREA
                                       SPA-AREA
      *
               MOVE    SI02-BUNBENCHK  TO  FLG-BUNBENCHK
           END-IF
      *
           IF    ( FLG-BUNBENCHK   NOT =   ZERO )
      *
               MOVE    "1"         TO  ITEM-BUNBENKBN
               MOVE    "1"         TO  ITEM-RIYOUKBN
               IF    ( SI02-BUNBENKBN  NOT =   SPACE )
                   MOVE    SI02-BUNBENKBN
                               TO  ITEM-BUNBENKBN
                   MOVE    SI02-RIYOUKBN
                               TO  ITEM-RIYOUKBN
               END-IF
      *
               PERFORM VARYING IDX-ODR    FROM    1   BY  1
                       UNTIL ( IDX-ODR    >   LNK-ODRITEM-MAX )
      *
                   EVALUATE    LNK-ODRITEM-ITEMID (IDX-ODR)
                   WHEN    NCLAIMID-BUNBENKBNJMA
                       PERFORM 20011-BUNBENKBN-CHK-SEC
                   WHEN    NCLAIMID-RIYOUKBNJMA
                       PERFORM 20011-RIYOUKBN-CHK-SEC
                   END-EVALUATE
      *
               END-PERFORM
           END-IF
      *
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    継続区分チェック処理
      *****************************************************************
       20011-SHOKAI-CHK-SEC            SECTION.
      *
           EVALUATE    LNK-ODRITEM-ITEM (IDX-ODR)
           WHEN    "01"
               MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                       TO  ITEM-SHOKAI
               MOVE    LNK-ODRITEM-MEMO (IDX-ODR)
                                       TO  WRK-YMD
               INSPECT WRK-YMD REPLACING  ALL "-" BY  "."
               PERFORM 800-SEIWA-HEN-SEC
               IF    ( FLG-HIZUKE-ERR  NOT =   ZERO )
                   MOVE    "0033"      TO  ERRCD-ERRCD
                   PERFORM 2100-ITEM-ERR-SET-SEC
               ELSE
                   MOVE    WRK-SYMD    TO  ITEM-LASTNYUINYMD
               END-IF
           WHEN    OTHER
               MOVE    "0016"          TO  ERRCD-ERRCD
               PERFORM 2100-ITEM-ERR-SET-SEC
           END-EVALUATE
      *
           .
       20011-SHOKAI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病棟コードチェック処理
      *****************************************************************
       20011-BTUNUM-CHK-SEC            SECTION.
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "5001"          TO  SYS-KANRICD
           MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                   TO  SYS-KBNCD
           MOVE    SODR-SRYYMD     TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI    =   ZERO )
               MOVE    SYSKANRI-REC
                                   TO  SYS-5001-REC
      *
               MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                   TO  ITEM-BTUNUM
      *
               MOVE    SYS-5001-BTU-KHNSRYCD
                                   TO  ITEM-SANTEICD
               MOVE    SYS-5001-BTU-SBT
                                   TO  ITEM-BTUSBT
      *
           ELSE
               MOVE    "0018"      TO  ERRCD-ERRCD
               PERFORM 2100-ITEM-ERR-SET-SEC
           END-IF
      *
           .
       20011-BTUNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病室コードチェック処理
      *****************************************************************
       20011-BRMNUM-CHK-SEC            SECTION.
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "5002"          TO  SYS-KANRICD
           MOVE    ITEM-BTUNUM (1:2)
                                   TO  SYS-KBNCD (1:2)
      *
           INITIALIZE                  ORCSBRMNUMAREA
           MOVE    "2"             TO  SBRMNUM-IN-PROCKBN
           MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                   TO  SBRMNUM-IN-BRMNUM
           CALL    "ORCSBRMNUM"    USING   ORCSBRMNUMAREA
      *
           MOVE    SBRMNUM-OT-BRMNUM
                                   TO  SYS-KBNCD (3:)
      *
           MOVE    SODR-SRYYMD     TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI    =   ZERO )
               MOVE    SYS-KBNCD   TO  ITEM-BRMNUM
           ELSE
               MOVE    "0020"      TO  ERRCD-ERRCD
               PERFORM 2100-ITEM-ERR-SET-SEC
           END-IF
      *
           .
       20011-BRMNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    室料差額チェック処理
      *****************************************************************
       20011-SAGAKU-CHK-SEC            SECTION.
      *
           IF    ( LNK-ODRITEM-ITEM (IDX-ODR)  =   "-" )
               MOVE    SPACE       TO  ITEM-SAGAKUCD
           ELSE
               PERFORM 200111-SAGAKUCD-CHK-SEC
           END-IF
      *
           .
       20011-SAGAKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    室料差額コードチェック処理
      *****************************************************************
       200111-SAGAKUCD-CHK-SEC         SECTION.
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "5113"          TO  SYS-KANRICD
           MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                   TO  SYS-KBNCD
           MOVE    SODR-SRYYMD     TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI    =   ZERO )
               MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                   TO  ITEM-SAGAKUCD
           ELSE
               MOVE    "0024"      TO  ERRCD-ERRCD
               PERFORM 2100-ITEM-ERR-SET-SEC
           END-IF
      *
           .
       200111-SAGAKUCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選定入院区分チェック処理
      *****************************************************************
       20011-SENTEIKBN-CHK-SEC          SECTION.
      *
           IF    ( LNK-ODRITEM-ITEM (IDX-ODR)
                                   =   "1" OR "2" )
               MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                   TO  ITEM-SENTEIKBN
           ELSE
               MOVE    "0156"      TO  ERRCD-ERRCD
               PERFORM 2100-ITEM-ERR-SET-SEC
           END-IF
      *
           .
       20011-SENTEIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    介護からの異動チェック処理
      *****************************************************************
       20011-KAIGOKBN-CHK-SEC          SECTION.
      *
           MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                       TO  ITEM-KAIGOKBN
      *
           .
       20011-KAIGOKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    担当医チェック処理
      *****************************************************************
       20011-DR-CHK-SEC                SECTION.
      *
           IF    ( ITEM-DRCD-MAX   =   ZERO )
               INITIALIZE              ITEM-DRCD-G
           END-IF
      *
           COMPUTE ITEM-DRCD-MAX   =   ITEM-DRCD-MAX   +   1
      *
           EVALUATE    TRUE
           WHEN  ( LNK-ODRITEM-ITEM (IDX-ODR)   =   "-" )
               INITIALIZE              ITEM-DRCD-G
           WHEN    OTHER
               INITIALIZE              SYSKANRI-REC
               MOVE    SPA-HOSPNUM TO  SYS-HOSPNUM
               MOVE    "1010"      TO  SYS-KANRICD
               MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                   TO  SYS-KBNCD
               MOVE    SODR-SRYYMD TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
               PERFORM 900-SYSKANRI-KEY10-SEL-SEC
               IF    ( FLG-SYSKANRI    =   ZERO )
                   MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                   TO  ITEM-DRCD (ITEM-DRCD-MAX)
               ELSE
                   MOVE    "0025"  TO  ERRCD-ERRCD
                   PERFORM 2100-ITEM-ERR-SET-SEC
               END-IF
           END-EVALUATE
      *
           .
       20011-DR-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院料初期値設定区分チェック処理
      *****************************************************************
       20011-AUTOSET-CHK-SEC         SECTION.
      *
           IF    ( LNK-ODRITEM-ITEM (IDX-ODR)
                                         =   "0" OR "1" )
               MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                       TO  ITEM-AUTOSET
           ELSE
               MOVE    "0196"      TO  ERRCD-ERRCD
               PERFORM 2100-ITEM-ERR-SET-SEC
           END-IF
      *
           .
       20011-AUTOSET-CHK-EXT.
           EXIT.
      *****************************************************************
      *    特定入院料チェック処理
      *****************************************************************
       20011-TOKNYUIN-CHK-SEC          SECTION.
      *
           EVALUATE    LNK-ODRITEM-ITEM  (IDX-ODR)(1:1)
           WHEN    "1"
               IF    ( LNK-ODRITEM-ITEM  (IDX-ODR) =   "101" )
                   MOVE    LNK-ODRITEM-ITEM  (IDX-ODR)
                               TO  ITEM-TOKNYUIN-G
                   MOVE    LNK-ODRITEM-MEMO (IDX-ODR)
                               TO  ITEM-TOKNYUIN-SRYCD
               END-IF
           WHEN    "2"
               INITIALIZE                  SYSKANRI-REC
               MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
               MOVE    "5110"          TO  SYS-KANRICD
               MOVE    LNK-ODRITEM-ITEM (IDX-ODR) (2:2)
                                       TO  SYS-KBNCD
               MOVE    SODR-SRYYMD     TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
               PERFORM 900-SYSKANRI-KEY10-SEL-SEC
               IF    ( FLG-SYSKANRI    =   ZERO )
                   MOVE    LNK-ODRITEM-ITEM  (IDX-ODR)
                                       TO  ITEM-TOKNYUIN-G
                   MOVE    LNK-ODRITEM-MEMO (IDX-ODR)
                                       TO  ITEM-TOKNYUIN-SRYCD
               END-IF
           WHEN    "3"
               INITIALIZE                  SYSKANRI-REC
               MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
               MOVE    "5111"          TO  SYS-KANRICD
               MOVE    LNK-ODRITEM-ITEM (IDX-ODR) (2:2)
                                       TO  SYS-KBNCD
               MOVE    SODR-SRYYMD     TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
               PERFORM 900-SYSKANRI-KEY10-SEL-SEC
               IF    ( FLG-SYSKANRI    =   ZERO )
                   MOVE    LNK-ODRITEM-ITEM  (IDX-ODR)
                                       TO  ITEM-TOKNYUIN-G
                   MOVE    LNK-ODRITEM-MEMO (IDX-ODR)
                                       TO  ITEM-TOKNYUIN-SRYCD
               END-IF
           WHEN    "4"
               INITIALIZE                  SYSKANRI-REC
               MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
               MOVE    "5112"          TO  SYS-KANRICD
               MOVE    LNK-ODRITEM-ITEM (IDX-ODR) (2:2)
                                       TO  SYS-KBNCD
               MOVE    SODR-SRYYMD     TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
               PERFORM 900-SYSKANRI-KEY10-SEL-SEC
               IF    ( FLG-SYSKANRI    =   ZERO )
                   MOVE    LNK-ODRITEM-ITEM  (IDX-ODR)
                                       TO  ITEM-TOKNYUIN-G
                   MOVE    LNK-ODRITEM-MEMO (IDX-ODR)
                                       TO  ITEM-TOKNYUIN-SRYCD
               END-IF
           END-EVALUATE
      *
           IF    ( ITEM-TOKNYUIN-G =   SPACE )
               MOVE    "0035"      TO  ERRCD-ERRCD
               PERFORM 2100-ITEM-ERR-SET-SEC
           END-IF
      *
           .
       20011-TOKNYUIN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    算定要件非該当区分チェック処理
      *****************************************************************
       20011-TOKHIGAITOKBN-CHK-SEC         SECTION.
      *
           MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                   TO  ITEM-TOKHIGAITOKBN
      *
           .
       20011-TOKHIGAITOKBN-CHK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院科チェック処理
      *****************************************************************
       20011-NYUINKA-CHK-SEC           SECTION.
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "1005"          TO  SYS-KANRICD
           MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                   TO  SYS-KBNCD
           MOVE    SODR-SRYYMD     TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI    =   ZERO )
               MOVE    SYSKANRI-REC TO SYS-1005-REC
      *
               MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                   TO  ITEM-NYUINKA
      *
           ELSE
               MOVE    "0002"      TO  ERRCD-ERRCD
               PERFORM 2100-ITEM-ERR-SET-SEC
           END-IF
      *
           .
       20011-NYUINKA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ仮設定区分チェック処理
      *****************************************************************
       20011-TEMPSET-CHK-SEC         SECTION.
      *
           IF    ( LNK-ODRITEM-ITEM (IDX-ODR)
                                   =   "0" OR "1" )
               MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                   TO  ITEM-TEMPSET
               IF    ( ITEM-TEMPSET    =   "1" )
                   MOVE    999     TO  ITEM-HKNCOMBI
               END-IF
           ELSE
               MOVE    "E"         TO  ITEM-TEMPSET
               MOVE    "0200"      TO  ERRCD-ERRCD
               PERFORM 2100-ITEM-ERR-SET-SEC
           END-IF
      *
           .
       20011-TEMPSET-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せチェック処理
      *****************************************************************
       20011-HKNCOMBI-CHK-SEC          SECTION.
      *
           EVALUATE    ITEM-TEMPSET
           WHEN    "1"
           WHEN    "E"
               CONTINUE
           WHEN    OTHER
               PERFORM 200111-HKNCOMBI-CHK-SEC
           END-EVALUATE
      *
           .
       20011-HKNCOMBI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せチェック処理
      *****************************************************************
       200111-HKNCOMBI-CHK-SEC         SECTION.
      *
           MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                   TO  WRK-COMBI-HKNCOMBI
           PERFORM 900-HKNCOMBI-KEY-SEL-SEC
           IF    ( FLG-HKNCOMBI        =   ZERO )
               IF    ( COMB-DELKBN NOT =   "1" )
                 AND ( SODR-SRYYMD    >=  COMB-TEKSTYMD )
                 AND ( SODR-SRYYMD    <=  COMB-TEKEDYMD )
      *
                   MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                   TO  ITEM-HKNCOMBI
               ELSE
                   MOVE    "0032"  TO  ERRCD-ERRCD
                   PERFORM 2100-ITEM-ERR-SET-SEC
               END-IF
           ELSE
                   MOVE    "0032"  TO  ERRCD-ERRCD
                   PERFORM 2100-ITEM-ERR-SET-SEC
           END-IF
      *
           .
       200111-HKNCOMBI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院会計区分チェック処理
      *****************************************************************
       20011-NACCTKBN-CHK-SEC          SECTION.
      *
           IF    ( LNK-ODRITEM-ITEM (IDX-ODR)
                                   =   "1" OR "2" )
               MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                   TO  ITEM-NACCTKBN
           ELSE
               MOVE    "0157"      TO  ERRCD-ERRCD
               PERFORM 2100-ITEM-ERR-SET-SEC
           END-IF
      *
           .
       20011-NACCTKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院加算チェック処理
      *****************************************************************
       20011-KSNCD-CHK-SEC             SECTION.
      *
           IF    ( ITEM-KSNCD-MAX  =   ZERO )
               INITIALIZE              ITEM-KSNCD-G
           END-IF
      *
           EVALUATE    LNK-ODRITEM-ITEM (IDX-ODR)
           WHEN    "-"
               INITIALIZE                  ITEM-KSNCD-G
           WHEN    OTHER
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL ( IDX1    >   3 )
      *
                   IF    ( ITEM-KSNCD (IDX1)   =   SPACE )
                       MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                       TO  ITEM-KSNCD (IDX1)
                       MOVE    IDX1    TO  ITEM-KSNCD-MAX
                       MOVE    999     TO  IDX1
                   END-IF
      *
               END-PERFORM
           END-EVALUATE
      *
           .
       20011-KSNCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    定期請求区分チェック処理
      *****************************************************************
       20011-TEIKIKBN-CHK-SEC          SECTION.
      *
           IF    ( LNK-ODRITEM-ITEM (IDX-ODR)
                                   =   "1" OR "2" OR "3" )
               MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                   TO  ITEM-TEIKIKBN
           ELSE
               MOVE    "0141"      TO  ERRCD-ERRCD
               PERFORM 2100-ITEM-ERR-SET-SEC
           END-IF
      *
           .
       20011-TEIKIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院患者照会表示チェック処理
      *****************************************************************
       20011-DISPKBN-CHK-SEC           SECTION.
      *
           IF    ( LNK-ODRITEM-ITEM (IDX-ODR)
                                   =   "1" OR "2" )
               MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                   TO  ITEM-DISPKBN
           ELSE
               MOVE    "0158"      TO  ERRCD-ERRCD
               PERFORM 2100-ITEM-ERR-SET-SEC
           END-IF
      *
           .
       20011-DISPKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    分娩区分チェック処理
      *****************************************************************
       20011-BUNBENKBN-CHK-SEC         SECTION.
      *
           IF    ( LNK-ODRITEM-ITEM (IDX-ODR)  NOT =   SPACE )
               IF    ( FLG-BUNBENCHK   NOT =   ZERO )
                   IF    ( LNK-ODRITEM-ITEM (IDX-ODR)
                                       =   "0" OR "1" OR "2" )
                       MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                       TO  ITEM-BUNBENKBN
                   ELSE
                       MOVE    "0162"  TO  ERRCD-ERRCD
                       PERFORM 2100-ITEM-ERR-SET-SEC
                   END-IF
               END-IF
           END-IF
      *
           .
       20011-BUNBENKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    直接支払制度利用区分チェック処理
      *****************************************************************
       20011-RIYOUKBN-CHK-SEC         SECTION.
      *
           IF    ( ITEM-BUNBENKBN         =    "1" OR "2" )
            AND  ( LNK-ODRITEM-ITEM (IDX-ODR)  NOT =   SPACE )
               IF    ( LNK-ODRITEM-ITEM (IDX-ODR)
                                       =   "0" OR "1" )
                   MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                       TO  ITEM-RIYOUKBN
               ELSE
                   MOVE    "0163"      TO  ERRCD-ERRCD
                   PERFORM 2100-ITEM-ERR-SET-SEC
               END-IF
           END-IF
      *
           .
       20011-RIYOUKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    項目チェックエラー設定処理
      *****************************************************************
       2100-ITEM-ERR-SET-SEC           SECTION.
      *
           IF    ( ERRCD-ITEM      =   SPACE )
               MOVE    ERRCD-ERRCD TO  ERRCD-ITEM
           END-IF
      *
           IF  ( LNK-ODRITEM-STATUS (IDX-ODR)  =   SPACE )
               MOVE    2       TO  LNK-ODRITEM-STATUS (IDX-ODR)
           END-IF
      *
           MOVE    ERRCD-ERRCD TO  LNK-ODRITEM-ERRCD (IDX-ODR)
      *
           .
       2100-ITEM-ERR-SET-EXT.
           EXIT.
      *****************************************************************
      *    関連チェック処理
      *****************************************************************
       2002-KANREN-CHK-SEC             SECTION.
      *
           IF    ( ERRCD-ORDER     =   SPACE )
      *        入院区分チェック処理
               PERFORM 20021-NYUKBN-CHK-SEC
           END-IF
      *
           IF    ( ERRCD-ORDER     =   SPACE )
      *        病棟チェック処理
               PERFORM 20021-BTUNUM-CHK-SEC
           END-IF
      *
           IF    ( ERRCD-ORDER     =   SPACE )
      *        病室チェック処理
               PERFORM 20021-BRMNUM-CHK-SEC
           END-IF
      *
           IF    ( ERRCD-ORDER     =   SPACE )
      *        選定入院区分チェック処理
               PERFORM 20021-SENTEIKBN-CHK-SEC
           END-IF
      *
           IF    ( ERRCD-ORDER     =   SPACE )
      *        入院科チェック処理
               PERFORM 20021-NYUINKA-CHK-SEC
           END-IF
      *
           IF    ( ERRCD-ORDER     =   SPACE )
      *        入院会計区分チェック処理
               PERFORM 20021-NACCTKBN-CHK-SEC
           END-IF
      *
           IF    ( ERRCD-ORDER     =   SPACE )
      *        定期請求区分チェック処理
               PERFORM 20021-TEIKIKBN-CHK-SEC
           END-IF
      *
           IF    ( ERRCD-ORDER     =   SPACE )
      *        入院患者照会表示チェック処理
               PERFORM 20021-DISPKBN-CHK-SEC
           END-IF
      *
           IF    ( ERRCD-ORDER     =   SPACE )
      *        保険組合せチェック処理
               PERFORM 20021-HKNCOMBI-CHK-SEC
           END-IF
      *
           IF    ( ERRCD-ORDER     =   SPACE )
      *        継続区分チェック処理
               PERFORM 20021-SHOKAI-CHK-SEC
           END-IF
      *
           IF    ( ERRCD-ORDER     =   SPACE )
      *        入院履歴チェック処理
               PERFORM 20021-NYUINRRK-CHK-SEC
           END-IF
      *
           IF    ( ERRCD-ORDER     =   SPACE )
      *        入院料チェック処理
               PERFORM 20021-NYUINRYO-CHK-SEC
           END-IF
      *
           IF    ( ERRCD-ORDER     =   SPACE )
      *        入院加算チェック処理
               PERFORM 20021-KSNCD-CHK-SEC
           END-IF
      *
           .
       2002-KANREN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病棟チェック処理
      *****************************************************************
       20021-BTUNUM-CHK-SEC          SECTION.
      *
           IF    ( ITEM-BTUNUM         =   SPACE )
               MOVE    "0018"          TO  ERRCD-ORDER
           END-IF
      *
           .
       20021-BTUNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病室チェック処理
      *****************************************************************
       20021-BRMNUM-CHK-SEC          SECTION.
      *
           IF    ( ITEM-BRMNUM         =   SPACE )
               MOVE    "0020"          TO  ERRCD-ORDER
           END-IF
      *
           .
       20021-BRMNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選定入院区分チェック処理
      *****************************************************************
       20021-SENTEIKBN-CHK-SEC          SECTION.
      *
           IF    ( ITEM-SENTEIKBN      =   SPACE )
               MOVE    "0156"          TO  ERRCD-ORDER
           END-IF
      *
           .
       20021-SENTEIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院科チェック処理
      *****************************************************************
       20021-NYUINKA-CHK-SEC           SECTION.
      *
           IF    ( ITEM-NYUINKA    =   SPACE )
               MOVE    "0002"          TO  ERRCD-ORDER
           END-IF
      *
           .
       20021-NYUINKA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院会計区分チェック処理
      *****************************************************************
       20021-NACCTKBN-CHK-SEC          SECTION.
      *
           IF    ( ITEM-NACCTKBN       =   SPACE )
               MOVE    "0157"          TO  ERRCD-ORDER
           END-IF
      *
           .
       20021-NACCTKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    定期請求区分チェック処理
      *****************************************************************
       20021-TEIKIKBN-CHK-SEC          SECTION.
      *
           IF    ( ITEM-TEIKIKBN       =   SPACE )
               MOVE    "0141"          TO  ERRCD-ORDER
           END-IF
      *
           .
       20021-TEIKIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院患者照会表示チェック処理
      *****************************************************************
       20021-DISPKBN-CHK-SEC          SECTION.
      *
           IF    ( ITEM-DISPKBN        =   SPACE )
               MOVE    "0158"          TO  ERRCD-ORDER
           END-IF
      *
           .
       20021-DISPKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せチェック処理
      *****************************************************************
       20021-HKNCOMBI-CHK-SEC          SECTION.
      *
           IF    ( ITEM-HKNCOMBI   =   ZERO )
               MOVE    "0032"          TO  ERRCD-ORDER
           END-IF
      *
           .
       20021-HKNCOMBI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院区分チェック処理
      *****************************************************************
       20021-NYUKBN-CHK-SEC            SECTION.
      *
           MOVE    SPACE           TO  ITEM-NYUKBN
      *
           EVALUATE    ITEM-TOKNYUIN-KBN
           WHEN    SPACE
               MOVE    "01"        TO  ITEM-NYUKBN
           WHEN    "1"
               MOVE    "02"        TO  ITEM-NYUKBN
           WHEN    "2"
               IF    ( ITEM-TOKNYUIN-NYUIN
                       =   "35" OR "36" OR "37"
                        OR "38" OR "39"OR "40" )
                   MOVE    "01"    TO  ITEM-NYUKBN
               ELSE
                   MOVE    "02"    TO  ITEM-NYUKBN
               END-IF
           WHEN    "3"
               INITIALIZE              SYSKANRI-REC
               MOVE    SPA-HOSPNUM TO  SYS-HOSPNUM
               MOVE    "5111"      TO  SYS-KANRICD
               MOVE    ITEM-TOKNYUIN-NYUIN
                                   TO  SYS-KBNCD
               MOVE    SODR-SRYYMD TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
               PERFORM 900-SYSKANRI-KEY10-SEL-SEC
               MOVE    SYSKANRI-REC TO SYS-5111-REC
               IF    ( SYS-5111-BRM-NYUINRYOKBN
                                       =   "03" OR "05" OR "06" )
                   MOVE    "01"    TO  ITEM-NYUKBN
               ELSE
                   MOVE    "02"    TO  ITEM-NYUKBN
               END-IF
           END-EVALUATE
      *
           .
       20021-NYUKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    継続区分チェック処理
      *****************************************************************
       20021-SHOKAI-CHK-SEC            SECTION.
      *
           EVALUATE    ITEM-SHOKAI
           WHEN    SPACE
               MOVE    TRRK-REC        TO  PTNYUINRRK-REC
               PERFORM 900-PTNYUINRRK-KEY77-SEL-SEC
               IF    ( FLG-PTNYUINRRK  =   ZERO )
                   COMPUTE ITEM-SHONUM =   PTNYUINRRK-SHONUM   +   1
               ELSE
                   COMPUTE ITEM-SHONUM =   1
               END-IF
           WHEN    "01"
               MOVE    TRRK-REC        TO  PTNYUINRRK-REC
               MOVE    ITEM-LASTNYUINYMD
                                       TO  PTNYUINRRK-NYUINYMD
               DISPLAY "PTNYUINRRK-RRKNUM=" PTNYUINRRK-RRKNUM
               DISPLAY "PTNYUINRRK-NYUINYMD=" PTNYUINRRK-NYUINYMD
               PERFORM 900-PTNYUINRRK-KEY79-SEL-SEC
               IF    ( FLG-PTNYUINRRK  =   ZERO )
                   MOVE    PTNYUINRRK-SHONUM
                                       TO  ITEM-SHONUM
               ELSE
                   MOVE    "0036"      TO  ERRCD-ORDER
               END-IF
           END-EVALUATE
      *
           IF    ( ERRCD-ORDER     =   SPACE )
               IF    ( ITEM-SHOKAI         =   "01" )
                   IF    ( ITEM-KAIGOKBN   =   "1" OR "2" )
                       CONTINUE
                   ELSE
                       MOVE    "0140"  TO  ERRCD-ORDER
                   END-IF
               ELSE
                   MOVE    "1"             TO  ITEM-KAIGOKBN
               END-IF
           END-IF
      *
           .
       20021-SHOKAI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院料チェック処理
      *****************************************************************
       20021-NYUINRYO-CHK-SEC          SECTION.
      *
           EVALUATE    TRUE
           WHEN  ( ITEM-AUTOSET        =   "1" )
      *
               INITIALIZE                      SNYUINRYO-AREA
               MOVE    "1"                 TO  SNYUINRYO-AUTOSET
               MOVE    SODR-SRYYMD         TO  SNYUINRYO-SRYYMD
               MOVE    "01"                TO  SNYUINRYO-SYORIKBN
               MOVE    ITEM-BTUNUM         TO  SNYUINRYO-BTUNUM
               MOVE    ITEM-BRMNUM(3:)     TO  SNYUINRYO-BRMNUM
               MOVE    ITEM-HKNCOMBI       TO  SNYUINRYO-HKNCOMBI
               CALL    "ORCSNYUINRYO"      USING
                                           SNYUINRYO-AREA
                                           SPA-AREA
               MOVE    SNYUINRYO-BTU-KHNSRYCD
                                           TO  ITEM-SANTEICD
               MOVE    SNYUINRYO-TOKNYUIN-G
                                           TO  ITEM-TOKNYUIN-G
           WHEN  ( ITEM-SANTEICD       =   SPACE )
             AND ( ITEM-TOKNYUIN-G     =   SPACE )
               INITIALIZE                      SNYUINRYO-AREA
               MOVE    "A"                 TO  SNYUINRYO-AUTOSET
               MOVE    SODR-SRYYMD         TO  SNYUINRYO-SRYYMD
               MOVE    "01"                TO  SNYUINRYO-SYORIKBN
               MOVE    ITEM-BTUNUM         TO  SNYUINRYO-BTUNUM
               MOVE    ITEM-BRMNUM(3:)     TO  SNYUINRYO-BRMNUM
               MOVE    ITEM-HKNCOMBI       TO  SNYUINRYO-HKNCOMBI
               CALL    "ORCSNYUINRYO"      USING
                                           SNYUINRYO-AREA
                                           SPA-AREA
               MOVE    SNYUINRYO-BTU-KHNSRYCD
                                           TO  ITEM-SANTEICD
               MOVE    SNYUINRYO-TOKNYUIN-G
                                           TO  ITEM-TOKNYUIN-G
           END-EVALUATE
      *
      *    入院料が取得できていない場合
           IF    ( ITEM-SANTEICD       =   SPACE )
            AND  ( ITEM-TOKNYUIN-G     =   SPACE )
               MOVE    "0030"          TO  ERRCD-ORDER
           END-IF
      *
           IF    ( ERRCD-ORDER         =   SPACE )
               IF    ( ITEM-TOKNYUIN-G     NOT =   SPACE )
      *
                   INITIALIZE                  ORCSTOKNYUIN-AREA
                   MOVE    ITEM-TOKNYUIN-KBN
                                       TO  LNK-TOKNYUIN-KBN
                   MOVE    ITEM-TOKNYUIN-NYUIN
                                       TO  LNK-TOKNYUIN-NYUIN
                   MOVE    ITEM-BTUNUM TO  LNK-TOKNYUIN-BTUNUM
                   IF    ( ITEM-TOKHIGAITOKBN  =   "1" )
                       MOVE    "1"     TO  LNK-TOKNYUIN-SANTEIYOUKENKBN
                   END-IF
                   MOVE    SODR-SRYYMD TO  LNK-TOKNYUIN-SANTEIYM
                   CALL    "ORCSTOKNYUIN"  USING
                                           ORCSTOKNYUIN-AREA
                                           SPA-AREA
                   IF    ( ITEM-TOKHIGAITOKBN  =   "0" OR "1" )
                       IF    ( LNK-TOKNYUIN-GAI-SRYCD  =  SPACE )
                           MOVE   LNK-TOKNYUIN-SRYCD
                                           TO  ITEM-TOKNYUIN-SRYCD
                       ELSE
                           MOVE   LNK-TOKNYUIN-GAI-SRYCD
                                           TO  ITEM-TOKNYUIN-SRYCD
                       END-IF
                   ELSE
                       MOVE    "0155"      TO  ERRCD-ORDER
                   END-IF
               END-IF
           END-IF
      *
           .
       20021-NYUINRYO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院加算チェック処理
      *****************************************************************
       20021-KSNCD-CHK-SEC          SECTION.
      *
           INITIALIZE                      SI01-AREA
           MOVE    SODR-PTID           TO  SI01-PTID
           MOVE    SODR-SRYYMD         TO  SI01-SRYYMD
           MOVE    TRRK-NYUINYMD       TO  SI01-NYUINYMD
           MOVE    ITEM-BTUSBT         TO  SI01-BTUSBT
           IF    ( ITEM-TOKNYUIN-SRYCD NOT =   SPACE )
               MOVE    ITEM-TOKNYUIN-SRYCD
                                       TO  SI01-SNTNYUINCD
           ELSE
               MOVE    ITEM-SANTEICD   TO  SI01-SNTNYUINCD
           END-IF
           CALL    "ORCSI01"           USING
                                       SI01-AREA
                                       SPA-AREA
      *
           PERFORM VARYING IDX1  FROM  1   BY  1
                   UNTIL ( IDX1  >     ITEM-KSNCD-MAX )
                    OR   ( ERRCD-ORDER NOT   =   SPACE )
      *
               MOVE    ZERO    TO      IDXK
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL ( IDX2    >   SI01-KSN-MAX )
                        OR   ( IDXK    NOT =   ZERO )
                   IF    ( SI01-KSN-SRYCD (IDX2)
                                 =     ITEM-KSNCD (IDX1))
                       MOVE    IDX2    TO  IDXK
                   END-IF
               END-PERFORM
      *
               IF    ( IDXK    =   ZERO )
                   MOVE    "0159"      TO  ERRCD-ORDER
               END-IF
      *
           END-PERFORM
      *
           .
       20021-KSNCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴チェック処理
      *****************************************************************
       20021-NYUINRRK-CHK-SEC          SECTION.
      *
      *
           .
       20021-NYUINRRK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴追加処理
      *****************************************************************
       2101-PTNYUINRRK-UPDATE-SEC      SECTION.
      *
           MOVE    TRRK-REC        TO  PTNYUINRRK-REC
      *
           MOVE    ITEM-BTUNUM     TO  PTNYUINRRK-BTUNUM
           MOVE    ITEM-BRMNUM     TO  PTNYUINRRK-BRMNUM
           MOVE    ITEM-SAGAKUCD   TO  PTNYUINRRK-RM-SAGAKU
           MOVE    ITEM-SENTEIKBN  TO  PTNYUINRRK-SENTEIKBN
           MOVE    ITEM-NYUINKA    TO  PTNYUINRRK-NYUINKA
      *
           IF    ( ITEM-SHOKAI     =   SPACE )
               MOVE    "1"         TO  PTNYUINRRK-SHOKAIKBN
           ELSE
               MOVE    "2"         TO  PTNYUINRRK-SHOKAIKBN
           END-IF
      *
           MOVE    ITEM-SHONUM     TO  PTNYUINRRK-SHONUM
      *
           MOVE    ITEM-KAIGOKBN   TO  PTNYUINRRK-KAIGOKBN
      *
           MOVE    ITEM-DRCD (1)   TO  PTNYUINRRK-DRCD1
           MOVE    ITEM-DRCD (2)   TO  PTNYUINRRK-DRCD2
           MOVE    ITEM-DRCD (3)   TO  PTNYUINRRK-DRCD3
      *
           MOVE    ITEM-HKNCOMBI   TO  PTNYUINRRK-HKNCOMBINUM
      *
           MOVE    ITEM-TOKNYUIN-KBN
                                   TO  PTNYUINRRK-TOKUTEI-KBN
      *
           MOVE    ITEM-TOKNYUIN-NYUIN
                                   TO  PTNYUINRRK-TOKUTEI-NYUIN
      *
           IF    ( ITEM-TOKNYUIN-G        NOT =   SPACE )
            AND  ( ITEM-TOKHIGAITOKBN     NOT =   "0"  )
               MOVE    ITEM-TOKHIGAITOKBN
                                   TO  PTNYUINRRK-TOKU-SANTEIYOUKENKBN
           ELSE
               MOVE    SPACE       TO  PTNYUINRRK-TOKU-SANTEIYOUKENKBN
           END-IF
      *
           MOVE    ITEM-NACCTKBN   TO  PTNYUINRRK-NYUCALKBN
           MOVE    ITEM-KSNCD (1)  TO  PTNYUINRRK-KSNSRYCD(1)
           MOVE    ITEM-KSNCD (2)  TO  PTNYUINRRK-KSNSRYCD(2)
           MOVE    ITEM-KSNCD (3)  TO  PTNYUINRRK-KSNSRYCD(3)
           MOVE    ITEM-TEIKIKBN   TO  PTNYUINRRK-TEIKI-SEIKYUKBN
           MOVE    ITEM-DISPKBN    TO  PTNYUINRRK-KENSAKU-DISPKBN
      *
           MOVE    SODR-OPID       TO  PTNYUINRRK-OPID
           MOVE    SODR-UPYMD      TO  PTNYUINRRK-UPYMD
           MOVE    SODR-UPHMS      TO  PTNYUINRRK-UPHMS
      *
           MOVE    PTNYUINRRK-REC  TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBUPDATE-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               DISPLAY "ORCSDR12 ERROR 001 :PTNYUINRRK UPDATE ERR"
                       " PTID="        SODR-PTID
                       " KARTE-KEY="   SODR-KARTE-KEY
               MOVE    1               TO  FLG-END
           END-IF
      *
           IF    ( FLG-END             =   ZERO )
      *
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "upd4"              TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
               IF    ( MCP-RC      NOT =   ZERO )
                   DISPLAY "ORCSDR12 ERROR 012 :PTNYUINRRK UPDATE ERR"
                           " PTID="        SODR-PTID
                           " KARTE-KEY="   SODR-KARTE-KEY
                   MOVE    1               TO  FLG-END
               END-IF
           END-IF
      *
           MOVE    PTNYUINRRK-REC      TO  TRRK-REC
      *
           .
       2101-PTNYUINRRK-UPDATE-EXT.
           EXIT.
      *****************************************************************
      *    オーダー情報再編成処理
      *****************************************************************
       2101-ORDER-RECONF-SEC           SECTION.
      *
           CALL    "ORCSODRS01"        USING   SODR-AREA
                                               SODRS01-AREA
                                               LNK-ODRITEM-REC
                                               MCPAREA
                                               SPA-AREA
           MOVE    SODRS01-RC      TO  FLG-END
      *
           .
       2101-ORDER-RECONF-EXT.
           EXIT.
      *****************************************************************
      *    オーダー追加処理
      *****************************************************************
       2101-ORDER-INSERT-SEC           SECTION.
      *
           PERFORM 21011-ORDER-HEN-SEC
      *
           PERFORM VARYING IDX-ODR FROM    1   BY  1
                   UNTIL ( IDX-ODR >   LNK-ODRITEM-MAX )
                    OR   ( FLG-END =   1 )
      *
                PERFORM 21011-ODRITEM-HEN-SEC
      *
           END-PERFORM
      *
           .
       2101-ORDER-INSERT-EXT.
           EXIT.
      *****************************************************************
      *    ユーザプログラム実行処理
      *****************************************************************
       2101-USERPG-CALL-SEC            SECTION.
      *
           INITIALIZE                      ORCSUSERCHKAREA
           MOVE    "I01"               TO  ORCSUSERCHK-GMNPG
           MOVE    "2"                 TO  ORCSUSERCHK-SYORI
           MOVE    SPA-SYSYMD          TO  ORCSUSERCHK-SYSYMD
           MOVE    TRRK-TENNYUYMD      TO  ORCSUSERCHK-SRYYMD
           MOVE    SODR-PTID           TO  ORCSUSERCHK-PTID
           MOVE    SODR-PTNUM          TO  ORCSUSERCHK-PTNUM
           MOVE    TRRK-NYUINKA        TO  ORCSUSERCHK-SRYKA
           MOVE    TRRK-HKNCOMBINUM    TO  ORCSUSERCHK-HKNCOMBI
           MOVE    TRRK-DRCD1          TO  ORCSUSERCHK-DRCD
           MOVE    01                  TO  ORCSUSERCHK-SYORI-FLG
           CALL    "ORCSUSERCHK"           USING
                                           ORCSUSERCHKAREA
                                           SPA-AREA
      *
           .
       2101-USERPG-CALL-EXT.
           EXIT.
      *****************************************************************
      *    PUSH通知処理
      *****************************************************************
       2101-PUSHEVENT-SEC            SECTION.
      *
           INITIALIZE                      SI03-AREA
      *
           MOVE    "01"                TO  SI03-REQNUM
           MOVE    SODR-PTNUM          TO  SI03-PTNUM
           MOVE    TRRK-NYUINYMD       TO  SI03-NYUINYMD
           CALL    "ORCSI03"           USING
                                       SI03-AREA
                                       SPA-AREA
      *
           .
       2101-PUSHEVENT-EXT.
           EXIT.
      *****************************************************************
      *    オーダ編集処理
      *****************************************************************
       21011-ORDER-HEN-SEC             SECTION.
      *
           INITIALIZE                      ORDER-REC
      *
           MOVE   SODR-HOSPNUM          TO  ORDER-HOSPNUM
           MOVE   SODR-KARTE-KEY       TO  ORDER-KARTE-KEY
           MOVE   SODR-ORDERNUM        TO  ORDER-ORDERNUM
           MOVE   SODR-NYUGAIKBN       TO  ORDER-NYUGAIKBN
           MOVE   SODR-PTNUM           TO  ORDER-PTNUM
           MOVE   SODR-PTID            TO  ORDER-PTID
           MOVE   SODR-SRYKA           TO  ORDER-SRYKA
           MOVE   SODR-SRYYMD          TO  ORDER-SRYYMD
           MOVE   SODR-HKNCOMBI        TO  ORDER-HKNCOMBI
           MOVE   SODR-ORDERID         TO  ORDER-ORDERID
           MOVE   SODR-UKEYMD          TO  ORDER-UKEYMD
           MOVE   SODR-UKEHMS          TO  ORDER-UKEHMS
           MOVE   SODR-KEYGENERATE     TO  ORDER-KEYGENERATE
      *
           MOVE   ERRCD-ORDER          TO  ORDER-ERRCD1
           MOVE   ERRCD-ITEM           TO  ORDER-ERRCD2
      *
           IF    ( ERRCD-ITEM          =   SPACE )
            AND  ( ERRCD-ORDER         =   SPACE )
               MOVE    TRRK-RRKNUM     TO  ORDER-RRKNUM
               MOVE    TRRK-RRKEDANUM  TO  ORDER-RRKEDANUM
           ELSE
               MOVE    2               TO  ORDER-STATUS
           END-IF
      *
           IF    ( SODR-PROCKBN    =   "1" )
               MOVE     1              TO  ORDER-MODIFY
           END-IF
      *
           MOVE    SODR-OPID           TO  ORDER-OPID
           MOVE    SODR-UPYMD          TO  ORDER-CREYMD
                                           ORDER-UPYMD
           MOVE    SODR-UPHMS          TO  ORDER-UPHMS
      *
      *
           PERFORM 900-ORDER-KEY-SEL-SEC
           IF    ( FLG-ORDER           =   ZERO )
               DISPLAY "ORCSDR12 ERROR 006:ORDER INSERT CHK ERR"
                       " PTID="        SODR-PTID
                       " KARTE-KEY="   SODR-KARTE-KEY
               MOVE    1               TO  FLG-END
               GO  TO  21011-ORDER-HEN-EXT
           END-IF
      *
           MOVE    ORDER-REC           TO  MCPDATA-REC
           MOVE    "tbl_order"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               DISPLAY "ORCSDR12 ERROR 007:ORDER INSERT ERR"
                       " PTID="        SODR-PTID
                       " KARTE-KEY="   SODR-KARTE-KEY
               MOVE    1               TO  FLG-END
           END-IF
      *
           .
       21011-ORDER-HEN-EXT.
           EXIT.
      *****************************************************************
      *    オーダ項目編集処理
      *****************************************************************
       21011-ODRITEM-HEN-SEC           SECTION.
      *
           MOVE    LNK-ODRITEM-OCC (IDX-ODR)
                                       TO  ODRITEM-REC
      *
           IF    ( LNK-ODRITEM-ITEMID (IDX-ODR)
                                   =   NCLAIMID-SHOKAIJMA
                                    OR NCLAIMID-BTUNUMJMA
                                    OR NCLAIMID-BRMNUMJMA
                                    OR NCLAIMID-SAGAKUJMA
                                    OR NCLAIMID-SENTEIKBNJMA
                                    OR NCLAIMID-NYUINKAJMA
                                    OR NCLAIMID-KAIGOKBNJMA
                                    OR NCLAIMID-DRJMA
                                    OR NCLAIMID-AUTOSETJMA
                                    OR NCLAIMID-TOKNYUINJMA
                                    OR NCLAIMID-TOKHIGAITOKBNJMA
                                    OR NCLAIMID-TEMPSETJMA
                                    OR NCLAIMID-HKNCOMBIJMA
                                    OR NCLAIMID-NACCTKBNJMA
                                    OR NCLAIMID-KSNCDJMA
                                    OR NCLAIMID-TEIKIKBNJMA
                                    OR NCLAIMID-DISPKBNJMA
                                           )
               CONTINUE
           ELSE
               MOVE    3               TO  ODRITEM-STATUS
               MOVE    "9001"          TO  ODRITEM-ERRCD
           END-IF
      *
           MOVE    SODR-OPID           TO  ODRITEM-OPID
           MOVE    SODR-UPYMD          TO  ODRITEM-CREYMD
                                           ODRITEM-UPYMD
           MOVE    SODR-UPHMS          TO  ODRITEM-UPHMS
      *
      *
           PERFORM 900-ODRITEM-KEY-SEL-SEC
           IF    ( FLG-ODRITEM           =   ZERO )
               DISPLAY "ORCSDR12 ERROR 008:ODRITEM INSERT CHK ERR"
                       " PTID="        SODR-PTID
                       " KARTE-KEY="   SODR-KARTE-KEY
               MOVE    1               TO  FLG-END
               GO  TO  21011-ODRITEM-HEN-EXT
           END-IF
      *
           MOVE    ODRITEM-REC         TO  MCPDATA-REC
           MOVE    "tbl_odritem"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               DISPLAY "ORCSDR12 ERROR 009:ODRITEM INSERT ERR"
                       " PTID="        SODR-PTID
                       " KARTE-KEY="   SODR-KARTE-KEY
               MOVE    1               TO  FLG-END
           END-IF
      *
           .
       21011-ODRITEM-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入院会計作成サブ呼出
      *****************************************************************
       2101-ORCSODRNACCT-CALL-SEC      SECTION.
      *
           INITIALIZE                  SODRNACCT-AREA
           MOVE    NORDERID-IDOBTUID
                                   TO  SODRNACCT-ORDERID
           MOVE    SODR-OPID       TO  SODRNACCT-OPID
           MOVE    SODR-UPYMD      TO  SODRNACCT-UPYMD
           MOVE    SODR-UPHMS      TO  SODRNACCT-UPHMS
           MOVE    ITEM-NYUKBN     TO  SODRNACCT-NYUKBN
      *
           MOVE    ITEM-KSNCD-MAX  TO  SODRNACCT-KSN-MAX
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   ITEM-KSNCD-MAX )
               MOVE    ITEM-KSNCD (IDX1)
                                   TO  SODRNACCT-KSN-SRYCD (IDX1)
           END-PERFORM
      *
           MOVE    TRRK-REC        TO  LNK-PTNYUINRRK-REC
      *
           CALL    "ORCSODRNACCT"      USING
                                       SODRNACCT-AREA
                                       LNK-PTNYUINRRK-REC
                                       SPA-AREA
           IF    ( SODRNACCT-RC    NOT =   ZERO )
               MOVE    1           TO  FLG-END
           END-IF
      *
           .
       2101-ORCSODRNACCT-CALL-EXT.
           EXIT.
      *****************************************************************
      *    分娩管理情報更新処理
      *****************************************************************
       2101-ORCSI02-CALL-SEC           SECTION.
      *
           INITIALIZE                  SI02-AREA
           MOVE    "2"             TO  SI02-REQID
           MOVE    ITEM-BUNBENKBN  TO  SI02-BUNBENKBN
           MOVE    ITEM-RIYOUKBN   TO  SI02-RIYOUKBN
           MOVE    TRRK-REC        TO  SNRRK-AREA
           CALL    "ORCSI02"       USING
                                   SI02-AREA
                                   SNRRK-AREA
                                   MCPAREA
                                   SPA-AREA
           IF     ( MCP-RC         NOT =  ZERO )
               DISPLAY "ORCSDR12 ERROR 010:BUNBEN UPDATE ERR"
                       " PTID="        SODR-PTID
                       " KARTE-KEY="   SODR-KARTE-KEY
               MOVE    1               TO  FLG-END
           END-IF
      *
           .
       2101-ORCSI02-CALL-EXT.
           EXIT.
      *****************************************************************
      *    患者情報更新処理
      *****************************************************************
       2101-PTINF-UPDATE-SEC           SECTION.
      *
      *    入院歴より最新の歴情報を取得
           INITIALIZE                  PTNYUINRRK-REC
           MOVE    SODR-HOSPNUM     TO  PTNYUINRRK-HOSPNUM
           MOVE    SODR-PTID       TO  PTNYUINRRK-PTID
           PERFORM 900-PTNYUINRRK-KEY3-SEL-SEC
      *
      *    患者マスタ検索
           MOVE    ZERO                TO  FLG-PTINF
           INITIALIZE  PTINF-REC
           MOVE    SODR-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SODR-PTID           TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
      *
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
      *    患者マスタクローズ
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( FLG-PTINF           NOT =   ZERO )
               DISPLAY "ORCSDR12 ERROR 010:PTINF UPDATE CHK ERR"
                       " PTID="        SODR-PTID
                       " KARTE-KEY="   SODR-KARTE-KEY
               MOVE    1               TO  FLG-END
               GO  TO  2101-PTINF-UPDATE-EXT
           END-IF
      *
      *    患者情報の入院歴番号を更新する
           MOVE   PTNYUINRRK-RRKNUM
                                       TO  PTINF-RRKNUM
           MOVE   PTNYUINRRK-RRKEDANUM
                                       TO  PTINF-RRKEDANUM
      *
           MOVE    SODR-OPID           TO  PTINF-OPID
           MOVE    SODR-UPYMD          TO  PTINF-UPYMD
           MOVE    SODR-UPHMS          TO  PTINF-UPHMS
      *
      *    患者情報更新
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBUPDATE-SEC
           IF     ( MCP-RC         NOT =  ZERO )
               DISPLAY "ORCSDR12 ERROR 011:PTINF UPDATE ERR"
                       " PTID="        SODR-PTID
                       " KARTE-KEY="   SODR-KARTE-KEY
               MOVE    1               TO  FLG-END
           END-IF
      *
           .
       2101-PTINF-UPDATE-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           IF    ( ERRCD-ITEM      =   SPACE )
            AND  ( ERRCD-ORDER     =   SPACE )
               MOVE    ZERO        TO  SODR-ERRFLG
           ELSE
               MOVE    1           TO  SODR-ERRFLG
           END-IF
      *
           MOVE    ERRCD-ORDER     TO  SODR-ERRCD
      *
           MOVE    FLG-END         TO  SODR-RC
      *
           IF    ( SODR-RC         =   ZERO )
            AND  ( SODR-ERRFLG     =   ZERO )
               MOVE    TRRK-RRKNUM TO  SODR-RRKNUM
               MOVE    TRRK-RRKEDANUM
                                   TO  SODR-RRKEDANUM
               MOVE    TRRK-SHONUM TO  SODR-SHONUM
           END-IF
      *
           IF    ( FLG-END         NOT =   ZERO )
               MOVE    "tbl_ptnyuinrrk"
                                   TO  MCP-TABLE
               MOVE    "abort"     TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
           END-IF
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理
      *****************************************************************
       900-PTNYUINRRK-KEY3-SEL-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNYUINRRK-KEY3-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理
      *****************************************************************
       900-PTNYUINRRK-KEY77-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key77"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key77"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNYUINRRK-KEY77-SEL-EXT.
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
           MOVE    SODR-PTID       TO  BUNBEN-PTID
           MOVE    "1"             TO  BUNBEN-NYUGAIKBN
           MOVE    TRRK-RRKNUM     TO  BUNBEN-RRKNUM
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
      *    西暦日本語変換処理
      *****************************************************************
       800-SEIWA-HEN-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-HIZUKE-ERR
           MOVE    SPACE               TO  WRK-SYMD
                                           WRK-HENYMDG
      *
      *    日付画面チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    WRK-YMD             TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
           IF  ( SGDAY-RC              =   ZERO )
               MOVE    SGDAY-OTDAY     TO  WRK-HENYMDG
               MOVE    SGDAY-SDAY      TO  WRK-SYMD
           ELSE
               MOVE    1               TO  FLG-HIZUKE-ERR
           END-IF
      *
           .
       800-SEIWA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    期間計算処理
      *****************************************************************
       800-KIKAN-CALC-SEC              SECTION.
      *
           INITIALIZE                  WRK-NISSU-G
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY5-AREA
           MOVE    "51"                TO  LNK-DAY5-IRAI
           MOVE    WRK-STYMD           TO  LNK-DAY5-START
           MOVE    WRK-EDYMD           TO  LNK-DAY5-END
           CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                           LNK-DAY5-AREA
           IF      ( STS-DAY-RC1       =   ZERO )
               MOVE    LNK-DAY5-NISSU1 TO  WRK-NISSU1
               MOVE    LNK-DAY5-NISSU2 TO  WRK-NISSU2
           END-IF
      *
           .
       800-KIKAN-CALC-EXT.
           EXIT.
      *****************************************************************
      *    カレンダー処理
      *****************************************************************
       800-CALENDAR-SEC                SECTION.
      *
      *    増減パターン 1:日／2:月／3:年
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-YMD             TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-KEISANYMD
      *
           .
       800-CALENDAR-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理(KEY79)
      *****************************************************************
       900-PTNYUINRRK-KEY79-SEL-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key79"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key79"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNYUINRRK-KEY79-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理(KEY10)
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
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
      *    入院履歴検索処理(KEY)
      *****************************************************************
       900-PTNYUINRRK-KEY-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    1               TO  FLG-PTNYUINRRK
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
      *    患者入院履歴情報検索処理
      *****************************************************************
       900-PTNYUINRRK-KEY75-SEL-SEC      SECTION.
      *
           MOVE    ZERO            TO   FLG-PTNYUINRRK
      *
           MOVE    TRRK-REC        TO  PTNYUINRRK-REC
           MOVE    TRRK-NYUINYMD   TO  PTNYUINRRK-TAIINYMD
           MOVE    PTNYUINRRK-REC  TO  MCPDATA-REC
      *
           MOVE    "tbl_ptnyuinrrk"   TO  MCP-TABLE
           MOVE    "key75"            TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTNYUINRRK-REC
           ELSE
               MOVE    1           TO  FLG-PTNYUINRRK
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key75"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-PTNYUINRRK-KEY75-SEL-EXT.
           EXIT.
      *****************************************************************
      *    オーダー検索処理(KEY)
      *****************************************************************
       900-ORDER-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-ORDER
      *
           MOVE    ORDER-REC           TO  MCPDATA-REC
           MOVE    "tbl_order"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    1               TO  FLG-ORDER
           END-IF
      *
           MOVE    "tbl_order"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-ORDER-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    オーダ項目検索処理(KEY)
      *****************************************************************
       900-ODRITEM-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-ODRITEM
      *
           MOVE    ODRITEM-REC         TO  MCPDATA-REC
           MOVE    "tbl_odritem"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    1               TO  FLG-ODRITEM
           END-IF
      *
           MOVE    "tbl_odritem"       TO  MCP-TABLE
           MOVE    "key"               TO MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-ODRITEM-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    オーダーFETCH処理(KEY3)
      *****************************************************************
       900-ORDER-KEY3-FET-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-ORDER
      *
           MOVE    ORDER-REC           TO  MCPDATA-REC
           MOVE    "tbl_order"         TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  ORDER-REC
           ELSE
               INITIALIZE                  ORDER-REC
               MOVE    1               TO  FLG-ORDER
           END-IF
      *
           .
       900-ORDER-KEY3-FET-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ検索処理(KEY)
      *****************************************************************
       900-HKNCOMBI-KEY-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-HKNCOMBI
      *
           INITIALIZE                      HKNCOMBI-REC
           MOVE    SODR-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    SODR-PTID           TO  COMB-PTID
           MOVE    WRK-COMBI-HKNCOMBI  TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-HKNCOMBI-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
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
       910-DB-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
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
       910-DB-SELECT-EXT.
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
      *    ＤＢインサート処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ更新処理
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ削除処理
      *****************************************************************
       910-DBDELETE-SEC                SECTION.
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    ＰＵＳＨＥＶＥＮＴ処理
      *****************************************************************
       910-PUSHEVENT-SEC               SECTION.
      *
           MOVE    "PUSHEVENT"         TO      MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-PUSHEVENT-EXT.
           EXIT.
      *****************************************************************
      *    排他制御処理（チェックのみ）
      *****************************************************************
       990-LOCK-CHK-SEC                SECTION.
      *
           MOVE    MCP-WINDOW          TO  WRK-MCP-WINDOW
      *
      *    排他制御処理
           MOVE    "I01"               TO  SPA-MOTOPG
                                           MCP-WINDOW
           MOVE    1                   TO  SLOCK-MODE
           MOVE    SODR-PTID           TO  SLOCK-PTID
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           IF    ( SLOCK-RETURN    NOT =   ZERO )
               MOVE    "9999"          TO  ERRCD-ORDER
           END-IF
      *
      *    排他制御解除処理
           MOVE    ZERO                TO  SLOCK-PTID
           MOVE    3                   TO  SLOCK-MODE
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
      *
           MOVE    WRK-MCP-WINDOW      TO  MCP-WINDOW
      *
           .
       990-LOCK-CHK-EXT.
           EXIT.
      *
