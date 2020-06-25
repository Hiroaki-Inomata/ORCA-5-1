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
       PROGRAM-ID.                     ORCSODR10.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : オーダー編集（医療区分・ＡＤＬ点数登録）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  14/05/30    NACL−太田    新規作成
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
           03  FLG-ORDER               PIC 9(01).
           03  FLG-ODRITEM             PIC 9(01).
           03  FLG-NYUINACCT           PIC 9(01).
           03  FLG-RYOCOM              PIC 9(01).
           03  FLG-HKNCOMBI            PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-END                 PIC 9(01).
           03  FLG-ORCSNUM             PIC 9(01).
           03  FLG-BREAK               PIC 9(01).
           03  FLG-OVER                PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-NISSU               PIC 9(05).
           03  CNT-COMMA               PIC 9(05).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
           03  IDX1                    PIC 9(05).
           03  IDX2                    PIC 9(05).
           03  IDX3                    PIC 9(05).
           03  IDX4                    PIC 9(05).
           03  IDXA                    PIC 9(05).
           03  IDXD                    PIC 9(05).
           03  IDXH                    PIC 9(05).
           03  IDXI                    PIC 9(05).
           03  IDXJ                    PIC 9(05).
           03  IDXM                    PIC 9(05).
           03  IDX-MED                 PIC 9(05).
           03  IDX-ADL                 PIC 9(05).
           03  IDXP                    PIC 9(05).
           03  IDX-ODR                 PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD                PIC X(10).
           03  WRK-ZOGENPTN            PIC X(01).
           03  WRK-ZOGEN               PIC S9(05).
           03  WRK-COMBI-HKNCOMBI      PIC 9(04).
           03  WRK-SYS-KANRICD         PIC X(04).
           03  WRK-SYS-KBNCD           PIC X(08).
           03  WRK-DIALOG              PIC 9(02).
           03  WRK-MCP-WINDOW          PIC X(64).
           03  WRK-SNUM-INX            PIC X(26).
           03  WRK-SNUM-OUTNUM         PIC 9(12).
           03  WRK-MED-ID.
               05  WRK-MED-ID9         PIC 9(02).
           03  WRK-KANACHK-INPUT       PIC X(100).
           03  WRK-KANACHK-MAX         PIC 9(05).
           03  WRK-ZAINUM              PIC 9(08).
           03  WRK-DATA                PIC X(10).
           03  WRK-STIDX               PIC 9(05).
           03  WRK-EDIDX               PIC 9(05).
           03  WRK-LASTYMD.
               05  WRK-LASTYM          PIC X(06).
               05  WRK-LASTDD          PIC 9(02).
           03  WRK-SRYYM               PIC X(06).
           03  WRK-MED-DATA            PIC 9(01).
           03  WRK-ADL-TEN             PIC 9(03).
           03  WRK-DESIGNR-TEN         PIC 9(03).
      *
       01  ITEM-AREA.
           03  ITEM-SRYYMD.
               05  ITEM-SRYYM              PIC X(06).
               05  ITEM-SRYDD              PIC 9(02).
           03  ITEM-MED-AREA.
               05  ITEM-MED-MAX            PIC 9(03).
               05  ITEM-MED-OCC            OCCURS  100.
                   07  ITEM-MED-ID         PIC X(02).
                   07  ITEM-MED-DATA       PIC X(01).
                   07  ITEM-MED-ALL        PIC X(128).
                   07  ITEM-MED-ZAINUM     PIC 9(04).
                   07  ITEM-MED-IDX-MEDH20 PIC 9(03).
                   07  ITEM-MED-IDX-ID     PIC 9(03).
           03  ITEM-ADL-AREA.
               05  ITEM-ADL-MAX            PIC 9(03).
               05  ITEM-ADL-OCC            OCCURS  4.
                   07  ITEM-ADL-ID         PIC X(01).
                   07  ITEM-ADL-TEN        PIC X(01).
                   07  ITEM-ADL-ALL        PIC X(128).
                   07  ITEM-ADL-ZAINUM     PIC 9(04).
                   07  ITEM-ADL-IDX-ID     PIC 9(03).
           03  ITEM-DESIGNR-AREA.
               05  ITEM-DESIGNR-ALL        PIC X(128).
               05  ITEM-DESIGNR-IDX-ID     PIC 9(03).
      *
       01  TSRYYM-AREA.
           03  TSRYYM-SRYYMD.
               05  TSRYYM-SRYYM            PIC X(06).
               05  TSRYYM-SRYDD            PIC 9(02).
           03  TSRYYM-NYUIN-NISSU          PIC 9(03).
           03  TSRYYM-NYUIN-DAY-G.
               05  TSRYYM-NYUIN-DAY        PIC 9(01)
                                           OCCURS 31.
      *
       01  TRYOCOM-AREA.
        02 TRYOCOM-MAX                     PIC 9(03).
        02 TRYOCOM-OCC                     OCCURS  100.
           COPY    "CPRYOCOM.INC"          REPLACING   //RYOCOM//
                                           BY         //TRYOCOM//.
       01  WRYOCOM-AREA.
           COPY    "CPRYOCOM.INC"          REPLACING   //RYOCOM//
                                           BY         //WRYOCOM//.
      *
       01  ERRCD-AREA.
           03  ERRCD-ITEM-G.
               05  ERRCD-ITEM          PIC X(04).
               05  ERRCD-ORDER         PIC X(04).
           03  ERRCD-ERRCD             PIC X(04).
      *
       01  CONST-AREA.
           03  CONST-H300401           PIC X(08)   VALUE "20180401".
      *
      *    項目IDコピー句
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
           COPY  "CPMEDKBN.INC".
           COPY  "CPADLKBN.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1005.INC".
      *
      *    療養コメント
       01  RYOCOM-REC.
           COPY    "CPRYOCOM.INC".
      *
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    オーダーテーブル
       01  ORDER-REC.
           COPY    "CPORDER.INC".
      *
      *    オーダ項目テーブル
       01  ODRITEM-REC.
           COPY    "CPODRITEM.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    オーダー共通サブ
           COPY    "CPORCSODRS01.INC".
      *
      *    医療区分、ＡＤＬ区分編集サブ
           COPY    "CPORCSS005.INC".
           COPY    "CPORCSS011.INC".
      *
      *    医療区分・ＡＤＬ点数取得サブ
           COPY    "CPORCSNINF04.INC".
           COPY    "CPORCHCN68S01.INC".
      *
      *    入院会計照会用テーブル作成
           COPY    "CPORCSS010.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    入院会計作成サブ
           COPY    "CPORCSODRNACCT.INC".
       01  LNK-PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC"  REPLACING  //PTNYUINRRK-//
                                       BY         //LNK-PTNYUINRRK-//.
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
           COPY    "CPORCSGDAY.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    排他制御
           COPY    "CPORCSLOCK.INC".
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
           02  LNK-ODRITEM-OCC         OCCURS   150.
           COPY    "CPODRITEM.INC"     REPLACING  //ODRITEM-//
                                       BY         //LNK-ODRITEM-//.
           COPY    "MCPAREA".
           COPY    "COMMON-SPA".
      *
      ******************************************************************
      *
       PROCEDURE                       DIVISION
                                       USING   SODR-AREA
                                               LNK-ODRITEM-REC
                                               MCPAREA
                                               SPA-AREA.
      *
      *****************************************************************
      *    主　　処理
      *
      *01  LNK-ODRITEM-REC.
      *    02  LNK-ODRITEM-MAX         PIC 9(03).
      *    02  LNK-ODRITEM-OCC         OCCURS   150.
      *                                         ↑他は30なので注意
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
      *
           PERFORM 100-INIT-SEC
      *
           IF    ( FLG-END     =   ZERO )
               PERFORM 200-MAIN-SEC
           END-IF
      *
           PERFORM 300-END-SEC
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
           INITIALIZE                  CNT-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  ERRCD-AREA
           INITIALIZE                  ITEM-AREA
           INITIALIZE                  SODR-OT
           INITIALIZE                  TRRK-REC
           INITIALIZE                  TRYOCOM-AREA
           INITIALIZE                  TSRYYM-AREA
      *
           PERFORM 800-MCNDATE-SEC
      *
           MOVE    SODR-SRYYMD     TO  ITEM-SRYYMD
      *
           MOVE    SODR-SRYYMD     TO  WRK-SYMD
           MOVE    "01"            TO  WRK-SYMD(7:2)
           MOVE    "1"             TO  WRK-ZOGENPTN
           MOVE    -1              TO  WRK-ZOGEN
           PERFORM 800-CALENDAR-SEC
           MOVE    WRK-SYMD        TO  WRK-LASTYMD
      *
           IF    ( SODR-SRCKBN     =    "1" OR "2" )
               PERFORM 990-LOCK-CHK-SEC
           END-IF
      *
           IF    ( ERRCD-ITEM-G  =   SPACE )
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    SODR-HOSPNUM    TO  PTNYUINRRK-HOSPNUM
               MOVE    SODR-PTID       TO  PTNYUINRRK-PTID
               MOVE    SODR-SRYYMD     TO  PTNYUINRRK-TENNYUYMD
                                           PTNYUINRRK-TENSTUYMD
               PERFORM 900-PTNYUINRRK-KEY42-SEL-SEC
               IF    ( FLG-PTNYUINRRK  =   ZERO )
                   MOVE    PTNYUINRRK-REC  TO  TRRK-REC
               ELSE
                   MOVE    "0075"          TO  ERRCD-ORDER
               END-IF
           END-IF
      *
           IF    ( ERRCD-ITEM-G  =   SPACE )
      *
               INITIALIZE                  SS005-AREA
               MOVE    SODR-SRYYMD     TO  SS005-SRYYM
               CALL    "ORCSS005"          USING
                                           SS005-AREA
                                           SPA-AREA
      *
               INITIALIZE                  HCN68S01-AREA
               MOVE    SPA-PTID        TO  HCN68S01-PTID
               MOVE    SODR-SRYYMD (1:6)
                                       TO  HCN68S01-SRYYM
               MOVE    SODR-SRYYMD     TO  HCN68S01-SRYYMD
               CALL    "ORCHCN68S01"       USING
                                           SPA-AREA
                                           HCN68S01-AREA
      *
               PERFORM 1001-TRYOCOM-EDIT-SEC
      *
           END-IF
      *
           MOVE    SODR-SRYYMD         TO  TSRYYM-SRYYMD
           PERFORM VARYING IDXD    FROM    1   BY  1
                   UNTIL ( IDXD    >   31 )
      *
               MOVE    IDXD            TO  TSRYYM-SRYDD
      *
               IF    ( TRRK-NYUINYMD   <=  TSRYYM-SRYYMD )
                AND  ( TRRK-TAIINYMD   >=  TSRYYM-SRYYMD )
                   IF    ( HCN68S01-NYUIN-DAY (IDXD)
                                       >   ZERO )
                       MOVE    HCN68S01-NYUIN-DAY (IDXD)
                                       TO  TSRYYM-NYUIN-DAY (IDXD)
                       COMPUTE TSRYYM-NYUIN-NISSU
                           =   TSRYYM-NYUIN-NISSU
                           +   1
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    療養レセプトコメントマスタ退避領域編集処理
      *****************************************************************
       1001-TRYOCOM-EDIT-SEC           SECTION.
      *
           MOVE    ZERO            TO  IDXA
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   MEDH20-MAX )
      *
               IF    ( SODR-SRYYMD >=  MEDH20-STYMD (IDX0) )
                AND  ( SODR-SRYYMD <=  MEDH20-EDYMD (IDX0) )
      *
                   COMPUTE IDXA    =   IDXA    +   1
      *
                   MOVE    SPA-HOSPNUM     TO  TRYOCOM-HOSPNUM (IDXA)
                   MOVE    "1"             TO  TRYOCOM-NYUGAIKBN (IDXA)
                   MOVE    SPA-PTID        TO  TRYOCOM-PTID  (IDXA)
                   MOVE    SODR-SRYYMD (1:6)
                                           TO  TRYOCOM-SRYYM (IDXA)
                   MOVE    MEDH20-ZAINUM (IDX0)
                                           TO  TRYOCOM-ZAINUM (IDXA)
                   MOVE    "2"             TO  TRYOCOM-ZAISKBKBN (IDXA)
                   MOVE    MEDH20-CDL    (IDX0)
                                           TO  TRYOCOM-SRYCD (IDXA)
      *
               END-IF
      *
           END-PERFORM
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   ADL-MAX )
      *
               IF    ( SODR-SRYYMD >=  ADL-STYMD (IDX0) )
                AND  ( SODR-SRYYMD <=  ADL-EDYMD (IDX0) )
      *
                   COMPUTE IDXA    =   IDXA    +   1
      *
                   MOVE    SPA-HOSPNUM     TO  TRYOCOM-HOSPNUM (IDXA)
                   MOVE    "1"             TO  TRYOCOM-NYUGAIKBN (IDXA)
                   MOVE    SPA-PTID        TO  TRYOCOM-PTID  (IDXA)
                   MOVE    SODR-SRYYMD (1:6)
                                           TO  TRYOCOM-SRYYM (IDXA)
      *
                   MOVE    ADL-ZAINUM (IDX0)
                                           TO  TRYOCOM-ZAINUM (IDXA)
                   MOVE    "1"             TO  TRYOCOM-ZAISKBKBN (IDXA)
      *
               END-IF
      *
           END-PERFORM
      *
           IF    ( SODR-SRYYMD     >=  CONST-H300401 )
      *
               COMPUTE IDXA    =   IDXA    +   1
      *
               MOVE    SPA-HOSPNUM     TO  TRYOCOM-HOSPNUM (IDXA)
               MOVE    "1"             TO  TRYOCOM-NYUGAIKBN (IDXA)
               MOVE    SPA-PTID        TO  TRYOCOM-PTID  (IDXA)
               MOVE    SODR-SRYYMD (1:6)
                                       TO  TRYOCOM-SRYYM (IDXA)
               MOVE    DESIGNR-ZAINUM-EVAL
                                       TO  TRYOCOM-ZAINUM (IDXA)
               MOVE    "A"             TO  TRYOCOM-ZAISKBKBN (IDXA)
               MOVE    DESIGNR-SRYCD-EVAL
                                       TO  TRYOCOM-SRYCD (IDXA)
      *
               MOVE    TRYOCOM-OCC (IDXA)
                                       TO  TRYOCOM-OCC (IDXA + 1)
               COMPUTE IDXA    =   IDXA    +   1
      *
               MOVE    DESIGNR-ZAINUM  TO  TRYOCOM-ZAINUM (IDXA)
               MOVE    "A"             TO  TRYOCOM-ZAISKBKBN (IDXA)
               MOVE    DESIGNR-SRYCD   TO  TRYOCOM-SRYCD (IDXA)
      *
           END-IF
      *
           MOVE    IDXA            TO  TRYOCOM-MAX
      *
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   TRYOCOM-MAX )
      *
               MOVE    TRYOCOM-ZAINUM (IDXA)
                                   TO  WRK-ZAINUM
               MOVE    TRYOCOM-SRYYM (IDXA)
                                   TO  WRK-SRYYM
               PERFORM 900-RYOCOM-KEY-SEL-SEC
               IF    ( FLG-RYOCOM  =   ZERO )
      *
                   IF    ( RYOCOM-ZAISKBKBN    =   "2" )
                       MOVE    RYOCOM-MONTH
                                   TO  TRYOCOM-MONTH (IDXA)
                   END-IF
      *
                   PERFORM VARYING IDXD    FROM    1   BY  1
                           UNTIL ( IDXD    >   31 )
                       IF    ( RYOCOM-ZAISKBKBN    =   "1" )
                        AND  ( RYOCOM-MONTH        >   ZERO )
                           IF    ( HCN68S01-RYOYO-DAY (IDXD)
                                                   >   ZERO )
                               MOVE    RYOCOM-MONTH
                                   TO  TRYOCOM-DAY (IDXA IDXD)
                           END-IF
                       ELSE
                           MOVE    RYOCOM-DAY (IDXD)
                                   TO  TRYOCOM-DAY (IDXA IDXD)
                       END-IF
                   END-PERFORM
               END-IF
      *
           END-PERFORM
      *
           .
       1001-TRYOCOM-EDIT-EXT.
           EXIT.
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
                  PERFORM 2101-RYOCOM-INSERT-SEC
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
               EVALUATE    LNK-ODRITEM-ITEMID (IDX-ODR)
               WHEN    NCLAIMID-MEDIDJMA
                   PERFORM 20011-MEDID-CHK-SEC
               WHEN    NCLAIMID-MEDDATAJMA
                   PERFORM 20011-MEDDATA-CHK-SEC
               WHEN    NCLAIMID-MEDALLJMA
                   PERFORM 20011-MEDALL-CHK-SEC
               WHEN    NCLAIMID-ADLIDJMA
                   PERFORM 20011-ADLID-CHK-SEC
               WHEN    NCLAIMID-ADLTENJMA
                   PERFORM 20011-ADLTEN-CHK-SEC
               WHEN    NCLAIMID-ADLALLJMA
                   PERFORM 20011-ADLALL-CHK-SEC
               WHEN    NCLAIMID-DESIGNRALLJMA
                   PERFORM 20011-DESIGNRALL-CHK-SEC
               END-EVALUATE
      *
           END-PERFORM
      *
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    医療区分項目番号チェック処理
      *****************************************************************
       20011-MEDID-CHK-SEC             SECTION.
      *
           COMPUTE IDX-MED =   IDX-MED     +   1
           MOVE    IDX-MED     TO   ITEM-MED-MAX
      *
           MOVE    IDX-ODR TO  ITEM-MED-IDX-ID (IDX-MED)
      *
           MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                               TO  ITEM-MED-ID (IDX-MED)
           MOVE    ITEM-MED-ID (IDX-MED)
                               TO  WRK-SNUM-INX
      *
           PERFORM 800-ORCSNUM-SEC
           IF    ( FLG-ORCSNUM         =   ZERO )
               MOVE    WRK-SNUM-OUTNUM TO  WRK-MED-ID9
      *
               MOVE    ZERO            TO  IDXH
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   MEDH20-MAX )
                        OR   ( IDXH    >   ZERO )
      *
                   IF    ( MEDH20-ID (IDX0)
                                       =   WRK-MED-ID )
                    AND  ( SODR-SRYYMD >=  MEDH20-STYMD(IDX0))
                    AND  ( SODR-SRYYMD <=  MEDH20-EDYMD(IDX0))
                       MOVE    MEDH20-ZAINUM (IDX0)
                                       TO  ITEM-MED-ZAINUM (IDX-MED)
                       MOVE    IDX0    TO  IDXH
                                           ITEM-MED-IDX-MEDH20 (IDX-MED)
                   END-IF
      *
               END-PERFORM
      *
               IF    ( IDXH            =   ZERO )
                   MOVE    "0146"      TO  ERRCD-ERRCD
                   PERFORM 2100-ITEM-ERR-SET-SEC
               END-IF
      *
           ELSE
               MOVE    "0146"          TO  ERRCD-ERRCD
               PERFORM 2100-ITEM-ERR-SET-SEC
           END-IF
      *
           .
       20011-MEDID-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療日の状態チェック処理
      *****************************************************************
       20011-MEDDATA-CHK-SEC           SECTION.
      *
           MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                       TO  ITEM-MED-DATA (IDX-MED)
      *
           .
       20011-MEDDATA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    複数日設定チェック処理
      *****************************************************************
       20011-MEDALL-CHK-SEC            SECTION.
      *
           MOVE    LNK-ODRITEM-MEMO (IDX-ODR)
                                       TO  ITEM-MED-ALL (IDX-MED)
      *
           .
       20011-MEDALL-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ADL項目番号チェック処理
      *****************************************************************
       20011-ADLID-CHK-SEC             SECTION.
      *
           COMPUTE IDX-ADL     =   IDX-ADL    +   1
           MOVE    IDX-ADL     TO  ITEM-ADL-MAX
      *
           MOVE    IDX-ODR             TO  ITEM-ADL-IDX-ID (IDX-ADL)
      *
           MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                       TO  ITEM-ADL-ID (IDX-ADL)
      *
           MOVE    ZERO            TO  IDXH
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   ADL-MAX )
                    OR   ( IDXH    >   ZERO )
      *
               IF    ( ADL-ID (IDX0)   =   ITEM-ADL-ID (IDX-ADL) )
                   MOVE    ADL-ZAINUM (IDX0)
                                       TO  ITEM-ADL-ZAINUM (IDX-ADL)
                   MOVE    IDX0        TO  IDXH
               END-IF
      *
           END-PERFORM
      *
           IF    ( IDXH            =   ZERO )
               MOVE    "0147"          TO  ERRCD-ERRCD
               PERFORM 2100-ITEM-ERR-SET-SEC
           END-IF
      *
           .
       20011-ADLID-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療日の点数チェック処理
      *****************************************************************
       20011-ADLTEN-CHK-SEC            SECTION.
      *
           MOVE    LNK-ODRITEM-ITEM (IDX-ODR)
                                       TO  ITEM-ADL-TEN (IDX-ADL)
      *
           .
       20011-ADLTEN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    複数日設定チェック処理
      *****************************************************************
       20011-ADLALL-CHK-SEC            SECTION.
      *
           MOVE    LNK-ODRITEM-MEMO (IDX-ODR)
                                       TO  ITEM-ADL-ALL (IDX-ADL)
      *
           .
       20011-ADLALL-CHK-EXT.
           EXIT.
      *****************************************************************
      *    複数日設定チェック処理
      *****************************************************************
       20011-DESIGNRALL-CHK-SEC            SECTION.
      *
           MOVE    LNK-ODRITEM-MEMO (IDX-ODR)
                                   TO  ITEM-DESIGNR-ALL
           MOVE    IDX-ODR         TO  ITEM-DESIGNR-IDX-ID
      *
           .
       20011-DESIGNRALL-CHK-EXT.
           EXIT.
      *****************************************************************
      *    項目未処理状態設定処理
      *****************************************************************
       2100-ITEM-UNPROCSTS-SET-SEC        SECTION.
      *
           MOVE    3               TO  LNK-ODRITEM-STATUS (IDX-ODR)
      *
           .
       2100-ITEM-UNPROCSTS-SET-EXT.
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
           IF    ( ERRCD-ITEM-G        =   SPACE )
      *        医療区分項目チェック
               PERFORM 20021-MED-CHK-SEC
           END-IF
      *
           IF    ( ERRCD-ITEM-G        =   SPACE )
      *        ＡＤＬ区分項目チェック処理
               PERFORM 20021-ADL-CHK-SEC
           END-IF
      *
           IF    ( ERRCD-ITEM-G        =   SPACE )
      *        ＤＥＳＩＧＮ−Ｒチェック処理
               PERFORM 20021-DESIGNR-CHK-SEC
           END-IF
      *
           .
       2002-KANREN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    医療区分項目チェック
      *****************************************************************
       20021-MED-CHK-SEC            SECTION.
      *
           PERFORM VARYING IDX-MED FROM    1   BY  1
                   UNTIL ( IDX-MED     >   ITEM-MED-MAX )
      *
               IF    ( ITEM-MED-ZAINUM (IDX-MED)   >   ZERO )
      *
                   PERFORM VARYING IDXA    FROM    1   BY  1
                           UNTIL ( IDXA    >   TRYOCOM-MAX )
      *
                       IF    ( TRYOCOM-ZAINUM (IDXA)
                                       =   ITEM-MED-ZAINUM (IDX-MED))
      *
                           MOVE    ITEM-MED-IDX-MEDH20 (IDX-MED)
                                               TO  IDXH
                           IF    ( ITEM-MED-ALL (IDX-MED)  =   SPACE )
                            OR   ( MEDH20-ALLKBN(IDXH) =   1 )
                               PERFORM 200211-MED-DATA-CHK-SEC
                           ELSE
                               PERFORM 200211-MED-ALL-CHK-SEC
                           END-IF
      *
                           MOVE    "UPDATE"    TO  TRYOCOM-OPID (IDXA)
      *
                           MOVE    TRYOCOM-MAX TO  IDXA
      *
                       END-IF
      *
                   END-PERFORM
               END-IF
      *
           END-PERFORM
      *
           .
       20021-MED-CHK-EXT.
           EXIT.
      *****************************************************************
      *    医療区分項目内容チェック
      *****************************************************************
       200211-MED-DATA-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  WRK-MED-DATA
      *
           IF    ( ERRCD-ITEM-G        =   SPACE )
      *
               MOVE    ITEM-MED-DATA (IDX-MED)
                                       TO  WRK-SNUM-INX
               PERFORM 800-ORCSNUM-SEC
               IF    ( FLG-ORCSNUM      =  ZERO )
                AND  ( WRK-SNUM-OUTNUM >=  0 )
                AND  ( WRK-SNUM-OUTNUM <=  1 )
                   MOVE    WRK-SNUM-OUTNUM TO  WRK-MED-DATA
               ELSE
                   MOVE    "0148"          TO  ERRCD-ERRCD
                   MOVE    ITEM-MED-IDX-ID (IDX-MED)
                                           TO  IDX-ODR
                   PERFORM 2100-ITEM-ERR-SET-SEC
               END-IF
           END-IF
      *
           IF    ( ERRCD-ITEM-G            =   SPACE )
                   IF    ( MEDH20-ALLKBN(IDXH) =   1 )
                       IF    ( TSRYYM-NYUIN-NISSU    =   ZERO )
                           MOVE    "0075"      TO  ERRCD-ERRCD
                           MOVE    ITEM-MED-IDX-ID (IDX-MED)
                                               TO  IDX-ODR
                           PERFORM 2100-ITEM-ERR-SET-SEC
                       END-IF
                   ELSE
                       IF    ( TSRYYM-NYUIN-DAY (ITEM-SRYDD)
                                           =   ZERO )
                           MOVE    "0075"      TO  ERRCD-ERRCD
                           MOVE    ITEM-MED-IDX-ID (IDX-MED)
                                               TO  IDX-ODR
                           PERFORM 2100-ITEM-ERR-SET-SEC
                       END-IF
                   END-IF
           END-IF
      *
           IF    ( ERRCD-ITEM-G    =   SPACE )
      *
               IF    ( MEDH20-ALLKBN(IDXH) =   1 )
                   MOVE    WRK-MED-DATA
                                       TO  TRYOCOM-MONTH (IDXA)
               ELSE
                   MOVE    WRK-MED-DATA
                                       TO  TRYOCOM-DAY (IDXA ITEM-SRYDD)
               END-IF
      *
               PERFORM 2002111-NISSU-CHK-SEC
               IF    (  FLG-OVER NOT =   ZERO )
                   MOVE    "0151"          TO  ERRCD-ERRCD
                   MOVE    ITEM-MED-IDX-ID (IDX-MED)
                                           TO  IDX-ODR
                   PERFORM 2100-ITEM-ERR-SET-SEC
               END-IF
      *
           END-IF
      *
           .
       200211-MED-DATA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    医療区分項目チェック（複数日指定）
      *****************************************************************
       200211-MED-ALL-CHK-SEC           SECTION.
      *
           MOVE    ZERO            TO  IDXD
           MOVE    1               TO  IDXP
           MOVE    ZERO            TO  FLG-BREAK
      *
           MOVE    ZERO            TO  CNT-COMMA
           INSPECT ITEM-MED-ALL (IDX-MED) TALLYING CNT-COMMA FOR ALL ","
           IF    ( CNT-COMMA       NOT =   30 )
               MOVE    "0150"          TO  ERRCD-ERRCD
               MOVE    ITEM-MED-IDX-ID (IDX-MED)
                                       TO  IDX-ODR
               PERFORM 2100-ITEM-ERR-SET-SEC
           END-IF
      *
           PERFORM UNTIL ( FLG-BREAK   NOT =   ZERO )
                    OR   ( IDXD   >=   31 )
                    OR   ( ERRCD-ITEM-G    NOT =   SPACE )
      *
               COMPUTE IDXD    =   IDXD    +  1
      *
               MOVE    SPACE       TO  WRK-DATA
               UNSTRING    ITEM-MED-ALL (IDX-MED) DELIMITED BY ","
               INTO        WRK-DATA
               WITH POINTER IDXP
               NOT  ON OVERFLOW
                  MOVE    1        TO  FLG-BREAK
               END-UNSTRING
      *
               EVALUATE    WRK-DATA
               WHEN   SPACE
                   CONTINUE
               WHEN   "0"
               WHEN   "1"
                   IF    ( TSRYYM-NYUIN-DAY (IDXD)
                                           =   ZERO )
                       MOVE    "0075"      TO  ERRCD-ERRCD
                       MOVE    ITEM-MED-IDX-ID (IDX-MED)
                                           TO  IDX-ODR
                       PERFORM 2100-ITEM-ERR-SET-SEC
                   END-IF
      *
                   IF    ( ERRCD-ITEM-G        =   SPACE )
                       IF    ( WRK-DATA        =   "0" )
                           MOVE    0   TO  TRYOCOM-DAY (IDXA IDXD)
                       ELSE
                           MOVE    1   TO  TRYOCOM-DAY (IDXA IDXD)
                       END-IF
                   END-IF
      *
               WHEN   OTHER
                   MOVE    "0149"          TO  ERRCD-ERRCD
                   MOVE    ITEM-MED-IDX-ID (IDX-MED)
                                           TO  IDX-ODR
                   PERFORM 2100-ITEM-ERR-SET-SEC
               END-EVALUATE
      *
           END-PERFORM
      *
           IF    ( ERRCD-ITEM-G        =   SPACE )
               PERFORM 2002111-NISSU-CHK-SEC
               IF    (  FLG-OVER NOT =   ZERO )
                   MOVE    "0151"          TO  ERRCD-ERRCD
                   MOVE    ITEM-MED-IDX-ID (IDX-MED)
                                           TO  IDX-ODR
                   PERFORM 2100-ITEM-ERR-SET-SEC
               END-IF
           END-IF
      *
           .
       200211-MED-ALL-CHK-EXT.
           EXIT.
      *****************************************************************
      *    日数チェック
      *****************************************************************
       2002111-NISSU-CHK-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-OVER
      *
           MOVE    TRYOCOM-OCC (IDXA)  TO  WRYOCOM-AREA
      *
           IF    ( WRYOCOM-ZAINUM          =   2004
                                            OR 2020
                                            OR 2022
                                            OR 2027
                                            OR 2031 )
      *
               MOVE    ZERO    TO  CNT-NISSU
      *
               MOVE    WRK-LASTYM
                               TO  WRK-SRYYM
               MOVE    WRYOCOM-ZAINUM
                               TO  WRK-ZAINUM
               PERFORM 900-RYOCOM-KEY-SEL-SEC
               IF    ( FLG-RYOCOM  =   ZERO )
                   COMPUTE WRK-STIDX   =   WRK-LASTDD
                   MOVE    ZERO    TO  WRK-EDIDX
                   IF    ( WRYOCOM-ZAINUM  =   2020 )
                       COMPUTE WRK-EDIDX
                           =   WRK-LASTDD
                           -   13
                   ELSE
                       COMPUTE WRK-EDIDX
                           =   WRK-LASTDD
                           -   6
                   END-IF
      *
                   PERFORM VARYING IDXD    FROM    WRK-STIDX   BY  -1
                           UNTIL ( IDXD        <   WRK-EDIDX )
                            OR   ( RYOCOM-DAY (IDXD)
                                               =   ZERO )
      *
                        COMPUTE CNT-NISSU   =   CNT-NISSU   +   1
      *
                   END-PERFORM
               END-IF
      *
               PERFORM VARYING IDXD    FROM    1   BY  1
                       UNTIL ( IDXD    >   31 )
                        OR   ( FLG-OVER        NOT = ZERO )
      *
                   IF    ( WRYOCOM-DAY (IDXD)  =   ZERO )
                       MOVE    ZERO    TO  CNT-NISSU
                   ELSE
                       COMPUTE CNT-NISSU
                           =   CNT-NISSU   +   1
                       IF    ( WRYOCOM-ZAINUM  =   2020 )
                           IF    ( CNT-NISSU   >   14 )
                               MOVE    1       TO  FLG-OVER
                           END-IF
                       ELSE
                           IF    ( CNT-NISSU   >   7 )
                               MOVE    1       TO  FLG-OVER
                           END-IF
                       END-IF
                   END-IF
      *
               END-PERFORM
           END-IF
      *
           .
       2002111-NISSU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ＡＤＬ区分項目チェック処理
      *****************************************************************
       20021-ADL-CHK-SEC           SECTION.
      *
           PERFORM VARYING IDX-ADL FROM    1   BY  1
                   UNTIL ( IDX-ADL >   ITEM-ADL-MAX )
      *
               IF    ( ITEM-ADL-ZAINUM (IDX-ADL)   >   ZERO )
      *
                   PERFORM VARYING IDXA    FROM    1   BY  1
                           UNTIL ( IDXA    >   TRYOCOM-MAX )
      *
                       IF    ( TRYOCOM-ZAINUM (IDXA)
                                       =   ITEM-ADL-ZAINUM (IDX-ADL))
      *
                           IF    ( ITEM-ADL-ALL (IDX-ADL)  =   SPACE )
                               PERFORM 200211-ADL-TEN-CHK-SEC
                           ELSE
                               PERFORM 200211-ADL-ALL-CHK-SEC
                           END-IF
      *
                           MOVE    "UPDATE"    TO  TRYOCOM-OPID (IDXA)
      *
                           MOVE    TRYOCOM-MAX TO  IDXA
      *
                       END-IF
      *
                   END-PERFORM
               END-IF
      *
           END-PERFORM
      *
           .
       20021-ADL-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ADL区分項目点数チェック
      *****************************************************************
       200211-ADL-TEN-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  WRK-ADL-TEN
      *
           IF    ( ERRCD-ITEM-G        =   SPACE )
               MOVE    ITEM-ADL-TEN (IDX-ADL)
                                       TO  WRK-SNUM-INX
               PERFORM 800-ORCSNUM-SEC
               IF    ( FLG-ORCSNUM      =  ZERO )
                AND  ( WRK-SNUM-OUTNUM >=  0 )
                AND  ( WRK-SNUM-OUTNUM <=  6 )
                   MOVE    WRK-SNUM-OUTNUM TO  WRK-ADL-TEN
               ELSE
                   MOVE    "0153"          TO  ERRCD-ERRCD
                   MOVE    ITEM-ADL-IDX-ID (IDX-ADL)
                                           TO  IDX-ODR
                   PERFORM 2100-ITEM-ERR-SET-SEC
               END-IF
           END-IF
      *
           IF    ( ERRCD-ITEM-G        =   SPACE )
               IF    ( TSRYYM-NYUIN-DAY (ITEM-SRYDD)
                                       =   ZERO )
                   MOVE    "0075"      TO  ERRCD-ERRCD
                   MOVE    ITEM-ADL-IDX-ID (IDX-ADL)
                                       TO  IDX-ODR
                   PERFORM 2100-ITEM-ERR-SET-SEC
               END-IF
           END-IF
      *
           IF    ( ERRCD-ITEM-G        =   SPACE )
               MOVE    WRK-ADL-TEN  TO TRYOCOM-DAY (IDXA ITEM-SRYDD)
           END-IF
      *
           .
       200211-ADL-TEN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ADL区分項目チェック（複数日指定）
      *****************************************************************
       200211-ADL-ALL-CHK-SEC           SECTION.
      *
           MOVE    ZERO            TO  IDXD
           MOVE    1               TO  IDXP
           MOVE    ZERO            TO  FLG-BREAK
      *
           MOVE    ZERO            TO  CNT-COMMA
           INSPECT ITEM-ADL-ALL (IDX-ADL) TALLYING CNT-COMMA FOR ALL ","
           IF    ( CNT-COMMA       NOT =   30 )
               MOVE    "0154"          TO  ERRCD-ERRCD
               MOVE    ITEM-ADL-IDX-ID (IDX-ADL)
                                       TO  IDX-ODR
               PERFORM 2100-ITEM-ERR-SET-SEC
           END-IF
      *
           PERFORM UNTIL ( FLG-BREAK   NOT =   ZERO )
                    OR   ( IDXD    >=  31 )
                    OR   ( ERRCD-ITEM-G    NOT =   SPACE )
      *
               COMPUTE IDXD    =   IDXD    +  1
      *
               MOVE    ZERO        TO  WRK-ADL-TEN
      *
               MOVE    SPACE       TO  WRK-DATA
               UNSTRING    ITEM-ADL-ALL (IDX-ADL) DELIMITED BY ","
               INTO        WRK-DATA
               WITH POINTER IDXP
               NOT  ON OVERFLOW
                  MOVE    1        TO  FLG-BREAK
               END-UNSTRING
      *
               IF    ( WRK-DATA    =   SPACE )
                   CONTINUE
               ELSE
                   MOVE    WRK-DATA    TO  WRK-SNUM-INX
                   PERFORM 800-ORCSNUM-SEC
                   IF    ( FLG-ORCSNUM     =   ZERO )
                    AND  ( WRK-SNUM-OUTNUM >=  0 )
                    AND  ( WRK-SNUM-OUTNUM <=  6 )
                       MOVE    WRK-SNUM-OUTNUM
                                       TO  WRK-ADL-TEN
                       IF    ( TSRYYM-NYUIN-DAY (IDXD)
                                       =   ZERO )
                           MOVE    "0075"      TO  ERRCD-ERRCD
                           MOVE    ITEM-ADL-IDX-ID (IDX-ADL)
                                               TO  IDX-ODR
                           PERFORM 2100-ITEM-ERR-SET-SEC
                       END-IF
                   ELSE
                       MOVE    "0153"      TO  ERRCD-ERRCD
                       MOVE    ITEM-ADL-IDX-ID (IDX-ADL)
                                           TO  IDX-ODR
                       PERFORM 2100-ITEM-ERR-SET-SEC
                   END-IF
      *
                   IF    ( ERRCD-ITEM-G        =   SPACE )
                       MOVE    WRK-ADL-TEN  TO TRYOCOM-DAY (IDXA IDXD)
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
       200211-ADL-ALL-CHK-EXT.
           EXIT.
      *****************************************************************
      *    DESIGNR項目チェック処理
      *****************************************************************
       20021-DESIGNR-CHK-SEC           SECTION.
      *
           MOVE    ZERO            TO  IDXI
                                       IDXJ
      *
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   TRYOCOM-MAX )
                    OR  (( IDXI    >   ZERO )
                     AND ( IDXJ    >   ZERO ))
      *
               EVALUATE    TRYOCOM-ZAINUM (IDXA)
               WHEN    DESIGNR-ZAINUM-EVAL
                   MOVE    IDXA    TO  IDXI
               WHEN    DESIGNR-ZAINUM
                   MOVE    IDXA    TO  IDXJ
               END-EVALUATE
      *
           END-PERFORM
      *
           IF    ( IDXI                    >   ZERO )
            AND  ( IDXJ                    >   ZERO )
            AND  ( ITEM-DESIGNR-ALL    NOT =   SPACE )
      *
               PERFORM 200211-DESIGNR-CHK-SEC
      *
               MOVE    "UPDATE"    TO  TRYOCOM-OPID (IDXI)
               MOVE    "UPDATE"    TO  TRYOCOM-OPID (IDXJ)
           END-IF
      *
           .
       20021-DESIGNR-CHK-EXT.
           EXIT.
      *****************************************************************
      *    DESIGNR項目チェック（複数日指定）
      *****************************************************************
       200211-DESIGNR-CHK-SEC          SECTION.
      *
           MOVE    ZERO            TO  IDXD
           MOVE    1               TO  IDXP
           MOVE    ZERO            TO  FLG-BREAK
      *
           MOVE    ZERO            TO  CNT-COMMA
           INSPECT ITEM-DESIGNR-ALL
                                   TALLYING CNT-COMMA FOR ALL ","
           IF    ( CNT-COMMA       NOT =   30 )
               MOVE    "0202"          TO  ERRCD-ERRCD
               MOVE    ITEM-DESIGNR-IDX-ID
                                       TO  IDX-ODR
               PERFORM 2100-ITEM-ERR-SET-SEC
           END-IF
      *
           PERFORM UNTIL ( FLG-BREAK   NOT =   ZERO )
                    OR   ( IDXD    >=  31 )
                    OR   ( ERRCD-ITEM-G    NOT =   SPACE )
      *
               COMPUTE IDXD    =   IDXD    +  1
      *
               MOVE    ZERO        TO  WRK-DESIGNR-TEN
      *
               MOVE    SPACE       TO  WRK-DATA
               UNSTRING  ITEM-DESIGNR-ALL DELIMITED BY ","
               INTO      WRK-DATA
               WITH POINTER IDXP
               NOT  ON OVERFLOW
                  MOVE    1        TO  FLG-BREAK
               END-UNSTRING
      *
               EVALUATE    WRK-DATA
               WHEN    SPACE
                   CONTINUE
               WHEN    "-"
                   MOVE    ZERO        TO  TRYOCOM-DAY (IDXI IDXD)
                                           TRYOCOM-DAY (IDXJ IDXD)
               WHEN    OTHER
                   MOVE    WRK-DATA    TO  WRK-SNUM-INX
                   PERFORM 800-ORCSNUM-SEC
                   IF    ( FLG-ORCSNUM     =   ZERO )
                    AND  ( WRK-SNUM-OUTNUM >=  0 )
                    AND  ( WRK-SNUM-OUTNUM <=  66 )
                       MOVE    WRK-SNUM-OUTNUM
                                       TO  WRK-DESIGNR-TEN
                       IF    ( TSRYYM-NYUIN-DAY (IDXD)
                                       =   ZERO )
                           MOVE    "0075"      TO  ERRCD-ERRCD
                           MOVE    ITEM-DESIGNR-IDX-ID
                                               TO  IDX-ODR
                           PERFORM 2100-ITEM-ERR-SET-SEC
                       END-IF
                   ELSE
                       MOVE    "0203"      TO  ERRCD-ERRCD
                       MOVE    ITEM-DESIGNR-IDX-ID
                                           TO  IDX-ODR
                       PERFORM 2100-ITEM-ERR-SET-SEC
                   END-IF
      *
                   IF    ( ERRCD-ITEM-G        =   SPACE )
                       MOVE    1               TO TRYOCOM-DAY(IDXI IDXD)
                       MOVE    WRK-DESIGNR-TEN TO TRYOCOM-DAY(IDXJ IDXD)
                   END-IF
               END-EVALUATE
      *
           END-PERFORM
      *
           .
       200211-DESIGNR-CHK-EXT.
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
      *    オーダ編集処理
      *****************************************************************
       21011-ORDER-HEN-SEC             SECTION.
      *
           INITIALIZE                      ORDER-REC
      *
           MOVE   SODR-HOSPNUM         TO  ORDER-HOSPNUM
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
               IF    ( SODR-KEYGENERATE    =   2 )
                       MOVE    1       TO  ORDER-STATUS
               ELSE
                   IF    ( WRK-DIALOG      =   ZERO )
                       MOVE    2       TO  ORDER-STATUS
                   ELSE
                       MOVE    5       TO  ORDER-STATUS
                   END-IF
               END-IF
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
               DISPLAY "ORCSDR10 ERROR 001:ORDER INSERT CHK ERR"
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
               DISPLAY "ORCSDR10 ERROR 002:ORDER INSERT ERR"
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
                                       =   NCLAIMID-MEDIDJMA
                                        OR NCLAIMID-MEDDATAJMA
                                        OR NCLAIMID-MEDALLJMA
                                        OR NCLAIMID-ADLIDJMA
                                        OR NCLAIMID-ADLTENJMA
                                        OR NCLAIMID-ADLALLJMA
                                        OR NCLAIMID-DESIGNRALLJMA
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
           IF    ( FLG-ODRITEM         =   ZERO )
               DISPLAY "ORCSDR10 ERROR 003:ODRITEM INSERT CHK ERR"
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
               DISPLAY "ORCSDR10 ERROR 004:ODRITEM INSERT ERR"
                       " PTID="        SODR-PTID
                       " KARTE-KEY="   SODR-KARTE-KEY
               MOVE    1               TO  FLG-END
           END-IF
      *
           .
       21011-ODRITEM-HEN-EXT.
           EXIT.
      *****************************************************************
      *    療養レセプトコメントマスタ登録処理
      *****************************************************************
       2101-RYOCOM-INSERT-SEC          SECTION.
      *
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   TRYOCOM-MAX )
                    OR   ( FLG-END         NOT =   ZERO )
      *
               IF    ( TRYOCOM-OPID (IDXA)
                                   =   "UPDATE" )
                   MOVE    TRYOCOM-OCC (IDXA)
                                           TO  RYOCOM-REC
                   MOVE    SMCNDATE-YMD    TO  RYOCOM-CREYMD
                                               RYOCOM-UPYMD
                   MOVE    SMCNDATE-HMS    TO  RYOCOM-UPHMS
                   MOVE    RYOCOM-REC      TO  MCPDATA-REC
                   MOVE    "tbl_ryocom"    TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 910-DBDELETE-SEC
                   IF    ( MCP-RC          =   ZERO )
                       MOVE    RYOCOM-REC      TO  MCPDATA-REC
                       MOVE    "tbl_ryocom"    TO  MCP-TABLE
                       MOVE    "key"           TO  MCP-PATHNAME
                       PERFORM 910-DBINSERT-SEC
                       IF    ( MCP-RC          =   ZERO )
                           CONTINUE
                       ELSE
                          DISPLAY "ORCSDR10 ERROR 005:RYOCOM INSERT ERR"
                                   " PTID="        SODR-PTID
                                   " KARTE-KEY="   SODR-KARTE-KEY
                           MOVE    1               TO  FLG-END
                       END-IF
                   ELSE
                       DISPLAY "ORCSDR10 ERROR 006:RYOCOM DELETE ERR"
                               " PTID="        SODR-PTID
                               " KARTE-KEY="   SODR-KARTE-KEY
                       MOVE    1               TO  FLG-END
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           IF    ( FLG-END         =   ZERO )
               INITIALIZE                  SS011-AREA
               MOVE    SPA-PTID        TO  SS011-PTID
               MOVE    SODR-SRYYMD (1:6)
                                       TO  SS011-SRYYM
               MOVE    1               TO  SS011-NYUUPDFLG
               CALL    "ORCSS011"      USING
                                       SS011-AREA
                                       SPA-AREA
               IF    ( SS011-RC        NOT =   ZERO )
                   DISPLAY "ORCSDR10 ERROR 007:ORCSS011 ERR"
                           " PTID="        SODR-PTID
                           " KARTE-KEY="   SODR-KARTE-KEY
                   MOVE    1               TO  FLG-END
               END-IF
           END-IF
      *
           IF    ( FLG-END         =   ZERO )
               INITIALIZE                  SS010-AREA
               MOVE    SPA-PTID        TO  SS010-PTID
               MOVE    SODR-SRYYMD (1:6)
                                       TO  SS010-SRYYM
               CALL    "ORCSS010"      USING
                                       SS010-AREA
                                       SPA-AREA
               IF    ( SS010-RC        NOT =   ZERO )
                   DISPLAY "ORCSDR10 ERROR 008:ORCSS010 ERR"
                           " PTID="        SODR-PTID
                           " KARTE-KEY="   SODR-KARTE-KEY
                   MOVE    1               TO  FLG-END
               END-IF
           END-IF
      *
           .
       2101-ORCSODRNACCT-CALL-EXT.
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
           IF    ( FLG-END         NOT =   ZERO )
               MOVE    "tbl_ptnyuinrrk"
                                   TO  MCP-TABLE
               MOVE    "abort"     TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
           END-IF
      *
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    全角変換処理
      *****************************************************************
       800-ZENKAKU-SEC                 SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"             TO  KANACHK-SYORI
           MOVE  WRK-KANACHK-INPUT TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           IF    ( KANACHK-RC      =   ZERO )
               MOVE    KANACHK-OUT-INPUT
                                   TO  WRK-KANACHK-INPUT
               MOVE    KANACHK-MAX TO  WRK-KANACHK-MAX
           END-IF
      *
           .
       800-ZENKAKU-EXT.
           EXIT.
      *****************************************************************
      *    数値変換処理
      *****************************************************************
       800-ORCSNUM-SEC                 SECTION.
      *
           MOVE    ZERO                TO  FLG-ORCSNUM
      *
           IF    ( FUNCTION LENGTH(FUNCTION TRIM(WRK-SNUM-INX)) =   1 )
            AND  ( FUNCTION TRIM(WRK-SNUM-INX)  IS  NOT NUMERIC )
               MOVE    1               TO  FLG-ORCSNUM
           ELSE
      *
               INITIALIZE                      ORCSNUMAREA
               MOVE    WRK-SNUM-INX        TO  SNUM-INX
               CALL    "ORCSNUM"           USING   ORCSNUMAREA
               IF    ( SNUM-RC         NOT =   ZERO  )
                OR   ( SNUM-MINKBN     NOT =   SPACE )
                OR   ( SNUM-SYOKBN     NOT =   SPACE )
                   MOVE    1               TO  FLG-ORCSNUM
               ELSE
                   MOVE    SNUM-OUTNUM     TO  WRK-SNUM-OUTNUM
               END-IF
           END-IF
      *
           .
      *
       800-ORCSNUM-EXT.
           EXIT.
      *****************************************************************
      *    カレンダー処理
      *****************************************************************
       800-CALENDAR-SEC               SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-SYMD
      *
           .
       800-CALENDAR-EXT.
           EXIT.
      *****************************************************************
      *    マシン日付取得サブ
      *****************************************************************
       800-MCNDATE-SEC         SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           .
      *
       800-MCNDATE-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理(KEY42)
      *****************************************************************
       900-PTNYUINRRK-KEY42-SEL-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key42"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key42"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNYUINRRK-KEY42-SEL-EXT.
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
      *    システム管理検索処理(KEY10)
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           INITIALIZE                      SYSKANRI-REC
      *
           MOVE    WRK-SYS-KANRICD     TO  SYS-KANRICD
           MOVE    WRK-SYS-KBNCD       TO  SYS-KBNCD
           MOVE    SODR-SRYYMD         TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           MOVE    SODR-HOSPNUM        TO  SYS-HOSPNUM
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
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
      *    療養レセプトコメントマスタ検索処理
      *****************************************************************
       900-RYOCOM-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-RYOCOM
      *
           INITIALIZE                  RYOCOM-REC
           MOVE    SPA-HOSPNUM     TO  RYOCOM-HOSPNUM
           MOVE    "1"             TO  RYOCOM-NYUGAIKBN
           MOVE    SPA-PTID        TO  RYOCOM-PTID
           MOVE    WRK-SRYYM       TO  RYOCOM-SRYYM
           MOVE    WRK-ZAINUM      TO  RYOCOM-ZAINUM
           MOVE    RYOCOM-REC      TO  MCPDATA-REC
           MOVE    "tbl_ryocom"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  RYOCOM-REC
           ELSE
               MOVE    1           TO  FLG-RYOCOM
               INITIALIZE              RYOCOM-REC
           END-IF
      *
           MOVE    "tbl_ryocom"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-RYOCOM-KEY-SEL-EXT.
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
