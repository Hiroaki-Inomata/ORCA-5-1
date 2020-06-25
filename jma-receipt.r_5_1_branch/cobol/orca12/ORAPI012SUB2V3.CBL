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
       PROGRAM-ID.             ORAPI012SUB2V3.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : 保険組合せ情報返却編集サブ
      *  管理者            : 
      *  作成日付    作業者        記述
      *  17/07/XX    NACL-多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
      *INPUT-OUTPUT                SECTION.
      *FILE-CONTROL.
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-PARA                PIC 9(01).
           03  FLG-HKNCOMBI            PIC 9(01).
           03  FLG-HKNNUM              PIC 9(01).
      *
           03  FLG-SYORI               PIC 9(01).
      *
      *    カウント領域
      *01  CNT-AREA.
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDY2                PIC 9(04).
           03  IDH                 PIC 9(04).
           03  IDH2                PIC 9(04).
           03  IDD1                PIC 9(04).
           03  IDD2                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY             PIC X(04).
               05  WRK-SMM             PIC X(02).
               05  WRK-SDD             PIC X(02).
           03  WRK-HEN-DATE.
               05  WRK-HEN-YY          PIC X(04).
               05  WRK-HEN-H1          PIC X(01).
               05  WRK-HEN-MM          PIC X(02).
               05  WRK-HEN-H2          PIC X(01).
               05  WRK-HEN-DD          PIC X(02).
      *    時間編集
           03  WRK-THMS.
               05  WRK-THH             PIC X(02).
               05  WRK-TMM             PIC X(02).
               05  WRK-TSS             PIC X(02).
           03  WRK-HEN-TIME.
               05  WRK-HEN-THH         PIC X(02).
               05  WRK-HEN-T1          PIC X(01).
               05  WRK-HEN-TMM         PIC X(02).
               05  WRK-HEN-T2          PIC X(01).
               05  WRK-HEN-TSS         PIC X(02).
      *
           03  WRK-SYSYMD              PIC X(08).
      *
           03  WRK-ENDYMD.
               05  WRK-ENDYY           PIC 9(04).
               05  WRK-ENDMM           PIC 9(02).
               05  WRK-ENDDD           PIC 9(02).
      *
           03  WRK-HKNCOMBINUM         PIC X(04).
      *
           03  WRK-HKNNUM              PIC X(03).
           03  WRK-HOJOKBN             PIC X(01).
      *    負担率・額編集
           03  WRK-FTNRATE             PIC 9(02)V99.
           03  WRK-Z9                  PIC 9.ZZ.
           03  WRK-FTNRATEMEI          PIC X(04).
           03  WRK-ZZ9                 PIC ZZZZZ9.
      *
           03  WRK-INDATA              PIC X(20).
           03  WRK-OTDATA              PIC X(20).
      *
       01  WRK-HENSYU-AREA.
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HMM         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HDD         PIC Z9.
      *
      *    保険組合せの保存
       01  TBL-HKNCOMBI-AREA.
           02  TBL-HKNCOMBI-REC        OCCURS    200.
           COPY    "CPHKNCOMBI.INC"
                              REPLACING  //COMB-// BY //TBL-COMB-//.
      *
       01  TBL-HKNCOMB-MAX             PIC 9(04).
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *    保険番号
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    パラメタテーブル
       01  PARA-REC.
           COPY    "CPPARA.INC".
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *
      *    補助区分名称編集パラメタ
           COPY    "CPORAPI012SUB1.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    パラメタ
           COPY    "CPORAPI012SUB2V3.INC".
      *    スパ領域
           COPY    "COMMON-SPA".
      *    患者画面スパ領域
           COPY    "P02SCR-SPA".
      *
       PROCEDURE                   DIVISION    USING
           ORAPI012SUB2V3AREA
           SPA-AREA
           SPA-P02SCRAREA
           .
      *
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  AP12SUB2V3-COMBINATION-INF-G
           INITIALIZE                  AP12SUB2V3-ERRCD
           MOVE    ZERO                TO  AP12SUB2V3-MAX
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
      *
      *    初期　処理
           PERFORM 100-INIT-SEC
      *    主　処理
           IF      AP12SUB2V3-ERRCD    =   SPACE
               PERFORM 200-MAIN-SEC
           END-IF
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC               SECTION.
      *
           IF      AP12SUB2V3-SYSYMD   =   SPACE
               MOVE    SPA-SYSYMD          TO  WRK-SYSYMD
           ELSE
               MOVE    AP12SUB2V3-SYSYMD   TO  WRK-SYSYMD
           END-IF
      *
           IF      AP12SUB2V3-SYORIKBN =   "2"
      *        処理中
               MOVE    2               TO  FLG-SYORI
           ELSE
      *        登録内容
               MOVE    1               TO  FLG-SYORI
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC               SECTION.
      *
      *    最大件数
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "HKNCOMBI"          TO  PARA-FILEMEI
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      PARA-EDANUM         >   200
      *        ３年前までを対象
               MOVE    WRK-SYSYMD          TO  WRK-ENDYMD
               COMPUTE WRK-ENDYY           =   WRK-ENDYY
                                           -   3
               MOVE    01                  TO  WRK-ENDDD
      *        警告
               MOVE    "73"                TO  AP12SUB2V3-ERRCD
           ELSE
               MOVE    SPACE               TO  WRK-ENDYMD
           END-IF
           PERFORM 2001-HKNCOMBI-SET-SEC
      *
           IF      IDY                 >   200
      *        ２年前までを対象
               MOVE    WRK-SYSYMD          TO  WRK-ENDYMD
               COMPUTE WRK-ENDYY           =   WRK-ENDYY
                                           -   2
               MOVE    01                  TO  WRK-ENDDD
      *        警告
               MOVE    "74"                TO  AP12SUB2V3-ERRCD
      *
               PERFORM 2001-HKNCOMBI-SET-SEC
           END-IF
      *
           IF      IDY                 >   200
      *        １年前までを対象
               MOVE    WRK-SYSYMD          TO  WRK-ENDYMD
               COMPUTE WRK-ENDYY           =   WRK-ENDYY
                                           -   1
               MOVE    01                  TO  WRK-ENDDD
      *        警告
               MOVE    "75"                TO  AP12SUB2V3-ERRCD
      *
               PERFORM 2001-HKNCOMBI-SET-SEC
           END-IF
      *
           IF      TBL-HKNCOMB-MAX     >   ZERO
               PERFORM 2002-HKNCOMBI-HENSYU-SEC
           END-IF
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ出力領域保険情報編集処理
      *****************************************************************
       2001-HKNCOMBI-SET-SEC         SECTION.
      *
           MOVE    ZERO                TO  IDY
           INITIALIZE                      TBL-HKNCOMBI-AREA
           MOVE    ZERO                TO  TBL-HKNCOMB-MAX
      *
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "HKNCOMBI"          TO  PARA-FILEMEI
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
           PERFORM UNTIL     FLG-PARA      =   1
      *
               MOVE    PARA-DATA-REC       TO  HKNCOMBI-REC
      *
               IF     (WRK-ENDYMD      NOT =   SPACE )
                AND   (COMB-TEKEDYMD       <   WRK-ENDYMD)
      *            対象外
                   CONTINUE
               ELSE
      *            対象
                   ADD     1               TO  IDY
                   IF      IDY             >   200
                       MOVE    1               TO  FLG-PARA
                   ELSE
                       MOVE    HKNCOMBI-REC    TO  TBL-HKNCOMBI-REC
                                                             (IDY)
                       MOVE    IDY             TO  TBL-HKNCOMB-MAX
                   END-IF
               END-IF
               IF      FLG-PARA            =   ZERO
                   MOVE    "tbl_para"          TO  MCP-TABLE
                   MOVE    "key5"              TO  MCP-PATHNAME
                   PERFORM 990-PARA-READ-SEC
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       2001-HKNCOMBI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ出力領域保険情報編集処理
      *****************************************************************
       2002-HKNCOMBI-HENSYU-SEC         SECTION.
      *
           INITIALIZE                      AP12SUB2V3-COMBINATION-INF-G
           MOVE    ZERO                TO  AP12SUB2V3-MAX
           MOVE    ZERO                TO  WRK-HKNCOMBINUM
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   TBL-HKNCOMB-MAX
               MOVE    ZERO                TO  WRK-HKNCOMBINUM
               MOVE    ZERO                TO  IDH2
      *        保険組合せ番号の降順
               PERFORM VARYING     IDH     FROM    1   BY  1
                       UNTIL       IDH     >   TBL-HKNCOMB-MAX
                   IF      (TBL-COMB-HKNCOMBINUM(IDH)  NOT =   ZERO)
                      AND  (TBL-COMB-HKNCOMBINUM(IDH)
                                               >   WRK-HKNCOMBINUM )
                       MOVE    IDH             TO  IDH2
                       MOVE    TBL-COMB-HKNCOMBINUM(IDH)
                                               TO  WRK-HKNCOMBINUM
                   END-IF
               END-PERFORM
               IF      IDH2            >   ZERO
                   MOVE    TBL-HKNCOMBI-REC (IDH2) TO  HKNCOMBI-REC
                   PERFORM 20011-COMBINATION-HEN-SEC
                   MOVE    ZERO            TO  TBL-COMB-HKNCOMBINUM
                                                             (IDH2)
                   MOVE    IDY                 TO  AP12SUB2V3-MAX
                END-IF
           END-PERFORM
      *
           .
       2002-HKNCOMBI-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ出力領域保険情報編集処理
      *****************************************************************
       20011-COMBINATION-HEN-SEC     SECTION.
      *
      *    保険組合せ番号
           MOVE    COMB-HKNCOMBINUM
                                   TO  AP12SUB2V3-COMBINATION-NUMBER
                                                               (IDY)
      *
           IF      COMB-HKNID      =   ZERO
      *        保険なし
               CONTINUE
           ELSE
      *        保険ＩＤ
               MOVE    COMB-HKNID      TO  AP12SUB2V3-COMB-MAIN-ID
                                                               (IDY)
      *        保険の種類
               MOVE    COMB-HKNNUM     TO  AP12SUB2V3-COMB-MAIN-CLASS
                                                               (IDY)
      *        保険の名称
               MOVE    COMB-SYU-TANSEIDONAME
                                   TO  AP12SUB2V3-COMB-MAIN-NAME (IDY)
      *        継続区分
               MOVE    COMB-CONTKBN    TO  AP12SUB2V3-COMB-CONTINUATION
                                                             (IDY)
      *        補助区分
               MOVE    COMB-HOJOKBN    TO  AP12SUB2V3-COMB-ASSISTANCE
                                                             (IDY)
      *        補助区分名称
               MOVE    COMB-HKNNUM     TO  WRK-HKNNUM
               MOVE    COMB-HOJOKBN    TO  WRK-HOJOKBN
               PERFORM 200111-HOJOKBN-NAME-HEN-SEC
               MOVE    API012SUB1-HOJOKBN-NAME
                                       TO  AP12SUB2V3-COMB-ASSI-NAME
                                                                  (IDY)
      *        本人家族区分
               MOVE    COMB-HONKZKKBN  TO  AP12SUB2V3-COMB-FAMILY (IDY)
           END-IF
      *
      *    入院負担割合
           COMPUTE WRK-FTNRATE     =   COMB-NYU-KFTNRATE
                                   /   100
           MOVE    WRK-FTNRATE     TO  WRK-Z9
           MOVE    WRK-Z9          TO  WRK-FTNRATEMEI
           MOVE    WRK-FTNRATEMEI  TO  AP12SUB2V3-COMBINATION-RATE-ADM
                                                               (IDY)
      *    外来負担割合
           COMPUTE WRK-FTNRATE     =   COMB-GAI-KFTNRATE
                                   /   100
           MOVE    WRK-FTNRATE     TO  WRK-Z9
           MOVE    WRK-Z9          TO  WRK-FTNRATEMEI
           MOVE    WRK-FTNRATEMEI  TO  AP12SUB2V3-COMBINATION-RATE-OUT
                                                               (IDY)
      *    有効開始日
           MOVE    COMB-TEKSTYMD   TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE    TO  AP12SUB2V3-COMB-START-DATE(IDY)
      *
           MOVE    COMB-TEKEDYMD   TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE    TO  AP12SUB2V3-COMB-END-DATE (IDY)
      *
      *    処理中の時、削除・新規編集
           IF      FLG-SYORI           =   2
      *        削除区分
               IF      COMB-DELKBN     =   "1"
                   MOVE    "Delete"        TO  AP12SUB2V3-COMB-MODE
                                                                (IDY)
               ELSE
      *            新規の判定
                   IF      COMB-CHGKBN     =   "A"
                       MOVE    "New"       TO  AP12SUB2V3-COMB-MODE
                                                               (IDY)
                   END-IF
               END-IF
           END-IF
      *
      *    保険
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-PTHKN-MAX )
                          OR  (COMB-HKNID  =   ZERO )
               IF      COMB-HKNID      =   SPA-PTHKN-HKNID (IDX)
      *            新規の時、ＩＤクリア
                   IF     (SPA-PTHKN-MODE  (IDX)   =   "1" )
                    AND   (FLG-SYORI               =   2   )
                       MOVE    ZERO        TO  AP12SUB2V3-COMB-MAIN-ID
                                                               (IDY)
                   END-IF
      *
      *            保険者番号
                   MOVE    SPA-PTHKN-HKNJANUM (IDX)
                                       TO  AP12SUB2V3-COMB-MAIN-NUMBER
                                                               (IDY)
      *            記号
                   MOVE    SPA-PTHKN-KIGO (IDX)
                                       TO  AP12SUB2V3-COMB-MARK  (IDY)
      *            番号
                   MOVE    SPA-PTHKN-NUM  (IDX)
                                       TO  AP12SUB2V3-COMB-NUMBER (IDY)
      *R02.4       枝番
                   MOVE    SPA-PTHKN-EDABAN  (IDX)
                                       TO  AP12SUB2V3-COMB-EDABAN (IDY)
                   MOVE    SPA-PTHKN-MAX   TO  IDX
               END-IF
           END-PERFORM
      *
      *    公費情報
           PERFORM VARYING     IDY2    FROM    1   BY  1
                   UNTIL      (IDY2    >   4      )
                          OR  (COMB-KOHID (IDY2)   =   ZERO )
      *        公費ＩＤ
               MOVE    COMB-KOHID (IDY2)
                                       TO  AP12SUB2V3-INSURANCE-ID
                                                             (IDY IDY2)
      *        公費の種類
               MOVE    COMB-KOHHKNNUM (IDY2)
                                   TO  AP12SUB2V3-INSURANCE-CLASS
                                                             (IDY IDY2)
      *        公費の種類名称
               MOVE    COMB-KOH-TANSEIDONAME (IDY2)
                                   TO  AP12SUB2V3-INSURANCE-NAME
                                                             (IDY IDY2)
      *        公費情報
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL      (IDX2    >   SPA-PTKOH-MAX )
                   IF      COMB-KOHID (IDY2)   =   SPA-PTKOH-KOHID(IDX2)
      *                新規の時、ＩＤクリア
                       IF     (SPA-PTKOH-MODE (IDX2)   =   "1" )
                        AND   (FLG-SYORI               =   2   )
                           MOVE    ZERO
                                       TO  AP12SUB2V3-INSURANCE-ID
                                                             (IDY IDY2)
                       END-IF
      *
      *                負担者番号
                       MOVE    SPA-PTKOH-FTNJANUM (IDX2)
                                   TO  AP12SUB2V3-PROVIDER-NUMBER
                                                             (IDY IDY2)
      *                受給者番号
                       MOVE    SPA-PTKOH-JKYSNUM (IDX2)
                                   TO  AP12SUB2V3-RECIPIENT-NUMBER
                                                             (IDY IDY2)
                       MOVE    SPA-PTKOH-MAX       TO  IDX2
                   END-IF
               END-PERFORM
      *        負担率・負担額編集
               PERFORM 200112-KOHFTN-HENSYU-SEC
           END-PERFORM
      *
           .
       20011-COMBINATION-HEN-EXT.
           EXIT.
      *****************************************************************
      *    補助区分名称編集
      *****************************************************************
       200111-HOJOKBN-NAME-HEN-SEC             SECTION.
      *
           INITIALIZE                  ORAPI012SUB1AREA
           MOVE    WRK-HKNNUM          TO  API012SUB1-HKNNUM
           MOVE    WRK-HOJOKBN         TO  API012SUB1-HOJOKBN
           MOVE    WRK-SYSYMD          TO  API012SUB1-SYSYMD
           CALL    "ORAPI012SUB1"          USING
                                           ORAPI012SUB1AREA
                                           SPA-AREA
      *
           .
       200111-HOJOKBN-NAME-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    負担率・負担額編集処理
      *****************************************************************
       200112-KOHFTN-HENSYU-SEC        SECTION.
      *
           MOVE    SPACE               TO  HKNNUM-REC
           MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
      *    保険番号（公費）
           MOVE    COMB-KOHHKNNUM (IDY2)   TO  HKN-HKNNUM
      *    支払区分
           MOVE    COMB-KOHPAYKBN (IDY2)   TO  HKN-PAYKBN
           MOVE    COMB-TEKSTYMD       TO  HKN-TEKSTYMD
           MOVE    COMB-TEKSTYMD       TO  HKN-TEKEDYMD
      *
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 980-HKNNUM-READ-SEC
           ELSE
               INITIALIZE              HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    本人−外来−回−負担割合
           COMPUTE WRK-FTNRATE         =   HKN-HON-GAIKFTNRATE
                                       /   100
           MOVE    WRK-FTNRATE         TO  WRK-Z9
           MOVE    WRK-Z9              TO  WRK-FTNRATEMEI
           MOVE    WRK-FTNRATEMEI      TO  AP12SUB2V3-RATE-OUTPATIENT
                                                             (IDY IDY2)
      *    外来上限額
           IF      HKN-HON-GAIKKOTGAK  =   ZERO
      *        本人−外来−日−上限額
               MOVE    HKN-HON-GAIDJGNGAK  TO  WRK-ZZ9
           ELSE
      *        本人−外来−回−上限額
               MOVE    HKN-HON-GAIKKOTGAK  TO  WRK-ZZ9
           END-IF
           MOVE    WRK-ZZ9             TO  WRK-INDATA
           PERFORM 800-MOJIHEN-SEC
           MOVE    WRK-OTDATA          TO  AP12SUB2V3-MONEY-OUTPATIENT
                                                             (IDY IDY2)
      *
      *    本人−入院−回−負担割合
           COMPUTE WRK-FTNRATE         =   HKN-HON-NYUKFTNRATE
                                       /   100
           MOVE    WRK-FTNRATE         TO  WRK-Z9
           MOVE    WRK-Z9              TO  WRK-FTNRATEMEI
           MOVE    WRK-FTNRATEMEI      TO  AP12SUB2V3-RATE-ADMISSION
                                                             (IDY IDY2)
      *    外来上限額
           IF      HKN-HON-NYUKKOTGAK  =   ZERO
      *        本人−入院−日−上限額
               MOVE    HKN-HON-NYUDJGNGAK  TO  WRK-ZZ9
           ELSE
      *        本人−入院−回−上限額
               MOVE    HKN-HON-NYUKKOTGAK  TO  WRK-ZZ9
           END-IF
           MOVE    WRK-ZZ9             TO  WRK-INDATA
           PERFORM 800-MOJIHEN-SEC
           MOVE    WRK-OTDATA          TO  AP12SUB2V3-MONEY-ADMISSION
                                                             (IDY IDY2)
           .
       200112-KOHFTN-HENSYU-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    左ずめ編集処理
      *****************************************************************
       800-MOJIHEN-SEC                SECTION.
      *
           MOVE    SPACE              TO  WRK-OTDATA
           MOVE    ZERO               TO  IDD2
           PERFORM VARYING     IDD1   FROM    1   BY  1
                   UNTIL       IDD1   >   20
               IF      WRK-INDATA(IDD1:1)     NOT =   SPACE
                   MOVE    WRK-INDATA(IDD1:)  TO  WRK-OTDATA
                   MOVE    20                 TO  IDD1
               END-IF
           END-PERFORM
           .
       800-MOJIHEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       801-DAYHEN01-SEC                SECTION.
      *
           INITIALIZE                  WRK-HEN-DATE
           MOVE    WRK-SYY             TO  WRK-HEN-YY
           MOVE    WRK-SMM             TO  WRK-HEN-MM
           MOVE    WRK-SDD             TO  WRK-HEN-DD
           MOVE    "-"                 TO  WRK-HEN-H1
           MOVE    "-"                 TO  WRK-HEN-H2
           IF      WRK-SYMD            =   "99999999"
               MOVE    "12"                TO  WRK-HEN-MM
               MOVE    "31"                TO  WRK-HEN-DD
           END-IF
           IF      WRK-SYMD            =   SPACE
               MOVE    SPACE           TO  WRK-HEN-DATE
           END-IF
      *
           INITIALIZE                  WRK-HEN-TIME
           MOVE    WRK-THH             TO  WRK-HEN-THH
           MOVE    WRK-TMM             TO  WRK-HEN-TMM
           MOVE    WRK-TSS             TO  WRK-HEN-TSS
           MOVE    ":"                 TO  WRK-HEN-T1
           MOVE    ":"                 TO  WRK-HEN-T2
           .
       801-DAYHEN01-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号マスタ次検索処理
      *****************************************************************
       980-HKNNUM-READ-SEC              SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  HKNNUM-REC
               MOVE    ZERO                TO  FLG-HKNNUM
           ELSE
               INITIALIZE              HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           .
       980-HKNNUM-READ-EXT.
           EXIT.
      *****************************************************************
      *    パラメタ情報読み込み
      *****************************************************************
       990-PARA-READ-SEC        SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PARA-REC
               MOVE    ZERO            TO  FLG-PARA
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
      *
           .
       990-PARA-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルCALL処理
      *****************************************************************
       900-ORCDBMAIN-SEC                SECTION.
      *
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       900-ORCDBMAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       920-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       920-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *
      *
