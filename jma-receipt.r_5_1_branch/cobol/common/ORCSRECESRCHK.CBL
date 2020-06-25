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
       PROGRAM-ID.             ORCSRECESRCHK.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : オンライン診療料等に関わる管理料・指導料の
      *                      算定履歴チェックサブ
      *  管理者            : 
      *  18/04/19    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.08.01    NACL-藤原    18/05/07  算定履歴取得方法の変更
      *  05.00.01    NACL-門脇    20/03/30  令和２年４月改正対応
      *  05.00.02    NACL-藤原    20/04/08  排尿自立支援加算、外来排尿自
      *                                     立指導料の算定履歴チェック追加
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
      *
       WORKING-STORAGE             SECTION.
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SANTEI          PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PTNYUINRRK      PIC 9(01).
      *
           03  FLG-CHK             PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX1                PIC 9(04).
      *
           03  IDXX                PIC 9(04).
           03  IDYY                PIC 9(04).
      *
       01  WRK-AREA.
           03  WRK-ST-IDX          PIC 9(03).
           03  WRK-ED-IDX          PIC 9(03).
      *
           03  WRK-NUM-X.
               05  WRK-NUM         PIC 9(02).
      *
           03  WRK-NYUINYMD        PIC X(08).
           03  WRK-SRYCD           PIC X(09).
      *
      *    ワーク入院履歴
       01  WRK-PTNRRK-REC.
           COPY    "CPPTNYUINRRK.INC"  REPLACING  //PTNYUINRRK-//
                                       BY         //WRK-PTNRRK-//.
      *
       01  WRK-CONS-AREA.
           03  WRK-CONS-SRYCD-TABLE.
      *        特定疾患療養管理料（診療所）
               05 FILLER           PIC X(09)   VALUE   "113001810".
      *        特定疾患療養管理料（１００床未満）
               05 FILLER           PIC X(09)   VALUE   "113001910".
      *        特定疾患療養管理料（１００床以上２００床未満）
               05 FILLER           PIC X(09)   VALUE   "113002010".
      *        特定疾患療養管理料（情報通信機器）
               05 FILLER           PIC X(09)   VALUE   "113029010".
               05 FILLER           PIC X(324)  VALUE   SPACE.
      *
      *        小児科療養指導料
               05 FILLER           PIC X(09)   VALUE   "113002210".
      *        小児科療養指導料（情報通信機器）
               05 FILLER           PIC X(09)   VALUE   "113029510".
               05 FILLER           PIC X(342)  VALUE   SPACE.
      *        
      *        てんかん指導料
               05 FILLER           PIC X(09)   VALUE   "113002850".
      *        てんかん指導料（情報通信機器）
               05 FILLER           PIC X(09)   VALUE   "113029610".
               05 FILLER           PIC X(342)  VALUE   SPACE.
      *
      *        難病外来指導管理料
               05 FILLER           PIC X(09)   VALUE   "113002910".
      *        難病外来指導管理料（情報通信機器）
               05 FILLER           PIC X(09)   VALUE   "113029710".
               05 FILLER           PIC X(342)  VALUE   SPACE.
      *
      *        糖尿病透析予防指導管理料
               05 FILLER           PIC X(09)   VALUE   "113013610".
      *        糖尿病透析予防指導管理料（特定地域）
               05 FILLER           PIC X(09)   VALUE   "113015610".
      *        糖尿病透析予防指導管理料（情報通信機器）
               05 FILLER           PIC X(09)   VALUE   "113030910".
               05 FILLER           PIC X(333)  VALUE   SPACE.
      *
      *        地域包括診療料２
               05 FILLER           PIC X(09)   VALUE   "113015810".
      *        地域包括診療料１
               05 FILLER           PIC X(09)   VALUE   "113024810".
      *        地域包括診療料（情報通信機器）
               05 FILLER           PIC X(09)   VALUE   "113031310".
               05 FILLER           PIC X(333)  VALUE   SPACE.
      *
      *        認知症地域包括診療料２
               05 FILLER           PIC X(09)   VALUE   "113018410".
      *        認知症地域包括診療料１
               05 FILLER           PIC X(09)   VALUE   "113025710".
      *        認知症地域包括診療料（情報通信機器）
               05 FILLER           PIC X(09)   VALUE   "113031410".
               05 FILLER           PIC X(333)  VALUE   SPACE.
      *
      *        生活習慣病管理料（処方箋を交付）（高血圧症を主病）
               05 FILLER           PIC X(09)   VALUE   "113003910".
      *        生活習慣病管理料（処方箋を交付しない）（高血圧症を主病）
               05 FILLER           PIC X(09)   VALUE   "113004010".
      *        生活習慣病管理料（処方箋を交付）（脂質異常症を主病）
               05 FILLER           PIC X(09)   VALUE   "113005810".
      *        生活習慣病管理料（処方箋を交付）（糖尿病を主病）
               05 FILLER           PIC X(09)   VALUE   "113005910".
      *        生活習慣病管理料（処方箋を交付しない）（脂質異常症を主病）
               05 FILLER           PIC X(09)   VALUE   "113006010".
      *        生活習慣病管理料（処方箋を交付しない）（糖尿病を主病）
               05 FILLER           PIC X(09)   VALUE   "113006110".
      *        生活習慣病管理料（情報通信機器）
               05 FILLER           PIC X(09)   VALUE   "113031510".
               05 FILLER           PIC X(297)  VALUE   SPACE.
      *
      *        在医総管（機能強化在支診等・病床有・難病等月２回以上・１人）
               05 FILLER           PIC X(09)   VALUE   "114030710".
      *        在医総管（機能強化在支診等・病床有・難病等月２回以上・２〜９人）
               05 FILLER           PIC X(09)   VALUE   "114030810".
      *        在医総管（機能強化在支診等・病床有・難病等月２回以上・１０人〜）
               05 FILLER           PIC X(09)   VALUE   "114030910".
      *        在医総管（機能強化在支診等・病床有・月２回以上・１人）
               05 FILLER           PIC X(09)   VALUE   "114031010".
      *        在医総管（機能強化在支診等・病床有・月２回以上・２〜９人）
               05 FILLER           PIC X(09)   VALUE   "114031110".
      *        在医総管（機能強化在支診等・病床有・月２回以上・１０人〜）
               05 FILLER           PIC X(09)   VALUE   "114031210".
      *        在医総管（機能強化在支診等・病床有・月１回・１人）
               05 FILLER           PIC X(09)   VALUE   "114031310".
      *        在医総管（機能強化在支診等・病床有・月１回・２〜９人）
               05 FILLER           PIC X(09)   VALUE   "114031410".
      *        在医総管（機能強化在支診等・病床有・月１回・１０人〜）
               05 FILLER           PIC X(09)   VALUE   "114031510".
      *        在医総管（機能強化在支診等・病床無・難病等月２回以上・１人）
               05 FILLER           PIC X(09)   VALUE   "114031610".
      *        在医総管（機能強化在支診等・病床無・難病等月２回以上・２〜９人）
               05 FILLER           PIC X(09)   VALUE   "114031710".
      *        在医総管（機能強化在支診等・病床無・難病等月２回以上・１０人〜）
               05 FILLER           PIC X(09)   VALUE   "114031810".
      *        在医総管（機能強化在支診等・病床無・月２回以上・１人）
               05 FILLER           PIC X(09)   VALUE   "114031910".
      *        在医総管（機能強化在支診等・病床無・月２回以上・２〜９人）
               05 FILLER           PIC X(09)   VALUE   "114032010".
      *        在医総管（機能強化在支診等・病床無・月２回以上・１０人〜）
               05 FILLER           PIC X(09)   VALUE   "114032110".
      *        在医総管（機能強化在支診等・病床無・月１回・１人）
               05 FILLER           PIC X(09)   VALUE   "114032210".
      *        在医総管（機能強化在支診等・病床無・月１回・２〜９人）
               05 FILLER           PIC X(09)   VALUE   "114032310".
      *        在医総管（機能強化在支診等・病床無・月１回・１０人〜）
               05 FILLER           PIC X(09)   VALUE   "114032410".
      *        在医総管（在支診等・難病等月２回以上・１人）
               05 FILLER           PIC X(09)   VALUE   "114032510".
      *        在医総管（在支診等・難病等月２回以上・２〜９人）
               05 FILLER           PIC X(09)   VALUE   "114032610".
      *        在医総管（在支診等・難病等月２回以上・１０人〜）
               05 FILLER           PIC X(09)   VALUE   "114032710".
      *        在医総管（在支診等・月２回以上・１人）
               05 FILLER           PIC X(09)   VALUE   "114032810".
      *        在医総管（在支診等・月２回以上・２〜９人）
               05 FILLER           PIC X(09)   VALUE   "114032910".
      *        在医総管（在支診等・月２回以上・１０人〜）
               05 FILLER           PIC X(09)   VALUE   "114033010".
      *        在医総管（在支診等・月１回・１人）
               05 FILLER           PIC X(09)   VALUE   "114033110".
      *        在医総管（在支診等・月１回・２〜９人）
               05 FILLER           PIC X(09)   VALUE   "114033210".
      *        在医総管（在支診等・月１回・１０人〜）
               05 FILLER           PIC X(09)   VALUE   "114033310".
      *        在医総管（在支診等以外・難病等月２回以上・１人）
               05 FILLER           PIC X(09)   VALUE   "114033410".
      *        在医総管（在支診等以外・難病等月２回以上・２〜９人）
               05 FILLER           PIC X(09)   VALUE   "114033510".
      *        在医総管（在支診等以外・難病等月２回以上・１０人〜）
               05 FILLER           PIC X(09)   VALUE   "114033610".
      *        在医総管（在支診等以外・月２回以上・１人）
               05 FILLER           PIC X(09)   VALUE   "114033710".
      *        在医総管（在支診等以外・月２回以上・２〜９人）
               05 FILLER           PIC X(09)   VALUE   "114033810".
      *        在医総管（在支診等以外・月２回以上・１０人〜）
               05 FILLER           PIC X(09)   VALUE   "114033910".
      *        在医総管（在支診等以外・月１回・１人）
               05 FILLER           PIC X(09)   VALUE   "114034010".
      *        在医総管（在支診等以外・月１回・２〜９人）
               05 FILLER           PIC X(09)   VALUE   "114034110".
      *        在医総管（在支診等以外・月１回・１０人〜）
               05 FILLER           PIC X(09)   VALUE   "114034210".
               05 FILLER           PIC X(36)   VALUE   SPACE.
      *
      *        精神科在宅患者支援管理料１（集中的支援必要）（単一建物１人）
               05 FILLER           PIC X(09)   VALUE   "180057210".
      *        精神科在宅患者支援管理料１（集中的支援必要）（単一建物２人以上）
               05 FILLER           PIC X(09)   VALUE   "180057310".
      *        精神科在宅患者支援管理料１（重度精神障害者）（単一建物１人）
               05 FILLER           PIC X(09)   VALUE   "180057410".
      *        精神科在宅患者支援管理料１（重度精神障害者）（単一建物２人以上）
               05 FILLER           PIC X(09)   VALUE   "180057510".
      *        精神科在宅患者支援管理料１（イ及びロ以外）（単一建物１人）
               05 FILLER           PIC X(09)   VALUE   "180057610".
      *        精神科在宅患者支援管理料１（イ及びロ以外）（単一建物２人以上）
               05 FILLER           PIC X(09)   VALUE   "180057710".
      *        精神科在宅患者支援管理料２（集中的支援必要）（単一建物１人）
               05 FILLER           PIC X(09)   VALUE   "180057810".
      *        精神科在宅患者支援管理料２（集中的支援必要）（単一建物２人以上）
               05 FILLER           PIC X(09)   VALUE   "180057910".
      *        精神科在宅患者支援管理料２（重度精神障害者）（単一建物１人）
               05 FILLER           PIC X(09)   VALUE   "180058010".
      *        精神科在宅患者支援管理料２（重度精神障害者）（単一建物２人以上）
               05 FILLER           PIC X(09)   VALUE   "180058110".
      *        精神科在宅患者支援管理料３（単一建物１人）
               05 FILLER           PIC X(09)   VALUE   "180064010".
      *        精神科在宅患者支援管理料３（単一建物２人以上）
               05 FILLER           PIC X(09)   VALUE   "180064110".
               05 FILLER           PIC X(252)  VALUE   SPACE.
      *
           03  WRK-CONS-SRYCD-TABLE-R          REDEFINES
                                       WRK-CONS-SRYCD-TABLE.
               05  WRK-CONS-SRYCD-G            OCCURS  10.
                   07  WRK-CONS-SRYCD
                                   PIC X(09)   OCCURS  40.
      *
           03  WRK-CONS-COMMENT-SRYCD-TABLE.
      *        特定疾患療養管理料算定患者
               05 FILLER           PIC X(09)   VALUE   "820100006".
      *        小児科療養指導料算定患者
               05 FILLER           PIC X(09)   VALUE   "820100007".
      *        てんかん指導料算定患者
               05 FILLER           PIC X(09)   VALUE   "820100008".
      *        難病外来指導管理料算定患者
               05 FILLER           PIC X(09)   VALUE   "820100009".
      *        糖尿病透析予防指導管理料算定患者
               05 FILLER           PIC X(09)   VALUE   "820100010".
      *        地域包括診療料算定患者
               05 FILLER           PIC X(09)   VALUE   "820100011".
      *        認知症地域包括診療料算定患者
               05 FILLER           PIC X(09)   VALUE   "820100012".
      *        生活習慣病管理料算定患者
               05 FILLER           PIC X(09)   VALUE   "820100013".
      *        在宅時医学総合管理料算定患者
               05 FILLER           PIC X(09)   VALUE   "820100014".
      *        精神科在宅患者支援管理料算定患者
               05 FILLER           PIC X(09)   VALUE   "820100015".
           03  WRK-CONS-COMMENT-SRYCD-TABLE-R  REDEFINES
                                      WRK-CONS-COMMENT-SRYCD-TABLE.
               05  WRK-CONS-COMMENT-SRYCD-G    OCCURS  10.
                   07  WRK-CONS-COMMENT-SRYCD
                                   PIC X(09).
      *
      *    チェックする算定の診療行為コードの最大数
           03  WRK-CONS-SRYCD-MAX  PIC 9(03)   VALUE   40.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    算定履歴
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *
      *    患者
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "COMMON-SPA".
      *
           COPY    "CPORCSRECESRCHK.INC".
      *
      *****************************************************************
      *
       PROCEDURE                   DIVISION    USING
                                   SPA-AREA
                                   ORCSRECESRCHKAREA.
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
           PERFORM 300-END-SEC
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
        DISPLAY "LNK-RECESRCHK-SYORI    =" LNK-RECESRCHK-SYORI
        DISPLAY "LNK-RECESRCHK PTID     =" LNK-RECESRCHK-PTID
        DISPLAY "LNK-RECESRCHK-NYUGAIKBN=" LNK-RECESRCHK-NYUGAIKBN
        DISPLAY "LNK-RECESRCHK SRYYM    =" LNK-RECESRCHK-SRYYM
        DISPLAY "LNK-RECESRCHK SRYYMD(1)=" LNK-RECESRCHK-SRYYMD (1)
        DISPLAY "LNK-RECESRCHK SRYYMD(2)=" LNK-RECESRCHK-SRYYMD (2)
        DISPLAY "LNK-RECESRCHK CHKKBN   =" LNK-RECESRCHK-CHKKBN
        DISPLAY "LNK-RECESRCHK CHKNO    =" LNK-RECESRCHK-CHKNO
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    ZERO            TO  LNK-RECESRCHK-RC
           INITIALIZE                  LNK-RECESRCHK-HENKYAKU-AREA
      *
      *    処理区分チェック
           EVALUATE    LNK-RECESRCHK-SYORI
               WHEN    SPACE
               WHEN    "1"
                   CONTINUE
               WHEN    OTHER
                   MOVE    8               TO  LNK-RECESRCHK-RC
                   MOVE    1                   TO  FLG-END
                   GO  TO  100-INIT-EXT
           END-EVALUATE
      *
           IF      LNK-RECESRCHK-SYORI =   SPACE
      *      入力値チェック
             IF    ( LNK-RECESRCHK-PTID      =   ZERO  )
             OR    ( LNK-RECESRCHK-SRYYM     =   SPACE )
             OR    ( LNK-RECESRCHK-CHKKBN    =   SPACE )
               MOVE    1                   TO  LNK-RECESRCHK-RC
             ELSE
      *        処理年月チェック
               INITIALIZE                      ORCSGDAYAREA
               MOVE    "1"                 TO  SGDAY-INTYPE
               MOVE    LNK-RECESRCHK-SRYYM TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   IF     LNK-RECESRCHK-SRYYM  <  "201804"
                       MOVE    2               TO  LNK-RECESRCHK-RC
                   END-IF
               ELSE
                   MOVE    2               TO  LNK-RECESRCHK-RC
               END-IF
      *
               IF      LNK-RECESRCHK-RC    =   ZERO
      *            患者ＩＤチェック
                   INITIALIZE                      PTINF-REC
                   MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
                   MOVE    LNK-RECESRCHK-PTID  TO  PTINF-PTID
                   PERFORM 900-PTINF-READ-SEC
                   IF      FLG-PTINF       NOT =   ZERO
                       MOVE    3                   TO  LNK-RECESRCHK-RC
                   END-IF
               END-IF
      *
               IF      LNK-RECESRCHK-RC    =   ZERO
      *            チェック区分チェック
                   EVALUATE    LNK-RECESRCHK-CHKKBN
                       WHEN    "1"
                       WHEN    "2"
                       WHEN    "3"
                       WHEN    "4"
                           CONTINUE
                       WHEN   OTHER
                           MOVE    4               TO  LNK-RECESRCHK-RC
                   END-EVALUATE
               END-IF
      *
               IF      LNK-RECESRCHK-RC    =   ZERO
      *            チェック番号チェック
                   IF      LNK-RECESRCHK-CHKNO >   10
                       MOVE    5               TO  LNK-RECESRCHK-RC
                   END-IF
               END-IF
             END-IF
           ELSE
      *      入力値チェック
             IF    ( LNK-RECESRCHK-PTID      =   ZERO  )
             OR    ( LNK-RECESRCHK-SRYYM     =   SPACE )
             OR    ( LNK-RECESRCHK-SRYYMD(1) =   SPACE )
             OR    ( LNK-RECESRCHK-NYUGAIKBN =   SPACE )
               MOVE    1                   TO  LNK-RECESRCHK-RC
             ELSE
      *        診療年月チェック
               INITIALIZE                      ORCSGDAYAREA
               MOVE    "1"                 TO  SGDAY-INTYPE
               MOVE    LNK-RECESRCHK-SRYYM TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   IF     LNK-RECESRCHK-SRYYM  <  "202004"
                       MOVE    2               TO  LNK-RECESRCHK-RC
                   END-IF
               ELSE
                   MOVE    2               TO  LNK-RECESRCHK-RC
               END-IF
      *
               IF      LNK-RECESRCHK-RC    =   ZERO
      *            基準日（初回入院日）チェック
                   INITIALIZE                      ORCSGDAYAREA
                   MOVE    LNK-RECESRCHK-SRYYMD (1)
                                               TO  SGDAY-INDAY
                   CALL    "ORCSGDAY"          USING
                                               ORCSGDAYAREA
                   IF      SGDAY-RC            =   ZERO
      *                診療年月 ＞＝基準日（初回入院日）チェック
                       IF      LNK-RECESRCHK-SRYYM >=
                                           LNK-RECESRCHK-SRYYMD(1) (1:6)
                           CONTINUE
                       ELSE
                           MOVE    7               TO  LNK-RECESRCHK-RC
                       END-IF
                   ELSE
                       MOVE    7               TO  LNK-RECESRCHK-RC
                   END-IF
     *
                   IF    ( LNK-RECESRCHK-SRYYMD (2)
                                           NOT =   SPACE )
                   AND   ( LNK-RECESRCHK-RC    =   ZERO  )
                       INITIALIZE                      ORCSGDAYAREA
                       MOVE    LNK-RECESRCHK-SRYYMD (2)
                                                   TO  SGDAY-INDAY
                       CALL    "ORCSGDAY"          USING
                                                   ORCSGDAYAREA
                       IF      SGDAY-RC            =   ZERO
      *                    診療年月 ＞＝基準日（初回入院日）チェック
                           IF      LNK-RECESRCHK-SRYYM >=
                                           LNK-RECESRCHK-SRYYMD(2) (1:6)
                              CONTINUE
                           ELSE
                               MOVE    7           TO  LNK-RECESRCHK-RC
                           END-IF
                       ELSE
                           MOVE    7           TO  LNK-RECESRCHK-RC
                       END-IF
                   END-IF
               END-IF
      *
               IF      LNK-RECESRCHK-RC    =   ZERO
      *            患者ＩＤチェック
                   INITIALIZE                      PTINF-REC
                   MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
                   MOVE    LNK-RECESRCHK-PTID  TO  PTINF-PTID
                   PERFORM 900-PTINF-READ-SEC
                   IF      FLG-PTINF       NOT =   ZERO
                       MOVE    3                   TO  LNK-RECESRCHK-RC
                   END-IF
               END-IF
      *
               IF      LNK-RECESRCHK-RC    =   ZERO
      *            入外区分チェック
                   EVALUATE    LNK-RECESRCHK-NYUGAIKBN
                       WHEN    "1"
                       WHEN    "2"
                           CONTINUE
                       WHEN   OTHER
                           MOVE    6               TO  LNK-RECESRCHK-RC
                   END-EVALUATE
               END-IF
             END-IF
           END-IF
      *
           IF      LNK-RECESRCHK-RC    =   ZERO
               CONTINUE
           ELSE
               MOVE    1                   TO  FLG-END
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT
           .
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           EVALUATE    LNK-RECESRCHK-SYORI
               WHEN    SPACE
      *            オンライン診療料等に関わる算定履歴チェック処理
                   PERFORM 2001-SANTEI-CHK-SEC
               WHEN    "1"
      *            排尿自立に関わる算定履歴チェック処理
                   PERFORM 2002-SANTEI-CHK-SEC
           END-EVALUATE
      *
           MOVE    1               TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    オンライン診療料等に関わる算定履歴チェック処理
      *****************************************************************
       2001-SANTEI-CHK-SEC             SECTION.
      *
           INITIALIZE                  FLG-CHK
      *
           IF      LNK-RECESRCHK-CHKNO
                                   >   ZERO
               MOVE    LNK-RECESRCHK-CHKNO
                                       TO  WRK-ST-IDX
                                           WRK-ED-IDX
           ELSE
               EVALUATE    LNK-RECESRCHK-CHKKBN
                   WHEN    "1"
                       MOVE    1               TO  WRK-ST-IDX
                       MOVE    10              TO  WRK-ED-IDX
                   WHEN    "2"
                       MOVE    1               TO  WRK-ST-IDX
                       MOVE    8               TO  WRK-ED-IDX
                   WHEN    "3"
                       MOVE    9               TO  WRK-ST-IDX
                       MOVE    9               TO  WRK-ED-IDX
                   WHEN    "4"
                       MOVE    10              TO  WRK-ST-IDX
                       MOVE    10              TO  WRK-ED-IDX
               END-EVALUATE
           END-IF
      *
      *    処理年月以前の算定履歴をチェック
           PERFORM VARYING IDXX    FROM    WRK-ST-IDX  BY  1
                   UNTIL   IDXX    >       WRK-ED-IDX
               PERFORM VARYING IDYY    FROM    1   BY  1
                       UNTIL   IDYY    >       WRK-CONS-SRYCD-MAX
                       OR      WRK-CONS-SRYCD (IDXX IDYY)
                                       =       SPACE
                   DISPLAY "SRYCD=" IDXX " " IDYY " " 
                            WRK-CONS-SRYCD (IDXX IDYY)
      *
                   INITIALIZE                  SANTEI-REC
                   MOVE    SPA-HOSPNUM     TO  SANTEI-HOSPNUM
                   MOVE    LNK-RECESRCHK-PTID
                                           TO  SANTEI-PTID
                   MOVE    WRK-CONS-SRYCD (IDXX IDYY)
                                           TO  SANTEI-SRYCD
                   MOVE    ZERO            TO  SANTEI-NYUGAIKBN
                   MOVE    ZERO            TO  SANTEI-SRYKA
                   MOVE    ZERO            TO  SANTEI-HKNCOMBINUM
                   MOVE    SANTEI-REC      TO  MCPDATA-REC
                   MOVE    "tbl_santei"    TO  MCP-TABLE
                   MOVE    "key3"          TO  MCP-PATHNAME
                   PERFORM  900-SANTEI-READ-SEC
                   IF       FLG-SANTEI     =   ZERO
                       IF    ( LNK-RECESRCHK-FSANTEIYMD
                                               >   SANTEI-FSANTEIYMD )
                       OR    ( LNK-RECESRCHK-FSANTEIYMD
                                               =   SPACE             )
                           MOVE    SANTEI-SRYYM    TO
                                               LNK-RECESRCHK-SANTEIYM
                           MOVE    SANTEI-SRYCD    TO
                                               LNK-RECESRCHK-SRYCD
                           MOVE    SANTEI-FSANTEIYMD
                                                   TO
                                               LNK-RECESRCHK-FSANTEIYMD
                           MOVE    SANTEI-MSANTEID TO
                                               LNK-RECESRCHK-MSANTEID
                           MOVE    WRK-CONS-COMMENT-SRYCD (IDXX)
                                                   TO
                                               LNK-RECESRCHK-COMCD
                           MOVE    1               TO  FLG-CHK
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM
      *    
           IF      FLG-CHK         =   ZERO
      *        算定履歴情報編集なし
               INITIALIZE                  LNK-RECESRCHK-HENKYAKU-AREA
               MOVE    9               TO  LNK-RECESRCHK-RC
           END-IF
           .
       2001-SANTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    排尿自立に関わる算定履歴チェック処理
      *****************************************************************
       2002-SANTEI-CHK-SEC             SECTION.
      *
           INITIALIZE                  FLG-CHK
                                       WRK-NYUINYMD
      *
           EVALUATE    LNK-RECESRCHK-NYUGAIKBN
               WHEN    "1"
                   MOVE    LNK-RECESRCHK-SRYYMD (1)
                                           TO  WRK-NYUINYMD
      *            算定履歴チェック処理
                   PERFORM 20021-SANTEIRRK-CHECK-SEC
                   IF      LNK-RECESRCHK-RC    =   9
                   AND     LNK-RECESRCHK-SRYYMD (2)
                                           NOT =   SPACE
                       MOVE    ZERO            TO  LNK-RECESRCHK-RC
      *                直近の入院履歴の初回入院日取得処理
                       PERFORM 20022-NYUINRRK-NYUINYMD-CHECK-SEC
                   END-IF
               WHEN    "2"
      *            直近の入院履歴の初回入院日取得
                   PERFORM 20022-NYUINRRK-NYUINYMD-CHECK-SEC
           END-EVALUATE
           .
       2002-SANTEI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    算定履歴チェック処理
      *****************************************************************
       20021-SANTEIRRK-CHECK-SEC   SECTION.
      *??
         DISPLAY "20021-SANTEIRRK-CHECK-SEC WRK-NYUINYMD="
                                   WRK-NYUINYMD "#"
      *??
      *
      *    算定履歴チェック処理（排尿自立支援加算）
           MOVE    "190219010"     TO  WRK-SRYCD
      *??
      **  MOVE    "160221610"     TO  WRK-SRYCD
      *??
           PERFORM 200211-CHECK-SEC
      *
      *    算定履歴チェック処理（外来排尿自立指導料）
           IF      LNK-RECESRCHK-NYUGAIKBN
                                   =   "2"
               MOVE    "113032810"     TO  WRK-SRYCD
      *??
      **      MOVE    "160221010"     TO  WRK-SRYCD
      *??
               PERFORM 200211-CHECK-SEC
           END-IF
      *
           IF      FLG-CHK         =   ZERO
      *        算定履歴情報編集なし
               INITIALIZE                  LNK-RECESRCHK-HENKYAKU-AREA
               MOVE    9               TO  LNK-RECESRCHK-RC
           END-IF
      *??
         DISPLAY "20021-SANTEIRRK-CHECK-SEC LNK-RECESRCHK-RC="
                                   LNK-RECESRCHK-RC "#"
      *??
           .
       20021-SANTEIRRK-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴チェック処理
      *****************************************************************
       200211-CHECK-SEC   SECTION.
      *
      *??
           DISPLAY "WRK-SRYCD=" WRK-SRYCD "#"
      *??
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM     TO  SANTEI-HOSPNUM
           MOVE    LNK-RECESRCHK-PTID
                                   TO  SANTEI-PTID
           MOVE    WRK-SRYCD       TO  SANTEI-SRYCD
           MOVE    LNK-RECESRCHK-SRYYM
                                   TO  SANTEI-SRYYM
           MOVE    SANTEI-REC      TO  MCPDATA-REC
      *
           MOVE    "tbl_santei"    TO  MCP-TABLE
           MOVE    "key18"         TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_santei"    TO  MCP-TABLE
               MOVE    "key18"         TO  MCP-PATHNAME
               PERFORM 900-SANTEI-READ-N1-SEC
           ELSE
               MOVE    1               TO  FLG-SANTEI
               INITIALIZE                  SANTEI-REC
           END-IF
      *??
                   DISPLAY "FLG-SANTEI=" FLG-SANTEI "#"
      *??
      *
           PERFORM UNTIL   FLG-SANTEI      =   1
      *??
                   DISPLAY "SANTEI-SRYYM=" SANTEI-SRYYM "#"
      *??
               IF      WRK-NYUINYMD (1:6)  =   SANTEI-SRYYM
                   MOVE    WRK-NYUINYMD (7:2)
                                           TO  WRK-NUM-X
                   MOVE    WRK-NUM         TO  IDX1
               ELSE
                   MOVE    1               TO  IDX1
               END-IF
      *??
                   DISPLAY "START DAY=" IDX1 "#"
      *??
      *
               PERFORM VARYING IDX FROM    IDX1  BY  1
                       UNTIL   IDX >       31
                   IF      SANTEI-DAY (IDX)    >   ZERO
      *??
                   DISPLAY "SANTEI-DAY=" IDX "#" 
      *??
      *                通算回数
                       ADD     1       TO  LNK-RECESRCHK-KAISU
      *                初回算定日
                       IF      LNK-RECESRCHK-FSANTEIYMD
                                               =   SPACE
                           MOVE    SANTEI-SRYYM    TO
                                       LNK-RECESRCHK-FSANTEIYMD (1:6)
                           MOVE    IDX             TO  WRK-NUM
                           MOVE    WRK-NUM-X       TO
                                       LNK-RECESRCHK-FSANTEIYMD (7:2)
                       END-IF
      *
                       MOVE    1               TO  FLG-CHK
                   END-IF
               END-PERFORM
      *
               MOVE    "tbl_santei"    TO  MCP-TABLE
               MOVE    "key18"         TO  MCP-PATHNAME
               PERFORM 900-SANTEI-READ-N1-SEC
           END-PERFORM
      *
           MOVE    "tbl_santei"    TO  MCP-TABLE
           MOVE    "key18"         TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       200211-CHECK--EXT.
           EXIT.
      *
      *****************************************************************
      *    直近の入院履歴の初回入院日取得処理
      *****************************************************************
       20022-NYUINRRK-NYUINYMD-CHECK-SEC   SECTION.
      *
      *    基準日の直近の入院履歴を取得
           INITIALIZE                  PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM     TO  PTNYUINRRK-HOSPNUM
           MOVE    LNK-RECESRCHK-PTID
                                   TO  PTNYUINRRK-PTID
           EVALUATE    LNK-RECESRCHK-NYUGAIKBN
               WHEN    "1"
                   MOVE    LNK-RECESRCHK-SRYYMD (2)
                                           TO  PTNYUINRRK-NYUINYMD
               WHEN    "2"
                   MOVE    LNK-RECESRCHK-SRYYMD (1)
                                           TO  PTNYUINRRK-NYUINYMD
           END-EVALUATE
      *???
           display "20022-NYUINRRK-NYUINYMD-CHECK-SEC NYUINYMD="
                                  PTNYUINRRK-NYUINYMD "#"
      *???
           MOVE    PTNYUINRRK-REC  TO  MCPDATA-REC
           PERFORM 900-PTNYUINRRK-READ-SEC
           IF      FLG-PTNYUINRRK  =   ZERO
      *???
               display "PTNYUINRRK=" PTNYUINRRK-NYUINYMD "#"
                                     PTNYUINRRK-TAIINYMD "#"
                                     PTNYUINRRK-SHONUM "#"
      *???
      *        直近の入院履歴の初回入院年月日を取得
               MOVE    PTNYUINRRK-REC  TO  WRK-PTNRRK-REC
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    SPA-HOSPNUM     TO  PTNYUINRRK-HOSPNUM
               MOVE    LNK-RECESRCHK-PTID
                                       TO  PTNYUINRRK-PTID
               MOVE    WRK-PTNRRK-SHONUM
                                       TO  PTNYUINRRK-SHONUM
               EVALUATE    LNK-RECESRCHK-NYUGAIKBN
                   WHEN    "1"
                       MOVE    LNK-RECESRCHK-SRYYMD (2)
                                               TO  PTNYUINRRK-NYUINYMD
                   WHEN    "2"
                       MOVE    LNK-RECESRCHK-SRYYMD (1)
                                               TO  PTNYUINRRK-NYUINYMD
               END-EVALUATE
      *???
           display "20022-NYUINRRK NYUINYMD=" PTNYUINRRK-NYUINYMD "#"
      *???
               MOVE    PTNYUINRRK-REC  TO  MCPDATA-REC
               MOVE    "tbl_ptnyuinrrk"
                                       TO  MCP-TABLE
               MOVE    "key51"         TO  MCP-PATHNAME
               PERFORM   900-DBSELECT-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    "tbl_ptnyuinrrk"
                                           TO  MCP-TABLE
                   MOVE    "key51"         TO  MCP-PATHNAME
                   PERFORM   900-DBFETCH-SEC
                   IF      MCP-RC          =   ZERO
                       MOVE    ZERO            TO  FLG-PTNYUINRRK
                       MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
                       MOVE    PTNYUINRRK-NYUINYMD
                                               TO  WRK-NYUINYMD
                   ELSE
                       MOVE    1           TO  FLG-PTNYUINRRK
                   END-IF
               ELSE
                   MOVE    1               TO  FLG-PTNYUINRRK
               END-IF
      *
               MOVE    "tbl_ptnyuinrrk"
                                       TO  MCP-TABLE
               MOVE    "key51"         TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
           END-IF
      *
           IF      WRK-NYUINYMD    =   SPACE
               MOVE    10              TO  LNK-RECESRCHK-RC
           ELSE
      *        算定履歴チェック処理
               PERFORM 20021-SANTEIRRK-CHECK-SEC
           END-IF
           .
       20022-NYUINRRK-NYUINYMD-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           DISPLAY "LNK-RECESRCHK RC        =" LNK-RECESRCHK-RC
           DISPLAY "LNK-RECESRCHK SANTEIYM  =" LNK-RECESRCHK-SANTEIYM
           DISPLAY "LNK-RECESRCHK SRYCD     =" LNK-RECESRCHK-SRYCD
           DISPLAY "LNK-RECESRCHK FSANTEIYMD=" LNK-RECESRCHK-FSANTEIYMD
           DISPLAY "LNK-RECESRCHK MSANTEID  =" LNK-RECESRCHK-MSANTEID
           DISPLAY "LNK-RECESRCHK COMCD     =" LNK-RECESRCHK-COMCD
           DISPLAY "LNK-RECESRCHK-KAISU     =" LNK-RECESRCHK-KAISU
      *
           .
       300-END-EXT.
           EXIT. 
      *
      *****************************************************************
      *    算定履歴読込
      *****************************************************************
       900-SANTEI-READ-SEC          SECTION.
      *
           MOVE    "tbl_santei"    TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               PERFORM 900-SANTEI-READ-N-SEC
           ELSE
               MOVE    1               TO  FLG-SANTEI
               INITIALIZE                  SANTEI-REC
           END-IF
      *
           MOVE    "tbl_santei"    TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-SANTEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴読込
      *****************************************************************
       900-SANTEI-READ-N-SEC       SECTION.
      *
           MOVE    "tbl_santei"    TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC          =   ZERO
               MOVE    MCPDATA-REC     TO  SANTEI-REC
               MOVE    ZERO            TO  FLG-SANTEI
               IF      SANTEI-SRYYM    >    LNK-RECESRCHK-SRYYM
                   GO  TO  900-SANTEI-READ-N-SEC
               END-IF
           ELSE
               MOVE    1               TO  FLG-SANTEI
               INITIALIZE                  SANTEI-REC
           END-IF
      *
           .
       900-SANTEI-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴読込
      *****************************************************************
       900-SANTEI-READ-N1-SEC      SECTION.
      *
           MOVE    "tbl_santei"    TO  MCP-TABLE
           MOVE    "key18"         TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC          =   ZERO
               MOVE    MCPDATA-REC     TO  SANTEI-REC
               MOVE    ZERO            TO  FLG-SANTEI
      *        初回入院日の月から診療年月まで対象
               IF      SANTEI-SRYYM    <   WRK-NYUINYMD (1:6)
                   GO  TO  900-SANTEI-READ-N1-SEC
               END-IF
               IF      SANTEI-SRYYM    >   LNK-RECESRCHK-SRYYM
                   MOVE    1               TO  FLG-SANTEI
                   INITIALIZE                  SANTEI-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-SANTEI
               INITIALIZE                  SANTEI-REC
           END-IF
      *
           .
       900-SANTEI-READ-N1-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者読込
      *****************************************************************
       900-PTINF-READ-SEC          SECTION.
      *
           MOVE    PTINF-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_ptinf"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM   900-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC     TO  PTINF-REC
                   MOVE    ZERO            TO  FLG-PTINF
               ELSE
                   INITIALIZE                  PTINF-REC
                   MOVE    1               TO  FLG-PTINF
               END-IF
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴読込
      *****************************************************************
       900-PTNYUINRRK-READ-SEC    SECTION.
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key12"             TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-PTNYUINRRK-READ-N-SEC
           ELSE
               MOVE    1                    TO  FLG-PTNYUINRRK
           END-IF
      *                 
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key12"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-PTNYUINRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴読込
      *****************************************************************
       900-PTNYUINRRK-READ-N-SEC    SECTION.
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key12"             TO  MCP-PATHNAME
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC               =   ZERO
               MOVE    ZERO                 TO  FLG-PTNYUINRRK
               MOVE    MCPDATA-REC          TO  PTNYUINRRK-REC
      *        ６:自院歴 読み飛ばし
               IF      PTNYUINRRK-JTIKBN    =   "6"
                   GO  TO  900-PTNYUINRRK-READ-N-SEC
               END-IF
               EVALUATE    LNK-RECESRCHK-NYUGAIKBN
                   WHEN    "1" 
      *                初回入院日以降の入院日のときは読み飛ばし
                       IF      PTNYUINRRK-NYUINYMD
                                           >   LNK-RECESRCHK-SRYYMD (2)
                           GO  TO  900-PTNYUINRRK-READ-N-SEC
                       END-IF
                   WHEN    "2" 
      *                基準日以降の退院日のときは読み飛ばし
                       IF      PTNYUINRRK-TAIINYMD
                                           >   LNK-RECESRCHK-SRYYMD (1)
                           GO  TO  900-PTNYUINRRK-READ-N-SEC
                       END-IF
               END-EVALUATE
           ELSE
               MOVE    1                    TO  FLG-PTNYUINRRK
           END-IF
           .
       900-PTNYUINRRK-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
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
           PERFORM 900-ORCDBMAIN-SEC
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
           PERFORM 900-ORCDBMAIN-SEC
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
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
