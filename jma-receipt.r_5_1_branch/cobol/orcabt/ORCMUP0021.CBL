      *******************************************************************
      * Project code name "ORCA"
      * ����ɸ��쥻�ץȥ��եȡ�JMA standard receipt software��
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
       PROGRAM-ID.                 ORCMUP0021.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �ģº�������
      *  ����ݡ��ͥ��̾  : �����ƥ������������
      *                    : 
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  02/03/25    MCC-��ƣ    ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      * 01.00.01     NACL-��Ʀ��  06/05/22  MONFUNC�б�
      * 04.03.00     NACL-����    08/05/28  ���顼���������б�
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    ���ϡ������ƥ���������ե�����
           SELECT  IN-FILE         ASSIGN
                                   ASS-IN
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-IN.
      *    ���ϡ����顼�ե�����
           SELECT  ERR-FILE        ASSIGN
                                   ASS-ERR
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-ERR.
      *    ���ϡ��ץ롼�եե�����
           SELECT  PLF-FILE        ASSIGN
                                   ASS-PLF
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-PLF.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    ���ϡ������ƥ���������ե�����
       FD  IN-FILE.
       01  SYSWK-REC.
           COPY    "CPSYSKANRIWK.INC".
      *
      *    ���ϡ����顼�ե�����
       FD  ERR-FILE.
       01  ERR-SYSWK-REC.
           COPY    "CPSYSKANRIWK.INC"       REPLACING
                                       //SYSWK-// BY //ERR-SYSWK-//.
      *
      *    ���ϡ��ץ롼�եե�����
       FD  PLF-FILE.
       01  PLF-REC                 PIC X(1000).
      *
       WORKING-STORAGE             SECTION.
grpsys*
grpsys     COPY    "COMMON-SPA".
      *
      *    �ե���������ΰ�
       01  ASS-AREA.
           03  ASS-IN                  PIC X(256).
           03  ASS-ERR                 PIC X(256).
           03  ASS-PLF                 PIC X(256).
      *
      *    �ե饰�ΰ�
       01  STS-AREA.
           03  STS-PARA                PIC X(02).
           03  STS-IN                  PIC X(02).
           03  STS-ERR                 PIC X(02).
           03  STS-PLF                 PIC X(02).
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-PARA                PIC 9(01).
           03  FLG-IN                  PIC 9(01).
           03  FLG-PLF                 PIC 9(01).
      *
           03  FLG-SYORI               PIC 9(01).
           03  FLG-SYS                 PIC 9(01).
      *
      *    ��������ΰ�
       01  CNT-AREA.
           03  CNT-PARA                PIC 9(06).
           03  CNT-IN                  PIC 9(06).
           03  CNT-INS                 PIC 9(06).
           03  CNT-UPD                 PIC 9(06).
           03  CNT-DEL                 PIC 9(06).
           03  CNT-ERR                 PIC 9(06).
           03  CNT-PLF                 PIC 9(06).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDX-LIKE                PIC 9(04).
      *
      *
       01  SYS-DATE.
           03  SYS-YY                  PIC 9(02).
           03  SYS-MM                  PIC 9(02).
           03  SYS-DD                  PIC 9(02).
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-SYMD2.
               05  WRK-SYY2            PIC 9(02).
               05  WRK-SMM2            PIC 9(02).
               05  WRK-SDD2            PIC 9(02).
      *
      *�����祭�������ѹ���
           03  WRK-SYSUPD-KEY.
               05  WRK-SYSUP-HOSPNUM   PIC 9(02).
               05  WRK-SYSUP-KANRICD   PIC X(04).
               05  WRK-SYSUP-KBNCD     PIC X(08).
               05  WRK-SYSUP-STYUKYMD  PIC X(08).
               05  WRK-SYSUP-EDYUKYMD  PIC X(08).
      *
           03  WRK-ERRCD               PIC 9(01). 
      *
           03  WRK-IN                  PIC X(01). 
           03  WRK-HOMEDIR             PIC X(128).
      *
      *    ���ϥե������ΰ�
       01  MES-AREA.
           03  MES-TITLE1.
               05  FILLER              PIC X(26) VALUE
                                       "��PGID:ORCMUP0021.CBL��".
               05  MES-TITLE1-YY       PIC X(04) VALUE SPACE.
               05  FILLER              PIC X(01) VALUE ".".
               05  MES-TITLE1-MM       PIC X(02) VALUE SPACE.
               05  FILLER              PIC X(01) VALUE ".".
               05  MES-TITLE1-DD       PIC X(02) VALUE SPACE.
               05  FILLER              PIC X(02) VALUE SPACE.
               05  MES-TITLE1-HOSPNUM  PIC 9(02).
      *
       01  WRK-MSG-AREA.
           03  WRK-MSG-01              PIC X(30).
           03  FILLER                  PIC X(01)   VALUE   SPACE.
           03  WRK-SYORIKBN            PIC X(01).
           03  FILLER                  PIC X(01)   VALUE   ",".
           03  WRK-KANRICD             PIC X(04).
           03  FILLER                  PIC X(01)   VALUE   ",".
           03  WRK-KBNCD               PIC X(08).
           03  FILLER                  PIC X(01)   VALUE   ",".
           03  WRK-KANRITBL            PIC X(100).
      *
       01  CONST-AREA.
           03  CONST-HOMEDIR           PIC X(28)  VALUE
               "/var/lib/jma-receipt/master/".
      *****************************************************************
      *    �ե��������
      *****************************************************************
      *
      *    �����Ѵ��ΰ�
           COPY    "CPORCSNUM.INC".
      *
           COPY    "ENUM-VALUE".
      *    ����ե�����̾�Խ�
           COPY    "CPORCSGETTEMP.INC".
      *
      *****************************************************************
      *    �ե��������
      *****************************************************************
      *
      *    �����ƥ�����ޥ���
           COPY    "CPSYSKANRI.INC".
      *
      *    ����ִ�������
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    ���̥ѥ�᥿
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPMUPLINK.INC".
      *
       PROCEDURE                       DIVISION
           USING
           LNK-AREA.
      *
      ****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL ( FLG-IN      =   1 )
                    OR   ( LNK-RC  NOT =   ZERO )
      *
           PERFORM 300-END-SEC
      *
      *****STOP    RUN
           .
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    �������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           MOVE    ZERO            TO  FLG-AREA
           MOVE    ZERO            TO  CNT-AREA
           MOVE    ZERO            TO  IDX-AREA
           INITIALIZE                  WRK-AREA
                                       SPA-AREA
      *
           MOVE    LNK-HOSPNUM     TO  SPA-HOSPNUM
      *
      *
      *    DB OPEN
      *     PERFORM 100-DBOPEN-SEC
      *     IF      MCP-RC      NOT =   ZERO
      *         DISPLAY "DBOPEN ERR"
      *     END-IF.
      *
      *    �ե��������
           PERFORM 101-ASSIN-GET-SEC
      *
      *    ���ϡ��������
           OPEN    INPUT           IN-FILE
           IF  STS-IN          NOT =   ZERO
               MOVE    1           TO  FLG-IN
               DISPLAY "*(ORCMUP0021)* IN-FILE OPN STS["
                                       STS-IN "]"
           END-IF
      *
      *    ���ϥե����롡�������
           PERFORM 2700-OUTLST-INIT-SEC
      *
      *    �����ƥ���������ե������ɹ��߽���
           IF      FLG-IN              =   ZERO
               PERFORM 900-IN-KENSAKU-SEC
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ե�����������
      *****************************************************************
       101-ASSIN-GET-SEC                SECTION.
      *
           INITIALIZE                      SGETTEMP-AREA
           CALL    "ORCSGETTEMP"   USING   SGETTEMP-AREA
      *
           IF      SGETTEMP-MW         =   SPA-PANDA
               MOVE    CONST-HOMEDIR       TO  WRK-HOMEDIR
           ELSE
               MOVE    SGETTEMP-TEMPDIR    TO  WRK-HOMEDIR
           END-IF
      *
      *    ���ϥե�����
           MOVE    SPACE               TO  ASS-IN
           STRING  WRK-HOMEDIR         DELIMITED  BY  SPACE
                   SPA-HOSPNUM         DELIMITED  BY  SIZE
                   "/"                 DELIMITED  BY  SIZE
                   LNK-DBFILE          DELIMITED  BY  SPACE
                   "/"                 DELIMITED  BY  SIZE
                   LNK-UPFILE          DELIMITED  BY  SPACE
                                       INTO    ASS-IN
           END-STRING
      *
      *    ���顼�ե�����
           MOVE    SPACE               TO  ASS-ERR
           STRING  WRK-HOMEDIR         DELIMITED  BY  SPACE
                   SPA-HOSPNUM         DELIMITED  BY  SIZE
                   "/"                 DELIMITED  BY  SIZE
                   "orca-mstlog/"      DELIMITED  BY  SIZE
                   LNK-UPFILE          DELIMITED  BY  SPACE
                   ".ERR"              DELIMITED  BY  SIZE
                                       INTO    ASS-ERR
           END-STRING
      *
      *    �ץ롼�եե�����
           MOVE    SPACE               TO  ASS-PLF
           STRING  WRK-HOMEDIR         DELIMITED  BY  SPACE
                   SPA-HOSPNUM         DELIMITED  BY  SIZE
                   "/"                 DELIMITED  BY  SIZE
                   "orca-mstlog/"      DELIMITED  BY  SIZE
                   LNK-UPFILE          DELIMITED  BY  SPACE
                   ".PRF"              DELIMITED  BY  SIZE
                                       INTO    ASS-PLF
           END-STRING
      *
      *    DISPLAY "LNK-UPFILE:" LNK-UPFILE
      *    DISPLAY "ASS-IN:" ASS-IN
      *    DISPLAY "ASS-ERR:" ASS-ERR
      *    DISPLAY "ASS-PLF:" ASS-PLF
      *
           .
       101-ASSIN-GET-EXT.
           EXIT.
      *****************************************************************
      *    �ᥤ�����
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           EVALUATE    SYSWK-SYORIKBN
      *        �ɲ�
               WHEN    "I"
                   PERFORM 2010-INSERT-SYORI-SEC
      *        ����
               WHEN    "U"
                   PERFORM 2020-UPDATE-SYORI-SEC
      *        ���
               WHEN    "D"
                   PERFORM 2030-DELETE-SYORI-SEC
           END-EVALUATE
      *
      *    �����ƥ���������ե������ɹ��߽���
           PERFORM 900-IN-KENSAKU-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ɲ�  ����
      *****************************************************************
       2010-INSERT-SYORI-SEC               SECTION.
      *
      *    ñ���ɲ�
           IF      SYSWK-EDYUKYMD2     =   SPACE
               PERFORM    2011-INSERT-SYORI-SEC
           ELSE
               PERFORM    2012-INSERT-SYORI-SEC
           END-IF
      *
           .
       2010-INSERT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ñ���ɲ�  ����
      *****************************************************************
       2011-INSERT-SYORI-SEC               SECTION.
      *
           INITIALIZE                  SYSKANRI-REC
      *****MOVE    SYSWK-KEY           TO  SYS-KEY
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSWK-KANRICD       TO  SYS-KANRICD
           MOVE    SYSWK-KBNCD         TO  SYS-KBNCD
           MOVE    SYSWK-STYUKYMD      TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM                     910-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYS
           END-IF
      *
           MOVE  "tbl_syskanri"        TO  MCP-TABLE
           MOVE  "key10"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    �ɲå��顼
           IF      FLG-SYS             =   ZERO
                   MOVE    1               TO  WRK-ERRCD
                   PERFORM 2040-ERR-SYORI-SEC
                   GO      TO      2011-INSERT-SYORI-EXT
           END-IF
      *
      *    �ģ��ɲ�
           PERFORM 20101-SYSKANRI-INS-SEC
      *
           .
       2011-INSERT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ɲ�  ����
      *****************************************************************
       2012-INSERT-SYORI-SEC               SECTION.
      *
           INITIALIZE                  SYSKANRI-REC
      *****MOVE    SYSWK-KEY           TO  SYS-KEY
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSWK-KANRICD       TO  SYS-KANRICD
           MOVE    SYSWK-KBNCD         TO  SYS-KBNCD
           MOVE    SYSWK-STYUKYMD      TO  SYS-STYUKYMD
           MOVE    SYSWK-EDYUKYMD      TO  SYS-EDYUKYMD
           PERFORM   VARYING IDX-LIKE FROM  1  BY  1
             UNTIL  (SYS-KBNCD(IDX-LIKE:1)   =   SPACE)  OR
                    (IDX-LIKE          >    8)
                   CONTINUE
           END-PERFORM
           IF      IDX-LIKE    <=   8
               MOVE   "%"      TO   SYS-KBNCD(IDX-LIKE:1)
           END-IF
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key4"             TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM                     910-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYS
           END-IF
      *
      *    �ɲù���
           MOVE    ZERO                TO  FLG-SYORI
           PERFORM UNTIL   (FLG-SYS        =   1  )  OR
                           (FLG-SYORI      =   1  )
               IF     (SYS-STYUKYMD    <=  SYSWK-EDYUKYMD2 ) AND
                      (SYS-EDYUKYMD    >=  SYSWK-EDYUKYMD2 )
      *            ����
                   MOVE    SYS-KEY             TO  SYSUPD-KEY
                   MOVE    SYSWK-EDYUKYMD2     TO  SYS-EDYUKYMD
                   PERFORM 20102-SYSKANRI-UPD-SEC
      *
      *            �ģ��ɲ�
                   PERFORM 20121-SYSKANRI-INS-SEC
      *
                   MOVE    1                   TO  FLG-SYORI
               ELSE
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key4"              TO  MCP-PATHNAME
                   PERFORM                     910-SYSKANRI-READ-SEC
               END-IF
           END-PERFORM
      *
           MOVE  "tbl_syskanri"        TO  MCP-TABLE
           MOVE  "key4"                TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-SYORI           =   ZERO
      *
      *        �ģ��ɲ�
               PERFORM 20121-SYSKANRI-INS-SEC
           END-IF
      *
           .
       2012-INSERT-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    �ģ��ɲ�  ����
      *****************************************************************
       20121-SYSKANRI-INS-SEC           SECTION.
      *
           INITIALIZE                  SYSKANRI-REC
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSWK-KANRICD       TO  SYS-KANRICD
           MOVE    SYSWK-KBNCD         TO  SYS-KBNCD
           MOVE    SYSWK-STYUKYMD      TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM                     910-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYS
           END-IF
      *
           MOVE  "tbl_syskanri"        TO  MCP-TABLE
           MOVE  "key10"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF    ( FLG-SYS         =   ZERO )
               MOVE    1               TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
           ELSE
      *
               INITIALIZE                  SYSKANRI-REC
      *
               PERFORM 201011-SYSKANRI-EDIT-SEC
      *
               MOVE    LNK-MNTYMD          TO  SYS-CREYMD
                                               SYS-UPYMD
               MOVE    LNK-MNTHMS          TO  SYS-UPHMS
      *
               MOVE    SYSKANRI-REC        TO  MCPDATA-REC
      *
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC          NOT =   ZERO
                   MOVE    1               TO  LNK-RC
                   MOVE    7               TO  WRK-ERRCD
                   PERFORM 2040-ERR-SYORI-SEC
               ELSE
                   ADD     1               TO  CNT-INS
               END-IF
           END-IF
           .
       20121-SYSKANRI-INS-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    �ģ��ɲ�  ����
      *****************************************************************
       20101-SYSKANRI-INS-SEC           SECTION.
      *
           INITIALIZE                  SYSKANRI-REC
      *
           PERFORM 201011-SYSKANRI-EDIT-SEC
      *
           MOVE    LNK-MNTYMD          TO  SYS-CREYMD
                                           SYS-UPYMD
           MOVE    LNK-MNTHMS          TO  SYS-UPHMS
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO
               MOVE    1               TO  LNK-RC
               MOVE    7               TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
           ELSE
               ADD     1               TO  CNT-INS
           END-IF
           .
       20101-SYSKANRI-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ��ɲ�  ����
      *****************************************************************
       201011-SYSKANRI-EDIT-SEC        SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSWK-KANRICD       TO  SYS-KANRICD
           MOVE    SYSWK-KBNCD         TO  SYS-KBNCD
           MOVE    SYSWK-STYUKYMD      TO  SYS-STYUKYMD
           MOVE    SYSWK-EDYUKYMD      TO  SYS-EDYUKYMD
           MOVE    SYSWK-REC(38:)      TO  SYSKANRI-REC(31:)
      *
           .
       201011-SYSKANRI-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¹���  ����
      *****************************************************************
       20102-SYSKANRI-UPD-SEC           SECTION.
      *
           MOVE    LNK-MNTYMD          TO  SYS-UPYMD
           MOVE    LNK-MNTHMS          TO  SYS-UPHMS
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "upd1"              TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO
               MOVE    2               TO  LNK-RC
               MOVE    2               TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
           END-IF
      *
           .
       20102-SYSKANRI-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģº��  ����
      *****************************************************************
       20103-SYSKANRI-DEL-SEC           SECTION.
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO
               MOVE    3               TO  LNK-RC
               MOVE    4               TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
           END-IF
           .
       20103-SYSKANRI-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ����  ����
      *****************************************************************
       2020-UPDATE-SYORI-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-SYORI
      *
           INITIALIZE                  SYSKANRI-REC
      *****MOVE    SYSWK-KEY           TO  SYS-KEY
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSWK-KANRICD       TO  SYS-KANRICD
           MOVE    SYSWK-KBNCD         TO  SYS-KBNCD
           MOVE    SYSWK-STYUKYMD      TO  SYS-STYUKYMD
           MOVE    SYSWK-EDYUKYMD      TO  SYS-EDYUKYMD
           PERFORM   VARYING IDX-LIKE FROM  1  BY  1
             UNTIL  (SYS-KBNCD(IDX-LIKE:1)   =   SPACE)  OR
                    (IDX-LIKE          >    8)
                   CONTINUE
           END-PERFORM
           IF      IDX-LIKE    <=   8
               MOVE   "%"      TO   SYS-KBNCD(IDX-LIKE:1)
           END-IF
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM                     910-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYS
           END-IF
           PERFORM UNTIL   (FLG-SYS        =   1  )  OR
                           (FLG-SYORI      =   1  )
               IF     (SYS-STYUKYMD    <=  SYSWK-STYUKYMD ) AND
                      (SYS-EDYUKYMD    >=  SYSWK-EDYUKYMD )
      *            ����
                   MOVE    SYS-KEY             TO  WRK-SYSUPD-KEY
      *************MOVE    SYSWK-KEY           TO  SYS-KEY
grpsys             MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
                   MOVE    SYSWK-KANRICD       TO  SYS-KANRICD
                   MOVE    SYSWK-KBNCD         TO  SYS-KBNCD
                   MOVE    SYSWK-STYUKYMD      TO  SYS-STYUKYMD
                   MOVE    SYSWK-EDYUKYMD      TO  SYS-EDYUKYMD
                   MOVE    SYSWK-REC(38:)      TO  SYSKANRI-REC(31:)
      *
                   MOVE    WRK-SYSUPD-KEY      TO  SYSUPD-KEY
                   PERFORM 20102-SYSKANRI-UPD-SEC
                   ADD     1                   TO  CNT-UPD
      *
                   MOVE    1                   TO  FLG-SYORI
               ELSE
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key4"              TO  MCP-PATHNAME
                   PERFORM                     910-SYSKANRI-READ-SEC
               END-IF
           END-PERFORM
      *
           MOVE  "tbl_syskanri"        TO  MCP-TABLE
           MOVE  "key4"                TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    �������顼
           IF      FLG-SYORI           =   ZERO
               MOVE    3               TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
           END-IF
           .
       2020-UPDATE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���  ����
      *****************************************************************
       2030-DELETE-SYORI-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-SYORI
      *
           INITIALIZE                  SYSKANRI-REC
      *****MOVE    SYSWK-KEY           TO  SYS-KEY
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSWK-KANRICD       TO  SYS-KANRICD
           MOVE    SYSWK-KBNCD         TO  SYS-KBNCD
           MOVE    SYSWK-STYUKYMD      TO  SYS-STYUKYMD
           MOVE    SYSWK-EDYUKYMD      TO  SYS-EDYUKYMD
           PERFORM   VARYING IDX-LIKE FROM  1  BY  1
             UNTIL  (SYS-KBNCD(IDX-LIKE:1)   =   SPACE)  OR
                    (IDX-LIKE          >    8)
                   CONTINUE
           END-PERFORM
           IF      IDX-LIKE    <=   8
               MOVE   "%"      TO   SYS-KBNCD(IDX-LIKE:1)
           END-IF
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM                     910-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYS
           END-IF
           PERFORM UNTIL   (FLG-SYS        =   1  )  OR
                           (FLG-SYORI      =   1  )
               IF     (SYS-STYUKYMD    <=  SYSWK-STYUKYMD ) AND
                      (SYS-EDYUKYMD    >=  SYSWK-EDYUKYMD )
      *            ���
                   PERFORM 20103-SYSKANRI-DEL-SEC
                   ADD     1                   TO  CNT-UPD
      *
                   MOVE    1                   TO  FLG-SYORI
               ELSE
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key4"              TO  MCP-PATHNAME
                   PERFORM                     910-SYSKANRI-READ-SEC
               END-IF
           END-PERFORM
      *
           MOVE  "tbl_syskanri"        TO  MCP-TABLE
           MOVE  "key4"                TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    ������顼
           IF      FLG-SYORI           =   ZERO
               MOVE    5               TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
           END-IF
      *
           .
       2030-DELETE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���顼����
      *****************************************************************
       2040-ERR-SYORI-SEC            SECTION.
      *
      *    ���顼����
           MOVE    SYSWK-REC          TO  ERR-SYSWK-REC
           WRITE                          ERR-SYSWK-REC
           ADD     1                  TO  CNT-ERR
      *
      *    ���顼��å���������
           PERFORM 20401-PLF-OUT-SEC
      *
           .
       2040-ERR-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���顼��å��������Ͻ���
      *****************************************************************
       20401-PLF-OUT-SEC            SECTION.
      *
           MOVE    SYSWK-SYORIKBN        TO  WRK-SYORIKBN
           MOVE    SYSWK-KANRICD         TO  WRK-KANRICD
           MOVE    SYSWK-KBNCD           TO  WRK-KBNCD
           MOVE    SYSWK-KANRITBL        TO  WRK-KANRITBL
           EVALUATE    WRK-ERRCD
               WHEN    1
                   MOVE    "�����ƥ������Ͽ�Ѥ�"
                                              TO  WRK-MSG-01
               WHEN    2
                   MOVE    "�������顼"       TO  WRK-MSG-01
               WHEN    3
                   MOVE    "�����оݤʤ�"     TO  WRK-MSG-01
               WHEN    4
                   MOVE    "������顼"       TO  WRK-MSG-01
               WHEN    5
                   MOVE    "����оݤʤ�"     TO  WRK-MSG-01
               WHEN    7
                   MOVE    "�ɲå��顼"       TO  WRK-MSG-01
           END-EVALUATE
      *
           MOVE    WRK-MSG-AREA       TO  PLF-REC
           WRITE                          PLF-REC
           ADD     1                  TO  CNT-PLF
      *
           .
       20401-PLF-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ϥե����롡�������
      *****************************************************************
       2700-OUTLST-INIT-SEC            SECTION.
      *
           OPEN    OUTPUT              ERR-FILE
           IF  STS-ERR             NOT =   ZERO
               DISPLAY "*(ORCMUP0021)* OUTFILE OPN STS[" STS-ERR "]"
               GO      TO              2700-OUTLST-INIT-EXT
           END-IF
      *
      *    ���顼�쥳���ɽ��ϥե����롡�����ץ�
           OPEN    OUTPUT              PLF-FILE
           IF  STS-PLF             NOT =   ZERO
               DISPLAY "*(ORCMUP0021)* PLFFILE OPN STS[" STS-PLF "]"
               GO      TO              2700-OUTLST-INIT-EXT
           END-IF
           ACCEPT  SYS-DATE            FROM    DATE
           COMPUTE WRK-SYY             =   SYS-YY    +   2000
           MOVE    WRK-SYY             TO  MES-TITLE1-YY
           MOVE    SYS-MM              TO  MES-TITLE1-MM
           MOVE    SYS-DD              TO  MES-TITLE1-DD
           MOVE    SPA-HOSPNUM         TO  MES-TITLE1-HOSPNUM
           MOVE    MES-TITLE1          TO  PLF-REC
           WRITE   PLF-REC
      *
           .
       2700-OUTLST-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��λ����
      *****************************************************************
       300-END-SEC                     SECTION.
      *
      *    DB DISCONNECT
      *     PERFORM 900-DBDISCONNECT-SEC
      *
      *    ���ϥե����롡��λ����
           CLOSE                       IN-FILE
           CLOSE                       ERR-FILE
           CLOSE                       PLF-FILE
      *
           IF    ( LNK-RC              =   ZERO )
               IF    ( CNT-IN      NOT =   LNK-COUNT )
      *
                   MOVE    99              TO  LNK-RC
      *
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    1               TO  JOB-HOSPNUM
                   MOVE    "tbl_jobkanri"  TO  MCP-TABLE
                   MOVE    "abort"         TO  MCP-PATHNAME
                   MOVE    JOBKANRI-REC    TO  MCPDATA-REC
                   MOVE    "DBSELECT"      TO  MCP-FUNC
                   PERFORM 900-ORCDBMAIN-SEC
               END-IF
      *
           END-IF
      *
           DISPLAY "*(ORCMUP0021)* INPUT  /I   CNT[" CNT-IN  "]"
           DISPLAY "*(ORCMUP0021)*  INSERT     CNT[" CNT-INS "]"
           DISPLAY "*(ORCMUP0021)*  UPDATE     CNT[" CNT-UPD "]"
           DISPLAY "*(ORCMUP0021)*  DELETE     CNT[" CNT-DEL "]"
           DISPLAY "*(ORCMUP0021)* ERR    /O   CNT[" CNT-ERR "]"
           DISPLAY "*(ORCMUP0021)* PLF    /O   CNT[" CNT-PLF "]"
           DISPLAY "*(ORCMUP0021)* RC             [" LNK-RC "]"
      **** ACCEPT  WRK-IN
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ƥ���������ե������ɹ��߽���
      *****************************************************************
       900-IN-KENSAKU-SEC          SECTION.
      *
          READ    IN-FILE
              AT  END
                  MOVE    1               TO  FLG-IN
              NOT AT  END
                  MOVE    ZERO            TO  FLG-IN
                  ADD     1               TO  CNT-IN
          END-READ
      *
          .
       900-IN-KENSAKU-EXT.
          EXIT.
      *
      *****************************************************************
      *    �����ƥ�����ޥ����ɹ�
      *****************************************************************
       910-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SYSKANRI-REC
               MOVE    ZERO                TO  FLG-SYS
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1                   TO  FLG-SYS
           END-IF
      *
           .
       910-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������륯����������
      *****************************************************************
       990-DBCLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
      *
       990-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ¡������ץ����
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBOPEN"        TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBSTART"       TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ¡�������������
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBCOMMIT"      TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ơ��֥륢����������
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