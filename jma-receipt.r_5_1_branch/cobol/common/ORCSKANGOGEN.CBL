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
       PROGRAM-ID.                 ORCSKANGOGEN.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ����
      *  ����ݡ��ͥ��̾  : ��ִǸ����������������������֡�ʿ������ǯ�ٲ�����
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  13/10/31    NACL�ݾ�Ʀ��  ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-PTINF                   PIC 9(01).
           03  FLG-TENSU                   PIC 9(01).
           03  FLG-NYUINACCT               PIC 9(01).
           03  FLG-NYUINACT                PIC 9(01).
           03  FLG-PTNYUINRRK              PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                         PIC 9(05).
           03  IDX1                        PIC 9(05).
           03  IDX2                        PIC 9(05).
           03  IDX3                        PIC 9(05).
           03  IDX4                        PIC 9(05).
           03  IDX5                        PIC 9(05).
           03  IDX6                        PIC 9(05).
           03  IDX7                        PIC 9(05).
           03  IDX8                        PIC 9(05).
           03  IDX9                        PIC 9(05).
           03  IDX-ST                      PIC 9(02).
           03  IDX-ED                      PIC 9(02).
           03  IDX-BTU                     PIC 9(02).
      *
       01  CONS-AREA.
      *
           03  CONS-KANGOGEN-AREA.
      *    ��ִǸ����������������ʰ�������������������
             05   CONS-KANGOGEN-SRYCD1     PIC X(09) VALUE "190200970".
      *    ��ִǸ����������������ʷ������������������
             05   CONS-KANGOGEN-SRYCD2     PIC X(09) VALUE "190205270".
      *    ��ִǸ�������������������������������������
             05   CONS-KANGOGEN-SRYCD3     PIC X(09) VALUE "190205370".
      *    ��ִǸ����������������������±�������������
             05   CONS-KANGOGEN-SRYCD4     PIC X(09) VALUE "190205470".
      *    ��ִǸ����������������ʾ㳲�Ի�����������������
             05   CONS-KANGOGEN-SRYCD5     PIC X(09) VALUE "190205870".
      *    ��ִǸ������������������ϰ���祱��������������
             05   CONS-KANGOGEN-SRYCD6     PIC X(09) VALUE "190212670".
      *    ���������ִǸ����������������ʰ�������������������
             05   CONS-KANGOGEN-SRYCD7     PIC X(09) VALUE "190214470".
           03  CONS-KANGOGEN-R     REDEFINES  CONS-KANGOGEN-AREA.
             05   CONS-KANGOGEN-SRYCD      PIC X(09)  OCCURS  7.
      *
      *    ����ΰ�
       01  WRK-AREA.
      *
      *    ��ִǸ��������������������û����������򥨥ꥢ
           03  WRK-CHUKSNCD-AREA.
              05  WRK-CHUKSNCD             PIC X(04)  OCCURS  7.
      *    ����Ψ�ʡ�����򥨥ꥢ
           03  WRK-KANGOGEN-TEN-AREA.
              05  WRK-KANGOGEN-TEN         PIC 9(05)  OCCURS  7.
      *
      *    �������ꤵ�줿��ִǸ������������������о������򥨥ꥢ
           03  WRK-BTU-TAIHI-AREA.
              05  WRK-BTU-TAIHI            OCCURS  99.
                  07  WRK-BTU-TAIHI-BTUNUM PIC X(02).
                  07  WRK-BTU-TAIHI-GENHIT PIC X(01)   OCCURS  31.
      *
      *    ���������������򥨥ꥢ
           03  WRK-PTNYUINRRK-BTUNUM       PIC X(02)   OCCURS  31.
      *
      *    ���Ը����о������ꥢ
           03  WRK-PT-GENHIT-AREA.
               05  WRK-PT-GENHIT           PIC X(01)   OCCURS  31.
      *
      *    ���������ֹ����򥨥ꥢ
           03  WRK-RRKNUM                  PIC 9(03).
      *
      *    �����������ΰ�
           03  WRK-MATSUYMD.
               05  WRK-MATSUYY             PIC 9(04).
               05  WRK-MATSUMM             PIC 9(02).
               05  WRK-MATSUDD             PIC 9(02).
      *
      *    �����Ѵ��ΰ�
           03  WRK-YMD                     PIC X(08).
           03  WRK-YMDR                    REDEFINES WRK-YMD.
               05  WRK-YY                  PIC 9(04).
               05  WRK-MM                  PIC 9(02).
               05  WRK-DD                  PIC 9(02).
      *
      *    ����ǯ�𻻽��ѥ��ꥢ
           03  WRK-NENREI-AREA.
              05  WRK-NENREI               PIC 9(03).
              05  WRK-YMDS1.
                  07  WRK-YYS1             PIC 9(04).
                  07  WRK-MMS1             PIC 9(02).
                  07  WRK-DDS1             PIC 9(02).
              05  WRK-YMDS2.
                  07  WRK-YYS2             PIC 9(04).
                  07  WRK-MMS2             PIC 9(02).
                  07  WRK-DDS2             PIC 9(02).
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �����ƥ�����ޥ���
           COPY    "CPSYSKANRI.INC".
      *    �ʰ��ŵ��ؾ���ݴ��ܾ����
           COPY    "CPSK1001.INC".
      *    �ʰ��ŵ��ؾ����
           COPY    "CPSK5000.INC".
      *    ����������
           COPY    "CPSK5001.INC".
      *    ���¼������
           COPY    "CPSK5002.INC".
      *    ����ִǸ��������������������
           COPY    "CPSK5008.INC".
      *
      *    ���ԥޥ���
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    ������������ޥ�����
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    �����ޥ�����
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    �������Ų�ץޥ�����
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    �������Ź԰٥ޥ�����
       01  NYUINACT-REC.
           COPY    "CPNYUINACT.INC".
      *
      *    �������û������å�
       01  NYUKSNCHK-REC.
           COPY    "CPNYUKSNCHK.INC".
      *
      *    �Ż�����ɽ�ޥ���
       01  ETENSU1-REC.
           COPY   "CPETENSU1.INC".
      *
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *    �����Ѵ�����
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
           COPY    "CPORCSGDAY.INC".
      *
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    ��ִǸ��������������������ѥ�᥿
           COPY    "CPORCSKANGOGEN.INC".
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           ORCSKANGOGEN-AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
           INITIALIZE                      LNK-KANGOGEN-HENKYAKU-AREA
           MOVE    ZERO                TO  LNK-KANGOGEN-RC
      *
      *    �ѥ�᡼�������å�
           PERFORM 100-PRM-CHK-SEC
           DISPLAY "============================================="
           DISPLAY "LNK-KANGOGEN-RC = " LNK-KANGOGEN-RC
           DISPLAY "============================================="
           IF    ( LNK-KANGOGEN-RC       NOT =   ZERO )
               GO  TO  000-PROC-EXT
           END-IF
      *
           PERFORM    100-KANGOGEN-GET-SEC
           .
      *
       000-PROC-EXT.
           EXIT   PROGRAM
           .
      *
      *****************************************************************
      *    �ѥ�᡼�������å�
      *****************************************************************
       100-PRM-CHK-SEC                     SECTION.
      *
      *    �±����Ž���̼���
           INITIALIZE                        SYS-1001-REC
           MOVE    LNK-KANGOGEN-HOSPNUM  TO  SYS-1001-HOSPNUM
           MOVE    "1001"                TO  SYS-1001-KANRICD
           MOVE    "*"                   TO  SYS-1001-KBNCD
           MOVE    LNK-KANGOGEN-SANTEIYM TO  SYS-1001-STYUKYMD(1:6)
           MOVE    "01"                  TO  SYS-1001-STYUKYMD(7:2)
           MOVE    SYS-1001-STYUKYMD     TO  SYS-1001-EDYUKYMD
           MOVE    SYS-1001-REC          TO  MCPDATA-REC
           MOVE    "tbl_syskanri"        TO  MCP-TABLE
           MOVE    "key10"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           MOVE    "tbl_syskanri"        TO  MCP-TABLE
           MOVE    "key10"               TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF       MCP-RC               =   ZERO
               MOVE  MCPDATA-REC         TO  SYS-1001-REC
           END-IF
      *    �����ƥ������������
           MOVE    "tbl_syskanri"        TO  MCP-TABLE
           MOVE    "key10"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    �±������å�
           IF     SYS-1001-HOSPSBT       =   1
               CONTINUE
           ELSE
               MOVE    1               TO  LNK-KANGOGEN-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    ����ǯ������å�
           IF     (LNK-KANGOGEN-SANTEIYM     =   SPACE)   OR
                  (LNK-KANGOGEN-SANTEIYM    <=  "201803")
               MOVE    2               TO  LNK-KANGOGEN-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    ���ԥޥ���������
           INITIALIZE                        PTINF-REC
           MOVE    LNK-KANGOGEN-HOSPNUM  TO  PTINF-HOSPNUM
           MOVE    LNK-KANGOGEN-PTID     TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF            NOT =   ZERO
               MOVE    3                TO  LNK-KANGOGEN-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    ������Υ����å���������ץޥ�����������
           INITIALIZE                      NYUINACCT-REC
           MOVE    LNK-KANGOGEN-HOSPNUM  TO  NACCT-HOSPNUM
           MOVE    LNK-KANGOGEN-PTID     TO  NACCT-PTID
           MOVE    LNK-KANGOGEN-SANTEIYM TO  NACCT-SRYYM
           MOVE    "5"                   TO  NACCT-ZAISKBKBN
           MOVE    "tbl_nyuinacct"       TO  MCP-TABLE
           MOVE    "key17"               TO  MCP-PATHNAME
           PERFORM 910-NYUINACCT-SELECT-SEC
           IF      FLG-NYUINACCT         NOT =   ZERO
               MOVE    4                 TO  LNK-KANGOGEN-RC
           ELSE
               MOVE    "tbl_nyuinacct"  TO  MCP-TABLE
               MOVE    "key17"          TO  MCP-PATHNAME
               PERFORM 910-NYUINACCT-READ-SEC
               IF   FLG-NYUINACCT       =   ZERO
                    CONTINUE
               ELSE
                    MOVE    4           TO  LNK-KANGOGEN-RC
               END-IF
           END-IF
      *    ������ץ�������
           MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
           MOVE    "key17"                 TO  MCP-PATHNAME
           PERFORM   990-DBCLOSE-SEC
      *
           IF    LNK-KANGOGEN-RC     NOT =   ZERO
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
      *    �����ƥ�����˸��������������Ϥ����뤫�����å�
      *   �ʤ�����ϥ���ΰ�ؤ������Ԥ���
           INITIALIZE                      SYSKANRI-REC
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    LNK-KANGOGEN-HOSPNUM
                                       TO  SYS-HOSPNUM
           MOVE    "5008"              TO  SYS-KANRICD
           MOVE    LNK-KANGOGEN-SANTEIYM
                                       TO  SYS-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-STYUKYMD(7:2)
           MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           PERFORM   UNTIL   MCP-RC    NOT =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-5008-REC
               IF      SYS-5008-YAKANGEN-AREA   NOT =   SPACE
                   PERFORM   VARYING   IDX  FROM   1   BY  1
                      UNTIL  IDX       >    12
                      IF  (SYS-5008-YAKANGENYMD(IDX) NOT =  SPACE) AND
                          (SYS-5008-YAKANGENYMD(IDX)(1:6)
                                       =   LNK-KANGOGEN-SANTEIYM)
                         MOVE   SYS-5008-KBNCD(1:2)   TO  IDX-BTU
                         MOVE   SYS-5008-KBNCD
                                       TO  WRK-BTU-TAIHI-BTUNUM(IDX-BTU)
                         MOVE   SYS-5008-YAKANGENYMD(IDX)(7:2)
                                       TO  IDX2
                         MOVE    "1"   TO  WRK-BTU-TAIHI-GENHIT
                                                        (IDX-BTU IDX2)
                      END-IF
                   END-PERFORM
               END-IF
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
           END-PERFORM
      *
      **     PERFORM  VARYING  IDX1  FROM  1  BY  1
      **      UNTIL   (IDX1  >  99)
      **        IF  WRK-BTU-TAIHI-BTUNUM(IDX1)  NOT =  SPACE
      **          DISPLAY "IDX1 = " IDX1
      **          DISPLAY "WRK-BTU-TAIHI-BTUNUM(IDX1) = "
      **                   WRK-BTU-TAIHI-BTUNUM(IDX1)
      **          PERFORM  VARYING  IDX2  FROM  1  BY  1
      **            UNTIL  IDX2  >  31
      **               IF  WRK-BTU-TAIHI-GENHIT(IDX1 IDX2)  NOT =  SPACE
      **                  DISPLAY "IDX2 = " IDX2
      **                  DISPLAY "WRK-BTU-TAIHI-GENHIT(IDX1 IDX2) = "
      **                           WRK-BTU-TAIHI-GENHIT(IDX1 IDX2)
      **               END-IF
      **          END-PERFORM
      **        END-IF
      **     END-PERFORM
      *
      *    �����ƥ������������
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF   WRK-BTU-TAIHI-AREA   =   SPACE
               MOVE    5           TO  LNK-KANGOGEN-RC
           END-IF
           .
       100-PRM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��ִǸ��������������������μ����߽���
      *****************************************************************
       100-KANGOGEN-GET-SEC            SECTION.
      *
      *    CONS-AREA����ִǸ����������������ˤĤ������û������ɤμ���
           PERFORM   VARYING  IDX   FROM   1  BY  1
             UNTIL   IDX   >   7
                INITIALIZE                           TNS-TENSU-REC
                MOVE    LNK-KANGOGEN-HOSPNUM     TO  TNS-HOSPNUM
                MOVE    CONS-KANGOGEN-SRYCD(IDX) TO  TNS-SRYCD
                MOVE    LNK-KANGOGEN-SANTEIYM    TO  TNS-YUKOSTYMD(1:6)
                MOVE    "01"                     TO  TNS-YUKOSTYMD(7:2)
                MOVE    TNS-YUKOSTYMD            TO  TNS-YUKOEDYMD
                MOVE    "tbl_tensu"              TO  MCP-TABLE
                MOVE    "key"                    TO  MCP-PATHNAME
                PERFORM 910-TENSU-SELECT-SEC
                MOVE    "tbl_tensu"              TO  MCP-TABLE
                MOVE    "key"                    TO  MCP-PATHNAME
                PERFORM 910-TENSU-READ-SEC
                IF      FLG-TENSU                =   ZERO
                        MOVE    TNS-CHUKSNCD     TO  WRK-CHUKSNCD(IDX)
                        MOVE    TNS-TEN      TO  WRK-KANGOGEN-TEN (IDX)
                END-IF
      *
                MOVE    "tbl_tensu"              TO  MCP-TABLE
                MOVE    "key"                    TO  MCP-PATHNAME
                PERFORM 990-DBCLOSE-SEC
           END-PERFORM
      **     DISPLAY  "WRK-CDKSNCD(1) = " WRK-CHUKSNCD(1)
      **     DISPLAY  "WRK-CDKSNCD(2) = " WRK-CHUKSNCD(2)
      **     DISPLAY  "WRK-CDKSNCD(3) = " WRK-CHUKSNCD(3)
      **     DISPLAY  "WRK-CDKSNCD(4) = " WRK-CHUKSNCD(4)
      **     DISPLAY  "WRK-CDKSNCD(5) = " WRK-CHUKSNCD(5)
      **     DISPLAY  "WRK-CDKSNCD(6) = " WRK-CHUKSNCD(6)
      **     DISPLAY  "WRK-CDKSNCD(7) = " WRK-CHUKSNCD(7)
      *
      *    �����������
           INITIALIZE                           STS-AREA-DAY
           INITIALIZE                           LNK-DAY7-AREA
           MOVE      "71"                   TO  LNK-DAY7-IRAI
           MOVE    LNK-KANGOGEN-SANTEIYM    TO  LNK-DAY7-YM
           CALL    "ORCSDAY"     USING      STS-AREA-DAY
                                            LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI          TO  WRK-MATSUYMD
      *
      *    �������򤫤鳺�����������������ֹ�����
      *    �ʿ������򤫤���ɤߡ�
      *    MOVE    ZERO                     TO  WRK-RRKNUM
           INITIALIZE                           PTNYUINRRK-REC
           MOVE    LNK-KANGOGEN-HOSPNUM     TO  PTNYUINRRK-HOSPNUM
           MOVE    LNK-KANGOGEN-PTID        TO  PTNYUINRRK-PTID
           MOVE    LNK-KANGOGEN-SANTEIYM    TO  PTNYUINRRK-NYUINYMD(1:6)
           MOVE    "99"                     TO  PTNYUINRRK-NYUINYMD(7:2)
           MOVE    LNK-KANGOGEN-SANTEIYM    TO  PTNYUINRRK-TAIINYMD(1:6)
           MOVE    "01"                     TO  PTNYUINRRK-TAIINYMD(7:2)
           MOVE    "tbl_ptnyuinrrk"         TO  MCP-TABLE
           MOVE    "key74"                  TO  MCP-PATHNAME
           PERFORM 910-PTNYUINRRK-SELECT-SEC
           IF      FLG-PTNYUINRRK           =   ZERO
               MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
               MOVE    "key74"              TO  MCP-PATHNAME
               PERFORM   910-PTNYUINRRK-READ-SEC
               PERFORM   UNTIL    FLG-PTNYUINRRK  NOT =  ZERO
      *           ���ϥ���ǥå���
                  IF   PTNYUINRRK-TENNYUYMD(1:6) < LNK-KANGOGEN-SANTEIYM
                      MOVE   1              TO  IDX-ST
                  END-IF
                  IF   PTNYUINRRK-TENNYUYMD(1:6) = LNK-KANGOGEN-SANTEIYM
                      MOVE   PTNYUINRRK-TENNYUYMD(7:2) TO  IDX-ST
                  END-IF
      *           ��λ����ǥå���
                  IF   PTNYUINRRK-TENSTUYMD(1:6) > LNK-KANGOGEN-SANTEIYM
                      MOVE   WRK-MATSUDD              TO  IDX-ED
                  END-IF
                  IF   PTNYUINRRK-TENSTUYMD(1:6) = LNK-KANGOGEN-SANTEIYM
                      MOVE   PTNYUINRRK-TENSTUYMD(7:2) TO  IDX-ED
                  END-IF
      *           �����ֹ楻�å�
                  PERFORM    VARYING  IDX    FROM  IDX-ST  BY  1
                    UNTIL    IDX   >    IDX-ED
                     IF    WRK-PTNYUINRRK-BTUNUM(IDX)  =   SPACE
                        MOVE   PTNYUINRRK-BTUNUM
                                     TO  WRK-PTNYUINRRK-BTUNUM(IDX)
                     ELSE
                        IF     PTNYUINRRK-RRKNUM  =   WRK-RRKNUM
      *                     Ʊ�����������ֹ�ʰ�ư�������ˤǤ����
      *                     �֤������򤷤ʤ�
                            CONTINUE
                        ELSE
      *                     �ۤʤ����������ֹ��Ʊ���������ˤǤ����
      *                     �֤������򤹤�
                            MOVE   PTNYUINRRK-BTUNUM
                                     TO  WRK-PTNYUINRRK-BTUNUM(IDX)
                        END-IF
                     END-IF
                  END-PERFORM
      *           �������ֹ������ؤ�
                  MOVE   PTNYUINRRK-RRKNUM     TO  WRK-RRKNUM
      *
                  MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
                  MOVE    "key74"              TO  MCP-PATHNAME
                  PERFORM   910-PTNYUINRRK-READ-SEC
               END-PERFORM
           END-IF
      *    �����������򥯥�����
           MOVE     "tbl_ptnyuinrrk"        TO  MCP-TABLE
           MOVE     "key74"                 TO  MCP-PATHNAME
           PERFORM  990-DBCLOSE-SEC
      *
      **     PERFORM   VARYING  IDX   FROM   1  BY  1
      **       UNTIL   IDX   >   31
      **           DISPLAY "IDX = " IDX
      **           DISPLAY "WRK-PTNYUINRRK-BTUNUM(IDX) = "
      **                    WRK-PTNYUINRRK-BTUNUM(IDX)
      **     END-PERFORM
      *
      *    �������狼�鸺���о���������
           PERFORM     VARYING  IDX1  FROM  1  BY  1
             UNTIL     IDX1   >   31
                IF  WRK-PTNYUINRRK-BTUNUM(IDX1)  NOT =  SPACE
                   MOVE   WRK-PTNYUINRRK-BTUNUM(IDX1)  TO  IDX-BTU
                   IF     WRK-BTU-TAIHI-GENHIT(IDX-BTU IDX1)  =  "1"
                       MOVE   "1"         TO   WRK-PT-GENHIT(IDX1)
                   END-IF
                END-IF
           END-PERFORM
      **     PERFORM     VARYING  IDX1  FROM  1  BY  1
      **       UNTIL     IDX1   >   31
      **             DISPLAY "IDX1 = " IDX1
      **             DISPLAY "WRK-PT-GENHIT(IDX1) = " WRK-PT-GENHIT(IDX1)
      **     END-PERFORM
      *
           IF      WRK-PT-GENHIT-AREA   NOT =   SPACE
               INITIALIZE                           NYUINACCT-REC
               MOVE  LNK-KANGOGEN-HOSPNUM       TO  NACCT-HOSPNUM
               MOVE  LNK-KANGOGEN-PTID          TO  NACCT-PTID
               MOVE  LNK-KANGOGEN-SANTEIYM      TO  NACCT-SRYYM
               MOVE  "1"                        TO  NACCT-ZAISKBKBN
               MOVE  "tbl_nyuinacct"            TO  MCP-TABLE
               MOVE  "key35"                    TO  MCP-PATHNAME
               PERFORM 910-NYUINACCT-SELECT-SEC
               IF      FLG-NYUINACCT            =   ZERO
                  MOVE    "tbl_nyuinacct"       TO  MCP-TABLE
                  MOVE    "key35"               TO  MCP-PATHNAME
                  PERFORM   910-NYUINACCT-READ-SEC
                  PERFORM   UNTIL   (FLG-NYUINACCT    =   1)
      *              ���������԰٥ơ��֥���ɤ߹���
                     PERFORM    910-NYUINACT-GET-SEC
      *              �����ޥ������������������û������ɼ���
                      INITIALIZE                    TNS-TENSU-REC
                      MOVE    LNK-KANGOGEN-HOSPNUM
                                                TO  TNS-HOSPNUM
                      MOVE    NSRY-SRYCD(1)     TO  TNS-SRYCD
                      MOVE    LNK-KANGOGEN-SANTEIYM
                                                TO  TNS-YUKOSTYMD(1:6)
                      MOVE    "01"              TO  TNS-YUKOSTYMD(7:2)
                      MOVE    TNS-YUKOSTYMD     TO  TNS-YUKOEDYMD
                      MOVE    "tbl_tensu"       TO  MCP-TABLE
                      MOVE    "key"             TO  MCP-PATHNAME
                      PERFORM 910-TENSU-SELECT-SEC
                      MOVE    "tbl_tensu"       TO  MCP-TABLE
                      MOVE    "key"             TO  MCP-PATHNAME
                      PERFORM 910-TENSU-READ-SEC
                      IF      FLG-TENSU         =   ZERO
                         PERFORM   VARYING  IDX1  FROM  1  BY  1
                           UNTIL   IDX1   >    31
                           IF  (WRK-PT-GENHIT(IDX1)     =   "1")   AND
                               (NACCT-DAY(IDX1)         =    1)
                             IF   TNS-CHUKSNCD  =   WRK-CHUKSNCD(1)
                                 MOVE  CONS-KANGOGEN-SRYCD(1)
                                                TO  LNK-KANGOGEN-SRYCD
                                                               (IDX1)
                                 MOVE  WRK-KANGOGEN-TEN (1)
                                                TO  LNK-KANGOGEN-TEN
                                                               (IDX1)
                             END-IF
                             IF   TNS-CHUKSNCD  =   WRK-CHUKSNCD(2)
                                 MOVE  CONS-KANGOGEN-SRYCD(2)
                                                TO  LNK-KANGOGEN-SRYCD
                                                               (IDX1)
                                 MOVE  WRK-KANGOGEN-TEN (2)
                                                TO  LNK-KANGOGEN-TEN
                                                               (IDX1)
                             END-IF
                             IF   TNS-CHUKSNCD  =   WRK-CHUKSNCD(3)
                                 MOVE  CONS-KANGOGEN-SRYCD(3)
                                                TO  LNK-KANGOGEN-SRYCD
                                                               (IDX1)
                                 MOVE  WRK-KANGOGEN-TEN (3)
                                                TO  LNK-KANGOGEN-TEN
                                                               (IDX1)
                             END-IF
                             IF   TNS-CHUKSNCD  =   WRK-CHUKSNCD(4)
                                 MOVE  CONS-KANGOGEN-SRYCD(4)
                                                TO  LNK-KANGOGEN-SRYCD
                                                               (IDX1)
                                 MOVE  WRK-KANGOGEN-TEN (4)
                                                TO  LNK-KANGOGEN-TEN
                                                               (IDX1)
                             END-IF
                             IF   TNS-CHUKSNCD  =   WRK-CHUKSNCD(5)
                                 MOVE  CONS-KANGOGEN-SRYCD(5)
                                                TO  LNK-KANGOGEN-SRYCD
                                                               (IDX1)
                                 MOVE  WRK-KANGOGEN-TEN (5)
                                                TO  LNK-KANGOGEN-TEN
                                                               (IDX1)
                             END-IF
                             IF   TNS-CHUKSNCD  =   WRK-CHUKSNCD(6)
                                 MOVE  CONS-KANGOGEN-SRYCD(6)
                                                TO  LNK-KANGOGEN-SRYCD
                                                               (IDX1)
                                 MOVE  WRK-KANGOGEN-TEN (6)
                                                TO  LNK-KANGOGEN-TEN
                                                               (IDX1)
                             END-IF
                             IF   TNS-CHUKSNCD  =   WRK-CHUKSNCD(7)
                                 MOVE  CONS-KANGOGEN-SRYCD(7)
                                                TO  LNK-KANGOGEN-SRYCD
                                                               (IDX1)
                                 MOVE  WRK-KANGOGEN-TEN (7)
                                                TO  LNK-KANGOGEN-TEN
                                                               (IDX1)
                             END-IF
                           END-IF
                         END-PERFORM
                      END-IF
      *
                      MOVE    "tbl_tensu"      TO  MCP-TABLE
                      MOVE    "key"           TO  MCP-PATHNAME
                      PERFORM 990-DBCLOSE-SEC
      *
                     MOVE    "tbl_nyuinacct"   TO  MCP-TABLE
                     MOVE    "key35"           TO  MCP-PATHNAME
                     PERFORM 910-NYUINACCT-READ-SEC
                  END-PERFORM
               END-IF
      *        ������ץ�������
               MOVE    "tbl_nyuinacct"         TO  MCP-TABLE
               MOVE    "key35"                 TO  MCP-PATHNAME
               PERFORM   990-DBCLOSE-SEC
           END-IF
      *
      **     PERFORM     VARYING  IDX1  FROM  1  BY  1
      **       UNTIL     IDX1   >   31
      **             DISPLAY "IDX1 = " IDX1
      **             DISPLAY "LNK-KANGOGEN-SRYCD(IDX1) = "
      **                      LNK-KANGOGEN-SRYCD(IDX1)
      **     END-PERFORM
      *
           IF   LNK-KANGOGEN-TBL   =   SPACE
               MOVE    5           TO  LNK-KANGOGEN-RC
           END-IF
      *
           .
      *
       100-KANGOGEN-GET-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    ���ԥޥ������ɹ�
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM   910-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC TO  PTINF-REC
                   MOVE    ZERO        TO  FLG-PTINF
               ELSE
                   INITIALIZE              PTINF-REC
                   MOVE    1           TO  FLG-PTINF
               END-IF
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   990-DBCLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ������������ޥ���������
      *****************************************************************
       910-PTNYUINRRK-SELECT-SEC        SECTION.
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           PERFORM   910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-PTNYUINRRK
           ELSE
               MOVE    1                  TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       910-PTNYUINRRK-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ������������ޥ������ɹ�
      *****************************************************************
       910-PTNYUINRRK-READ-SEC         SECTION.
      *
           PERFORM   910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PTNYUINRRK-REC
               MOVE    ZERO                TO  FLG-PTNYUINRRK
           ELSE
               INITIALIZE                      PTNYUINRRK-REC
               MOVE    1                   TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       910-PTNYUINRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ޥ�������
      *****************************************************************
       910-TENSU-SELECT-SEC        SECTION.
      *
           MOVE    TNS-TENSU-REC          TO  MCPDATA-REC
           PERFORM   910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-TENSU
           ELSE
               MOVE    1                  TO  FLG-TENSU
           END-IF
      *
           .
       910-TENSU-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ޥ����ɹ�
      *****************************************************************
       910-TENSU-READ-SEC        SECTION.
      *
           PERFORM   910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  TNS-TENSU-REC
               MOVE    ZERO               TO  FLG-TENSU
           ELSE
               INITIALIZE                     TNS-TENSU-REC
               MOVE    1                  TO  FLG-TENSU
           END-IF
      *
           .
       910-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ������ץޥ���������
      *****************************************************************
       910-NYUINACCT-SELECT-SEC        SECTION.
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           PERFORM   910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-NYUINACCT
           ELSE
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
           .
       910-NYUINACCT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ������ץޥ������ɹ�
      *****************************************************************
       910-NYUINACCT-READ-SEC        SECTION.
      *
           PERFORM   910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
               MOVE    ZERO            TO  FLG-NYUINACCT
           ELSE
               INITIALIZE                  NYUINACCT-REC
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
           .
       910-NYUINACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������Ź԰ټ���
      *****************************************************************
       910-NYUINACT-GET-SEC            SECTION.
      *
           INITIALIZE                      NYUINACT-REC
           MOVE    NACCT-HOSPNUM       TO  NSRY-HOSPNUM
           MOVE    NACCT-NYUGAIKBN     TO  NSRY-NYUGAIKBN
           MOVE    NACCT-PTID          TO  NSRY-PTID
           MOVE    NACCT-SRYYM         TO  NSRY-SRYYM
           MOVE    NACCT-ZAINUM        TO  NSRY-ZAINUM
           MOVE    "tbl_nyuinact"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-NYUINACT-SELECT-SEC
           IF      FLG-NYUINACT        =   ZERO
               MOVE    "tbl_nyuinact"  TO  MCP-TABLE
               MOVE    "key9"          TO  MCP-PATHNAME
               PERFORM   910-NYUINACT-READ-SEC
           END-IF
      *
      *    �����԰٥�������
           MOVE     "tbl_nyuinact"     TO  MCP-TABLE
           MOVE     "key9"             TO  MCP-PATHNAME
           PERFORM   990-DBCLOSE-SEC
      *
           .
       910-NYUINACT-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������Ź԰٥ޥ���������
      *****************************************************************
       910-NYUINACT-SELECT-SEC        SECTION.
      *
           MOVE    NYUINACT-REC        TO  MCPDATA-REC
           PERFORM   910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-NYUINACT
           ELSE
               MOVE    1                  TO  FLG-NYUINACT
           END-IF
      *
           .
       910-NYUINACT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������Ź԰٥ޥ������ɹ�
      *****************************************************************
       910-NYUINACT-READ-SEC         SECTION.
      *
           PERFORM   910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  NYUINACT-REC
               MOVE    ZERO                TO  FLG-NYUINACT
           ELSE
               INITIALIZE                      NYUINACT-REC
               MOVE    1                   TO  FLG-NYUINACT
           END-IF
      *
           .
       910-NYUINACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ơ��֥븡������
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
      *    �ģ��ɤ߹��߽���
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ¥�����������
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
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