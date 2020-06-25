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
      ******************************************************************
      *
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCL0012.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �ạ̃��ɣ�
      *  ����ݡ��ͥ��̾  : �ạ̃��ɣ������Ѥ�����
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  05/12/06    NACL-¿��Ǽ   ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      * 03.05.00     NACL-¿��Ǽ  07/05/09  ���롼�׿����б�
      * 04.08.00     NACL-¿��Ǽ  16/05/XX  API�б�
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
      *FILE-CONTROL.
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-UKETUKE         PIC 9(01).
           03  FLG-OK              PIC 9(01).
      *
      *    ź���ΰ�
      *01  IDX-AREA.
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-SYSYMD.
               05  WRK-SYSYY         PIC 9(04).
               05  WRK-SYSMM         PIC 9(02).
               05  WRK-SYSDD         PIC 9(02).
      *API
           03  WRK-ERRCD             PIC X(02).
           03  WRK-UPDCNT            PIC 9(02).
      *
      *    �����ƥ��ΰ�
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    ����
       01  UKETUKE-REC.
           COPY    "CPUKETUKE.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *   �����Ѵ�����
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    �ޥ������ռ�������
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ���̥ѥ�᥿
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
       LINKAGE                     SECTION.
      *
       01  LNK-PTID             PIC 9(10).
       01  LNK-SRYKA            PIC X(02).
       01  LNK-TIME             PIC X(06).
       01  LNK-HOSPNUM          PIC 9(02).
       01  LNK-DRCD             PIC X(05).
       01  LNK-HKNCOMBI         PIC X(04).
       01  LNK-RC               PIC X(02).
       01  LNK-YMD              PIC X(08).
      *API
       01  LNK-ENDRC            PIC X(02).
      *
      *
      ******************************************************************
      *
       PROCEDURE                       DIVISION
                                       USING   LNK-PTID
                                               LNK-SRYKA
                                               LNK-TIME
                                               LNK-HOSPNUM
                                               LNK-DRCD
                                               LNK-HKNCOMBI
                                               LNK-RC
                                               LNK-YMD
                                               LNK-ENDRC
           .
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           EXIT    PROGRAM
           .
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *    �����ƥ����ե��å�
           ACCEPT  SYS-YMD         FROM    DATE
           COMPUTE WRK-SYSYY       =   SYS-YY      +   2000
           MOVE    SYS-MM              TO  WRK-SYSMM
           MOVE    SYS-DD              TO  WRK-SYSDD
      *
           INITIALIZE              SPA-AREA
           MOVE    LNK-HOSPNUM         TO  SPA-HOSPNUM
           MOVE    LNK-PTID            TO  SPA-PTID
           MOVE    LNK-YMD             TO  WRK-SYSYMD
      *
           MOVE    SPACE               TO  LNK-ENDRC
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    ����������
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *    ���եơ��֥��ɤ߹���
           INITIALIZE                  UKETUKE-REC
           MOVE    LNK-HOSPNUM         TO  UKE-HOSPNUM
           MOVE    LNK-PTID            TO  UKE-PTID
           MOVE    WRK-SYSYMD          TO  UKE-UKEYMD
           MOVE    LNK-TIME            TO  UKE-UKETIME
           MOVE    UKETUKE-REC         TO  MCPDATA-REC
      *
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 910-UKETUKE-READ-SEC
           ELSE
               INITIALIZE                  UKETUKE-REC
               MOVE    1               TO  FLG-UKETUKE
           END-IF
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM UNTIL     (FLG-UKETUKE      =  1  )  OR
                             (FLG-OK           =  1  )
      *
               IF     (UKE-UKEID           >   ZERO  )   AND
                      (UKE-KAIKEITIME      =   ZERO  )   AND
                      (UKE-SRYKA           =   LNK-SRYKA )
                   IF      (LNK-HKNCOMBI       =   "@@@@")  OR
                          ((LNK-HKNCOMBI   NOT =   "@@@@")  AND
                           (LNK-HKNCOMBI       =   UKE-HKNCOMBI))
      *                ������λ
                       MOVE    1                   TO  FLG-OK
                       MOVE    LNK-RC              TO  UKE-SRFLG
                       MOVE    SMCNDATE-YMD        TO  UKE-UPYMD
                       MOVE    SMCNDATE-HMS        TO  UKE-UPHMS
      *
                       MOVE    UKETUKE-REC         TO  MCPDATA-REC
      *
                       MOVE    "DBUPDATE"          TO  MCP-FUNC
                       MOVE    "tbl_uketuke"       TO  MCP-TABLE
      *****            MOVE    "key"               TO  MCP-PATHNAME
      *ver.4.7   SRFLG�Τ߹���
                       MOVE    "upd1"               TO  MCP-PATHNAME
grpsys                 CALL    "ORCDBMAIN"         USING   MCPAREA
                                                           MCPDATA-REC
                                                           SPA-AREA
                      IF      MCP-RC          NOT =   ZERO 
                           DISPLAY "ORCL0012 UKETUKE UPD ERR:"  MCP-RC
                                   ",KEY:" UKE-KEY
      *API�б�
                           MOVE    "01"            TO  WRK-ERRCD
                       ELSE
                           ADD     1               TO  WRK-UPDCNT
      *API�б�
                       END-IF
                   END-IF
               END-IF
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 910-UKETUKE-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    ��λ������
      *****************************************************************
       300-END-SEC                SECTION.
      *
      *API
           IF      WRK-ERRCD           =   SPACE
               IF      WRK-UPDCNT          =   ZERO
      *            �оݤʤ�
                   MOVE    "01"            TO  LNK-ENDRC
               ELSE
      *            ��������
                   MOVE    "00"            TO  LNK-ENDRC
               END-IF
           ELSE
      *        �������顼
               MOVE    "99"            TO  LNK-ENDRC
           END-IF
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���եޥ������ɹ�
      *****************************************************************
       910-UKETUKE-READ-SEC         SECTION.
      *
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-UKETUKE
               MOVE    MCPDATA-REC         TO  UKETUKE-REC
           ELSE
               MOVE    1                   TO  FLG-UKETUKE
           END-IF
      *
           .
       910-UKETUKE-READ-EXT.
           EXIT.