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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCSHOSPNUM.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ��̳����
      *  ����ݡ��ͥ��̾  : �ޥ�����˥塼
      *  ������            : 
      *  07/01/29    NACL�ݰ�ƣ��   ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  04.08.00    NACL-¿��Ǽ  15/04/28  �ѥ���ɥ����å��ɲ�
      *  04.08.00    NACL-����    15/11/17  �ѥ���ɥ����å��ѻ�
      *  04.08.00    NACL-¿��Ǽ  15/11/20  API��SYSUSER�����ɲ�
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-OK              PIC 9(01).
           03  FLG-SYSBASE         PIC 9(01).
           03  FLG-SYSUSER         PIC 9(01).
           03  FLG-PASSWD          PIC 9(01).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-USER.
               05  WRK-USERHOSPNUM PIC 9(02).
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �����ƥ����
       01  SYSBASE-REC.
           COPY    "CPSYSBASE.INC".
      *
      *    �����ƥ�桼����
       01  SYSUSER-REC.
           COPY    "CPSYSUSER.INC".
      *
      *    �ѥ����
           COPY    "CPPASSWD.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
      *     
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
        LINKAGE                     SECTION.
           COPY    MCPAREA.
           COPY    COMMON-SPA.
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPA-AREA.
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                     FLG-AREA
      *
      *    �ѥ������Ͽ�����å���API�б���
      *    IF      SPA-MOTOPG          =   "API"
      *        PERFORM 100-PASSWD-CHECK-SEC
      *    END-IF
      *
      *    ���ŵ��ؼ����ֹ�����
           IF      SPA-ERRCD           =   SPACE
               PERFORM 100-HOSPNUM-SET-SEC
           END-IF
      *
      *    �����ƥ���ܥ����å�����
           IF      SPA-ERRCD           =   SPACE
               PERFORM 100-SYSBASE-CHECK-SEC
               IF      FLG-OK              =   ZERO
                   MOVE    ZERO                TO  SPA-HOSPNUM
                   MOVE    "1002"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    ���ŵ��ؼ����ֹ��������
      *****************************************************************
       100-HOSPNUM-SET-SEC       SECTION.
      *
           MOVE    ZERO                TO  SPA-HOSPNUM
      *
           MOVE    "1"                 TO  SPA-NOCHK-HOSPNUM
      *
           INITIALIZE                      SYSUSER-REC
           MOVE    SPA-OPID            TO  SYSUSER-USERID
           MOVE    SYSUSER-REC         TO  MCPDATA-REC
           MOVE    "tbl_sysuser"       TO  MCP-TABLE 
           MOVE    "key"               TO  MCP-PATHNAME 
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sysuser"   TO  MCP-TABLE 
               MOVE    "key"           TO  MCP-PATHNAME 
               PERFORM 900-SYSUSER-READ-SEC
           ELSE
               INITIALIZE                  SYSUSER-REC
               MOVE    1               TO  FLG-SYSUSER
           END-IF
           MOVE    "tbl_sysuser"       TO  MCP-TABLE 
           MOVE    "key"               TO  MCP-PATHNAME 
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-SYSUSER         =   ZERO
      *        ���ŵ��ؼ����ֹ�
               MOVE    SYSUSER-HOSPNUM     TO  SPA-HOSPNUM
      *        �����ѡ��Х�����
               MOVE    SYSUSER-SUPERVISOR  TO  SPA-SUPERVISOR
      *ginbee ������������å�
               IF     (MCP-MIDDLEWARE-NAME =   "ginbee")
                 AND  (SPA-HOSPNUM     NOT =   1)
                   MOVE    "1023"              TO  SPA-ERRCD
               END-IF
           ELSE
               MOVE    "1002"              TO  SPA-ERRCD
      *
      *ginbee API �б�
               IF     (MCP-MIDDLEWARE-NAME =   "ginbee" )
                 AND  (SPA-MOTOPG          =   "API"    )
                   MOVE    SPACE               TO  SPA-ERRCD
                   MOVE    01                  TO  SPA-HOSPNUM
                   MOVE    SPACE               TO  SPA-SUPERVISOR
               END-IF
      *
           END-IF
      *
           MOVE    SPACE               TO  SPA-NOCHK-HOSPNUM
      *
           .
       100-HOSPNUM-SET-EXT.
           EXIT.
      *****************************************************************
      *    �����ƥ���ܥ����å�����
      *****************************************************************
       100-SYSBASE-CHECK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           MOVE    ZERO                TO  FLG-SYSBASE
           INITIALIZE                      SYSBASE-REC
           MOVE    SYSBASE-REC         TO  MCPDATA-REC
           MOVE    "tbl_sysbase"       TO  MCP-TABLE 
           MOVE    "all"               TO  MCP-PATHNAME 
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sysbase"   TO  MCP-TABLE 
               MOVE    "all"           TO  MCP-PATHNAME 
               PERFORM 900-SYSBASE-READ-SEC
           ELSE
               INITIALIZE                  SYSBASE-REC
               MOVE    1               TO  FLG-SYSBASE
           END-IF
           PERFORM UNTIL   FLG-SYSBASE =   1
               IF  SPA-HOSPNUM         =   SYSBASE-HOSPNUM
                   MOVE    1           TO  FLG-OK
                   MOVE    1           TO  FLG-SYSBASE
               END-IF
               IF  FLG-SYSBASE         =   ZERO
                   MOVE    "tbl_sysbase"   TO  MCP-TABLE 
                   MOVE    "all"           TO  MCP-PATHNAME 
                   PERFORM 900-SYSBASE-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_sysbase"       TO  MCP-TABLE 
           MOVE    "all"               TO  MCP-PATHNAME 
           PERFORM 990-DBCLOSE-SEC
           .
       100-SYSBASE-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    �ѥ���ɥ����å�����
      *****************************************************************
       100-PASSWD-CHECK-SEC       SECTION.
      *
           MOVE    "1"                 TO  SPA-NOCHK-HOSPNUM
      *
           INITIALIZE                      PASSWD-REC
           MOVE    SPA-OPID            TO  PASSWD-USERID
           MOVE    PASSWD-REC          TO  MCPDATA-REC
           MOVE    "tbl_passwd"        TO  MCP-TABLE 
           MOVE    "key"               TO  MCP-PATHNAME 
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_passwd"    TO  MCP-TABLE 
               MOVE    "key"           TO  MCP-PATHNAME 
               PERFORM 910-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PASSWD-REC
                   MOVE    ZERO            TO  FLG-PASSWD
               ELSE
                   MOVE    1               TO  FLG-PASSWD
               END-IF
           ELSE
               INITIALIZE                  PASSWD-REC
               MOVE    1               TO  FLG-PASSWD
           END-IF
           MOVE    "tbl_passwd"        TO  MCP-TABLE 
           MOVE    "key"               TO  MCP-PATHNAME 
           PERFORM 990-DBCLOSE-SEC
      *    �ѥ���ɤʤ�
           IF      FLG-PASSWD      NOT =   ZERO
               MOVE    "1002"              TO  SPA-ERRCD
           END-IF
      *
           MOVE    SPACE               TO  SPA-NOCHK-HOSPNUM
           .
       100-PASSWD-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    �����ƥ�����ɹ�����
      *****************************************************************
       900-SYSBASE-READ-SEC            SECTION.
      *
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYSBASE-REC
               MOVE    ZERO            TO  FLG-SYSBASE
           ELSE
               MOVE    1               TO  FLG-SYSBASE
           END-IF
           .
       900-SYSBASE-READ-EXT.
           EXIT.
      *****************************************************************
      *    �����ƥ�����ɹ�����
      *****************************************************************
       900-SYSUSER-READ-SEC            SECTION.
      *
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYSUSER-REC
               MOVE    ZERO            TO  FLG-SYSUSER
           ELSE
               MOVE    1               TO  FLG-SYSUSER
           END-IF
           .
       900-SYSUSER-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ơ��֥븡������
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ơ��֥�FETCH����
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       910-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ơ��֥륯����������
      *****************************************************************
       990-DBCLOSE-SEC                 SECTION.
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