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
       PROGRAM-ID.             ORCSFIXREDDATA.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ����
      *  ����ݡ��ͥ��̾  : monpe�����ǡ����㤭�̤콤������(COBOL¦)
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  07/06/20    NACL������    ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
       WORKING-STORAGE         SECTION.
      *
      *    �ե饰�ΰ�
      *01  FLG-AREA.
      *
      *    ź���ΰ�
      *01  IDX-AREA.
      *    03  IDX0                        PIC 9(05).
      *
       01  WRK-AREA.
           03  WRK-PATH                     PIC X(128).
           03  WRK-STDPATH                  PIC X(128).
           03  WRK-PATCHPATH                PIC X(128).
           03  WRK-SITEPATH                 PIC X(128).
           03  WRK-ERRMSG                   PIC X(500).
      *
           COPY  "CPORCSFIXREDDATA.INC"    REPLACING //SFIXREDDATA-//
                                           BY        //WKSFIXREDDATA-//.
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *****************************************************************
      *    ���֥ץ����ΰ�
      *****************************************************************
      *
      *    �ե�����ǥ��쥯�ȥ���ֻ��ꥵ��
           COPY  "CPMKPASS.INC".
      *
      *    �ե�����¸�ߥ����å�
           COPY    "CPORCSFILECHK.INC".
      *
           COPY   MCPAREA.
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY  "CPORCSFIXREDDATA.INC".
           COPY  "COMMON-SPA".
      *
       PROCEDURE                    DIVISION    USING
                                                SFIXREDDATA-AREA
                                                SPA-AREA.
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
      *    CALL    "ORCSENCODING"      USING
      *                                MCPAREA
      *                                SPA-AREA
      *
      *    IF    ( SPA-ENCODING    =   2 )
      *
      *        PERFORM 100-INIT-SEC
      *
      *        PERFORM 200-MAIN-SEC
      *
      *        PERFORM 300-END-SEC
      *
      *    END-IF
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    �������
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
      *    INITIALIZE                  FLG-AREA
      *    INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           INITIALIZE                  ORCSMKPASSAREA
           STRING  "form/"             DELIMITED   BY  SIZE
                   SFIXREDDATA-FORMID  DELIMITED   BY  SPACE
           INTO    MKPASS-IN-01
           END-STRING
           CALL    "ORCSMKPASS"        USING
                                       ORCSMKPASSAREA
           MOVE    MKPASS-OT-01    TO  WRK-STDPATH
      *
           INITIALIZE                  ORCSMKPASSAREA
           STRING  "patch-lib/form/"   DELIMITED   BY  SIZE
                   SFIXREDDATA-FORMID  DELIMITED   BY  SPACE
           INTO    MKPASS-IN-01
           END-STRING
           CALL    "ORCSMKPASS"        USING
                                       ORCSMKPASSAREA
           MOVE    MKPASS-OT-01    TO  WRK-PATCHPATH
      *
           INITIALIZE                  ORCSMKPASSAREA
           STRING  "form/"             DELIMITED   BY  SIZE
                   SFIXREDDATA-FORMID  DELIMITED   BY  SPACE
           INTO    MKPASS-IN-01
           END-STRING
           CALL    "ORCSMKPASSSITE"    USING
                                       ORCSMKPASSAREA
           MOVE    MKPASS-OT-01    TO  WRK-SITEPATH
      *
           IF    ( SFIXREDDATA-SITEKBN =   "1" )
               INITIALIZE              ORCSFILECHKAREA
               MOVE    WRK-PATCHPATH   TO  FILECHK-NAME
               CALL    "ORCSFILECHK"       USING
                                           ORCSFILECHKAREA
               IF    ( FILECHK-RC      =   ZERO )
                   MOVE    WRK-PATCHPATH   TO  WRK-PATH
               ELSE
                   MOVE    WRK-STDPATH     TO  WRK-PATH
               END-IF
           ELSE
                   MOVE    WRK-SITEPATH    TO  WRK-PATH
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    �����
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           MOVE    SPACE               TO  WKSFIXREDDATA-PATH
                                           WKSFIXREDDATA-DATA
      *
           MOVE    1                   TO  SFIXREDDATA-PATHLEN
           STRING  SFIXREDDATA-PATH        DELIMITED   BY  SIZE
           INTO    WKSFIXREDDATA-PATH
           WITH    POINTER             SFIXREDDATA-PATHLEN
           END-STRING
           COMPUTE SFIXREDDATA-PATHLEN =   SFIXREDDATA-PATHLEN -   1
      *
           MOVE    1                   TO  SFIXREDDATA-DATALEN
           STRING  SFIXREDDATA-DATA        DELIMITED   BY  SIZE
           INTO    WKSFIXREDDATA-DATA
           WITH    POINTER             SFIXREDDATA-DATALEN
           END-STRING
           COMPUTE SFIXREDDATA-DATALEN =   SFIXREDDATA-DATALEN -   1
      *
           MOVE    WRK-PATH        TO  SFIXREDDATA-PATH
           CALL    "fixreddata"        USING
                                       SFIXREDDATA-CLNK
           IF    ( SFIXREDDATA-RC  =   1 )
               MOVE    SPACE       TO  WRK-ERRMSG
               STRING  "** fixed cobdata for "
                                               DELIMITED   BY  SIZE
                       SFIXREDDATA-FORMID      DELIMITED   BY  SPACE
                       " **"                   DELIMITED   BY  SIZE
                       LOW-VALUE               DELIMITED   BY  SIZE
               INTO    WRK-ERRMSG
               END-STRING
      *
               CALL    "coblog"    USING   WRK-ERRMSG
      *
           ELSE
               MOVE    WKSFIXREDDATA-DATA
                                   TO  SFIXREDDATA-DATA
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    ��λ����
      *****************************************************************
       300-END-SEC                     SECTION.
      *
      *
           .
       300-END-EXT.
           EXIT.