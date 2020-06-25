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
       PROGRAM-ID.             ORCSACCESS.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ����
      *  ����ݡ��ͥ��̾  : �ե�����¸�ߥ����å�
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  07/10/18    NACL������    ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
       01  F-OK                    PIC 9(01)   VALUE 0.
       01  X-OK                    PIC 9(01)   VALUE 1.
       01  W-OK                    PIC 9(01)   VALUE 2.
       01  R-OK                    PIC 9(01)   VALUE 4.
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *    C�ؿ�Ϣ���ΰ�
           COPY    "CPORCACCESS.INC".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "CPORCSACCESS.INC".
      *
       PROCEDURE                   DIVISION    USING
           SACCESS-AREA
           .
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
      *    INITIALIZE                  FLG-AREA
      *    INITIALIZE                  IDX-AREA
      *    INITIALIZE                  WRK-AREA
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *****************************************************************
      *    �������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           IF    ( SACCESS-MODE    =   "R" OR "W" OR "X" )
               CONTINUE
           ELSE
               MOVE    "R"         TO  SACCESS-MODE
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    �����
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           INITIALIZE              CACCESS-AREA
           INSPECT CACCESS-IN-FILENAME TALLYING CACCESS-STR-LEN
                                       FOR ALL " "
           EVALUATE    SACCESS-MODE
           WHEN    "W"
               MOVE    W-OK        TO  CACCESS-MODE
           WHEN    "X"
               MOVE    X-OK        TO  CACCESS-MODE
               MOVE    "COB_LIBRARY_PATH"
                                   TO  CACCESS-ENVNAME
           WHEN    OTHER
               MOVE    R-OK        TO  CACCESS-MODE
           END-EVALUATE
      *
           MOVE    SACCESS-IN-FILENAME
                                   TO  CACCESS-IN-FILENAME
           CALL    "orcaccess"     USING
                                   CACCESS-AREA
           MOVE    CACCESS-OT-FILENAME
                                   TO  SACCESS-OT-FILENAME
           MOVE    CACCESS-FULLPATH
                                   TO  SACCESS-FULLPATH
           MOVE    CACCESS-OT-DIRNAME
                                   TO  SACCESS-DIRNAME
           MOVE    CACCESS-FILE-EXISTS
                                   TO  SACCESS-EXISTS
      *
           MOVE    CACCESS-RC      TO  SACCESS-RC
      *
           EVALUATE    SACCESS-MODE
           WHEN    "W"
               IF    ( CACCESS-RC  NOT =   ZERO )
                AND  ( CACCESS-WRITABLE
                                       =   1 )
                AND  ( CACCESS-FILE-EXISTS
                                       =   ZERO )
                   MOVE    ZERO    TO  SACCESS-RC
               END-IF
      *    WHEN    "X"
      *    WHEN    OTHER
           END-EVALUATE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    ��λ����
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *
           .
       300-END-EXT.
           EXIT.
      *