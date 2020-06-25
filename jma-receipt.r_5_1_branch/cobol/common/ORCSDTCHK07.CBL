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
       PROGRAM-ID.             ORCSDTCHK07.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �ǡ��������å�
      *  ����ݡ��ͥ��̾  : ������̾�����å��Խ�
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
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-OVER                    PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX0                        PIC 9(05).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-BYOMEICD-G.
               05  WRK-BYOMEICD            PIC X(07).
               05  WRK-BYOMEICD-MAX        PIC 9(05).
               05  WRK-BYOMEICDL-G.
                   07  WRK-BYOMEICDL       PIC X(07)
                                           OCCURS      21.
      *
       01  CONST-AREA.
           03  CONST-MICODE            PIC X(07)   VALUE   "0000999".
           03  CONST-UTAGAICD          PIC X(07)   VALUE   "ZZZ8002".
           03  CONST-KYUSEICD          PIC X(07)   VALUE   "ZZZ4012".
           03  CONST-MKCD-MAX          PIC 9(05)   VALUE   21.
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *
      *
      *****************************************************************
      *    ���֥ץ����ΰ�
      *****************************************************************
      *
      *    ��̾�Խ�����
           COPY    "CPORCSMKBYOMEI.INC".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
       LINKAGE                     SECTION.
      *
      *    �ǡ��������å������꡼
       01  QDTCHK-REC.
           COPY    "CPQDTCHK.INC".
      *
      *
       PROCEDURE                    DIVISION    USING
                                                QDTCHK-REC.
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           PERFORM 100-INIT-SEC.
      *
           PERFORM 200-MAIN-SEC.
      *
           PERFORM 300-END-SEC.
      *
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
           INITIALIZE                  FLG-AREA
                                       IDX-AREA
                                       WRK-AREA
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    �����
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           INITIALIZE                  ORCSMKBYOMEIAREA
           MOVE    QDTCHK-UTAGAIFLG TO SMKBYO-UTAGAIFLG
           MOVE    QDTCHK-PTBYOMEI TO  SMKBYO-BYOMEI
           CALL    "ORCSMKBYOMEI"  USING   ORCSMKBYOMEIAREA
           MOVE    SMKBYO-BYOMEI   TO  QDTCHK-PTBYOMEI
      *
           IF    ( QDTCHK-BYOMEICD (1)         =   CONST-MICODE )
               CONTINUE
           ELSE
               PERFORM 2001-EDIT-BYOMEICD-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    ��̾�������Խ�����
      *****************************************************************
       2001-EDIT-BYOMEICD-SEC          SECTION.
      *
           INITIALIZE                  WRK-BYOMEICD-G
           MOVE    ZERO            TO  FLG-OVER
      *
           IF    ( SMKBYO-STS-KYUSEI   =   1 )
               MOVE    CONST-KYUSEICD  TO  WRK-BYOMEICD
               PERFORM 20011-EDIT-BYOMEICD-SEC
           END-IF
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   CONST-MKCD-MAX )
                    OR   ( QDTCHK-BYOMEICD (IDX0)  =   SPACE )
                    OR   ( FLG-OVER    =   1 )
      *
               MOVE    QDTCHK-BYOMEICD (IDX0)
                                       TO  WRK-BYOMEICD
               PERFORM 20011-EDIT-BYOMEICD-SEC
      *
           END-PERFORM
      *
           IF    ( SMKBYO-STS-UTAGAI   =   1 )
               MOVE    CONST-UTAGAICD  TO  WRK-BYOMEICD
               PERFORM 20011-EDIT-BYOMEICD-SEC
           END-IF
      *
           IF    ( FLG-OVER        =   ZERO )
               MOVE    WRK-BYOMEICDL-G TO  QDTCHK-BYOMEICD-G
           ELSE
               INITIALIZE              QDTCHK-BYOMEICD-G
               MOVE    CONST-MICODE    TO  QDTCHK-BYOMEICD (1)
           END-IF
      *
           .
       2001-EDIT-BYOMEICD-EXT.
           EXIT.
      *****************************************************************
      *    ��̾�������Խ�����
      *****************************************************************
       20011-EDIT-BYOMEICD-SEC          SECTION.
      *
           IF    ( WRK-BYOMEICD-MAX   >=   CONST-MKCD-MAX )
               MOVE    1           TO  FLG-OVER
           ELSE
               COMPUTE WRK-BYOMEICD-MAX
                    =  WRK-BYOMEICD-MAX    +   1
      *
               MOVE    WRK-BYOMEICD    TO  WRK-BYOMEICDL
                                               (WRK-BYOMEICD-MAX)
      *
           END-IF
      *
           .
       20011-EDIT-BYOMEICD-EXT.
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