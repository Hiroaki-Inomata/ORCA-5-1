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
       IDENTIFICATION                  DIVISION.
       PROGRAM-ID.                     ORCBS02.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ��������
      *  ����ݡ��ͥ��̾  : ������ѹ������Խ�
      *  ������            :
      *  ��������    ��ȼ�        ����
      *  03/04/08    NACL������    ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  03.05.00    NACL-����     07/05/24  ���롼�׿����б�
      *****************************************************************
      *
       ENVIRONMENT                         DIVISION.
       CONFIGURATION                       SECTION.
       INPUT-OUTPUT                        SECTION.
       FILE-CONTROL.
      *
      *    ������ѥѥ�᥿�ե�����
           SELECT  BS01-FILE   ASSIGN      PRFNAME
                                   ORGANIZATION    IS  SEQUENTIAL.
      *
       DATA                                DIVISION.
       FILE                                SECTION.
      *
      *    �ѥ�᡼���ե�����
       FD  BS01-FILE.
       01  BS01-REC.
           COPY    "CPPARA.INC"    REPLACING //PARA-//
                                   BY        //BS01-//.
      *
       WORKING-STORAGE                     SECTION.
      *
      *    �ѥ�᡼���ե�����̾
           COPY    "CPASGNPARA.INC"
                                   REPLACING  //ASGNPARA//
                                   BY         //PRFNAME//.
      *
      *    �ե饰
       01  FLG-AREA.
           03  FLG-BS01                        PIC 9(01).
      *
      *    ����ǥå���
       01  IDX-AREA.
           03  IDX0                            PIC 9(05).
           03  IDX1                            PIC 9(05).
           03  IDXP                            PIC 9(05).
      *
       01  WRK-AREA.
           03  WRK-MEISAI                      PIC X(80).
           03  WRK-STR                         PIC X(10000).
           03  WRK-YMD                         PIC X(08).
           03  WRK-HMS                         PIC X(06).
           03  WRK-SEIKYUZ                     PIC ---,---,--9.
           03  WRK-SEIKYU-TAX-SAIZ             PIC ---,---,--9.
           03  WRK-MISYUZ                      PIC ---,---,--9.
           03  WRK-GOKEIZ                      PIC ---,---,--9.
           03  WRK-NYUKINZ                     PIC ---,---,--9.
           03  WRK-GOKEI                       PIC S9(10).
      *
           COPY    "MCPAREA".
           COPY    "COMMON-SPA".
           COPY    "CPSYUPARA03.INC".
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *****************************************************************
      *    ���֥ץ����ΰ�
      *****************************************************************
      *
      *    ����ե�����̾�Խ�
           COPY    "CPORCSGETTEMP.INC".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
       LINKAGE                 SECTION.
      *
       01  COMMAND-PARAM       PIC X(400).
      *
      ****************************************************************
       PROCEDURE           DIVISION
               USING
           COMMAND-PARAM.
      *
      *****************************************************************
      *    �����
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
                                       IDX-AREA
                                       WRK-AREA
                                       PRFNAME
                                       SPA-AREA
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                               INTO    SPA-HOSPNUM
                                       SPA-PTNUM
                                       SPA-OPID
                                       PRFNAME
                                       WRK-YMD
                                       WRK-HMS
           END-UNSTRING
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    PRFNAME         TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP" USING SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAME
                                   TO  PRFNAME
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    �����
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           OPEN    INPUT   BS01-FILE
      *
           PERFORM 900-BS01-READ-SEC
      *
           MOVE    ZERO                TO  IDX1
           PERFORM UNTIL ( FLG-BS01    NOT =   ZERO )
      *
               COMPUTE IDX1    =   IDX1    +   1
      *
               MOVE    BS01-DATA-REC       TO  SYUPARA03-AREA
               MOVE    SYUPARA03-SYU-DATA  TO  SYUNOU-REC
      *
               IF    ( IDX1    =   1 )
                   MOVE    ALL "="     TO  WRK-MEISAI
                   DISPLAY WRK-MEISAI
                   DISPLAY WRK-YMD  " " WRK-HMS " " BS01-TERMID
                   DISPLAY "�桼��̾:"  SPA-OPID
                   IF    ( SYUPARA03-HAKKOFLG  =   4 )
                       DISPLAY "�����������μ����ѹ�"
                       DISPLAY "�����ֹ桧" SPA-PTNUM
                       DISPLAY "����ǯ�" SYU-SRYYMD(1:6)
                   ELSE
                       DISPLAY "�������μ����ѹ�"
                       DISPLAY "�����ֹ桧" SPA-PTNUM
                       DISPLAY "����ǯ������" SYU-SRYYMD
                   END-IF
               END-IF
      *
               PERFORM 2001-EDIT-SEC
      *
               PERFORM 900-BS01-READ-SEC
      *
           END-PERFORM
      *
           CLOSE   BS01-FILE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �Խ�����
      *****************************************************************
       2001-EDIT-SEC                   SECTION.
      *
           MOVE    ALL "-"         TO  WRK-MEISAI
           DISPLAY WRK-MEISAI
           DISPLAY "���Ųʡ�"  SYUPARA03-SRYKAMEI
           MOVE    SPACE           TO  WRK-STR
           MOVE    1               TO  IDXP
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SYUPARA03-SRYKA-MAX )
               IF    ( IDX0        >   1 )
                   STRING  ","         DELIMITED BY SIZE
                   INTO    WRK-STR
                   WITH POINTER    IDXP
                   END-STRING
               END-IF
               STRING  SYUPARA03-SRYKA (IDX0)
                                   DELIMITED BY SIZE
               INTO    WRK-STR
               WITH POINTER    IDXP
               END-STRING
           END-PERFORM
           DISPLAY WRK-STR(1:IDXP)
      *
           DISPLAY "�ݸ����ࡧ"  SYUPARA03-HKNCOMBIMEI
           MOVE    SPACE           TO  WRK-STR
           MOVE    1               TO  IDXP
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SYUPARA03-HKNCOMBI-MAX )
               IF    ( IDX0        >   1 )
                   STRING  ","         DELIMITED BY SIZE
                   INTO    WRK-STR
                   WITH POINTER    IDXP
                   END-STRING
               END-IF
               STRING  SYUPARA03-HKNCOMBI (IDX0)
                                   DELIMITED BY SIZE
               INTO    WRK-STR
               WITH POINTER    IDXP
               END-STRING
           END-PERFORM
           DISPLAY WRK-STR(1:IDXP)
      *
           MOVE    SPACE           TO  WRK-STR
           MOVE    1               TO  IDXP
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SYUPARA03-GRP-DENPNUM-MAX )
               IF    ( IDX0        >   1 )
                   STRING  ","         DELIMITED BY SIZE
                   INTO    WRK-STR
                   WITH POINTER    IDXP
                   END-STRING
               END-IF
               STRING  SYUPARA03-GRP-DENPNUM (IDX0)
                                   DELIMITED BY SIZE
               INTO    WRK-STR
               WITH POINTER    IDXP
               END-STRING
           END-PERFORM
           DISPLAY "��ɼ�ֹ桧" WRK-STR(1:IDXP)
      *
           MOVE    SYUPARA03-ORG-SEIKYU    TO  WRK-SEIKYUZ
           MOVE    SYUPARA03-ORG-SEIKYU-TAX-SAI
                                           TO  WRK-SEIKYU-TAX-SAIZ
           MOVE    SYUPARA03-ORG-MISYU     TO  WRK-MISYUZ
           COMPUTE WRK-GOKEI
               =   SYUPARA03-ORG-SEIKYU
               +   SYUPARA03-ORG-MISYU
           MOVE    WRK-GOKEI               TO  WRK-GOKEIZ
           MOVE    SYUPARA03-ORG-NYUKIN    TO  WRK-NYUKINZ
           DISPLAY "�ѹ�����"
                   "��������ۡ�"     WRK-SEIKYUZ " ��"
                   "�����ǡʺƷǡˡ�" WRK-SEIKYU-TAX-SAIZ " ��"
                   "��������ۡ�"     WRK-MISYUZ " ��"
                   "�������ۡ�"     WRK-GOKEIZ " ��"
                   "��������ۡ�"     WRK-NYUKINZ " ��"
      *
           MOVE    SYUPARA03-SEIKYU        TO  WRK-SEIKYUZ
           MOVE    SYUPARA03-SEIKYU-TAX-SAI
                                           TO  WRK-SEIKYU-TAX-SAIZ
           MOVE    SYUPARA03-MISYU         TO  WRK-MISYUZ
           COMPUTE WRK-GOKEI
               =   SYUPARA03-SEIKYU
               +   SYUPARA03-MISYU
           MOVE    WRK-GOKEI               TO  WRK-GOKEIZ
           MOVE    SYUPARA03-NYUKIN        TO  WRK-NYUKINZ
           DISPLAY "�ѹ��塧"
                   "��������ۡ�"     WRK-SEIKYUZ " ��"
                   "�����ǡʺƷǡˡ�" WRK-SEIKYU-TAX-SAIZ " ��"
                   "��������ۡ�"     WRK-MISYUZ " ��"
                   "�������ۡ�"     WRK-GOKEIZ " ��"
                   "��������ۡ�"     WRK-NYUKINZ " ��"
      *
           .
       2001-EDIT-EXT.
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
      *
      *****************************************************************
      *    ������ѥѥ�᥿�ե������ɹ�����
      *****************************************************************
       900-BS01-READ-SEC               SECTION.
      *
           MOVE    ZERO            TO  FLG-BS01
      *
           READ    BS01-FILE
           AT  END
               MOVE    1           TO  FLG-BS01
           END-READ
      *
           .
       900-BS01-READ-EXT.
           EXIT.
      *