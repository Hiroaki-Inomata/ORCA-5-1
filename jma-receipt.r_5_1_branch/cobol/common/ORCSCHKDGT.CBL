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
       PROGRAM-ID.             ORCSCHKDGT.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    :
      *  ����ݡ��ͥ��̾  : �����å��ǥ��åȻ��Сʥ����å���
      *  �ֵѥ��顼������  : 00:���顼�ʤ�
      *                      10:���ϥѥ�᡼������
      *                      01:���ϥ����å��ǥ��åȸ���
      *                                      �ʽ�����ʬ�ᣲ�Τߡ�
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  00/12/01    MCC-����      ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    MCC-����     01/04/17  ���Ѽ�����ˡ��ü����Ԥ�
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    �ե饰�ΰ�
       01  STS-AREA.
           03  STS-KNJMST          PIC X(02).
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END             PIC X(03).
      *
      *    ����ǥå����ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(03).
      *----(01.00.01) LINE ADD START --------------------------------
           03  IDY                 PIC 9(03).
      *----(01.00.01) LINE ADD END   --------------------------------
      *
      *    ��������ΰ�
       01  CNT-AREA.
           03  CNT-IN              PIC 9(06).
           03  CNT-OUT             PIC 9(06).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-CD              PIC 9(01).
           03  WRK-CD1             PIC 9(02).
           03  WRK-CD2             PIC 9(02).
           03  WRK-CD2-R           REDEFINES   WRK-CD2.
               05  WRK-CD2-1       PIC 9(01).
               05  WRK-CD2-2       PIC 9(01).
           03  WRK-KEI             PIC 9(03).
           03  WRK-GIV             PIC 9(03).
           03  WRK-AMARI           PIC 9(03).
           03  WRK-END             PIC 9(02).
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
        LINKAGE                    SECTION.
      *
           COPY     "CPORCCHKDGT.INC".
      *
       PROCEDURE                   DIVISION
                                   USING    LNK-CHKDGT-AREA.
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL       FLG-END     =   "END"
      *
           PERFORM 300-END-SEC
      *
           EXIT PROGRAM
           .
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  LNK-OUT
      *
           PERFORM     110-PARA-CHECK-SEC
           IF  LNK-O-ERRCD         NOT =   ZERO
               MOVE    "END"           TO  FLG-END
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ϥѥ�᡼�������å�����
      *****************************************************************
       110-PARA-CHECK-SEC              SECTION.
      *
           IF    LNK-I-SYORIKBN        =   1   OR   2
               CONTINUE
           ELSE
               MOVE    10              TO  LNK-O-ERRCD
               GO      TO              110-PARA-CHECK-EXT
           END-IF
      *
           MOVE    ZERO                TO  WRK-END
           PERFORM     VARYING     IDX   FROM    1   BY  1
                       UNTIL      (IDX               >   20   )
                       OR         (LNK-I-CODE(IDX:1) =   SPACE)
               MOVE    IDX             TO  WRK-END
           END-PERFORM
           IF    LNK-I-CODE(1:WRK-END) IS  NUMERIC
               CONTINUE
           ELSE
               MOVE    10              TO  LNK-O-ERRCD
               GO      TO              110-PARA-CHECK-EXT
           END-IF
           IF    (LNK-I-SYORIKBN       =   1 )  AND
                 (WRK-END              >   19)
               MOVE    10              TO  LNK-O-ERRCD
               GO      TO              110-PARA-CHECK-EXT
           END-IF
      *
           IF    (LNK-I-SYORIKBN       =   2    )  AND
                 (LNK-I-CODE(2:1)      =   SPACE)
               MOVE    10              TO  LNK-O-ERRCD
               GO      TO              110-PARA-CHECK-EXT
           END-IF
           .
       110-PARA-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *----(01.00.01) LINE DEL START --------------------------------
      **    ������å��ǥ��åȻ��С�
      *     IF    LNK-I-SYORIKBN    =   1
      *         MOVE    20          TO  WRK-END
      *     ELSE
      **    ������å��ǥ��åȻ��и塢��ӡ�
      **    ���������ɤκǸ夫�飲���ܤΥ���ǥå����������Ƥ���
      *----(01.00.01) LINE DEL END   --------------------------------
           PERFORM     VARYING     IDX   FROM    1   BY  1
                       UNTIL      (IDX               >   20   )
                       OR         (LNK-I-CODE(IDX:1) =   SPACE)
               MOVE    IDX         TO  WRK-END
           END-PERFORM
      *----(01.00.01) LINE ADD START --------------------------------
           IF  LNK-I-SYORIKBN      =   "2"
      *----(01.00.01) LINE ADD END   --------------------------------
      *    ���������ɤκǸ夫�飲���ܤΥ���ǥå����������Ƥ���
               COMPUTE     WRK-END     =   WRK-END   -   1
               IF    WRK-END           =   ZERO
                   MOVE    "END"       TO  FLG-END
                   GO      TO          200-MAIN-EXT
               END-IF
           END-IF
      *
      *----(01.00.01) LINE ADD START --------------------------------
           MOVE    WRK-END         TO  IDY
      *----(01.00.01) LINE ADD END   --------------------------------
           MOVE    ZERO            TO  WRK-KEI
           PERFORM     VARYING     IDX   FROM    1   BY  1
                       UNTIL      (IDX               >   20     )
                       OR         (IDX               >   WRK-END)
                       OR         (LNK-I-CODE(IDX:1) =   SPACE  )
      *----(01.00.01) LINE ADD START --------------------------------
                       OR         (IDY               =   ZERO   )
      *----(01.00.01) LINE ADD END   --------------------------------
      *
      *----(01.00.01) LINE MOD START --------------------------------
      *         MOVE    LNK-I-CODE (IDX:1)    TO  WRK-CD1
               MOVE    LNK-I-CODE (IDY:1)    TO  WRK-CD
               MOVE    WRK-CD              TO  WRK-CD1
      *----(01.00.01) LINE MOD END   --------------------------------
      *
               DIVIDE    2   INTO  IDX     GIVING      WRK-GIV
                                           REMAINDER   WRK-AMARI
               IF    WRK-AMARI     >   ZERO
                   COMPUTE     WRK-CD2     =   WRK-CD1     *   2
               ELSE
                   COMPUTE     WRK-CD2     =   WRK-CD1     *   1
               END-IF
               IF    WRK-CD2       >=  10
                   ADD     WRK-CD2-1       TO  WRK-KEI
                   ADD     WRK-CD2-2       TO  WRK-KEI
               ELSE
                   ADD     WRK-CD2         TO  WRK-KEI
               END-IF
      *----(01.00.01) LINE ADD START --------------------------------
               COMPUTE     IDY     =   IDY     -   1
      *----(01.00.01) LINE ADD END   --------------------------------
           END-PERFORM
           DIVIDE    10  INTO    WRK-KEI   GIVING      WRK-GIV
                                           REMAINDER   WRK-AMARI
           IF    WRK-AMARI             =   ZERO
               MOVE    ZERO            TO  LNK-O-CHKDGT
           ELSE
               COMPUTE   LNK-O-CHKDGT  =   10    -   WRK-AMARI
           END-IF
      *
      *    ������å��ǥ��åȻ��С�
           IF    LNK-I-SYORIKBN    =   1
               MOVE    LNK-I-CODE      TO  LNK-O-CODE
               MOVE    LNK-O-CHKDGT    TO  LNK-O-CODE(IDX:1)
               MOVE    ZERO            TO  LNK-O-ERRCD
           ELSE
      *    ������å��ǥ��åȻ��и塢��ӡ�
               COMPUTE     WRK-END     =   WRK-END   +   1
               MOVE    LNK-I-CODE (WRK-END:1)
                                           TO  WRK-CD
               IF    LNK-O-CHKDGT      =   WRK-CD
                   MOVE    LNK-I-CODE      TO  LNK-O-CODE
                   MOVE    ZERO            TO  LNK-O-ERRCD
               ELSE
                   MOVE    LNK-I-CODE      TO  LNK-O-CODE
                   MOVE    LNK-O-CHKDGT    TO  LNK-O-CODE (WRK-END:1)
                   MOVE    01              TO  LNK-O-ERRCD
               END-IF
           END-IF
      *
           MOVE    "END"           TO  FLG-END
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��λ��������
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *
           .
       300-END-EXT.
           EXIT.
      *