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
       PROGRAM-ID.                 ORCSTANKI3HKT.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ����
      *  ����ݡ��ͥ��̾  : û���ںߣ����
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  15/08/28    NACL�ݾ�Ʀ��  ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  04.08.00    NACL-¿��Ǽ  17/04/XX  ���θ��������û�����б�
      *  04.08.00    NACL-¿��Ǽ  17/06/XX  �����ʣ���Ƚ��������б�
      *  05.00.00    NACL-��Ʀ��  18/10/10  û���ںߣ��λ�����Ƚ�꽤��
      *              NACL-��Ʀ��  18/10/11  û���ںߣ������Ƚ���ɲ�
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
           03  FLG-PTINF                   PIC 9(01).
           03  FLG-TANKI3                  PIC 9(01).
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                         PIC 9(04).
      *
      *    ���󥹥������
       01  CONSTANT-AREA.
      *
           03   CONS-TANKI3HKT-AREA.
      *      Ǣ��ʵ��������Ƚ����
             05   CONS-HKT1                PIC X(09) VALUE "160061710".
      *      ��ճ�Ū����Ƚ����
             05   CONS-HKT2                PIC X(09) VALUE "160061810".
      *      ������Ū�����ʣ���Ƚ����
             05   CONS-HKT3                PIC X(09) VALUE "160061910".
      *      ������Ū�����ʣ���Ƚ����
             05   CONS-HKT4                PIC X(09) VALUE "160062010".
      *      �ȱֳ�Ū����Ƚ����
             05   CONS-HKT5                PIC X(09) VALUE "160062110".
      *      ����ʪ��Ū����Ƚ����
             05   CONS-HKT6                PIC X(09) VALUE "160062210".
      *      ����ԥ塼�����ؿ���
             05   CONS-HKT7                PIC X(09) VALUE "170015410".
      *      �������Ǵ����û����ʼ̿����ǡ�
             05   CONS-HKT8                PIC X(09) VALUE "170025210".
      *      �������Ǵ����û����ʴ���Ū���å��������ǡ�
             05   CONS-HKT9                PIC X(09) VALUE "170025310".
      *      �������Ǵ����û����ʳ˰�ؿ��ǡ�
             05   CONS-HKT10               PIC X(09) VALUE "170025410".
      *      �������Ǵ����û����ʥ���ԥ塼�����ؿ��ǡ�
             05   CONS-HKT11               PIC X(09) VALUE "170025510".
      *      �������Ǵ����û����ʳ˰�ؿ��ǡ�
             05   CONS-HKT12               PIC X(09) VALUE "170025610".
      *      �������Ǵ����û����ʥ���ԥ塼�����ؿ��ǡ�
             05   CONS-HKT13               PIC X(09) VALUE "170025710".
      *      �������Ǵ����û����ʳ˰�ؿ��ǡ�
             05   CONS-HKT14               PIC X(09) VALUE "170035710".
      *      �������Ǵ����û����ʥ���ԥ塼�����ؿ��ǡ�
             05   CONS-HKT15               PIC X(09) VALUE "170035810".
      *
           03  CONS-TANKI3HKT-AREA-R  REDEFINES  CONS-TANKI3HKT-AREA.
             05   CONS-TANKI3HKT-TBL      OCCURS  15.
                07   CONS-TANKI3HKT-SRYCD  PIC X(09).
      *
      *    ���θ��������û�
           03   CONS-TANKI3KSN-AREA.
      *      ���θ��������û���
             05   CONS-KSN1                PIC X(09) VALUE "160170170".
      *      ���θ��������û���
             05   CONS-KSN2                PIC X(09) VALUE "160182770".
      *      ���θ��������û���
             05   CONS-KSN3                PIC X(09) VALUE "160161610".
      *      ���θ��������û���
             05   CONS-KSN4                PIC X(09) VALUE "160185770".
      *
      *    ����ΰ�
       01  WRK-AREA.
      *
      *    û���ں߼������������Ƚ����
           03  WRK-TANKI3-DAY              PIC X(01)   OCCURS  31.
      *    �����������
           03  WRK-SANTEI-YMD.
               05  WRK-SANTEI-YY           PIC 9(04).
               05  WRK-SANTEI-MM           PIC 9(02).
               05  WRK-SANTEI-DD           PIC 9(02).
      *    ������������
           03  WRK-GETSUMATSU-YMD.
               05  WRK-GETSUMATSU-YY       PIC 9(04).
               05  WRK-GETSUMATSU-MM       PIC 9(02).
               05  WRK-GETSUMATSU-DD       PIC 9(02).
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    ���ԥޥ�����
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
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
      *   �����Ѵ�����
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ������̤��������������
           COPY    "CPORCSNYUINDAY.INC".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "CPORCSTANKI3HKT.INC".
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           ORCSTANKI3HKT-AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  LNK-TANKI3HKT-HENKYAKU-AREA
           MOVE    ZERO            TO  LNK-TANKI3HKT-RC
      *
      *    �ѥ�᡼�������å�
           PERFORM 100-PRM-CHK-SEC
           IF    ( LNK-TANKI3HKT-RC       NOT =   ZERO )
               GO  TO  000-PROC-EXT
           END-IF
      *
      *    �����
           IF      LNK-TANKI3HKT-RC      =    ZERO
               PERFORM 200-MAIN-SEC
           END-IF
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    �ѥ�᡼�������å�
      *****************************************************************
       100-PRM-CHK-SEC                     SECTION.
      *
      *    ���ԥޥ���������
      *    INITIALIZE                         PTINF-REC
      *    MOVE    LNK-TANKI3HKT-HOSPNUM  TO  PTINF-HOSPNUM
      *    MOVE    LNK-TANKI3HKT-PTID     TO  PTINF-PTID
      *    PERFORM 900-PTINF-READ-SEC
      *    IF      FLG-PTINF              NOT =   ZERO
      *        MOVE    1                  TO  LNK-TANKI3HKT-RC
      *        GO  TO  100-PRM-CHK-EXT
      *    END-IF
      *
      *    ����ǯ���������å�
           IF      LNK-TANKI3HKT-SANTEIYMD  =   SPACE
               MOVE    2                  TO  LNK-TANKI3HKT-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
           .
       100-PRM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����
      *****************************************************************
       200-MAIN-SEC            SECTION.
      *
           EVALUATE    LNK-TANKI3HKT-SRYCD
      *        ����Ƚ��������оݥ����å�
               WHEN    CONS-TANKI3HKT-SRYCD(1)
               WHEN    CONS-TANKI3HKT-SRYCD(2)
               WHEN    CONS-TANKI3HKT-SRYCD(3)
               WHEN    CONS-TANKI3HKT-SRYCD(4)
               WHEN    CONS-TANKI3HKT-SRYCD(5)
               WHEN    CONS-TANKI3HKT-SRYCD(6)
      *        ����ԥ塼�����ؿ��ǥ����å�
               WHEN    CONS-TANKI3HKT-SRYCD(7)
      *      �������Ǵ����û���
               WHEN    CONS-TANKI3HKT-SRYCD(8)
               WHEN    CONS-TANKI3HKT-SRYCD(9)
               WHEN    CONS-TANKI3HKT-SRYCD(10)
               WHEN    CONS-TANKI3HKT-SRYCD(11)
      *      �������Ǵ����û���
               WHEN    CONS-TANKI3HKT-SRYCD(12)
               WHEN    CONS-TANKI3HKT-SRYCD(13)
      *      �������Ǵ����û���
               WHEN    CONS-TANKI3HKT-SRYCD(14)
               WHEN    CONS-TANKI3HKT-SRYCD(15)
                   MOVE    SPACE               TO  LNK-TANKI3HKT-KBN2
                   PERFORM 2001-MAIN-CHK-SEC
      *
      *        ����Ƚ�������ˤ������о�
               WHEN    CONS-KSN1
               WHEN    CONS-KSN2
               WHEN    CONS-KSN3
               WHEN    CONS-KSN4
                   IF      LNK-TANKI3HKT-HKTKBN    =   "1"
                       MOVE    "1"             TO  LNK-TANKI3HKT-KBN
                   ELSE
                       MOVE    SPACE           TO  LNK-TANKI3HKT-KBN
                   END-IF
                   MOVE    SPACE               TO  LNK-TANKI3HKT-KBN2
           END-EVALUATE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ����Ƚ��������оݥ����å�
      *****************************************************************
       2001-MAIN-CHK-SEC            SECTION.
      *
             MOVE    LNK-TANKI3HKT-SANTEIYMD  TO  WRK-SANTEI-YMD
      *
      *      �����������
             INITIALIZE                         STS-AREA-DAY
             INITIALIZE                         LNK-DAY7-AREA
             MOVE    "71"                   TO  LNK-DAY7-IRAI
             MOVE    LNK-TANKI3HKT-SANTEIYMD(1:6)
                                            TO  LNK-DAY7-YM
             CALL    "ORCSDAY"          USING   STS-AREA-DAY
                                                LNK-DAY7-AREA
             IF      STS-DAY-RC1            =   ZERO
                 MOVE    LNK-DAY7-KOYOMI    TO  WRK-GETSUMATSU-YMD
             END-IF
      *
      *      û���ں߼�Ѵ����������������ˤĤ��Ƽ���
             INITIALIZE                         ORCSNYUINDAY-AREA
             MOVE    LNK-TANKI3HKT-HOSPNUM  TO  LNK-NYUINDAY-HOSPNUM
             MOVE    LNK-TANKI3HKT-PTID     TO  LNK-NYUINDAY-PTID
             MOVE    LNK-TANKI3HKT-SANTEIYMD(1:6)
                                            TO  LNK-NYUINDAY-SANTEIYM
             CALL    "ORCSNYUINDAY"     USING   ORCSNYUINDAY-AREA
                                                SPA-AREA
      *
             MOVE     ZERO                  TO  FLG-TANKI3
             PERFORM     VARYING    IDX   FROM   1   BY   1
               UNTIL     IDX    >   WRK-GETSUMATSU-DD
                   IF  (LNK-NYUINDAY-KBN(IDX)          =  SPACE)  AND
                       (LNK-NYUINDAY-TANKI3-KBN(IDX)   =  "1"  OR  "2")
                      MOVE    1             TO  FLG-TANKI3
                   END-IF
                   IF    FLG-TANKI3         =   1
                      MOVE   "1"            TO  WRK-TANKI3-DAY(IDX)
                   END-IF
             END-PERFORM
      *
      *       PERFORM   VARYING  IDX   FROM  1  BY  1
      *         UNTIL   IDX   >   31
      *            DISPLAY  "IDX = " IDX
      *            DISPLAY  "LNK-NYUINDAY-KBN(IDX)        = "
      *                      LNK-NYUINDAY-KBN(IDX)
      *            DISPLAY  "LNK-NYUINDAY-TANKI-KBN(IDX)  = "
      *                      LNK-NYUINDAY-TANKI-KBN(IDX)
      *            DISPLAY  "LNK-NYUINDAY-TANKI3-KBN(IDX) = "
      *                      LNK-NYUINDAY-TANKI3-KBN(IDX)
      *            DISPLAY  "WRK-TANKI3-DAY(IDX)          = "
      *                      WRK-TANKI3-DAY(IDX)
      *       END-PERFORM
      *
             IF    WRK-TANKI3-DAY(WRK-SANTEI-DD)   =   "1"
                MOVE    "1"                 TO    LNK-TANKI3HKT-KBN
             END-IF
      *
           .
       2001-MAIN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ԥޥ������ɹ�
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM   900-DBFETCH-SEC
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
           PERFORM   900-CLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ£ӣţ̣ţãԽ���
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
      *    �ģ£ƣţԣãȽ���
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
      *    �ơ��֥륢����������
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
      *
      *****************************************************************
      *    �ơ��֥륯����������
      *****************************************************************
       900-CLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
      *
       900-CLOSE-EXT.
           EXIT.