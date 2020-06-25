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
       PROGRAM-ID.                 ORCSCNYUHEN.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ���Ź԰�����
      *  ����ݡ��ͥ��̾  : ������������Խ�����
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  08/11/06    NACL��¿��Ǽ  ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  04.05.00    NACL-¿��Ǽ  09/10/26  ¾���ౡ���б�
      *  04.07.00    NACL-¿��Ǽ  11/10/XX  Ʊ���������б�
      *****************************************************************
      *
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
      *FILE-CONTROL.
      *
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PTNYUINRRK      PIC 9(01).
      *
           03  FLG-SYORI-END       PIC 9(01).
           03  FLG-OK              PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX-D2              PIC 9(02).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-SRYYMD.
               05  WRK-SRYYM           PIC X(06).
               05  WRK-SRYDD           PIC X(02).
           03  WRK-STDD                PIC 9(02).
           03  WRK-EDDD                PIC 9(02).
           03  WRK-SYMD                PIC X(08).
           03  WRK-OLD-NYUINYMD        PIC X(08).
           03  WRK-OLD-TAIINYMD        PIC X(08).
      *
           03  WRK-OLD-RRKNUM          PIC 9(03).
      *
           03  WRK-TAI-TAIINYMD        PIC X(08).
      *
           03  WRK-TAIINYMD            PIC X(08).
           03  WRK-LASTTAIINYMD        PIC X(08).
           03  WRK-YMD.
               05  WRK-SYM             PIC 9(06).
               05  WRK-SDD             PIC 9(02).
      *
           03  WRK-SHONUM                  PIC 9(03).
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    ������������
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *   �����Ѵ�����
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *    ���Ź԰����������ѥ�᥿�ΰ�
           COPY    "CPORCSCNYUHEN.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCSCNYUHENAREA
           .
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           INITIALIZE                  ORCSCNYUHEN-OUT-AREA
      *
           IF      ORCSCNYUHEN-SRYYMD  =   SPACE
               MOVE    SPA-SYSYMD      TO   ORCSCNYUHEN-SRYYMD
           END-IF
           IF      ORCSCNYUHEN-KBN     =   SPACE
               MOVE    "1"             TO  ORCSCNYUHEN-KBN
           END-IF
      *
      *    ���������Խ�����
           PERFORM 100-MAIN-SYORI-SEC
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    ���������Խ�����
      *****************************************************************
       100-MAIN-SYORI-SEC              SECTION.
      *
      *    ��������Ƚ��
           MOVE    SPACE               TO  PTNYUINRRK-REC
           INITIALIZE                  PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-PTID            TO  PTNYUINRRK-PTID
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 910-PTNYUINRRK-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
      *    �����������ʤ��Ȥ�
           IF      FLG-PTNYUINRRK      =   1
               MOVE    ZERO                TO  ORCSCNYUHEN-RRK-NYUIN
           ELSE
               MOVE    1                   TO  ORCSCNYUHEN-RRK-NYUIN
           END-IF
      *
           MOVE    ORCSCNYUHEN-SRYYMD  TO  WRK-SRYYMD
           MOVE    ZERO                TO  FLG-SYORI-END
           MOVE    ZERO                TO  FLG-OK
           MOVE    SPACE               TO  WRK-OLD-NYUINYMD
           MOVE    SPACE               TO  WRK-OLD-TAIINYMD
           MOVE    SPACE               TO  WRK-TAI-TAIINYMD
           MOVE    ZERO                TO  WRK-OLD-RRKNUM
           PERFORM
                   UNTIL      (FLG-PTNYUINRRK      =   1 )
                          OR  (FLG-SYORI-END       =   1 )
      *      �����Ƚ��ϡ�������ʬ���оݳ�
             IF     ((SPA-NYUGAIKBN        =   "2" )
                 AND (PTNYUINRRK-JTIKBN    =   "6" ))  
      *              ¾���������оݳ�
               OR    (PTNYUINRRK-JTIKBN    =   "5" )
               CONTINUE
             ELSE
               EVALUATE    ORCSCNYUHEN-KBN
      *            �������դǤ�Ƚ��
                   WHEN    "1"
                       PERFORM 1001-SRYYMD-SYORI-SEC
      *            ���ŷ�Ǥ�Ƚ��
                   WHEN    "2"
                       PERFORM 1002-SRYYM-SYORI-SEC
               END-EVALUATE
      *        �ǽ��ౡ��
               IF     (WRK-LASTTAIINYMD        =   SPACE      )  OR
                      (PTNYUINRRK-TAIINYMD     >   WRK-LASTTAIINYMD)
                   MOVE    PTNYUINRRK-TAIINYMD     TO  WRK-LASTTAIINYMD
               END-IF
      *
      *        �ౡ���������ǽ�λ�Ȥ���
               IF     (PTNYUINRRK-TAIINYMD(1:6)    <   WRK-SRYYMD(1:6))
                   MOVE    1                   TO  FLG-SYORI-END
               END-IF
               MOVE    PTNYUINRRK-NYUINYMD TO  WRK-OLD-NYUINYMD
               MOVE    PTNYUINRRK-TAIINYMD TO  WRK-OLD-TAIINYMD
               MOVE    PTNYUINRRK-RRKNUM   TO  WRK-OLD-RRKNUM
           END-IF
      *
      *       ¾���ౡ���Խ�
               IF      (SPA-NYUGAIKBN       =   "2" )
                AND    (PTNYUINRRK-JTIKBN   =   "5" )
                   IF     (PTNYUINRRK-TAIINYMD <   WRK-SRYYMD )
                     AND ((WRK-TAI-TAIINYMD    =   SPACE     )   OR
                          (WRK-TAI-TAIINYMD    <   PTNYUINRRK-TAIINYMD))
                       MOVE    PTNYUINRRK-TAIINYMD  TO  WRK-TAI-TAIINYMD
                   END-IF
               END-IF
      *
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 910-PTNYUINRRK-READ-SEC
           END-PERFORM
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    �ǽ��ౡ��������
           MOVE    WRK-TAIINYMD        TO  ORCSCNYUHEN-MAE-TAIINYMD
           MOVE    WRK-LASTTAIINYMD    TO  ORCSCNYUHEN-LAST-TAIINYMD
      *    ¾�屡�ౡ��
           MOVE    WRK-TAI-TAIINYMD    TO  ORCSCNYUHEN-TAI-TAIINYMD
      *
      *    �������������򤬷�³�λ�
           MOVE    SPACE               TO  ORCSCNYUHEN-STR-NYUINYMD
           IF      WRK-SHONUM      NOT =   ZERO
               PERFORM 1003-STR-NYUINYMD-HEN-SEC
           END-IF
           .
       100-MAIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *   �������դǤ�Ƚ�����
      *****************************************************************
       1001-SRYYMD-SYORI-SEC            SECTION.
      *
      *    �����ౡ�����������λ����оݤȤ���
           IF    ((PTNYUINRRK-NYUINYMD NOT =   WRK-OLD-NYUINYMD ) AND
                  (PTNYUINRRK-TAIINYMD NOT =   WRK-OLD-TAIINYMD ))
      *H26.12     Ϣ������о�
             OR  ( PTNYUINRRK-RRKNUM   NOT =   WRK-OLD-RRKNUM   )
      *
               IF      PTNYUINRRK-TAIINYMD     =   WRK-OLD-NYUINYMD
                   IF      (ORCSCNYUHEN-KBN2   NOT =   "1"     )
                       IF    ( FLG-OK          =   1             )
                       AND   ( PTNYUINRRK-TAIINYMD =   WRK-SRYYMD)
                           MOVE    1           TO  ORCSCNYUHEN-DOUNYUKBN
                       END-IF
                       MOVE    ZERO            TO  FLG-OK
                   END-IF
               END-IF
      *!!!     Ʊ���������������о�
               IF     (PTNYUINRRK-TAIINYMD     =   WRK-OLD-NYUINYMD )
                 AND  (WRK-OLD-NYUINYMD        =   ORCSCNYUHEN-NYUINYMD)
      *            Ʊ����������
                   MOVE    PTNYUINRRK-TAIINYMD    TO  WRK-YMD
                   MOVE    WRK-SDD         TO  ORCSCNYUHEN-DOUNYUIN 
               END-IF
           END-IF
      *
      *
      *    ����������ž����
           IF      PTNYUINRRK-TENNYUYMD    <=  WRK-SRYYMD
      *        ž����
               IF      PTNYUINRRK-TENSTUYMD    >=  WRK-SRYYMD
      *            ����������
                   IF    ((PTNYUINRRK-TAIINYMD     =   WRK-SRYYMD)
                     OR   (PTNYUINRRK-TENSTUYMD    >   WRK-SRYYMD))
                     AND  (FLG-OK                  =   ZERO      )
                       PERFORM 1002-PTNYUINRRK-HEN-SEC
                       MOVE    1               TO  FLG-OK
      *                Ʊ����������
                       MOVE    ZERO            TO  ORCSCNYUHEN-DOUNYUIN
                   END-IF
               END-IF
           END-IF
      *    �������֤ǣ���
           IF     (PTNYUINRRK-NYUINYMD NOT =   WRK-OLD-NYUINYMD ) AND
      ************(PTNYUINRRK-NYUINYMD     <=  WRK-SRYYMD       )
                  (PTNYUINRRK-NYUINYMD(1:6)    <=  WRK-SRYYM    )
               IF     (PTNYUINRRK-NYUINYMD(1:6)
                                           <=  WRK-SRYYM  ) AND
                      (PTNYUINRRK-TAIINYMD (1:6)
                                           >=  WRK-SRYYM  )
      *            �����������������
                   PERFORM 1003-NYUINDAY-HEN-SEC
               END-IF
      *        �����������κǽ��ౡ��
               IF     (PTNYUINRRK-TAIINYMD     <=  WRK-SRYYMD ) AND
                      (WRK-TAIINYMD            =   SPACE      )
                   MOVE    PTNYUINRRK-TAIINYMD     TO  WRK-TAIINYMD
               END-IF
           END-IF
           .
       1001-SRYYMD-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *   ���ŷ�Ǥ�Ƚ�����
      *****************************************************************
       1002-SRYYM-SYORI-SEC            SECTION.
      *
      *    ����������ž����
           IF      PTNYUINRRK-TENNYUYMD(1:6)  <=  WRK-SRYYM
      *        ����ž����
               IF     (PTNYUINRRK-TENSTUYMD(1:6)   >=  WRK-SRYYM )
                 AND  (FLG-OK                  =   ZERO      )
                   PERFORM 1002-PTNYUINRRK-HEN-SEC
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-IF
      *    �����������������
      *    �������֤ǣ���
           IF     (PTNYUINRRK-NYUINYMD     NOT =   WRK-OLD-NYUINYMD) AND
                  (PTNYUINRRK-NYUINYMD(1:6)    <=  WRK-SRYYM   )
               IF     (PTNYUINRRK-NYUINYMD(1:6)
                                               <=  WRK-SRYYM ) AND
                      (PTNYUINRRK-TAIINYMD(1:6)
                                               >=  WRK-SRYYM )
                   PERFORM 1003-NYUINDAY-HEN-SEC
               END-IF
      *        �����������κǽ��ౡ��
               IF     (WRK-TAIINYMD            =   SPACE      )  OR
                      (WRK-TAIINYMD            <   PTNYUINRRK-TAIINYMD )
                   MOVE    PTNYUINRRK-TAIINYMD     TO  WRK-TAIINYMD
               END-IF
           END-IF
           .
       1002-SRYYM-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *   ���������Խ�����
      *****************************************************************
       1002-PTNYUINRRK-HEN-SEC            SECTION.
      *
      *    �������ౡ��
           MOVE    PTNYUINRRK-NYUINYMD     TO  ORCSCNYUHEN-NYUINYMD
           MOVE    PTNYUINRRK-TAIINYMD     TO  ORCSCNYUHEN-TAIINYMD
      *    ������
           MOVE    PTNYUINRRK-NYUINKA      TO  ORCSCNYUHEN-NYUINKA
      *    �ݸ��ȹ礻�ֹ�
           MOVE    PTNYUINRRK-HKNCOMBINUM  TO  ORCSCNYUHEN-HKNCOMBI
      *    ô���壱�ʼ缣���
           MOVE    PTNYUINRRK-DRCD1        TO  ORCSCNYUHEN-DRCD
      *
           IF     (PTNYUINRRK-TAIINYMD     =   WRK-SRYYMD)
               OR (PTNYUINRRK-NYUINYMD     =   WRK-SRYYMD)
      *           �������ౡ
               MOVE    2                   TO  ORCSCNYUHEN-NYUIN-KBN
           ELSE
      *        ������
               MOVE    1                   TO  ORCSCNYUHEN-NYUIN-KBN
           END-IF
      *ver.4.7
      *    ��³
           IF      PTNYUINRRK-SHOKAIKBN    =   "2"
      *        ��³
               MOVE    PTNYUINRRK-SHONUM   TO  WRK-SHONUM
           ELSE
               MOVE    ZERO                TO  WRK-SHONUM
           END-IF
           IF      PTNYUINRRK-TAIINYMD =   WRK-OLD-NYUINYMD
      *        �ౡ������������������
               MOVE    1               TO  ORCSCNYUHEN-DOUTAIKBN
           END-IF
           .
       1002-PTNYUINRRK-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   �����������Խ�����
      *****************************************************************
       1003-NYUINDAY-HEN-SEC            SECTION.
      *
           IF      PTNYUINRRK-TAIINYMD(1:6)
                                       =   WRK-SRYYMD(1:6)
               MOVE    PTNYUINRRK-TAIINYMD(7:2)
                                           TO  WRK-EDDD
           ELSE
               MOVE    31                  TO  WRK-EDDD
           END-IF
           IF      PTNYUINRRK-NYUINYMD(1:6)
                                       =   WRK-SRYYMD(1:6)
               MOVE    PTNYUINRRK-NYUINYMD(7:2)
                                           TO  WRK-STDD
           ELSE
               MOVE    01                  TO  WRK-STDD
           END-IF
      *    ������
           PERFORM VARYING IDX-D2      FROM    WRK-STDD BY  1
                   UNTIL   IDX-D2      >   WRK-EDDD
               IF    ((PTNYUINRRK-TENNYUYMD(1:6)
                                           =   WRK-SRYYM )  AND
                      (IDX-D2              =   WRK-STDD  ))
               OR    ((PTNYUINRRK-TENSTUYMD(1:6)
                                           =   WRK-SRYYM )  AND
                      (IDX-D2              =   WRK-EDDD  ))
      *            ���ౡ��
                   MOVE    2           TO  ORCSCNYUHEN-NYUDAY (IDX-D2)
               ELSE
      *            ������
                   MOVE    1           TO  ORCSCNYUHEN-NYUDAY (IDX-D2)
               END-IF
           END-PERFORM
           .
       1003-NYUINDAY-HEN-EXT.
           EXIT.
      *****************************************************************
      *    �������򤬷�³�����Խ�����
      *****************************************************************
       1003-STR-NYUINYMD-HEN-SEC             SECTION.
      *
      *    ��³�λ����������������
           INITIALIZE                  PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-PTID            TO  PTNYUINRRK-PTID
           MOVE    ORCSCNYUHEN-NYUINYMD    TO  PTNYUINRRK-NYUINYMD
           MOVE    WRK-SHONUM          TO  PTNYUINRRK-SHONUM
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key13"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key13"             TO  MCP-PATHNAME
               PERFORM 910-PTNYUINRRK-READ-SEC
            ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
           IF      FLG-PTNYUINRRK      =   ZERO
               MOVE    PTNYUINRRK-NYUINYMD
                                           TO  ORCSCNYUHEN-STR-NYUINYMD
           END-IF
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key13"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       1003-STR-NYUINYMD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    ���������Խ�����
      *****************************************************************
       520-SEIWA-HEN-SEC             SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE
               MOVE    SPACE               TO  WRK-HGO
               MOVE    WRK-SYMD(3:2)       TO  WRK-HYY
               MOVE    WRK-SYMD(5:2)       TO  WRK-HMM
               MOVE    WRK-SYMD(7:2)       TO  WRK-HDD
               MOVE    WRK-HENYMD          TO  WRK-HENYMDG
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF
           .
       520-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��������ޥ������ɹ�
      *****************************************************************
       910-PTNYUINRRK-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PTNYUINRRK-REC
               MOVE    ZERO                TO  FLG-PTNYUINRRK
           ELSE
               INITIALIZE                      PTNYUINRRK-REC
               MOVE    1                   TO  FLG-PTNYUINRRK
           END-IF
           .
       910-PTNYUINRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ơ��֥븡������
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    �ģ��ɤ߹��߽���
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
       990-DBCLOSE-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       990-DBCLOSE-EXT.
           EXIT.
      *