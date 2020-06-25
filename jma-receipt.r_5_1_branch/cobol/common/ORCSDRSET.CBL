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
       PROGRAM-ID.                 ORCSDRSET.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ����
      *  ����ݡ��ͥ��̾  : �ɥ������Խ��������å�����
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  03/02/24    NACL��¿��Ǽ  ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL��¿��Ǽ 04/06/25  ���Ž�ǣ���̾�ʾ����̤�
      *  01.00.02    NACL��¿��Ǽ 06/08/16  ����ô���������ɲ�
      *  03.05.00    NACL-¿��Ǽ  07/04/XX  ���롼�׿����б�
      *  04.03.00    NACL-¿��Ǽ  08/06/XX  ���磹�����б�
      *  04.04.00    NACL-¿��Ǽ  09/03/05  ����ô�����б��ʤ��ɲ�
      *  04.08.00    NACL-¿��Ǽ  16/01/XX  ���Ų�ô���塦��ɽ���б��ɲ�
      *****************************************************************
      *
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *
           03  FLG-CHK             PIC 9(01).
           03  FLG-CHK2            PIC 9(01).
           03  FLG-CHK3            PIC 9(01).
           03  FLG-ARI             PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-OVER            PIC 9(01).
           03  FLG-TANTOU          PIC 9(01).
      *
           03  FLG-LSDOCTOR            PIC 9(01).
           03  FLG-KADR-ARI            PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDX-D               PIC 9(04).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-DRCD-TBL-G.
               05  WRK-DRCD-TBL        OCCURS   99.
      *            �ɥ�����������
                   07  WRK-DRCD             PIC  X(04).
                   07  WRK-DRCD-H           PIC  X(01).
                   07  WRK-DRCDMEI          PIC  X(24).
               05  WRK-DRCD-IDX             PIC  9(04).
      *
           03  WRK-KEY-DRCD            PIC X(05).
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �����ƥ�����ޥ���
           COPY    "CPSYSKANRI.INC".
      *
      *    �ɥ�����
           COPY    "CPSK1010.INC".
      *    ô�������� 
           COPY  "CPSK1011.INC".
      *
      *    ���Ų��̥ɥ�����
       01  LIST-DOCTOR-REC.
           COPY    "CPLIST_DOCTOR.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *   �����Ѵ�����
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    �����Ѵ��ΰ�
           COPY    "CPORCSNUM.INC".
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
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
      *    �ɥ��������å��ΰ�
           COPY    "CPORCSDRSET.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCSDRSETAREA
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
           INITIALIZE                  ORCSDR-ERRCD
      *
      *
           EVALUATE    ORCSDR-KBN
      *        �ɥ������ơ��֥��Խ�
               WHEN    "1"
                   PERFORM 100-DR-HENSYU-SEC
      *            �ɥ����������ɤ���������ɥ������Խ�
                   IF      ORCSDR-IN-DRCD  NOT =   SPACE
                       PERFORM 200-DR-CHECK-SEC
                   END-IF
      *        �ɥ����������å�
               WHEN    "2"
                   PERFORM 200-DR-CHECK-SEC
      *        �ɥ����������å��ʽ��ɽ���ѡ�
               WHEN    "3"
                   PERFORM 300-DR-CHECK03-SEC
      *        �ɥ������ơ��֥��Խ�
               WHEN    "4"
                   PERFORM 100-DR-HENSYU-SEC
                   IF      FLG-TANTOU          =   1
      *                �ɥ������ɲ��Խ�
                       PERFORM 1003-SYS1010-ADDHEN-SEC
                   END-IF
      *            �ɥ����������ɤ���������ɥ������Խ�
                   IF      ORCSDR-IN-DRCD  NOT =   SPACE
                       PERFORM 200-DR-CHECK-SEC
                   END-IF
           END-EVALUATE
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    �ɥ������ơ��֥��Խ�����
      *****************************************************************
       100-DR-HENSYU-SEC            SECTION.
      *
           INITIALIZE                  ORCSDR-OUT-AREA
           INITIALIZE                  WRK-DRCD-TBL-G
           MOVE    ZERO                TO  IDX3
           MOVE    ZERO                TO  FLG-TANTOU
      *    ����ô����
           IF      ORCSDR-KBN2         =   "1"
               CONTINUE
           ELSE
               IF      ORCSDR-SRYKA    NOT =   SPACE
      *H27.12
      *            ���Ų��̤�Ƚ��
                   PERFORM 1003-LSDOCTOR-HEN-SEC
                   IF      FLG-KADR-ARI        =   ZERO
      *                ����ô����Ƚ��
                       PERFORM 1000-SYS1011-CHK-SEC
                   END-IF
               END-IF
           END-IF
      *
      *H27.12
           IF      FLG-KADR-ARI        =   1
               GO  TO      100-HENSYU-SYORI-EXT
           END-IF
      *
           IF      FLG-TANTOU          =   ZERO
               PERFORM 1001-SYS1010-HEN-SEC
           ELSE
               PERFORM 1002-SYS1011-HEN-SEC
           END-IF
      *
           .
       100-HENSYU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ɥ������ơ��֥��Խ�����
      *****************************************************************
       1001-SYS1010-HEN-SEC            SECTION.
      *
      *    ��������ʥɥ�������
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                  SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    ORCSDR-SYSYMD       TO  SYS-1010-STYUKYMD
           MOVE    ORCSDR-SYSYMD       TO  SYS-1010-EDYUKYMD
      *
      *    �����ƥ��������
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-1010-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  FLG-OVER
           PERFORM
                   UNTIL       (FLG-SYSKANRI   =   1    )
               MOVE    MCPDATA-REC             TO  SYS-1010-REC
               IF     (SYS-1010-KBNCD(1:1)     =   "1"  )
      *H27.12
      *            ɽ��ʬ�Τ�
                 AND  (SYS-1010-HYOJIFLG   NOT =   "1"  )
      *
                   MOVE    ZERO                TO  FLG-CHK
      *            ���̤λ����±��Ρ�
                   IF     (ORCSDR-SRYKA    NOT =   SPACE )  AND
                          (SPA-HOSPSBT         =   1     )
                       PERFORM 1001-SEMONKACD-CHK-SEC
                   ELSE
                       IF      ORCSDR-SRYKA    NOT =   SPACE
                           PERFORM 1001-SEMONKACD-CHK-SEC
                           IF      FLG-CHK         =   1
                               PERFORM 1002-DRCD-TBL-SEC
                           END-IF
                       END-IF
                       MOVE    1               TO  FLG-CHK
                   END-IF
                   IF      FLG-CHK             =   1
                       IF      IDX                 >=  99
                           MOVE    1               TO  FLG-OVER
                       ELSE
                           ADD     1                   TO  IDX
                           MOVE    SYS-1010-KBNCD(2:4)
                                                   TO  ORCSDR-DRCD
                                                                (IDX)
                           MOVE    SYS-1010-NAME   TO  ORCSDR-DRCDMEI
                                                                (IDX)
                           IF     (FLG-CHK2        =   1  )  AND
                                  (ORCSDR-DRCD-IDX =   ZERO )
                               MOVE    IDX         TO  ORCSDR-DRCD-IDX
                           END-IF
                       END-IF
                   END-IF
               END-IF
      *
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    IDX                 TO  ORCSDR-DRCD-MAX
      *
           IF     (FLG-OVER            =   1  )  AND
                  (IDX3                >   ZERO)
               MOVE    WRK-DRCD-TBL-G      TO  ORCSDR-DRCD-TBL-G
               MOVE    IDX3                TO  ORCSDR-DRCD-MAX
               MOVE    WRK-DRCD-IDX        TO  ORCSDR-DRCD-IDX
           END-IF
      *
           .
       1001-SYS1010-HEN-EXT.
           EXIT.
      *****************************************************************
      *    ����ô�������
      *****************************************************************
       1000-SYS1011-CHK-SEC            SECTION.
      *
           MOVE    SPACE               TO  SYS-1011-REC
           INITIALIZE                  SYS-1011-REC
           MOVE    "1011"              TO  SYS-1011-KANRICD
           MOVE    ORCSDR-SRYKA        TO  SYS-1011-KBNCD
           MOVE    ORCSDR-SYSYMD       TO  SYS-1011-STYUKYMD
           MOVE    ORCSDR-SYSYMD       TO  SYS-1011-EDYUKYMD
      *    �����ƥ��������
           MOVE    SPA-HOSPNUM         TO  SYS-1011-HOSPNUM
           MOVE    SYS-1011-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key10"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI         =   ZERO
                   MOVE    MCPDATA-REC           TO  SYS-1011-REC
                   IF      SYS-1011-DRCD(01)   NOT =   SPACE
                       MOVE    1               TO  FLG-TANTOU
                   END-IF
               END-IF
           ELSE
               INITIALIZE                  SYS-1010-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       1000-SYS1011-CHK-EXT.
           EXIT.
      *****************************************************************
      *    �ɥ����������ɸ�������
      *****************************************************************
       1002-SYS1011-HEN-SEC            SECTION.
      *
           MOVE    ZERO                TO  IDX
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL       IDX2    >   20
               IF      SYS-1011-DRCD(IDX2) NOT =   SPACE
                   MOVE    SYS-1011-DRCD(IDX2) TO  WRK-KEY-DRCD
                   PERFORM 10001-DRCDNAME-HEN-SEC
                   IF      FLG-SYSKANRI        =   ZERO
                       ADD     1               TO  IDX
                       MOVE    SYS-1011-DRCD(IDX2)(2:4)
                                               TO  ORCSDR-DRCD (IDX)
                       MOVE    SYS-1010-NAME       TO  ORCSDR-DRCDMEI
                                                              (IDX)
                   END-IF
               END-IF
           END-PERFORM
           MOVE    IDX                 TO  ORCSDR-DRCD-MAX
           MOVE    1                   TO  ORCSDR-DRCD-IDX
           .
       1002-SYS1011-HEN-EXT.
           EXIT.
      *****************************************************************
      *    �ɥ����������ɸ�������
      *****************************************************************
       10001-DRCDNAME-HEN-SEC            SECTION.
      *
      *    ��������ʥɥ�������
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                  SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    WRK-KEY-DRCD        TO  SYS-1010-KBNCD(1:5)
           MOVE    ORCSDR-SYSYMD       TO  SYS-1010-STYUKYMD
           MOVE    ORCSDR-SYSYMD       TO  SYS-1010-EDYUKYMD
      *
      *    �����ƥ��������
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-1010-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1010-REC
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       10001-DRCDNAME-HEN-EXT.
           EXIT.
      *H27.11
      *****************************************************************
      *    ���Ų�ô�������Խ�����
      *****************************************************************
       1003-LSDOCTOR-HEN-SEC               SECTION.
      *
           MOVE    ZERO                TO  FLG-KADR-ARI
           MOVE    ZERO                TO  IDX
      *
           INITIALIZE                  LIST-DOCTOR-REC
           MOVE    SPA-HOSPNUM         TO  LSDOCTOR-HOSPNUM
           MOVE    ORCSDR-SRYKA        TO  LSDOCTOR-SRYKA
      *
           MOVE    LIST-DOCTOR-REC     TO  MCPDATA-REC
           MOVE    "tbl_list_doctor"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_list_doctor"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-LSDOCTOR-READ-SEC
           ELSE
               INITIALIZE                  LIST-DOCTOR-REC
               MOVE    1               TO  FLG-LSDOCTOR
           END-IF
           PERFORM
               UNTIL  (FLG-LSDOCTOR    =   1  )
                   OR (IDX             >=  99 )
      *
               MOVE    LSDOCTOR-DRCD        TO  WRK-KEY-DRCD
               PERFORM 10001-DRCDNAME-HEN-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   ADD     1                   TO  IDX
                   MOVE    LSDOCTOR-DRCD(2:4)  TO  ORCSDR-DRCD (IDX)
                   MOVE    SYS-1010-NAME       TO  ORCSDR-DRCDMEI
                                                              (IDX)
                   MOVE    1                   TO  FLG-KADR-ARI
               END-IF
      *
               MOVE    "tbl_list_doctor"   TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-LSDOCTOR-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_list_doctor"    TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    IDX                 TO  ORCSDR-DRCD-MAX
           MOVE    1                   TO  ORCSDR-DRCD-IDX
           .
       1003-LSDOCTOR-HEN-EXT.
           EXIT.
      *****************************************************************
      *    ���Ų�ô������Ͽ�����å�����
      *****************************************************************
       10031-LSDOCTOR-CHK-SEC               SECTION.
      *
      *
           INITIALIZE                  LIST-DOCTOR-REC
           MOVE    SPA-HOSPNUM         TO  LSDOCTOR-HOSPNUM
           MOVE    ORCSDR-SRYKA        TO  LSDOCTOR-SRYKA
      *
           MOVE    LIST-DOCTOR-REC     TO  MCPDATA-REC
           MOVE    "tbl_list_doctor"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_list_doctor"   TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-LSDOCTOR-READ-SEC
           ELSE
               INITIALIZE                  LIST-DOCTOR-REC
               MOVE    1               TO  FLG-LSDOCTOR
           END-IF
           IF      FLG-LSDOCTOR         =   ZERO
      *        ���Ų��̤���
               MOVE    1                   TO  FLG-KADR-ARI
           ELSE
               MOVE    ZERO                TO  FLG-KADR-ARI
           END-IF
      *
           MOVE    "tbl_list_doctor"    TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       10031-LSDOCTOR-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���̤��Խ��ʿ��Ž�˽���
      *****************************************************************
       1002-DRCD-TBL-SEC            SECTION.
      *
           IF      IDX3                >=  99
               GO      TO      1002-DRCD-TBL-EXT
           END-IF
           ADD     1                   TO  IDX3
           MOVE    SYS-1010-KBNCD(2:4) TO  WRK-DRCD    (IDX3)
           MOVE    SYS-1010-NAME       TO  WRK-DRCDMEI (IDX3)
           IF     (FLG-CHK2            =   1  )  AND
                  (WRK-DRCD-IDX        =   ZERO )
               MOVE    IDX3            TO  WRK-DRCD-IDX
           END-IF
      *
           .
       1002-DRCD-TBL-EXT.
           EXIT.
      *****************************************************************
      *    ���̤�����ȥ����å�����
      *****************************************************************
       1001-SEMONKACD-CHK-SEC            SECTION.
      *
      *    ����ʥ�����
           MOVE    ZERO                TO  FLG-OK
           MOVE    ZERO                TO  FLG-CHK
           MOVE    ZERO                TO  FLG-CHK2
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2            >   5   )   OR
                              (FLG-CHK     NOT =   ZERO )
               IF      SYS-1010-SEMONKACD (IDX2)   NOT =   SPACE
                   MOVE    1               TO  FLG-OK
                   IF      SYS-1010-SEMONKACD (IDX2)
                                           =   ORCSDR-SRYKA
                       MOVE    1               TO  FLG-CHK
                       MOVE    1               TO  FLG-CHK2
                   END-IF
               END-IF
           END-PERFORM
      *
      *    ����ʤ��ʤ����������оݤȤ���
           IF      FLG-OK              =   ZERO
               MOVE    1               TO  FLG-CHK
           END-IF
           .
       1001-SEMONKACD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    �ɥ������ơ��֥��ɲ��Խ�����
      *****************************************************************
       1003-SYS1010-ADDHEN-SEC            SECTION.
      *
      *    ��������ʥɥ�������
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                  SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    ORCSDR-SYSYMD       TO  SYS-1010-STYUKYMD
           MOVE    ORCSDR-SYSYMD       TO  SYS-1010-EDYUKYMD
      *
      *    �����ƥ��������
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-1010-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    ORCSDR-DRCD-MAX     TO  IDX
           MOVE    ZERO                TO  FLG-OVER
           PERFORM
                   UNTIL       (FLG-SYSKANRI   =   1    )
               MOVE    MCPDATA-REC             TO  SYS-1010-REC
               IF      SYS-1010-KBNCD(1:1)     =   "1"
                   MOVE    ZERO                TO  FLG-CHK
                   MOVE    ZERO                TO  FLG-CHK3
                   PERFORM VARYING     IDX-D   FROM    1   BY  1
                           UNTIL      (IDX-D   >   ORCSDR-DRCD-MAX)
                                  OR  (FLG-CHK3    =   1     )
                       IF      SYS-1010-KBNCD(2:4)
                                               =   ORCSDR-DRCD (IDX-D)
                           MOVE    1           TO  FLG-CHK3
                       END-IF
                   END-PERFORM
                   IF      FLG-CHK3            =   ZERO
                       IF      ORCSDR-SRYKA    NOT =   SPACE
                           PERFORM 1001-SEMONKACD-CHK-SEC
                       END-IF
                   END-IF
                   IF      FLG-CHK             =   1
                       IF      IDX                 >=  99
                           MOVE    1               TO  FLG-OVER
                       ELSE
                           ADD     1                   TO  IDX
                           MOVE    SYS-1010-KBNCD(2:4)
                                                   TO  ORCSDR-DRCD
                                                                (IDX)
                           MOVE    SYS-1010-NAME   TO  ORCSDR-DRCDMEI
                                                                (IDX)
                       END-IF
                   END-IF
               END-IF
      *
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    IDX                 TO  ORCSDR-DRCD-MAX
      *
           .
       1003-SYS1010-ADDHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ɥ����������å�����
      *****************************************************************
       200-DR-CHECK-SEC            SECTION.
      *
           IF      ORCSDR-IN-DRCD      =   SPACE
               MOVE    SPACE           TO  ORCSDR-IN-DRCD-G
               GO      TO      200-DR-CHECK-EXT
           END-IF
      *    �������Ѵ����ơ��֥�����å�
           PERFORM 2001-DRCD-TBLCHK-SEC
           IF     (ORCSDR-ERRCD    NOT =   ZERO  )  OR
                  (FLG-OK              =   1     )
               GO      TO      200-DR-CHECK-EXT
           END-IF
      *
      *    �ꥹ�Ȥˤʤ���
      *    ��������ʥɥ�������
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                  SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    "1"                 TO  SYS-1010-KBNCD(1:1)
           MOVE    ORCSDR-IN-DRCD      TO  SYS-1010-KBNCD(2:4)
           MOVE    ORCSDR-SYSYMD       TO  SYS-1010-STYUKYMD
           MOVE    ORCSDR-SYSYMD       TO  SYS-1010-EDYUKYMD
      *
      *    �����ƥ��������
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-1010-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1010-REC
               MOVE    SPACE               TO  ORCSDR-IN-DRCD-H
               MOVE    SYS-1010-NAME       TO  ORCSDR-OUT-DRCDMEI
      *        �Ǹ�ιԤ��Խ�����
               IF      ORCSDR-DRCD-MAX     =   99
                   MOVE    99              TO  ORCSDR-DRCD-MAX
               ELSE
                   ADD     1               TO  ORCSDR-DRCD-MAX
               END-IF
               MOVE    ORCSDR-IN-DRCD-G    TO  ORCSDR-DRCD-TBL
                                                      (ORCSDR-DRCD-MAX)
           ELSE
               MOVE    1                   TO  ORCSDR-ERRCD
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       200-DR-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    �ꥹ�����ƥ����å������ʶ��̡�
      *****************************************************************
       2001-DRCD-TBLCHK-SEC         SECTION.
      *
      *    �������Ѵ�
           INITIALIZE                      ORCSNUMAREA
           MOVE    ORCSDR-IN-DRCD      TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-SEISUU         >   4     )   OR
                  (SNUM-SYOKBN         =   "1"   )   OR
                  (SNUM-MINKBN         =   "1"   )
               MOVE    2                   TO  ORCSDR-ERRCD
               GO      TO      2001-DRCD-TBLCHK-EXT
           ELSE
               MOVE    SNUM-OUTEDT(12:4)   TO  ORCSDR-IN-DRCD
           END-IF
           MOVE    SPACE               TO  ORCSDR-IN-DRCD-H
           MOVE    SPACE               TO  ORCSDR-OUT-DRCDMEI
      *
      *    �����ɥ����å�
           MOVE    ZERO                TO  ORCSDR-DRCD-IDX
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING IDX         FROM    1   BY  1
                   UNTIL   IDX         >   ORCSDR-DRCD-MAX
               IF      ORCSDR-DRCD (IDX)   =   ORCSDR-IN-DRCD
                   MOVE    ORCSDR-DRCD-TBL (IDX)
                                           TO  ORCSDR-IN-DRCD-G
                   MOVE    IDX             TO  ORCSDR-DRCD-IDX
                   MOVE    ORCSDR-DRCD-MAX TO  IDX
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           .
       2001-DRCD-TBLCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ɥ����������å������ʳ�����ɽ���ѡ�
      *****************************************************************
       300-DR-CHECK03-SEC            SECTION.
      *
           IF      ORCSDR-IN-DRCD      =   SPACE
               MOVE    SPACE           TO  ORCSDR-IN-DRCD-G
           END-IF
           MOVE    ZERO                TO  FLG-TANTOU
           IF      ORCSDR-DRCD-MAX     =    ZERO
      *        �ɥ������ơ��֥��Խ�
               PERFORM 100-DR-HENSYU-SEC
           ELSE
      *        ����ô����
      *        IF      ORCSDR-SRYKA    NOT =   SPACE
      *            PERFORM 1000-SYS1011-CHK-SEC
      *        END-IF
      *H27.12
               IF      ORCSDR-SRYKA    NOT =   SPACE
      *            ���Ų��̤�Ƚ��
                   PERFORM 10031-LSDOCTOR-CHK-SEC
                   IF      FLG-KADR-ARI        =   ZERO
      *                ����ô����Ƚ��
                       PERFORM 1000-SYS1011-CHK-SEC
                   END-IF
               END-IF
           END-IF
           IF      FLG-TANTOU          =   ZERO
             AND   FLG-KADR-ARI        =   ZERO
      *        ����ô����̵��
               IF      ORCSDR-IN-DRCD      =   SPACE
                   IF      ORCSDR-DRCD-IDX     =   ZERO
                       MOVE    1               TO  ORCSDR-DRCD-IDX
                   END-IF
                   MOVE    ORCSDR-DRCD-TBL (ORCSDR-DRCD-IDX)
                                           TO  ORCSDR-IN-DRCD-G
               ELSE
      *            �ɥ����������å�
                   PERFORM 200-DR-CHECK-SEC
               END-IF
           ELSE
      *        �ꥹ�Ȥˤʤ���硢�����ܤ����Ȥ���
               PERFORM 2001-DRCD-TBLCHK-SEC
               IF     (ORCSDR-ERRCD        =   ZERO  ) AND
                      (FLG-OK              =   ZERO  )
                   MOVE    ORCSDR-DRCD-TBL (01)
                                           TO  ORCSDR-IN-DRCD-G
                   MOVE    01              TO  ORCSDR-DRCD-IDX
               END-IF
           END-IF
           .
       300-DR-CHECK03-EXT.
           EXIT.
      *****************************************************************
      *    �����ƥ�����ޥ����ɹ�
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SYSKANRI
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ô����ޥ����ɤ߹��߽���
      *****************************************************************
       900-LSDOCTOR-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  LIST-DOCTOR-REC
               MOVE    ZERO            TO  FLG-LSDOCTOR
           ELSE
               INITIALIZE                  LIST-DOCTOR-REC
               MOVE    1               TO  FLG-LSDOCTOR
           END-IF
      *
           .
       900-LSDOCTOR-READ-EXT.
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
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
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