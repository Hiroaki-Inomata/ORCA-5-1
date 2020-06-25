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
       IDENTIFICATION                            DIVISION.
       PROGRAM-ID.                               ORCGZS02.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �����ޥ�������
      *  ����ݡ��ͥ��̾  : �����ޥ����������ղ����ƹ����ʣڣӣ�����
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  04/03/02    NACL��¿��Ǽ  ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL-¿��Ǽ  05/12/09  MONFUNC�б�
      *  03.04.00    NACL-¿��Ǽ  06/12/05  ��²���ɲá��ѥ�᥿�ѹ�¾
      *  03.05.00    NACL-¿��Ǽ  07/05/07  ���롼�׿����б�
      *  04.04.00    NACL-¿��Ǽ  09/01/05  ���󸡺�����ʬ�ɲ�
      *****************************************************************
      *
       ENVIRONMENT                               DIVISION.
       CONFIGURATION                             SECTION.
       INPUT-OUTPUT                              SECTION.
       FILE-CONTROL.
      *
       DATA                                      DIVISION.
       FILE                                      SECTION.
      *
       WORKING-STORAGE                           SECTION.
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
      *    �����ޥ��������Ѷ��̥ѥ�᥿
           COPY    "Z01COMMON-SPA".
      *
      *    �����̣ӣУ����ꥢ
           COPY    "Z01SCR-SPA".
      *
      *    �ե饰���ꥢ
       01  FLG-AREA.
           03  FLG-END                           PIC 9(01).
           03  FLG-TENSUPLUS                     PIC 9(01).
      *
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                               PIC 9(02).
           03  IDX1                              PIC 9(02).
           03  IDX2                              PIC 9(02).
           03  IDX-H                             PIC 9(04).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-CDSYU               PIC X(01).
           03  WRK-KETA                PIC 9(04).
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �����ޥ����ղå�����
        01 TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
      *
      *    ��������ޥ����ղå�����
        01 TBL-TENSUPLUS-AREA.
          02 TBL-TENSUPLUS-REC         OCCURS  30.
           COPY    "CPTENSUPLUS.INC"   REPLACING
                                       //TNSP-// BY //TBL-TNSP-//.
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *    �ģ¸���
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
      *
      *    �ޥ������ռ�������
           COPY    "CPORCSMCNDATE.INC".
      *
      *****************************************************************
      *    Ϣ���ΰ�
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *    �������þ��󹹿�����
           COPY    "CPORCGZS02.INC".
      *
        01 LNK-TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC"   REPLACING
                                       //TNSP-// BY //LNK-TNSP-//.
      *
       PROCEDURE                                 DIVISION    USING
                                                 MCPAREA
                                                 SPAAREA
                                                 ORCGZS02AREA
                                                 LNK-TENSUPLUS-REC
           .
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                              SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-WORK            TO  SPA-Z01KYOUTU
           MOVE    SPA-FREE            TO  SPA-Z01FREE
      *
      *    ����������
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *    �����ޥ����ղþ���������ޥ������ϰϥ����å�
           PERFORM 100-TENSUPLUS-CHK-SEC
      *
      *    �����ޥ����ղþ��󹹿�
           PERFORM 200-TENSUPLUS-UPD-SEC
      *
           MOVE    SPA-AREA                      TO  SPA-COMMON
      *
           EXIT    PROGRAM
           .
       000-PROC-EXT.
           EXIT.
      *
      *****************************************************************
      *  �����ޥ����ղþ�������å�����
      *****************************************************************
       100-TENSUPLUS-CHK-SEC          SECTION.
      *
           INITIALIZE                      TBL-TENSUPLUS-AREA
           MOVE    ZERO                TO  IDX-H
      *
           INITIALIZE                      TENSUPLUS-REC
           MOVE    SPA-Z01-SRYCD       TO  TNSP-SRYCD
      *
           MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
           MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC               =   ZERO
               MOVE    "tbl_tensuplus"     TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-TENSUPLUS-READ-SEC
           ELSE
               MOVE    1               TO  FLG-TENSUPLUS
           END-IF
           PERFORM UNTIL   FLG-TENSUPLUS   =   1
               ADD     1                   TO  IDX-H
               MOVE    TENSUPLUS-REC       TO  TBL-TENSUPLUS-REC (IDX-H)
      *
               MOVE    "tbl_tensuplus"     TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-TENSUPLUS-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBLOSEC-SEC
      *
           IF      IDX-H               =   ZERO
               GO      TO      100-TENSUPLUS-CHK-EXT
           END-IF
      *
      *    �����ޥ����ȴ��֤��碌��
           MOVE    1                   TO  IDX1
           MOVE    1                   TO  IDX2
           PERFORM UNTIL     (IDX1     >   IDX-H  )
                        AND  (IDX2     >   SPA-Z01-MAX )
               IF       IDX2           >   SPA-Z01-MAX
      *             ���äΤߺ��
                       MOVE    TBL-TENSUPLUS-REC (IDX1)
                                                   TO  TENSUPLUS-REC
                       MOVE    TNSP-SRYCD          TO  TNSP-UP-SRYCD
                       MOVE    TNSP-YUKOSTYMD      TO  TNSP-UP-YUKOSTYMD
                       MOVE    TNSP-YUKOEDYMD      TO  TNSP-UP-YUKOEDYMD
                       MOVE    SPA-HOSPNUM         TO  TNSP-UP-HOSPNUM
                       MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
                       MOVE    "DBDELETE"          TO  MCP-FUNC
                       MOVE    "tbl_tensuplus"     TO  MCP-TABLE
                       MOVE    "key"               TO  MCP-PATHNAME
grpsys                 CALL    "ORCDBMAIN"         USING   MCPAREA
                                                           MCPDATA-REC
                                                           SPA-AREA
                    ADD     1               TO  IDX1
               ELSE
      *
               EVALUATE    TRUE
                   WHEN    (SPA-Z01-TYUKOSTYMD(IDX2)
                                       =   TBL-TNSP-YUKOSTYMD(IDX1)) AND
                           (SPA-Z01-TYUKOEDYMD(IDX2)
                                       =   TBL-TNSP-YUKOEDYMD(IDX1))
                       ADD     1               TO  IDX1
                       ADD     1               TO  IDX2
                   WHEN    (SPA-Z01-TYUKOSTYMD(IDX2)
                                       =   TBL-TNSP-YUKOSTYMD(IDX1)) AND
                           (SPA-Z01-TYUKOEDYMD(IDX2)
                                   NOT =   TBL-TNSP-YUKOEDYMD(IDX1))
      *                ��λ���ѹ�
                       MOVE    TBL-TENSUPLUS-REC (IDX1)
                                                   TO  TENSUPLUS-REC
                       MOVE    TNSP-SRYCD          TO  TNSP-UP-SRYCD
                       MOVE    TNSP-YUKOSTYMD      TO  TNSP-UP-YUKOSTYMD
                       MOVE    TNSP-YUKOEDYMD      TO  TNSP-UP-YUKOEDYMD
                       MOVE    SPA-HOSPNUM         TO  TNSP-UP-HOSPNUM
      *
                       MOVE    SPA-Z01-TYUKOEDYMD(IDX2)
                                                   TO  TNSP-YUKOEDYMD
                       MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
                       MOVE    "DBUPDATE"          TO  MCP-FUNC
                       MOVE    "tbl_tensuplus"     TO  MCP-TABLE
                       MOVE    "upd1"              TO  MCP-PATHNAME
grpsys                 CALL    "ORCDBMAIN"         USING   MCPAREA
                                                           MCPDATA-REC
                                                           SPA-AREA
                       IF      MCP-RC           NOT =   ZERO
                           MOVE    "9004"          TO  SPA-ERRCD
                           DISPLAY "Z03 TENSUPLUS UPD ERR:" MCP-RC
                                   ",KEY:" TNSP-KEY
                       END-IF
                       ADD     1               TO  IDX1
                       ADD     1               TO  IDX2
                   WHEN    (SPA-Z01-TYUKOSTYMD(IDX2)
                                       >   TBL-TNSP-YUKOSTYMD(IDX1)) AND
                           (TBL-TNSP-YUKOSTYMD(IDX1)
                                       NOT =   SPACE  )
      *                �ղþ���Τߤξ�硢���
                       MOVE    TBL-TENSUPLUS-REC (IDX1)
                                                   TO  TENSUPLUS-REC
                       MOVE    TNSP-SRYCD          TO  TNSP-UP-SRYCD
                       MOVE    TNSP-YUKOSTYMD      TO  TNSP-UP-YUKOSTYMD
                       MOVE    TNSP-YUKOEDYMD      TO  TNSP-UP-YUKOEDYMD
                       MOVE    SPA-HOSPNUM         TO  TNSP-UP-HOSPNUM
                       MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
                       MOVE    "DBDELETE"          TO  MCP-FUNC
                       MOVE    "tbl_tensuplus"     TO  MCP-TABLE
                       MOVE    "key"               TO  MCP-PATHNAME
grpsys                 CALL    "ORCDBMAIN"         USING   MCPAREA
                                                           MCPDATA-REC
                                                           SPA-AREA
                       ADD     1               TO  IDX1
                   WHEN    (SPA-Z01-TYUKOSTYMD(IDX2)
                                   NOT =   SPACE     )  AND
                           (SPA-Z01-TYUKOSTYMD(IDX2)
                                       <   TBL-TNSP-YUKOSTYMD(IDX1))
                       ADD     1               TO  IDX2
                   WHEN    OTHER
                       IF      (SPA-Z01-TYUKOSTYMD(IDX2)
                                           NOT =   SPACE   )
                           ADD     1               TO  IDX2
                       ELSE
                           IF    (TBL-TNSP-YUKOSTYMD(IDX1)
                                                 =   SPACE  )
                           ADD     1               TO  IDX1
                           END-IF
                       END-IF
               END-EVALUATE
      *
               END-IF
           END-PERFORM
      *
           .
       100-TENSUPLUS-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *  �����ޥ����ղþ��󹹿�����
      *****************************************************************
       200-TENSUPLUS-UPD-SEC            SECTION.
      *
      *    �����ޥ����ղþ��󸡺�
           IF      SPA-Z01-SELNUM      >   ZERO
               MOVE    SPACE               TO  TENSUPLUS-REC
               MOVE    SPA-Z01-SRYCD       TO  TNSP-SRYCD
               MOVE    SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM)
                                           TO  TNSP-YUKOSTYMD
               MOVE    SPA-Z01-TYUKOEDYMD(SPA-Z01-SELNUM)
                                           TO  TNSP-YUKOEDYMD
      *
               MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
               MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
               MOVE    "tbl_tensuplus"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC               =   ZERO
                   MOVE    "tbl_tensuplus"     TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 990-TENSUPLUS-READ-SEC
               ELSE
                   MOVE    1               TO  FLG-TENSUPLUS
               END-IF
               MOVE    "tbl_tensuplus"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-DBLOSEC-SEC
           ELSE
      *        ����
               MOVE    1                   TO  FLG-TENSUPLUS
           END-IF
      *
      *    �������ղþ�����Ͽ����Ƥ��ʤ��Ȥ��Ϻ������ʤ�
      *    �����å�
           IF    ( LNK-TNSP-SHONAME         =   SPACE  ) AND
                 ( LNK-TNSP-SAIKETUKBN      =   ZERO  ) AND
                 ( LNK-TNSP-ZENKNSKBN       =   ZERO  ) AND
                 ( LNK-TNSP-INPCHKKBN       =   ZERO  ) AND
                 ( LNK-TNSP-TENPUYAKKBN     =   SPACE ) AND
                 ( LNK-TNSP-ALLSURYOKBN     =   SPACE ) AND
                 ( LNK-TNSP-KANZANTANICD    =   SPACE ) AND
                 ( LNK-TNSP-KANZANTANINAME  =   SPACE ) AND
                 ( LNK-TNSP-KANZANCHI       =   ZERO  )
      *        ��²��
             AND ( LNK-TNSP-SANTEIRRKKBN    =   ZERO  )
             AND ( LNK-TNSP-JGNCNT          =   ZERO  )
             AND ( LNK-TNSP-JGNCNT1D        =   ZERO  )
             AND ( LNK-TNSP-JGNCNT1W        =   ZERO  )
             AND ( LNK-TNSP-JGNCNTERR       =   ZERO  )
      *        ����̾����
             AND ( LNK-TNSP-SOURYOHENKBN    =   ZERO  )
             AND ( LNK-TNSP-IPN-KISAIKBN    =   ZERO  )
             AND ( LNK-TNSP-IPN-KANZANTANICD    =   SPACE )
             AND ( LNK-TNSP-IPN-KANZANCHI   =   ZERO  )
               IF      FLG-TENSUPLUS    =  1
                   GO  TO  200-TENSUPLUS-UPD-EXT
               END-IF
           END-IF
      *
      *    ͭ�����¹���
           IF      FLG-TENSUPLUS       =   ZERO
               EVALUATE    SPA-Z01-SYORIKBN
                   WHEN    3
      *                ����������
                       MOVE    ZS02-YUKOEDYMD      TO  TNSP-YUKOEDYMD
      *
                       MOVE    SPA-Z01-SRYCD       TO  TNSP-UP-SRYCD
                       MOVE    SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM)
                                                   TO  TNSP-UP-YUKOSTYMD
                       MOVE    SPA-Z01-TYUKOEDYMD(SPA-Z01-SELNUM)
                                                   TO  TNSP-UP-YUKOEDYMD
                       MOVE    SPA-HOSPNUM         TO  TNSP-UP-HOSPNUM
      *
                       MOVE    SPA-OPID            TO  TNSP-OPID
                       MOVE    SMCNDATE-YMD        TO  TNSP-UPYMD
                       MOVE    SMCNDATE-HMS        TO  TNSP-UPHMS
      *
                       MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
                       MOVE    "DBUPDATE"          TO  MCP-FUNC
                       MOVE    "tbl_tensuplus"     TO  MCP-TABLE
                       MOVE    "upd1"              TO  MCP-PATHNAME
grpsys                 CALL    "ORCDBMAIN"         USING   MCPAREA
                                                           MCPDATA-REC
                                                           SPA-AREA
                       IF      MCP-RC           NOT =   ZERO
                           MOVE    "9004"          TO  SPA-ERRCD
                           DISPLAY "Z02 TENSUPLUS UPD ERR:" MCP-RC
                                   ",KEY:" TNSP-KEY
                           GO      TO      200-TENSUPLUS-UPD-EXT
                       END-IF
      *
                   WHEN    1
                   WHEN    2
      *                ����������
                       MOVE    ZS02-NAI-YUKOSTYMD   TO  TNSP-YUKOSTYMD
                       MOVE    ZS02-NAI-YUKOEDYMD   TO  TNSP-YUKOEDYMD
                       PERFORM 2001-TENSUPLUS-HEN-SEC
      *
                       MOVE    SPA-Z01-SRYCD       TO  TNSP-UP-SRYCD
                       MOVE    SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM)
                                                   TO  TNSP-UP-YUKOSTYMD
                       MOVE    SPA-Z01-TYUKOEDYMD(SPA-Z01-SELNUM)
                                                   TO  TNSP-UP-YUKOEDYMD
                       MOVE    SPA-HOSPNUM         TO  TNSP-UP-HOSPNUM
      *
                       MOVE    SPA-OPID            TO  TNSP-OPID
                       MOVE    SMCNDATE-YMD        TO  TNSP-UPYMD
                       MOVE    SMCNDATE-HMS        TO  TNSP-UPHMS
      *
                       MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
                       MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
                       MOVE    "DBUPDATE"          TO  MCP-FUNC
                       MOVE    "tbl_tensuplus"     TO  MCP-TABLE
                       MOVE    "upd1"              TO  MCP-PATHNAME
grpsys                 CALL    "ORCDBMAIN"         USING   MCPAREA
                                                           MCPDATA-REC
                                                           SPA-AREA
                       IF      MCP-RC          NOT =   ZERO
                           MOVE    "9004"          TO  SPA-ERRCD
                           DISPLAY "Z02 TENSUPLUS UPD ERR:" MCP-RC
                                   ",KEY:" TNSP-KEY
                           GO      TO      200-TENSUPLUS-UPD-EXT
                       END-IF
               END-EVALUATE
           END-IF
      *
      *    �����ɲ�
           IF     (FLG-TENSUPLUS       =   1   )  OR 
                  (SPA-Z01-SYORIKBN    =   3    )
               INITIALIZE                      TENSUPLUS-REC
               MOVE    SPA-Z01-SRYCD       TO  TNSP-SRYCD
               MOVE    ZS02-NAI-YUKOSTYMD  TO  TNSP-YUKOSTYMD
               MOVE    ZS02-NAI-YUKOEDYMD  TO  TNSP-YUKOEDYMD
               PERFORM 2001-TENSUPLUS-HEN-SEC
      *
               MOVE    SPA-OPID            TO  TNSP-OPID
               MOVE    SMCNDATE-YMD        TO  TNSP-CREYMD
               MOVE    SMCNDATE-YMD        TO  TNSP-UPYMD
               MOVE    SMCNDATE-HMS        TO  TNSP-UPHMS
      *
               MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
               MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_tensuplus"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC              NOT =   ZERO
                   MOVE    "9004"          TO  SPA-ERRCD
                   DISPLAY "Z02 TENSUPLUS INS ERR:" MCP-RC
                          ",KEY:" TNSP-KEY
               END-IF
           END-IF
      *
           .
       200-TENSUPLUS-UPD-EXT.
           EXIT.
      *****************************************************************
      *    �����ޥ����ղþ����Խ�
      *****************************************************************
       2001-TENSUPLUS-HEN-SEC          SECTION.
      *
           MOVE    LNK-TNSP-INPCHKKBN      TO  TNSP-INPCHKKBN
      *    �η�����ʬ
           MOVE    LNK-TNSP-SAIKETUKBN     TO  TNSP-SAIKETUKBN
      *    ���󸡺�����ʬ
           MOVE    LNK-TNSP-ZENKNSKBN      TO  TNSP-ZENKNSKBN
      *    ����̾��
           MOVE    LNK-TNSP-SHONAME        TO  TNSP-SHONAME
           MOVE    LNK-TNSP-SHOYUKOKETA    TO  TNSP-SHOYUKOKETA
      *    ʬ���ʬ
           MOVE    LNK-TNSP-TENPUYAKKBN    TO  TNSP-TENPUYAKKBN
      *    �����̵��ܶ�ʬ
           MOVE    LNK-TNSP-ALLSURYOKBN    TO  TNSP-ALLSURYOKBN
      *    ���̴���ñ�̥�����
           MOVE    LNK-TNSP-KANZANTANICD   TO  TNSP-KANZANTANICD
      *    ���̴���ñ��̾��
           MOVE    LNK-TNSP-KANZANTANINAME TO  TNSP-KANZANTANINAME
      *    ���̴�����
           MOVE    LNK-TNSP-KANZANCHI      TO  TNSP-KANZANCHI
      *    ���������ʬ
           MOVE    LNK-TNSP-SANTEIRRKKBN   TO  TNSP-SANTEIRRKKBN
      *    ���²��
           MOVE    LNK-TNSP-JGNCNT         TO  TNSP-JGNCNT
      *    ����²��
           MOVE    LNK-TNSP-JGNCNT1D       TO  TNSP-JGNCNT1D
      *    ����²��
           MOVE    LNK-TNSP-JGNCNT1W       TO  TNSP-JGNCNT1W
      *    ���顼����
           MOVE    LNK-TNSP-JGNCNTERR      TO  TNSP-JGNCNTERR
      *    �����Խ���ʬ
           MOVE    LNK-TNSP-SOURYOHENKBN   TO  TNSP-SOURYOHENKBN
      *    ����̾���ܶ�ʬ
           MOVE    LNK-TNSP-IPN-KISAIKBN   TO  TNSP-IPN-KISAIKBN
      *    ����̾����ñ��
           MOVE    LNK-TNSP-IPN-KANZANTANICD
                                           TO  TNSP-IPN-KANZANTANICD
      *    ����̾����ñ��̾
           MOVE    LNK-TNSP-IPN-KANZANTANINAME
                                           TO  TNSP-IPN-KANZANTANINAME
      *    ����̾���ܴ�����
           MOVE    LNK-TNSP-IPN-KANZANCHI  TO  TNSP-IPN-KANZANCHI
      *    ¾���
           MOVE    LNK-TNSP-JGNCNTETCM     TO  TNSP-JGNCNTETCM
      *    ¾���
           MOVE    LNK-TNSP-JGNCNTETC      TO  TNSP-JGNCNTETC
      *
           .
       2001-TENSUPLUS-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ޥ����ղþ����ɹ�
      *****************************************************************
       990-TENSUPLUS-READ-SEC          SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  TENSUPLUS-REC
               MOVE    ZERO            TO  FLG-TENSUPLUS
           ELSE
               INITIALIZE                  TENSUPLUS-REC
               MOVE    1               TO  FLG-TENSUPLUS
           END-IF
           .
      *
       990-TENSUPLUS-READ-EXT.
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
      *****************************************************************
      *   �ģ¥�����������
      *****************************************************************
       990-DBLOSEC-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       990-DBLOSEC-EXT.
           EXIT.
      *
      *