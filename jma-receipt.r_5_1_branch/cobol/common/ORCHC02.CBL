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
       PROGRAM-ID.             ORCHC02.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ���Ź԰�
      *  ����ݡ��ͥ��̾  : ����䵡������ʣ�����
      *                      ��ORCHC02Q �� QR����������
      *                      �����Уǡ��ã��̣���
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  16/02/17    NACL��¿��Ǽ  ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
      *    ����䵶��̥ѥ�᥿
           COPY    "CPORCHC19.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCHC19AREA
           .
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
      *
           EVALUATE    TRUE
               WHEN    SPA-SRYYMD          <   "20120401"
      *        ʿ������ǯ����ޤ�
                   CALL    "ORCHC02V01"        USING   SPA-AREA
                                                       ORCHC19AREA
      *
               WHEN   (SPA-SRYYMD          >=  "20120401" )  AND
                      (SPA-SRYYMD          <   "20160401" )
      *        ʿ������ǯ�����ʿ������ǯ����ޤ�
                   CALL    "ORCHC02V02"        USING   SPA-AREA
                                                       ORCHC19AREA
      *
               WHEN   (SPA-SRYYMD          >=  "20160401" )   AND
                      (SPA-SRYYMD          <   "20180401" )
      *        ʿ������ǯ�����
                   CALL    "ORCHC02V03"        USING   SPA-AREA
                                                       ORCHC19AREA
      *
               WHEN   (SPA-SRYYMD          >=  "20180401" )
                 AND  (SPA-SRYYMD          <   "20200501" )
      *        ʿ������ǯ�����
                   CALL    "ORCHC02V04"        USING   SPA-AREA
                                                       ORCHC19AREA
      *
               WHEN   (SPA-SRYYMD          >=  "20200501" )
                 AND  (SPA-SRYYMD          <   "99999999" )
      *        ���£���ǯ�����ʻ����б���
                   CALL    "ORCHC02V05"        USING   SPA-AREA
                                                       ORCHC19AREA
           END-EVALUATE
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *